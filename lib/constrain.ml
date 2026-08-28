(** Constraint generation.

    One walk over the expression: names are resolved as they are met and
    constraints are stated as they are found — {!Hmx_solver} unifies eagerly,
    {!Hmx_null} collects — so there is no intermediate tree and no constraint
    language of our own to carry around. This is how Inferno's own client is
    written: [Infer.hastype] resolves and constrains in a single pass. *)

open Hmx_lattice

type env = { nulls : Hmx_null.state; scope : Resolve.env }

let env scope = { nulls = Hmx_null.create (); scope }

(** The parameter tree, in source order because parameters are bound
    positionally. It mirrors {!Sql.var}, but holds variables instead of types:
    the shapes are known while walking, the types only after solving. *)
type pvar =
  | PSingle of { id : Sql.param_id; ty : Hmx_solver.var; null : Hmx_null.t;
                 meta : Sql.Meta.t; in_list : bool }
  | PChoice of Sql.param_id * (Sql.param_id * pvar list option) list
  | PChoiceIn of { id : Sql.param_id; kind : Sql.in_or_not_in; vars : pvar list }
  | POption of { id : Sql.param_id; vars : pvar list;
                 pos : Sql.pos * Sql.pos; kind : Sql.option_actions_kind }
  | PTuple of { id : Sql.param_id;
                items : (Hmx_solver.var * Hmx_null.t * Sql.Meta.t) list;
                kind : Sql.in_or_not_in; pos : Sql.pos }
  | PReady of Sql.var   (** already built elsewhere: a subquery's own parameters *)

type t = { ty : Hmx_solver.var; null : Hmx_null.t; vars : pvar list }

let vars_of rs = List.concat_map (fun r -> r.vars) rs
let bool = Refined.of_base Base.Bool
let boolean () = Hmx_solver.at_least bool

let split (t : Resolve.ty) =
  (match t.base with Some r -> Hmx_solver.declared r | None -> Hmx_solver.fresh ()),
  (match t.null with Some n -> Hmx_null.const n | None -> Hmx_null.fresh ())

let rec gen env (e : Sql.expr) : t =
  match e with
  (* a literal is a constant: nothing said about its nullability means it is
     not null, not that it is unknown *)
  | Value v ->
    let t = Resolve.ty_of_sql v.collated in
    let ty, _ = split t in
    { ty; null = Hmx_null.const (match t.null with Some n -> n | None -> Null.NotNull); vars = [] }
  | Column col ->
    let ty, null = split (Resolve.apply_json_meta (Resolve.lookup_column env.scope col.collated)) in
    { ty; null; vars = [] }
  | Param (p, meta) -> param ~in_list:false p meta
  | Inparam (p, meta) -> param ~in_list:true p meta
  | Of_values col -> let ty, null = split (env.scope.of_values col) in { ty; null; vars = [] }
  | SelectExpr (select, usage) ->
    let t, vars = env.scope.subquery select usage in
    let vars = List.map (fun v -> PReady v) vars in
    (match usage with
     | `AsValue -> let ty, null = split t in { ty; null; vars }
     | `Exists -> { ty = boolean (); null = Hmx_null.const Null.NotNull; vars })
  | InChoice (id, kind, e) ->
    let r = gen env e in
    { r with vars = [ PChoiceIn { id; kind; vars = r.vars } ] }
  | OptionActions { choice; pos; kind } ->
    let id =
      match Resolve.choice_id choice with
      | Some id -> id
      | None -> conflict "an option block must switch on a parameter; use a plain choice otherwise"
    in
    let r = gen env choice in
    Hmx_solver.below r.ty bool;
    { ty = boolean (); null = r.null; vars = [ POption { id; vars = r.vars; pos; kind } ] }
  | InTupleList { value = { exprs; param_id; kind_in_tuple_list }; pos } ->
    if List.exists (function
      | Sql.Choices _ | InChoice _ | InTupleList _ | OptionActions _ -> true
      | Value _ | Column _ | Param _ | Inparam _ | Fun _ | SelectExpr _ | Case _ | Of_values _ -> false)
      exprs
    then conflict "unsupported expression kind for WHERE e IN @tuplelist";
    (* a column on the left carries its metadata into the tuple list *)
    let items = List.map (fun e ->
      let r = gen env e in
      let meta = match e with
        | Sql.Column col -> (Resolve.lookup_column env.scope col.collated).meta
        | _ -> Sql.Meta.empty ()
      in
      r.ty, r.null, meta) exprs
    in
    { ty = boolean (); null = Hmx_null.const Null.NotNull;
      vars = [ PTuple { id = param_id; items; kind = kind_in_tuple_list; pos } ] }
  | Choices (id, l) ->
    (* alternatives, so the result is the least type above every branch *)
    let branches = List.map (fun (n, e) -> n, Option.map (gen env) e) l in
    let rs = List.filter_map snd branches in
    let a = Hmx_solver.fresh () and n = Hmx_null.fresh () in
    List.iter (fun r -> Hmx_solver.same r.ty a) rs;
    Hmx_null.add env.nulls (Join (n, List.map (fun r -> r.null) rs));
    { ty = a; null = n;
      vars = [ PChoice (id, List.map (fun (n, r) -> n, Option.map (fun r -> r.vars) r) branches) ] }
  | Case { case; branches; else_ } -> gen_case env case branches else_
  | Fun { fn_name; kind; parameters; over } ->
    let sg =
      match Hmx_of_sql.of_func ~arity:(List.length parameters) kind with
      | Ok sg -> sg
      | Error msg -> conflict "%s: %s" fn_name msg
    in
    let order = match kind with Agg (With_order { order; _ }) -> order | _ -> [] in
    gen_call env ~name:fn_name ~sg ~args:parameters ~order
      ~guaranteed_row:(env.scope.grouping || Sql.over_has_a_row over)

and param ~in_list (p : Sql.Source_type.t Sql.param) meta =
  let ty, null = split (Resolve.ty_of_source p.typ) in
  { ty; null; vars = [ PSingle { id = p.id; ty; null; meta; in_list } ] }

and gen_case env scrutinee branches else_ =
  let head = Option.map (gen env) scrutinee in
  let whens = List.map (fun (b : Sql.case_branch) -> gen env b.when_) branches in
  let thens = List.map (fun (b : Sql.case_branch) -> gen env b.then_) branches in
  let else_r = Option.map (gen env) else_ in
  (* with a scrutinee the WHENs are compared to it, without one they are
     conditions *)
  (match head with
   | Some h ->
     let w = Hmx_solver.fresh () in
     Hmx_solver.same h.ty w;
     List.iter (fun r -> Hmx_solver.same r.ty w) whens
   | None -> List.iter (fun r -> Hmx_solver.below r.ty bool) whens);
  let some = function Some r -> [ r ] | None -> [] in
  let results = thens @ some else_r in
  let a = Hmx_solver.fresh () and n = Hmx_null.fresh () in
  List.iter (fun r -> Hmx_solver.same r.ty a) results;
  (* a CASE with no ELSE falls through to NULL *)
  Hmx_null.add env.nulls
    (match else_r with
     | None -> Eq (n, Hmx_null.const Null.Nullable)
     | Some _ -> Join (n, List.map (fun r -> r.null) results));
  { ty = a; null = n; vars = vars_of (some head @ whens @ thens @ some else_r) }

and gen_call env ~name ~sg ~args ~order ~guaranteed_row =
  let rs = List.map (gen env) args in
  let sch =
    match Hmx_sig.instantiate sg (List.length args) with
    | Ok sch -> sch
    | Error e -> conflict "%s: %s" name e
  in
  let shared = lazy (Hmx_solver.fresh ()) in
  List.iter2 (fun r formal ->
    match (formal : Hmx_sig.param_spec) with
    | Same -> Hmx_solver.same r.ty (Lazy.force shared)
    | As t -> Hmx_solver.below r.ty t
    | Free -> ())
    rs sch.formals;
  List.iter (fun p -> Hmx_solver.has (Lazy.force shared) p) sch.preds;
  let result =
    match sch.result with Ret_same -> Lazy.force shared | Ret t -> Hmx_solver.at_least t
  in
  let undeclared_param = function
    | Sql.Param ({ typ = { nullability = Depends; _ }; _ }, _)
    | Sql.Inparam ({ typ = { nullability = Depends; _ }; _ }, _) -> true
    | _ -> false
  in
  (* The whole parameter-nullability story, in one rule: a parameter that
     shares the scheme variable stands beside its siblings, so it is as nullable
     as they are. With no siblings the join is empty, which is NOT NULL. A
     comparison is the exception — NULL never matches there, so offering a
     nullable parameter would be useless. *)
  if not sg.compares then begin
    let indexed = List.mapi (fun i (arg, r) -> i, arg, r) (List.combine args rs) in
    List.iter (fun (i, arg, r) ->
      if List.nth sch.same_at i && undeclared_param arg then
        match List.filter_map (fun (j, _, r') ->
          if j <> i && List.nth sch.same_at j then Some r'.null else None) indexed with
        | [] -> ()
        | siblings -> Hmx_null.add env.nulls (Join (r.null, siblings)))
      indexed
  end;
  let nulls = List.map (fun r -> r.null) rs in
  let ret_null =
    match sch.result_null with
    | Hmx_sig.Join -> let n = Hmx_null.fresh () in Hmx_null.add env.nulls (Join (n, nulls)); n
    | Hmx_sig.Meet -> let n = Hmx_null.fresh () in Hmx_null.add env.nulls (Meet (n, nulls)); n
    | Hmx_sig.Const v -> Hmx_null.const v
    (* an aggregate over an empty group yields NULL, so a strict argument only
       survives when the group is known to have a row *)
    | Hmx_sig.Group_join ->
      if guaranteed_row then
        let n = Hmx_null.fresh () in Hmx_null.add env.nulls (Join (n, nulls)); n
      else Hmx_null.const Null.Nullable
    (* SET col = e: the result is the column, and the argument must fit it *)
    | Hmx_sig.Assign ->
      (match nulls with
       | col :: _ -> Hmx_null.add env.nulls (Join (col, nulls)); col
       | [] -> Hmx_null.fresh ())
  in
  (* the aggregate's own ORDER BY carries parameters, bound after the arguments *)
  let order = List.concat_map (fun (e, dir) ->
    (gen env e).vars @ (match dir with
      | Some (`Param p) ->
        [ PReady (Sql.Choice (p, [ Verbatim ("ASC", "ASC"); Verbatim ("DESC", "DESC") ])) ]
      | None | Some `Fixed -> []))
    order
  in
  { ty = result; null = ret_null; vars = vars_of rs @ order }

(** Turn the parameter tree into {!Sql.var}s once the types are known. *)
let rec to_var read = function
  | PSingle { id; ty; null; meta; in_list } ->
    let p = { Sql.id; typ = read ty null } in
    if in_list then Sql.SingleIn (p, meta) else Sql.Single (p, meta)
  | PChoice (id, branches) ->
    Sql.Choice (id, List.map (fun (n, vars) ->
      Sql.Simple (n, Option.map (List.map (to_var read)) vars)) branches)
  | PChoiceIn { id; kind; vars } ->
    Sql.ChoiceIn { param = id; kind; vars = List.map (to_var read) vars }
  | POption { id; vars; pos; kind } ->
    Sql.OptionActionChoice (id, List.map (to_var read) vars, pos, kind)
  | PTuple { id; items; kind; pos } ->
    Sql.TupleList (id, Where_in { value = (List.map (fun (ty, null, meta) -> read ty null, meta) items,
                                          kind); pos })
  | PReady v -> v

(** Walk, solve, and read everything back as declared types: the shape the rest
    of the compiler still speaks. *)
let solve_expr ?fallback scope e =
  match
    let env = env scope in
    let r = gen env e in
    Hmx_null.solve env.nulls;
    let read ty null =
      Hmx_of_sql.to_type (Hmx_solver.resolve ?fallback ty) (Hmx_null.get null)
    in
    read r.ty r.null, List.map (to_var read) r.vars
  with
  | result -> Ok result
  | exception Conflict msg -> Error msg

(** just the type, for tests and for anything that does not need parameters *)
let infer ?fallback scope e =
  match solve_expr ?fallback scope e with Ok (ty, _) -> Ok ty | Error e -> Error e
