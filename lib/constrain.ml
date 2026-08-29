(** Constraint generation.

    One walk over the expression: names are resolved as they are met and
    constraints are stated as they are found — {!Hmx_solver} unifies eagerly,
    {!Hmx_null} collects — so there is no intermediate tree and no constraint
    language of our own to carry around. This is how Inferno's own client is
    written: [Infer.hastype] resolves and constrains in a single pass.

    Metadata rides on the same walk. It flows along the shared variable of a
    signature — the argument positions whose value can reach the result
    unchanged — upward as what an expression {e says}, and downward as the
    context a parameter ends up carrying. *)

open Hmx_lattice
module Meta = Sql.Meta

type env = { nulls : Hmx_null.state; scope : Resolve.env }

let env scope = { nulls = Hmx_null.create (); scope }

(** The parameter tree, in source order because parameters are bound
    positionally. It mirrors {!Sql.var}, but holds variables instead of types:
    the shapes are known while walking, the types only after solving. The
    metadata is a cell for the same reason: it comes down from the context
    once the whole expression has been seen. *)
type pvar =
  | PSingle of { id : Sql.param_id; ty : Hmx_solver.var; null : Hmx_null.t;
                 meta : Meta.t ref; in_list : bool }
  | PChoice of Sql.param_id * (Sql.param_id * pvar list option) list
  | PChoiceIn of { id : Sql.param_id; kind : Sql.in_or_not_in; vars : pvar list }
  | POption of { id : Sql.param_id; vars : pvar list;
                 pos : Sql.pos * Sql.pos; kind : Sql.option_actions_kind }
  | PTuple of { id : Sql.param_id;
                items : (Hmx_solver.var * Hmx_null.t * Meta.t) list;
                kind : Sql.in_or_not_in; pos : Sql.pos }
  | PReady of Sql.var   (** already built elsewhere: a subquery's own parameters *)

(** An expression under inference.

    [own] is the metadata it contributes upward — [None] when it says nothing,
    which is different from saying "nothing is known". [push] takes the
    metadata coming down from the context and settles it on the parameters
    underneath; it runs once, after the walk. *)
type t = {
  ty : Hmx_solver.var;
  null : Hmx_null.t;
  vars : pvar list;
  own : Meta.t option;
  push : Meta.t -> unit;
}

let vars_of rs = List.concat_map (fun r -> r.vars) rs
let bool = Refined.of_base Base.Bool
let boolean () = Hmx_solver.at_least bool

(** a declared type as variables: what is not written is a fresh variable *)
let split (t : Sql.Type.t) =
  let base, null = Hmx_of_sql.of_type t in
  (match base with Some r -> Hmx_solver.declared r | None -> Hmx_solver.fresh ()),
  (match null with Some n -> Hmx_null.const n | None -> Hmx_null.fresh ())

(* ------------------------------------------------------------- metadata *)

(** what a set of alternatives agrees on *)
let agreed = function
  | [] -> Some (Meta.empty ())
  | l -> Meta.common_all (List.map (fun r -> r.own) l)

let sealed l = Meta.of_option (agreed l)

(** no context reaches these: their parameters only see their siblings *)
let settle rs = List.iter (fun r -> r.push (Meta.empty ())) rs

(** says nothing and passes nothing: a literal *)
let silent ~ty ~null = { ty; null; vars = []; own = None; push = ignore }

(** contributes nothing and lets nothing through *)
let opaque ~ty ~null children =
  { ty; null; vars = vars_of children; own = Some (Meta.empty ()); push = (fun _ -> settle children) }

(** a node that passes its context down to the branches it shares a type with *)
let node ~ty ~null ~vars same_domain push_children =
  let own = agreed same_domain in
  { ty; null; vars; own;
    push = (fun ctx -> push_children (Meta.of_option (Meta.common (Meta.declared ctx) own))) }

(* ---------------------------------------------------------------- walk *)

let rec gen env (e : Sql.expr) : t =
  match e with
  (* a literal is a constant: nothing said about its nullability means it is
     not null, not that it is unknown *)
  | Value v ->
    let ty, _ = split v.collated in
    silent ~ty ~null:(Hmx_null.const (Sql.Type.is_nullable v.collated))
  | Column col ->
    let c = env.scope.column col.collated in
    let ty, null = split (Resolve.apply_json_meta c) in
    { (silent ~ty ~null) with own = Meta.declared c.meta }
  | Param (p, meta) -> param ~in_list:false p meta
  | Inparam (p, meta) -> param ~in_list:true p meta
  | Of_values col -> let ty, null = split (env.scope.of_values col) in opaque ~ty ~null []
  | SelectExpr (select, usage) ->
    let t, vars, meta = env.scope.subquery select usage in
    let vars = List.map (fun v -> PReady v) vars in
    let ty, null =
      match usage with
      | `AsValue -> split t
      | `Exists -> boolean (), Hmx_null.const false
    in
    { ty; null; vars; own = Some meta; push = ignore }
  | InChoice (id, kind, e) ->
    let r = gen env e in
    node ~ty:r.ty ~null:r.null ~vars:[ PChoiceIn { id; kind; vars = r.vars } ] [ r ] r.push
  | OptionActions { choice; pos; kind } ->
    let id =
      match Resolve.choice_id choice with
      | Some id -> id
      | None -> conflict "an option block must switch on a parameter; use a plain choice otherwise"
    in
    let r = gen env choice in
    Hmx_solver.below r.ty bool;
    node ~ty:(boolean ()) ~null:r.null ~vars:[ POption { id; vars = r.vars; pos; kind } ] [ r ] r.push
  | InTupleList { value = { exprs; param_id; kind_in_tuple_list }; pos } ->
    if List.exists (function
      | Sql.Choices _ | InChoice _ | InTupleList _ | OptionActions _ -> true
      | Value _ | Column _ | Param _ | Inparam _ | Fun _ | SelectExpr _ | Case _ | Of_values _ -> false)
      exprs
    then conflict "unsupported expression kind for WHERE e IN @tuplelist";
    (* a column on the left carries its metadata into the tuple list *)
    let rs = List.map (gen env) exprs in
    let items = List.map2 (fun e r ->
      let meta = match e with
        | Sql.Column col -> (env.scope.column col.collated).meta
        | _ -> Meta.empty ()
      in
      r.ty, r.null, meta) exprs rs
    in
    { (opaque ~ty:(boolean ()) ~null:(Hmx_null.const false) rs) with
      vars = [ PTuple { id = param_id; items; kind = kind_in_tuple_list; pos } ] }
  | Choices (id, l) ->
    (* alternatives, so the result is the least type above every branch *)
    let branches = List.map (fun (n, e) -> n, Option.map (gen env) e) l in
    let rs = List.filter_map snd branches in
    let a = Hmx_solver.fresh () and n = Hmx_null.fresh () in
    List.iter (fun r -> Hmx_solver.same r.ty a) rs;
    Hmx_null.add env.nulls (Join (n, List.map (fun r -> r.null) rs));
    node ~ty:a ~null:n
      ~vars:[ PChoice (id, List.map (fun (n, r) -> n, Option.map (fun r -> r.vars) r) branches) ]
      rs (fun ctx -> List.iter (fun r -> r.push ctx) rs)
  | Case { case; branches; else_ } -> gen_case env case branches else_
  | Fun { fn_name; kind; parameters; over } ->
    let arity = List.length parameters in
    (* §6: the mode is an index on the judgment, not a type, so this is decided
       here and never reaches the solver *)
    if Hmx_sig.is_agg fn_name arity && not env.scope.allow_aggregates then
      conflict "%s is an aggregate and cannot appear here" fn_name;
    let sg =
      match kind with
      (* the one function whose type is not in its name *)
      | Sql.Cast t ->
        let t = Sql.Source_type.to_infer_type t in
        Hmx_sig.make ?ret:(Hmx_of_sql.of_kind t.t) (Args [ Free ])
      | Named | Agg_order _ ->
        match Hmx_sig.find fn_name arity with
        | Some sg -> sg
        (* an unknown function is accepted as untyped, as it always was *)
        | None -> Hmx_sig.make (Varargs { head = []; tail = [ Free ] })
    in
    let order = match kind with Agg_order { order; _ } -> order | Named | Cast _ -> [] in
    (* an aggregate takes a group to a value, so its argument is per-row again *)
    let inner =
      if Hmx_sig.is_agg fn_name arity
      then { env with scope = { env.scope with allow_aggregates = false } }
      else env
    in
    (* INSERT .. SELECT: the target column's metadata reaches the expression
       through the cast that fits it to the column *)
    let through = (match kind with Cast _ -> true | Named | Agg_order _ -> false)
                  && String.equal fn_name "insert_select" in
    gen_call inner ~name:fn_name ~sg ~through ~args:parameters ~order
      ~guaranteed_row:(env.scope.grouping || Sql.over_has_a_row over)

and param ~in_list (p : Sql.Source_type.t Sql.param) meta =
  let ty, null = split (Sql.Source_type.to_infer_type p.typ) in
  let meta = ref meta in
  { ty; null; vars = [ PSingle { id = p.id; ty; null; meta; in_list } ];
    own = None; push = (fun ctx -> meta := Meta.merge_right ctx !meta) }

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
     | None -> Eq (n, Hmx_null.const true)
     | Some _ -> Join (n, List.map (fun r -> r.null) results));
  (* the conditions are tested, not returned, so only the results share the
     node's metadata *)
  node ~ty:a ~null:n ~vars:(vars_of (some head @ whens @ thens @ some else_r)) results
    (fun ctx -> settle (some head @ whens); List.iter (fun r -> r.push ctx) results)

and gen_call env ~name ~sg ~through ~args ~order ~guaranteed_row =
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
    match sch.result with None -> Lazy.force shared | Some t -> Hmx_solver.at_least t
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
      else Hmx_null.const true
    (* SET col = e: the result is the column, and the argument must fit it *)
    | Hmx_sig.Assign ->
      (match nulls with
       | col :: _ -> Hmx_null.add env.nulls (Join (col, nulls)); col
       | [] -> Hmx_null.fresh ())
  in
  (* the aggregate's own ORDER BY carries parameters, bound after the arguments *)
  let ordered = List.map (fun (e, dir) -> gen env e, dir) order in
  let order_vars = List.concat_map (fun (r, dir) ->
    r.vars @ (match dir with
      | Some (`Param p) ->
        [ PReady (Sql.Choice (p, [ Verbatim ("ASC", "ASC"); Verbatim ("DESC", "DESC") ])) ]
      | None | Some `Fixed -> []))
    ordered
  in
  (* the arguments sharing the scheme variable are the ones whose value can
     reach the result unchanged, so metadata travels between them and, when
     the result is that variable, through to the context *)
  let same_at = if through then List.map (fun _ -> true) rs else sch.same_at in
  let returns_shared = through || sch.result = None in
  let args = List.combine same_at rs in
  let shared = List.filter_map (fun (same, r) -> if same then Some r else None) args in
  node ~ty:result ~null:ret_null ~vars:(vars_of rs @ order_vars)
    (if returns_shared then shared else [])
    (fun ctx ->
      settle (List.map fst ordered);
      List.iter (fun (same, r) ->
        r.push (if not same then Meta.empty () else if returns_shared then ctx else sealed shared))
        args)

(** Turn the parameter tree into {!Sql.var}s once the types are known. *)
let rec to_var read = function
  | PSingle { id; ty; null; meta; in_list } ->
    let p = { Sql.id; typ = read ty null } in
    if in_list then Sql.SingleIn (p, !meta) else Sql.Single (p, !meta)
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
    of the compiler still speaks. [ctx] is the metadata the surrounding
    context hands down — the column being assigned to, say. Returns the type,
    the parameters, and the metadata the expression carries. *)
let solve_expr ?(ctx = Meta.empty ()) ?fallback scope e =
  try
    let env = env scope in
    let r = gen env e in
    r.push ctx;
    Hmx_null.solve env.nulls;
    let read ty null =
      Hmx_of_sql.to_type (Hmx_solver.resolve ?fallback ty) (Hmx_null.get null)
    in
    Ok (read r.ty r.null, List.map (to_var read) r.vars, Meta.of_option r.own)
  with Conflict msg -> Error msg

(** just the type, for tests and for anything that does not need parameters *)
let infer ?fallback scope e =
  match solve_expr ?fallback scope e with Ok (ty, _, _) -> Ok ty | Error e -> Error e
