(** Stage 2: constraint generation.

    One recursion per node, producing a type variable, a nullability and the
    parameters found along the way. Constraints are stated as they are met —
    {!Hmx_solver} unifies eagerly and {!Hmx_null} collects — so there is no
    constraint language of our own to carry around. *)

open Hmx_lattice

type env = { nulls : Hmx_null.state }

let env () = { nulls = Hmx_null.create () }

(** The parameter tree, in source order because parameters are bound
    positionally. It mirrors {!Sql.var}, but holds variables instead of types:
    the shapes are known during generation, the types only after solving. *)
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
let bool () = Hmx_solver.at_least (Refined.of_base Base.Bool)

let split env (t : Resolved.ty) =
  (match t.base with Some r -> Hmx_solver.declared r | None -> Hmx_solver.fresh ()),
  (match t.null with Some n -> Hmx_null.N n | None -> Hmx_null.fresh env.nulls)

let rec gen env (e : Resolved.expr) : t =
  match e with
  (* a literal is a constant: nothing said about its nullability means it is
     not null, not that it is unknown *)
  | Lit t ->
    let ty, _ = split env t in
    { ty; null = Hmx_null.N (match t.null with Some n -> n | None -> Null.NotNull); vars = [] }
  | Col (_, t) -> let ty, null = split env t in { ty; null; vars = [] }
  | Param p ->
    let ty, null = split env p.ty in
    { ty; null; vars = [ PSingle { id = p.id; ty; null; meta = p.meta; in_list = p.in_list } ] }
  | Subquery { ty; kind = `AsValue; vars } ->
    let ty, null = split env ty in
    { ty; null; vars = List.map (fun v -> PReady v) vars }
  | Subquery { kind = `Exists; vars; _ } ->
    { ty = bool (); null = Hmx_null.N Null.NotNull; vars = List.map (fun v -> PReady v) vars }
  | InTupleList { id; items; kind; pos } ->
    let rs = List.map (fun (e, _) -> gen env e) items in
    let items = List.map2 (fun r (_, meta) -> r.ty, r.null, meta) rs items in
    { ty = bool (); null = Hmx_null.N Null.NotNull; vars = [ PTuple { id; items; kind; pos } ] }
  | InChoice { id; kind; expr } ->
    let r = gen env expr in
    { r with vars = [ PChoiceIn { id; kind; vars = r.vars } ] }
  | OptionActions { id; choice; pos; kind } ->
    let r = gen env choice in
    Hmx_solver.below r.ty (Refined.of_base Base.Bool);
    { ty = bool (); null = r.null; vars = [ POption { id; vars = r.vars; pos; kind } ] }
  | Choices (id, branches) ->
    (* alternatives, so the result is the least type above every branch *)
    let branches = List.map (fun (n, e) -> n, Option.map (gen env) e) branches in
    let rs = List.filter_map snd branches in
    let a = Hmx_solver.fresh () and n = Hmx_null.fresh env.nulls in
    List.iter (fun r -> Hmx_solver.same r.ty a) rs;
    Hmx_null.add env.nulls (Join (n, List.map (fun r -> r.null) rs));
    { ty = a; null = n;
      vars = [ PChoice (id, List.map (fun (n, r) -> n, Option.map (fun r -> r.vars) r) branches) ] }
  | Case { scrutinee; branches; else_ } -> gen_case env scrutinee branches else_
  | Call c -> gen_call env c

and gen_case env scrutinee branches else_ =
  let head = Option.map (gen env) scrutinee in
  let whens = List.map (fun (b : Resolved.branch) -> gen env b.when_) branches in
  let thens = List.map (fun (b : Resolved.branch) -> gen env b.then_) branches in
  let else_r = Option.map (gen env) else_ in
  (* with a scrutinee the WHENs are compared to it, without one they are
     conditions *)
  (match head with
   | Some h ->
     let w = Hmx_solver.fresh () in
     Hmx_solver.same h.ty w;
     List.iter (fun r -> Hmx_solver.same r.ty w) whens
   | None -> List.iter (fun r -> Hmx_solver.below r.ty (Refined.of_base Base.Bool)) whens);
  let some = function Some r -> [ r ] | None -> [] in
  let results = thens @ some else_r in
  let a = Hmx_solver.fresh () and n = Hmx_null.fresh env.nulls in
  List.iter (fun r -> Hmx_solver.same r.ty a) results;
  (* a CASE with no ELSE falls through to NULL *)
  Hmx_null.add env.nulls
    (match else_r with
     | None -> Eq (n, N Null.Nullable)
     | Some _ -> Join (n, List.map (fun r -> r.null) results));
  { ty = a; null = n; vars = vars_of (some head @ whens @ thens @ some else_r) }

and gen_call env (call : Resolved.call) =
  let rs = List.map (gen env) call.args in
  match Hmx_sig.instantiate call.sg (List.length call.args) with
  | Error e -> conflict "%s: %s" call.name e
  | Ok sch ->
    let shared = lazy (Hmx_solver.fresh ()) in
    List.iter2 (fun r formal ->
      match (formal : Hmx_sig.param_spec) with
      | Same -> Hmx_solver.same r.ty (Lazy.force shared)
      | As t -> Hmx_solver.below r.ty t
      | Free -> ())
      rs sch.formals;
    List.iter (fun p -> Hmx_solver.has (Lazy.force shared) p) sch.preds;
    let result =
      match sch.result with
      | Ret_same -> Lazy.force shared
      | Ret t -> Hmx_solver.at_least t
    in
    let undeclared_param (arg : Resolved.expr) =
      match arg with
      | Param { ty = { null = None; _ }; _ } -> true
      | Lit _ | Col _ | Param _ | Call _ | Case _ | Choices _ | InChoice _
      | InTupleList _ | OptionActions _ | Subquery _ -> false
    in
    (* The whole parameter-nullability story, in one rule: a parameter that
       shares the scheme variable stands beside its siblings, so it is as
       nullable as they are. With no siblings the join is empty, which is NOT
       NULL. A comparison is the exception — NULL never matches there, so a
       nullable parameter would be useless. *)
    if not call.sg.compares then begin
      let indexed = List.mapi (fun i (arg, r) -> i, arg, r) (List.combine call.args rs) in
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
      let fresh () = Hmx_null.fresh env.nulls in
      match sch.result_null with
      | Hmx_sig.Join -> let n = fresh () in Hmx_null.add env.nulls (Join (n, nulls)); n
      | Hmx_sig.Meet -> let n = fresh () in Hmx_null.add env.nulls (Meet (n, nulls)); n
      | Hmx_sig.Const v -> let n = fresh () in Hmx_null.add env.nulls (Eq (n, N v)); n
      (* an aggregate over an empty group yields NULL, so a strict argument only
         survives when the group is known to have a row *)
      | Hmx_sig.Group_join ->
        let n = fresh () in
        Hmx_null.add env.nulls
          (if call.guaranteed_row then Join (n, nulls) else Eq (n, N Null.Nullable));
        n
      (* SET col = e: the result is the column, and the argument must fit it *)
      | Hmx_sig.Assign ->
        (match nulls with
         | col :: _ -> Hmx_null.add env.nulls (Join (col, nulls)); col
         | [] -> fresh ())
    in
    (* the aggregate's own ORDER BY carries parameters, bound after the
       function's arguments *)
    let order = List.map (fun (e, dir) ->
      let r = gen env e in
      r.vars @ (match dir with
        | Some (`Param p) ->
          [ PReady (Sql.Choice (p, [ Verbatim ("ASC", "ASC"); Verbatim ("DESC", "DESC") ])) ]
        | None | Some `Fixed -> []))
      call.order
    in
    { ty = result; null = ret_null; vars = vars_of rs @ List.concat order }

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
    let items = List.map (fun (ty, null, meta) -> read ty null, meta) items in
    Sql.TupleList (id, Where_in { value = (items, kind); pos })
  | PReady v -> v

(** Generate, solve, and read everything back as declared types: the shape the
    rest of the compiler still speaks. *)
let solve_expr ?fallback e =
  match
    let env = env () in
    let r = gen env e in
    Hmx_null.solve env.nulls;
    let read ty null =
      Hmx_of_sql.to_type (Hmx_solver.resolve ?fallback ty) (Hmx_null.get env.nulls null)
    in
    read r.ty r.null, List.map (to_var read) r.vars
  with
  | result -> Ok result
  | exception Conflict msg -> Error msg

(** just the type, for tests and for anything that does not need parameters *)
let infer ?fallback e =
  match solve_expr ?fallback e with Ok (ty, _) -> Ok ty | Error e -> Error e
