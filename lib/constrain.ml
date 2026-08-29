
open Hmx_lattice
module Meta = Sql.Meta

type session = {
  nulls : Hmx_null.state;
  named : (string, Hmx_solver.var * Hmx_null.t) Hashtbl.t;
  mutable seen : (Sql.param_id * (Hmx_solver.var * Hmx_null.t)) list;
}

let create () = { nulls = Hmx_null.create (); named = Hashtbl.create 8; seen = [] }

let named session name =
  match Hashtbl.find_opt session.named name with
  | Some v -> v
  | None ->
    let v = Hmx_solver.fresh (), Hmx_null.fresh () in
    Hashtbl.add session.named name v; v

let alias session a b =
  let ta, na = named session a and tb, nb = named session b in
  Hmx_solver.same ta tb;
  Hmx_null.unify na nb

type env = { session : session; scope : Resolve.env }

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
  | PReady of Sql.var

type t = {
  ty : Hmx_solver.var;
  null : Hmx_null.t;
  vars : pvar list;
  own : Meta.t option;
  push : Meta.t -> unit;
  closed : Refined.t option;

}

let vars_of rs = List.concat_map (fun r -> r.vars) rs
let bool = Refined.of_base Base.Bool
let boolean () = Hmx_solver.at_least bool

let split (t : Sql.Type.t) =
  let base, null = Hmx_of_sql.of_type t in
  (match base with Some r -> Hmx_solver.at_least r | None -> Hmx_solver.fresh ()),
  (match null with Some n -> Hmx_null.const n | None -> Hmx_null.fresh ())

let closed_enum (t : Sql.Type.t) =
  match Hmx_of_sql.of_kind t.t with
  | Some r when Refine.is_closed_enum r.refine -> Some r
  | Some _ | None -> None

let agreed = function
  | [] -> Some (Meta.empty ())
  | l -> Meta.common_all (List.map (fun r -> r.own) l)

let sealed l = Meta.of_option (agreed l)

let settle rs = List.iter (fun r -> r.push (Meta.empty ())) rs

let silent ~ty ~null = { ty; null; vars = []; own = None; push = ignore; closed = None }

let opaque ~ty ~null children =
  { ty; null; vars = vars_of children; own = Some (Meta.empty ()); push = (fun _ -> settle children);
    closed = None }

let node ~ty ~null ~vars same_domain push_children =
  let own = agreed same_domain in
  { ty; null; vars; own; closed = None;
    push = (fun ctx -> push_children (Meta.of_option (Meta.common (Meta.declared ctx) own))) }

let compare_literals args rs =
  let closed = List.filter_map (fun r -> r.closed) rs in
  List.iter2 (fun arg r ->
    match arg with
    | Sql.Value _ -> List.iter (fun b -> Hmx_solver.below r.ty b) closed
    | _ -> ())
    args rs

let undeclared_param = function
  | Sql.Param ({ typ = { nullability = Depends; _ }; _ }, _)
  | Sql.Inparam ({ typ = { nullability = Depends; _ }; _ }, _) -> true
  | _ -> false

let rec gen env (e : Sql.expr) : t =
  match e with

  | Value v ->
    let ty, _ = split v.collated in
    silent ~ty ~null:(Hmx_null.const (Sql.Type.is_nullable v.collated))
  | Column col ->
    let c = env.scope.column col.collated in
    let ty, null = split (Resolve.apply_json_meta c) in
    { (silent ~ty ~null) with own = Meta.declared c.meta; closed = closed_enum c.domain }
  | Param (p, meta) -> param env ~in_list:false p meta
  | Inparam (p, meta) -> param env ~in_list:true p meta
  | Of_values col -> let ty, null = split (env.scope.of_values col) in opaque ~ty ~null []
  | SelectExpr (select, usage) ->
    let t, vars, meta = env.scope.subquery select usage in
    let vars = List.map (fun v -> PReady v) vars in
    let ty, null =
      match usage with
      | `AsValue -> split t
      | `Exists -> boolean (), Hmx_null.const false
    in
    { ty; null; vars; own = Some meta; push = ignore; closed = None }
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

    let branches = List.map (fun (n, e) -> n, Option.map (gen env) e) l in
    let rs = List.filter_map snd branches in
    let a = Hmx_solver.fresh () and n = Hmx_null.fresh () in
    List.iter (fun r -> Hmx_solver.same r.ty a) rs;
    Hmx_null.add env.session.nulls (Join (n, List.map (fun r -> r.null) rs));
    node ~ty:a ~null:n
      ~vars:[ PChoice (id, List.map (fun (n, r) -> n, Option.map (fun r -> r.vars) r) branches) ]
      rs (fun ctx -> List.iter (fun r -> r.push ctx) rs)
  | Case { case; branches; else_ } -> gen_case env case branches else_
  | Fun { fn_name; kind; parameters; over } ->
    let arity = List.length parameters in

    if Hmx_sig.is_agg fn_name arity && not env.scope.allow_aggregates then
      conflict "%s is an aggregate and cannot appear here" fn_name;
    let sg =
      match kind with

      | Sql.Cast t ->
        let t = Sql.Source_type.to_infer_type t in
        Hmx_sig.make ?ret:(Hmx_of_sql.of_kind t.t) (Args [ Free ])
      | Named | Agg_order _ ->
        match Hmx_sig.find fn_name arity with
        | Some sg -> sg

        | None -> Hmx_sig.make (Varargs { head = []; tail = [ Free ] })
    in
    let order = match kind with Agg_order { order; _ } -> order | Named | Cast _ -> [] in

    let inner =
      if Hmx_sig.is_agg fn_name arity
      then { env with scope = { env.scope with allow_aggregates = false } }
      else env
    in
    gen_call inner ~name:fn_name ~sg ~args:parameters ~order
      ~guaranteed_row:(env.scope.grouping || Sql.over_has_a_row over)

and param env ~in_list (p : Sql.Source_type.t Sql.param) meta =
  let ty, null = split (Sql.Source_type.to_infer_type p.typ) in
  (match p.id.value with
   | Some name ->
     let t, n = named env.session name in
     Hmx_solver.same ty t;
     Hmx_null.unify null n
   | None -> ());
  env.session.seen <- (p.id, (ty, null)) :: env.session.seen;
  let meta = ref meta in
  { ty; null; vars = [ PSingle { id = p.id; ty; null; meta; in_list } ];
    own = None; push = (fun ctx -> meta := Meta.merge_right ctx !meta); closed = None }

and gen_case env scrutinee branches else_ =
  let head = Option.map (gen env) scrutinee in
  let whens = List.map (fun (b : Sql.case_branch) -> gen env b.when_) branches in
  let thens = List.map (fun (b : Sql.case_branch) -> gen env b.then_) branches in
  let else_r = Option.map (gen env) else_ in

  (match head with
   | Some h ->
     let w = Hmx_solver.fresh () in
     Hmx_solver.same h.ty w;
     List.iter (fun r -> Hmx_solver.same r.ty w) whens;
     compare_literals (Option.get scrutinee :: List.map (fun (b : Sql.case_branch) -> b.when_) branches)
       (h :: whens)
   | None -> List.iter (fun r -> Hmx_solver.below r.ty bool) whens);
  let some = function Some r -> [ r ] | None -> [] in
  let results = thens @ some else_r in
  let a = Hmx_solver.fresh () and n = Hmx_null.fresh () in
  List.iter (fun r -> Hmx_solver.same r.ty a) results;

  let exhaustive =
    match head, scrutinee with
    | Some { closed = Some { refine = Enum { ctors; _ }; _ }; _ }, Some _ ->
      let named = List.filter_map (fun (b : Sql.case_branch) ->
        match b.when_ with
        | Value { collated = { t = StringLiteral s; _ }; _ } -> Some s
        | _ -> None) branches
      in
      Refine.Ctors.subset ctors (Refine.Ctors.of_list named)
    | _ -> false
  in
  Hmx_null.add env.session.nulls
    (match else_r with
     | None when not exhaustive -> Eq (n, Hmx_null.const true)
     | None | Some _ -> Join (n, List.map (fun r -> r.null) results));

  node ~ty:a ~null:n ~vars:(vars_of (some head @ whens @ thens @ some else_r)) results
    (fun ctx -> settle (some head @ whens); List.iter (fun r -> r.push ctx) results)

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
  if sg.compares then
    compare_literals args
      (List.map2 (fun same r -> if same then r else { r with closed = None }) sch.same_at rs);
  let result =
    match sch.result with None -> Lazy.force shared | Some t -> Hmx_solver.at_least t
  in

  if not sg.compares then begin
    let indexed = List.mapi (fun i (arg, r) -> i, arg, r) (List.combine args rs) in
    List.iter (fun (i, arg, r) ->
      if List.nth sch.same_at i && undeclared_param arg then
        match List.filter_map (fun (j, _, r') ->
          if j <> i && List.nth sch.same_at j then Some r'.null else None) indexed with
        | [] -> ()
        | siblings -> Hmx_null.add env.session.nulls (Above (r.null, siblings)))
      indexed
  end;
  let nulls = List.map (fun r -> r.null) rs in
  let ret_null =
    match sch.result_null with
    | Hmx_sig.Join -> let n = Hmx_null.fresh () in Hmx_null.add env.session.nulls (Join (n, nulls)); n
    | Hmx_sig.Meet -> let n = Hmx_null.fresh () in Hmx_null.add env.session.nulls (Meet (n, nulls)); n
    | Hmx_sig.Const v -> Hmx_null.const v

    | Hmx_sig.Group_join ->
      if guaranteed_row then
        let n = Hmx_null.fresh () in Hmx_null.add env.session.nulls (Join (n, nulls)); n
      else Hmx_null.const true
  in

  let ordered = List.map (fun (e, dir) -> gen env e, dir) order in
  let order_vars = List.concat_map (fun (r, dir) ->
    r.vars @ (match dir with
      | Some (`Param p) ->
        [ PReady (Sql.Choice (p, [ Verbatim ("ASC", "ASC"); Verbatim ("DESC", "DESC") ])) ]
      | None | Some `Fixed -> []))
    ordered
  in

  let returns_shared = sch.result = None in
  let args = List.combine sch.same_at rs in
  let shared = List.filter_map (fun (same, r) -> if same then Some r else None) args in

  let meta_shared = if sg.carries then shared else [] in
  let r =
    node ~ty:result ~null:ret_null ~vars:(vars_of rs @ order_vars)
      (if returns_shared then meta_shared else [])
      (fun ctx ->
        settle (List.map fst ordered);
        List.iter (fun (same, r) ->
          r.push (if not (same && sg.carries) then Meta.empty ()
                  else if returns_shared then ctx else sealed meta_shared))
          args)
  in

  { r with closed = if returns_shared then List.find_map (fun r -> r.closed) shared else None }

let rec gen_assign env ~lax ~(column : Sql.Type.t) (e : Sql.expr) : t =
  match e with
  | Choices (id, l) ->
    let branches = List.map (fun (n, e) -> n, Option.map (gen_assign env ~lax ~column) e) l in
    let rs = List.filter_map snd branches in
    let ty, _ = split column in
    let null = Hmx_null.fresh () in
    List.iter (fun r -> Hmx_solver.same r.ty ty) rs;
    Hmx_null.add env.session.nulls (Join (null, List.map (fun r -> r.null) rs));
    node ~ty ~null
      ~vars:[ PChoice (id, List.map (fun (n, r) -> n, Option.map (fun r -> r.vars) r) branches) ]
      rs (fun ctx -> List.iter (fun r -> r.push ctx) rs)
  | OptionActions { choice; pos; kind } ->
    let id =
      match Resolve.choice_id choice with
      | Some id -> id
      | None -> conflict "an option block must switch on a parameter; use a plain choice otherwise"
    in
    let r = gen_assign env ~lax ~column choice in
    node ~ty:r.ty ~null:r.null ~vars:[ POption { id; vars = r.vars; pos; kind } ] [ r ] r.push
  | e ->
    let r = gen env e in
    let ty, null = split column in

    (match Hmx_of_sql.of_kind column.t with
     | Some b -> Hmx_solver.below r.ty b
     | None -> Hmx_solver.same r.ty ty);

    if undeclared_param e then Hmx_null.add env.session.nulls (Above (r.null, [ null ]));
    let written =
      if lax then begin
        let n = Hmx_null.fresh () in
        Hmx_null.add env.session.nulls (Join (n, [ null; r.null ]));
        n
      end else begin

        Hmx_null.solve env.session.nulls;
        (match Hmx_null.value null, Hmx_null.value r.null with
         | Some false, Some true ->
           conflict "Cannot assign nullable value to a non-nullable column of type %s" (Sql.Type.show column)
         | _ -> ());
        Hmx_null.add env.session.nulls (Join (null, [ null; r.null ]));
        r.null
      end
    in
    node ~ty ~null:written ~vars:r.vars [ r ] r.push

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

let read ?fallback ty null =
  let null = Hmx_null.get null in
  match Hmx_solver.resolve_opt ?fallback ty with
  | Some r -> Hmx_of_sql.to_type r null
  | None -> { Sql.Type.t = Any; nullability = Hmx_of_sql.to_nullability null }

let solve ?(ctx = Meta.empty ()) ?fallback session scope walk =
  try
    let r = walk { session; scope } in
    r.push ctx;
    Hmx_null.solve session.nulls;
    let read = read ?fallback in
    Ok (read r.ty r.null, List.map (to_var read) r.vars, Meta.of_option r.own)
  with Conflict msg -> Error msg

let solve_expr ?ctx ?fallback session scope e = solve ?ctx ?fallback session scope (fun env -> gen env e)

let solve_assign ?ctx ?fallback ?(lax = false) session scope ~column e =
  solve ?ctx ?fallback session scope (fun env -> gen_assign env ~lax ~column e)

let read_param session ({ Sql.id; typ } : Sql.Type.t Sql.param) =
  Hmx_null.solve session.nulls;
  match List.find_opt (fun (k, _) -> k == id) session.seen with
  | Some (_, (ty, null)) -> { Sql.id; typ = read ty null }
  | None -> { Sql.id; typ }

let infer ?fallback scope e =
  match solve_expr ?fallback (create ()) scope e with Ok (ty, _, _) -> Ok ty | Error e -> Error e
