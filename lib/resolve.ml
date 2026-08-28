(** Stage 1: names to references, overloads to signatures.

    No types are inferred here. A column contributes the type its DDL declares,
    a parameter contributes whatever the user wrote (usually nothing), and
    everything else is left for stage 2. That separation is the point: the old
    [resolve_columns] collapses a column straight to its type and throws the
    reference away, which is why nullability narrowing then has to re-find
    columns by name in a table of its own. *)

open Hmx_lattice

type error = { pos : Sql.pos option; msg : string }

let fail ?pos fmt = Printf.ksprintf (fun msg -> Error { pos; msg }) fmt
let show_error { pos; msg } =
  match pos with None -> msg | Some (a, b) -> Printf.sprintf "%s (at %d:%d)" msg a b

(** a column in scope, with the tables it can be qualified by *)
type column = {
  name : string;
  sources : string list;
  ty : Resolved.ty;
  meta : Sql.Meta.t;
}

type env = {
  columns : column list;
  (** columns of a table or CTE named explicitly in a qualified reference but
      not part of the current scope *)
  named : string -> column list option;
  grouping : bool;
  (** an aggregate here is guaranteed to see a row, so a strict argument keeps
      a strict result *)
  guaranteed_row : bool;
  subquery : Sql.select_full -> [ `AsValue | `Exists ] -> (Resolved.ty * Sql.var list, error) result;
  of_values : string -> (Resolved.ty, error) result;
}

let ( let* ) = Result.bind

let rec map_result f = function
  | [] -> Ok []
  | x :: l -> let* y = f x in let* rest = map_result f l in Ok (y :: rest)

let opt_result f = function
  | None -> Ok None
  | Some x -> let* y = f x in Ok (Some y)

(* ------------------------------------------------------------ columns *)

let qualified tname c =
  match tname with
  | None -> true
  | Some (t : Sql.table_name) -> List.mem t.tn c.sources

(** The old resolver prefers a qualified match and falls back to an unqualified
    one, taking the last of several matches rather than reporting the
    ambiguity. That last part is kept: reporting it now would break queries
    that work today. *)
let lookup_column env ({ cname; tname } : Sql.col_name) =
  match List.filter (fun c -> String.equal c.name cname && qualified tname c) env.columns with
  | [ c ] -> Ok c
  | _ :: _ as l -> Ok (List.nth l (List.length l - 1))
  | [] ->
    let pool =
      match tname with
      | Some t -> (match env.named t.tn with Some l -> l | None -> [])
      | None -> env.columns
    in
    match List.filter (fun c -> String.equal c.name cname) pool with
    | [ c ] -> Ok c
    | [] -> fail "missing attribute: %s" cname
    | _ -> fail "duplicate attribute: %s" cname

(** JSON null is not SQL NULL, and DDL has no way to say which a column allows,
    so sqlgg carries it as metadata. It is a property of the column, decided
    before any inference, which is why it belongs here and not in the solver. *)
let apply_json_meta (c : column) =
  let json_null_kind = Sql.Meta.find_opt c.meta "json_null_kind" in
  let text_as_json = Sql.Meta.find_opt c.meta "text_as_json" in
  let is b = match c.ty.base with Some r -> Base.equal r.Refined.base b | None -> false in
  match json_null_kind, text_as_json with
  | None, None -> Ok c.ty
  | v, _ when is Base.Json ->
    let null = match v, c.ty.null with
      | Some "false", Some Null.NotNull -> Some Null.NotNull
      | _ -> Some Null.Nullable
    in
    Ok { c.ty with null }
  | v, Some "true" when is Base.Text ->
    let null = match v, c.ty.null with
      | Some "false", Some Null.NotNull -> Some Null.NotNull
      | _ -> Some Null.Nullable
    in
    Ok { c.ty with null }
  | _, Some _ -> fail "column %s has text_as_json meta, but its type is not Text" c.name
  | Some _, None -> fail "column %s has json_null_kind meta, but its type is not Json or Text" c.name

(* ------------------------------------------------------- expressions *)

let ty_of_sql t = let base, null = Hmx_of_sql.of_type t in { Resolved.base; null }
let ty_of_source t = ty_of_sql (Sql.Source_type.to_infer_type t)

let mode_of_kind : 'a Sql.func -> Resolved.mode = function
  | Agg _ -> Resolved.Agg
  | Null_handling _ | Comparison _ | Quantified_comparison _ | Logical _ | Negation
  | Arith _ | Membership | Range | Like _ | Ret _ | F _ | Col_assign _ | Multi _ -> Resolved.Scalar

(* the parameter an option-actions block switches on *)
let rec choice_id (e : Sql.expr) =
  match e with
  | Choices (id, _) -> Some id
  | InChoice (id, _, _) -> Some id
  | OptionActions { choice; _ } -> choice_id choice
  | Value _ | Param _ | Inparam _ | Fun _ | SelectExpr _ | Column _
  | InTupleList _ | Case _ | Of_values _ -> None

let rec expr env (e : Sql.expr) : (Resolved.expr, error) result =
  match e with
  | Value v -> Ok (Resolved.Lit (ty_of_sql v.collated))
  | Column col ->
    let* c = lookup_column env col.collated in
    let* ty = apply_json_meta c in
    Ok (Resolved.Col ({ table = col.collated.tname |> Option.map (fun (t : Sql.table_name) -> t.tn);
                        column = c.name }, ty))
  | Param (p, meta) ->
    Ok (Resolved.Param { id = p.id; ty = ty_of_source p.typ; meta; in_list = false })
  | Inparam (p, meta) ->
    Ok (Resolved.Param { id = p.id; ty = ty_of_source p.typ; meta; in_list = true })
  | InChoice (id, kind, e) -> let* e = expr env e in Ok (Resolved.InChoice { id; kind; expr = e })
  | Choices (id, l) ->
    let* branches = map_result (fun (n, e) ->
      let* e = opt_result (expr env) e in Ok (n, e)) l
    in
    Ok (Resolved.Choices (id, branches))
  | OptionActions { choice; pos; kind } ->
    (match choice_id choice with
     | None -> fail "an option block must switch on a parameter; use a plain choice otherwise"
     | Some id ->
       let* choice = expr env choice in
       Ok (Resolved.OptionActions { id; choice; pos; kind }))
  | InTupleList { value = { exprs; param_id; kind_in_tuple_list }; pos } ->
    (* a column on the left carries its metadata into the tuple list *)
    let* items = map_result (fun e ->
      let* r = expr env e in
      let meta =
        match e with
        | Sql.Column col ->
          (match lookup_column env col.collated with Ok c -> c.meta | Error _ -> Sql.Meta.empty ())
        | _ -> Sql.Meta.empty ()
      in
      Ok (r, meta)) exprs
    in
    let unsupported = List.exists (function
      | Resolved.Choices _, _ | InChoice _, _ | InTupleList _, _ | OptionActions _, _ -> true
      | (Lit _ | Col _ | Param _ | Call _ | Case _ | Subquery _), _ -> false) items
    in
    if unsupported then fail ~pos "unsupported expression kind for WHERE e IN @tuplelist"
    else Ok (Resolved.InTupleList { id = param_id; items; kind = kind_in_tuple_list; pos })
  | Case { case; branches; else_ } ->
    let* scrutinee = opt_result (expr env) case in
    let* branches = map_result (fun ({ when_; then_ } : Sql.case_branch) ->
      let* when_ = expr env when_ in
      let* then_ = expr env then_ in
      Ok { Resolved.when_; then_ }) branches
    in
    let* else_ = opt_result (expr env) else_ in
    Ok (Resolved.Case { scrutinee; branches; else_ })
  | Of_values col -> let* ty = env.of_values col in Ok (Resolved.Lit ty)
  | SelectExpr (select, usage) ->
    let* ty, vars = env.subquery select usage in
    Ok (Resolved.Subquery { ty; kind = usage; vars })
  | Fun { fn_name; kind; parameters; over } ->
    let arity = List.length parameters in
    match Hmx_of_sql.of_func ~arity kind with
    | Error msg -> fail "%s: %s" fn_name msg
    | Ok sg ->
      let* args = map_result (expr env) parameters in
      let* order =
        match kind with
        | Agg (With_order { order; _ }) ->
          map_result (fun (e, dir) -> let* e = expr env e in Ok (e, dir)) order
        | _ -> Ok []
      in
      Ok (Resolved.Call {
        name = fn_name; sg; args; order;
        mode = mode_of_kind kind;
        guaranteed_row = env.grouping || Sql.over_has_a_row over })

(* ----------------------------------------------------------- sources *)

(** What a FROM clause puts in scope.

    Everything here is declared types only — a column's type comes from its
    DDL and nothing about it is inferred — so it reuses {!Sql.Schema}, which
    already gets the hard parts right: [Schema.Join.join] does cross, natural
    and USING, and pads the optional side of an outer join to nullable. That
    code is relational algebra, not typing, and it survives the migration. *)

type catalog = {
  table : Sql.table_name -> (Sql.table_name Sql.Schema.Source.t, error) result;
  (** a nested SELECT; supplied by the caller until select resolution lands *)
  select : Sql.select_full -> (Sql.table_name Sql.Schema.Source.t, error) result;
  values : Sql.row_values -> (Sql.table_name Sql.Schema.Source.t, error) result;
}

let sourced (name : Sql.table_name) schema =
  List.map (fun (a : Sql.attr) -> { Sql.Schema.Source.Attr.attr = a; sources = [ name ] }) schema

(** an alias renames the source and, with a column list, renames the columns *)
let apply_alias alias schema =
  match (alias : Sql.source_alias option) with
  | None -> Ok schema
  | Some { table_name; column_aliases = None } ->
    Ok (List.map (fun (a : _ Sql.Schema.Source.Attr.t) ->
      { a with Sql.Schema.Source.Attr.sources = [ table_name ] }) schema)
  | Some { table_name; column_aliases = Some names } ->
    if List.length names <> List.length schema then
      fail "alias %s lists %d columns but the source has %d"
        table_name.tn (List.length names) (List.length schema)
    else
      Ok (List.map2 (fun (a : _ Sql.Schema.Source.Attr.t) (n : Sql.attr) ->
        { Sql.Schema.Source.Attr.attr = { a.attr with name = n.name }; sources = [ table_name ] })
        schema names)

let rec source cat ((kind, alias) : Sql.source) =
  let* schema =
    match kind with
    | `Table name -> cat.table name
    | `Select s -> cat.select s
    | `ValueRows v -> cat.values v
    | `Nested n -> nested cat n
  in
  apply_alias alias schema

and nested cat ((base, joins) : Sql.nested) =
  let* base = source cat base in
  let rec go acc = function
    | [] -> Ok acc
    | ({ Sql.value = (src, typ, cond); _ } : _ Sql.located) :: rest ->
      let* right = source cat src in
      (* the condition is resolved later, against the joined scope; only its
         shape matters for how the schemas combine *)
      let shape = match (cond : _ Sql.Schema.Join.condition) with
        | On _ -> Sql.Schema.Join.Default
        | Default -> Default
        | Natural -> Natural
        | Using l -> Using l
      in
      match Sql.Schema.Join.join typ.Sql.value shape acc right with
      | joined -> go joined rest
      | exception Sql.Schema.Error (_, msg) -> fail "%s" msg
  in
  go base joins

(** the columns a resolved FROM puts in scope *)
let scope_of_schema schema =
  List.map (fun (a : Sql.table_name Sql.Schema.Source.Attr.t) -> {
    name = a.attr.name;
    sources = List.map (fun (t : Sql.table_name) -> t.tn) a.sources;
    ty = ty_of_sql a.attr.domain;
    meta = a.attr.meta;
  }) schema
