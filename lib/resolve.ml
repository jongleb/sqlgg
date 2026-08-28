(** Scope: what names mean.

    A column resolves to the type its DDL declares, a table or CTE resolves to
    its columns, and a FROM clause combines them. No types are inferred here —
    that is {!Constrain}, which walks the expression once and resolves names as
    it goes, the way Inferno's own client does. *)

open Hmx_lattice

let fail fmt = conflict fmt

(** What the source says about a type; [None] means nothing was written.

    That absence is the point. Today {!Sql.Type.Any} plays two roles at once —
    the bottom of the lattice and "not known yet" — and the second role is what
    this [option] replaces. *)
type ty = { base : Refined.t option; null : bool option (** may be NULL *) }

(** a column in scope, with the tables it can be qualified by *)
type column = { name : string; sources : string list; ty : ty; meta : Sql.Meta.t }

type env = {
  columns : column list;
  (** columns of a table or CTE named explicitly in a qualified reference but
      not part of the current scope *)
  named : string -> column list option;
  (** an aggregate here is guaranteed to see a row, so a strict argument keeps
      a strict result *)
  grouping : bool;
  (** §6: an aggregate is a function of a group, so it has no meaning where
      rows are still being filtered — in WHERE, in GROUP BY, in a join
      condition, or inside another aggregate *)
  allow_aggregates : bool;
  subquery : Sql.select_full -> [ `AsValue | `Exists ] -> ty * Sql.var list;
  of_values : string -> ty;
}

(* ------------------------------------------------------------ columns *)

(** A qualified reference picks among the columns that carry that source; an
    unqualified one must be unique, and being present in two joined tables is
    an error rather than a silent pick. *)
let lookup_column env ({ cname; tname } : Sql.col_name) =
  let named name c = String.equal c.name name in
  match tname with
  | Some t ->
    (match List.filter (fun c -> named cname c && List.mem t.Sql.tn c.sources) env.columns with
     | [ c ] -> c
     | _ :: _ as l -> List.nth l (List.length l - 1)
     | [] ->
       (match env.named t.tn with
        | None -> fail "missing table: %s" t.tn
        | Some cols ->
          (match List.filter (named cname) cols with
           | [ c ] -> c
           | [] -> fail "missing attribute: %s" cname
           | _ -> fail "duplicate attribute: %s" cname)))
  | None ->
    match List.filter (named cname) env.columns with
    | [ c ] -> c
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
  | None, None -> c.ty
  | v, _ when is Base.Json ->
    let null = match v, c.ty.null with
      | Some "false", Some false -> Some false
      | _ -> Some true
    in
    { c.ty with null }
  | v, Some "true" when is Base.Text ->
    let null = match v, c.ty.null with
      | Some "false", Some false -> Some false
      | _ -> Some true
    in
    { c.ty with null }
  | _, Some _ -> fail "column %s has text_as_json meta, but its type is not Text" c.name
  | Some _, None -> fail "column %s has json_null_kind meta, but its type is not Json or Text" c.name

(* ------------------------------------------------------- expressions *)

let ty_of_sql t = let base, null = Hmx_of_sql.of_type t in { base; null }
let ty_of_source t = ty_of_sql (Sql.Source_type.to_infer_type t)

(* the parameter an option-actions block switches on *)
let rec choice_id (e : Sql.expr) =
  match e with
  | Choices (id, _) -> Some id
  | InChoice (id, _, _) -> Some id
  | OptionActions { choice; _ } -> choice_id choice
  | Value _ | Param _ | Inparam _ | Fun _ | SelectExpr _ | Column _
  | InTupleList _ | Case _ | Of_values _ -> None

(* ----------------------------------------------------------- sources *)

(** What a FROM clause puts in scope.

    Everything here is declared types only — a column's type comes from its
    DDL and nothing about it is inferred — so it reuses {!Sql.Schema}, which
    already gets the hard parts right: [Schema.Join.join] does cross, natural
    and USING, and pads the optional side of an outer join to nullable. That
    code is relational algebra, not typing, and it survives the migration. *)

type catalog = {
  table : Sql.table_name -> Sql.table_name Sql.Schema.Source.t;
  (** a nested SELECT; supplied by the caller until select resolution lands *)
  select : Sql.select_full -> Sql.table_name Sql.Schema.Source.t;
  values : Sql.row_values -> Sql.table_name Sql.Schema.Source.t;
}

let sourced (name : Sql.table_name) schema =
  List.map (fun (a : Sql.attr) -> { Sql.Schema.Source.Attr.attr = a; sources = [ name ] }) schema

(** an alias renames the source and, with a column list, renames the columns *)
let apply_alias alias schema =
  match (alias : Sql.source_alias option) with
  | None -> schema
  | Some { table_name; column_aliases = None } ->
    List.map (fun (a : _ Sql.Schema.Source.Attr.t) ->
      { a with Sql.Schema.Source.Attr.sources = [ table_name ] }) schema
  | Some { table_name; column_aliases = Some names } ->
    if List.length names <> List.length schema then
      fail "alias %s lists %d columns but the source has %d"
        table_name.tn (List.length names) (List.length schema)
    else
      List.map2 (fun (a : _ Sql.Schema.Source.Attr.t) (n : Sql.attr) ->
        { Sql.Schema.Source.Attr.attr = { a.attr with name = n.name }; sources = [ table_name ] })
        schema names

let rec source cat ((kind, alias) : Sql.source) =
  apply_alias alias
    (match kind with
     | `Table name -> cat.table name
     | `Select s -> cat.select s
     | `ValueRows v -> cat.values v
     | `Nested n -> nested cat n)

and nested cat ((base, joins) : Sql.nested) =
  List.fold_left (fun acc ({ Sql.value = (src, typ, cond); _ } : _ Sql.located) ->
    (* the condition is resolved later, against the joined scope; only its
       shape matters for how the schemas combine *)
    let shape = match (cond : _ Sql.Schema.Join.condition) with
      | On _ | Default -> Sql.Schema.Join.Default
      | Natural -> Natural
      | Using l -> Using l
    in
    match Sql.Schema.Join.join typ.Sql.value shape acc (source cat src) with
    | joined -> joined
    | exception Sql.Schema.Error (_, msg) -> fail "%s" msg)
    (source cat base) joins

(** the columns a resolved FROM puts in scope *)
let scope_of_schema schema =
  List.map (fun (a : Sql.table_name Sql.Schema.Source.Attr.t) -> {
    name = a.attr.name;
    sources = List.map (fun (t : Sql.table_name) -> t.tn) a.sources;
    ty = ty_of_sql a.attr.domain;
    meta = a.attr.meta;
  }) schema
