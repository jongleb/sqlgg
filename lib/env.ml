(** The evaluation context: what a statement knows while its schema and
    parameters are computed — the tables and CTEs in scope, the current
    schema, and the flags that steer typing. Name resolution against this
    context lives here; the recursive schema/parameter evaluation that consumes
    it is {!Syntax}. *)

open ExtLib
open Prelude
open Sql
open Narrowing

module Config = struct
  let debug = ref false
  (* If strict mode is not enabled, some dbs allow this. *)
  let allow_write_notnull_null = ref false
  let dynamic_select = ref false
end

type query_scope =
  | Top_level
  | Subquery
  | From_passthrough

type t = {
  tables : Tables.table list;
  schema : table_name Schema.Source.t;
  (*
    1. CTEs = tables for the current statement (not keeping during whole .sql)
    2. It merges with global tables during source resolving
    3. The Tables field mostly stores aliases and forms a scheme
  *)
  ctes : Tables.table list;
  query_has_grouping: bool;
  (* Check if the current query is an UPDATE statement *)

  allow_aggregates: bool;
  of_values_types: (string * Type.t) list; (* column types VALUES(col) reads, in ON DUPLICATE KEY UPDATE *)
  scope: query_scope;
  attr_refinement: Attr_refinement.t;

  insert_targets: attr list option;

  session: Constrain.session;
}

let compound_type (a : Type.t) (b : Type.t) =
  let null = Type.is_nullable a || Type.is_nullable b in
  match Hmx_of_sql.of_kind a.t, Hmx_of_sql.of_kind b.t with

  | None, None -> Some { a with Type.nullability = if null then Nullable else Strict }

  | Some a, Some b when
      (match Hmx_lattice.Base.lub [ a.base; b.base ] with
       | Some l -> not (Hmx_lattice.Base.equal l a.base || Hmx_lattice.Base.equal l b.base)
       | None -> true) -> None
  | ba, bb ->
    let v = Hmx_solver.fresh () in
    match
      Option.may (Hmx_solver.above v) ba;
      Option.may (Hmx_solver.above v) bb;
      Hmx_solver.resolve v
    with
    | exception Hmx_lattice.Conflict _ -> None

    | t when List.exists (fun (r : Hmx_lattice.Refined.t option) ->
        match r with
        | Some r -> List.mem (r.base, t.base) Hmx_lattice.Base.derived
        | None -> false) [ ba; bb ] -> None
    | t -> Some (Hmx_of_sql.to_type t null)

let compound t1 t2 = Schema.compound ~merge:compound_type t1 t2

let empty_env session = { query_has_grouping = false;
  tables = []; schema = [];
  ctes = [];
  allow_aggregates = true;
  of_values_types = [];
  scope = Top_level;
  attr_refinement = Attr_refinement.empty;
  insert_targets = None;
  session;
}

let schema_of ~env name =
  let result = Tables.get_from (env.ctes @ env.tables) name in
  Schema.Source.of_schema ~sources:[fst result] (snd result)

let values_or_all table names =
  let schema = Tables.get_schema table in
  match names with
  | Some names ->
    let req_missing =
      List.filter_map
        (fun { extra; name; _ } ->
          let open Constraints in
          if inter (of_list [Autoincrement; WithDefault; NotNull]) extra = of_list [NotNull]
            && not @@ List.mem name names then Some name
          else None
        )
        schema
    in
    begin match req_missing with
    | [] -> ()
    | fields ->
        fail "Fields: (%s) don't have a default value" (String.concat "," fields) end;
    Schema.project names schema
  | None -> schema

let static_cols what = List.map (function
  | AttrWithSources a -> a
  | DynamicWithSources _ -> failwith what)

let make_unique =
  List.unique ~cmp:(fun a1 a2 ->
    match Qualified_attr.named (Qualified_attr.of_attr a1), Qualified_attr.named (Qualified_attr.of_attr a2) with
    | Some k1, Some k2 -> Qualified_attr.equal k1 k2
    | None, _ | _, None -> false)

let all_columns = make_unique $ Schema.cross_all

let resolve_column ~env { cname; tname } =
  let open Schema.Source in
  (* unqualified: exactly one column of that name, or it is missing/ambiguous *)
  let unqualified schema =
    match List.find_all (Attr.by_name cname) schema with
    | [ x ] -> x
    | [] -> raise (Schema.Error (to_schema schema, "missing attribute : " ^ cname))
    | _ -> raise (Schema.Error (to_schema schema, "duplicate attribute : " ^ cname))
  in
  match tname with
  | None -> unqualified env.schema
  | Some t ->
    let here (sa : _ Attr.t) =
      sa.attr.name = cname && List.exists (fun (i : table_name) -> i.tn = t.tn) sa.sources in
    (* a qualified name resolves in scope (last wins on repeats), else by its table *)
    match List.find_all here env.schema with
    | [] -> unqualified (schema_of ~env t)
    | l -> List.last l

let resolve_column_opt ~env col =
  match resolve_column ~env col with
  | attr -> Some attr
  | exception (Schema.Error _ | Failure _) -> None

let as_column ~env = function
  | Sql.Column col -> resolve_column_opt ~env col.collated
  | _ -> None

let update_schema_with_aliases all_schema final_schema =
  let applied = all_schema |> List.filter (fun s1 -> List.for_all Schema.Source.Attr.(fun s2 -> s2.attr.name <> s1.attr.name) final_schema) in
  applied @ final_schema

let dynamic_allowed env =
  !Config.dynamic_select &&
  match env.scope with
  | Top_level | From_passthrough -> true
  | Subquery -> false

let make_dynamic_select ~env columns =
  if not (dynamic_allowed env) then
    columns
  else
    let module S = Set.Make(String) in
    let unique_name used base =
      if not (S.mem base used) then
        base
      else
        let rec aux n =
          let candidate = base ^ "_" ^ string_of_int n in
          if S.mem candidate used then aux (n + 1) else candidate
        in
        aux 1
    in
    let use_expanded_choices ~used ~idx ~column_pos ~schema =
      let rev_choices, used, idx =
        List.fold_left (fun (choices, used, idx) { Schema.Source.Attr.attr = { name; _ }; sources } ->
          let source = match sources with s :: _ -> Some s | [] -> None in
          let col_name = unique_name used name in
          let expr = Column { collated = { cname = name; tname = source }; collation = None } in
          let choice = ({ value = Some col_name; pos = Sql.dummy_pos }, Some expr), column_pos in
          choice :: choices, S.add col_name used, idx + 1
        ) ([], used, idx) schema
      in
      (used, idx, snd column_pos), List.rev rev_choices
    in
    let (_, _, last_col_end), choices_chunks =
      List.fold_left_map (fun (used, idx, _last_end) column ->
        match column.value with
        | Expr ({ value = e; pos = ep_start, ep_end }, alias) ->
          let base_name = Option.default begin match e with
            | Column { collated = { cname; _ }; _ } -> cname
            | _ -> Params.dynamic_col_param_name ^ string_of_int (idx + 1)
            end alias
          in
          let col_name = unique_name used base_name in
          let choice = (({ value = Some col_name; pos = (ep_start, ep_end) }, Some e), column.pos) in
          ((S.add col_name used, idx + 1, snd column.pos), [choice])
        | All ->
          use_expanded_choices ~used ~idx ~column_pos:column.pos ~schema:env.schema
        | AllOf t ->
          use_expanded_choices ~used ~idx ~column_pos:column.pos ~schema:(schema_of ~env t)
      ) (S.empty, 0, 0) columns
    in
    let all_choices = List.concat choices_chunks in
    match all_choices with
    | [] -> columns
    | (_, (first_pos, _)) :: _ ->
      let outer_pos = (first_pos, last_col_end) in
      let choices = List.map fst all_choices in
      [{ value = Expr ({ value = Choices ({ value = Some Params.dynamic_col_param_name; pos = outer_pos }, choices); pos = outer_pos }, None); pos = outer_pos }]
