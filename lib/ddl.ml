
open ExtLib
open Prelude
open Sql

let with_constraints attrs constraints : Schema.t =
  let constraints_table : (string, Constraints.t) Hashtbl.t = Hashtbl.create (List.length attrs) in
  let inherited : (string, Meta.t option) Hashtbl.t = Hashtbl.create (List.length attrs) in
  constraints |> List.iter begin function
    | `Foreign (cols, table, refs) ->
      let referenced = Tables.with_stored table [] (fun t -> t.columns) in
      let refs = match refs with [] -> Tables.get_primary_key_columns referenced | refs -> refs in
      if List.compare_lengths cols refs = 0 then
        List.iter2 (fun col name ->
          let meta =
            Stdlib.Option.bind (Tables.find_column ~name referenced)
              (fun (c : Tables.column) -> Meta.declared (Meta.of_domain c.attr.meta))
          in
          Hashtbl.replace inherited col (Meta.common (Hashtbl.find_default inherited col meta) meta)
        ) cols refs
    | `Primary _ | `Unique _ | `Ignore -> ()
  end;
  List.iter (fun attr ->
    Hashtbl.replace constraints_table attr.name attr.extra
  ) attrs;
  List.iter (fun constr ->
    match constr with
    | `Primary [] -> fail "Schema Error: PRIMARY KEY must have at least one column"
    | `Unique (_, []) -> fail "Schema Error: UNIQUE constraint must have at least one column"
    | `Primary [ col_name ] -> begin
      match Hashtbl.find_opt constraints_table col_name with
      | None -> fail "Schema Error: no such column: %s" col_name
      | Some constraints ->
        let new_constraints = Constraints.add PrimaryKey constraints in
        Hashtbl.replace constraints_table col_name new_constraints
      end
    | `Unique (_, [ col_name ]) -> begin
      match Hashtbl.find_opt constraints_table col_name with
      | None -> fail "Schema Error: no such column: %s" col_name
      | Some constraints ->
        let new_constraints = Constraints.add Unique constraints in
        Hashtbl.replace constraints_table col_name new_constraints
      end
    | `Primary cols -> begin
      List.iter (fun col ->
        match Hashtbl.find_opt constraints_table col with
        | None -> fail "Schema Error: no such column: %s" col
        | Some constraints ->
          let new_constraints = Constraints.add (Constraint.make_composite_primary cols) constraints in
          Hashtbl.replace constraints_table col new_constraints
      ) cols
    end
    | `Unique (_, cols) -> begin
      List.iter (fun col ->
        match Hashtbl.find_opt constraints_table col with
        | None -> fail "Schema Error: no such column: %s" col
        | Some constraints ->
          let new_constraints = Constraints.add (Constraint.make_composite_unique cols) constraints in
          Hashtbl.replace constraints_table col new_constraints
      ) cols
    end
    | `Foreign _ | `Ignore -> ()
  ) constraints;
  List.map (fun attr ->
    { attr with
      extra = Option.default attr.extra (Hashtbl.find_opt constraints_table attr.name);
      meta = Meta.merge_right (Meta.of_option (Hashtbl.find_default inherited attr.name None)) attr.meta }
  ) attrs

let create name schema constraints indexes =
  let attrs = List.map Alter_action_attr.to_attr schema in
  let attrs = with_constraints attrs constraints in
  let columns = List.map2 (fun (col : Alter_action_attr.t) attr ->
    {
      Tables.attr;
      source_kind = Option.map (fun k -> k.value) col.kind;
      default_sql = Alter_action_attr.default_sql col;
    }
  ) schema attrs in
  Tables.add_columns (name, columns);
  Tables.add_inline_indexes name ~indexes ~constraints

let alter name actions =
  List.iter (function
  | `Add (col,pos) ->
    let source_kind = Option.map (fun k -> k.value) col.Alter_action_attr.kind in
    let default_sql = Alter_action_attr.default_sql col in
    Tables.alter_add name ~col:{ attr = Alter_action_attr.to_attr col; source_kind; default_sql } ~pos
  | `Drop col ->
    Tables.alter_drop name ~col
  | `Change (oldcol,col,pos) ->
    let source_kind = Option.map (fun k -> k.value) col.Alter_action_attr.kind in
    let default_sql = Alter_action_attr.default_sql col in
    Tables.alter_change name ~oldcol ~col:{ attr = Alter_action_attr.to_attr col; source_kind; default_sql } ~pos
  | `RenameColumn (oldcol,newcol) ->
    Tables.rename_column name ~old_name:oldcol ~new_name:newcol
  | `RenameTable new_name ->
    Tables.rename name new_name
  | `DropPrimaryKey ->
    Tables.drop_primary_key name
  | `AddPrimaryKey cols ->
    Tables.add_primary_key name ~cols
  | `AlterColumnPG (col_name, change) ->
    Tables.alter_column_pg name ~col_name change.value
  | `AddIndex { add_idx_name = Some index_name; add_idx_kind = kind; add_idx_cols = cols } ->
    Tables.index_add name ~index_name ~kind ~cols
  | `AddIndex { add_idx_name = None; add_idx_kind = kind; add_idx_cols = cols } ->
    Tables.index_add_auto name ~kind ~cols
  | `DropIndex index_name ->
    Tables.index_drop name ~index_name
  | `RenameIndex (old_name, new_name) ->
    Tables.index_rename name ~old_name ~new_name
  | `AddConstraint _ | `DropConstraint _ -> ()
  | `TtlOptions (opts, _) ->
    let expr, enabled =
      List.fold_left (fun (expr, enabled) -> function
        | `TtlSet (col, n, unit) -> Some (col, n, String.uppercase_ascii unit), enabled
        | `TtlEnable v -> expr, Some (String.uppercase_ascii v <> "OFF"))
        (None, None) opts
    in
    let prev = Tables.get_ttl name in
    let ttl_enabled =
      Option.default (Option.map_default (fun (t : Tables.table_ttl) -> t.ttl_enabled) true prev) enabled
    in
    Tables.set_ttl name @@
      Option.map_default
        (fun (ttl_col, ttl_n, ttl_unit) -> Some { Tables.ttl_col; ttl_n; ttl_unit; ttl_enabled })
        (Option.map (fun (t : Tables.table_ttl) -> { t with ttl_enabled }) prev)
        expr
  | `RemoveTtl _ -> Tables.set_ttl name None
  | `Default_or_convert_to (cs, collation) ->
    let old = Tables.get_charset name in
    let collation =
      match Option.map (fun c -> c.value) collation with
      | Some _ as c -> c
      | None -> Stdlib.Option.bind old (fun o -> o.Tables.collation)
    in
    Tables.set_charset name { charset = cs; collation }) actions

let rename l = List.iter (fun (o, n) -> Tables.rename o n) l

let drop name = Tables.drop name

let create_index ~name ~table ~cols ~kind =
  let cols = List.map (fun x -> x.collated) cols in
  Schema.project cols (Tables.get_schema table) |> ignore;
  Tables.index_add table ~index_name:name ~kind ~cols

let create_type name ctors = User_types.add name (Type.make_enum_kind ctors)

let drop_type ~if_exists name = User_types.drop ~if_exists name

let create_routine name ret params =
  match ret with
  | None -> ()
  | Some (r : Source_type.kind collated located) ->
    let ret = Hmx_of_sql.of_kind (Source_type.kind_to_type_kind r.value.collated) in
    Hmx_sig.declare name.tn (List.length params)
      (Hmx_sig.make ?ret (Args (List.map (fun _ -> Hmx_sig.Free) params)))
