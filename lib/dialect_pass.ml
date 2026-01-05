open Sql

let check_unsigned_type pos = function
  | Source_type.Infer Type.UInt64 -> 
      [Dialect.get_unsigned_types pos]
  | UInt32 ->
      [Dialect.get_unsigned_types pos]
  | _ -> []

let check_collation_opt (collation : string located option) =
  match collation with
  | Some { value; pos } -> [Dialect.get_collation value pos]
  | None -> []

let check_collated (c : _ collated) =
  check_collation_opt c.collation

let rec analyze_expr = function
  | Value _ | Param _ | Inparam _ | Column _ | Of_values _ -> []
  | Choices (_, choices) ->
      List.concat_map (fun (_, expr_opt) -> 
        Option.map_default analyze_expr [] expr_opt
      ) choices
  | InChoice (_, _, expr) -> analyze_expr expr
  | Fun { parameters; _ } ->
      List.concat_map analyze_expr parameters
  | SelectExpr (select_full, _) -> analyze_select_full select_full
  | InTupleList { value = { exprs; _ }; _ } -> List.concat_map analyze_expr exprs
  | OptionActions { choice; _ } -> analyze_expr choice
  | Case { case; branches; else_; } ->
      let case_features = Option.map_default analyze_expr [] case in
      let branches_features = List.concat_map (fun { when_; then_ } ->
        analyze_expr when_ @ analyze_expr then_
      ) branches in
      let else_features = Option.map_default analyze_expr [] else_ in
      case_features @ branches_features @ else_features

and analyze_column = function
  | All | AllOf _ -> []
  | Expr (expr, _) -> analyze_expr expr

and analyze_source src =
  match src with
  | `Table _ -> []
  | `Select select_full -> analyze_select_full select_full
  | `Nested nested -> analyze_nested nested
  | `ValueRows row_values -> analyze_row_values row_values

and analyze_row_values { row_constructor_list; row_order; row_limit = _ } =
  let constructor_features = match row_constructor_list with
  | RowExprList expr_lists ->
      List.concat_map (List.concat_map analyze_expr) expr_lists
  | RowParam _ -> []
  in
  let order_features = List.concat_map (fun (expr, _) -> analyze_expr expr) row_order in
  constructor_features @ order_features

and analyze_nested ((src_kind, _), joins) =
  let src_features = analyze_source src_kind in
  let joins_features = List.concat_map (fun { value = ((join_src_kind, _), join_typ, join_cond); pos } ->
    let subquery_features = match join_src_kind with
    | `Select select_full ->
        Dialect.get_join_source join_src_kind pos :: analyze_select_full select_full
    | _ -> []
    in
    let joined_features = analyze_source join_src_kind in
    let cond_features = match join_cond with
    | Schema.Join.On expr -> analyze_expr expr
    | Schema.Join.Using _ -> []
    | Schema.Join.Natural -> []
    | Schema.Join.Default -> []
    in
    let join_typ_features = match join_typ.value with
    | Schema.Join.Inner -> []
    | Straight -> [Dialect.get_straight_join join_typ.pos]
    | Left | Right | Full -> []
    in
    subquery_features @ joined_features @ cond_features @ join_typ_features
  ) joins in
  src_features @ joins_features

and analyze_select { columns; from; where; group; having } =
  let columns_features = List.concat_map analyze_column columns in
  let from_features = Option.map_default analyze_nested [] from in
  let where_features = Option.map_default analyze_expr [] where in
  let group_features = List.concat_map analyze_expr group in
  let having_features = Option.map_default analyze_expr [] having in
  columns_features @ from_features @ where_features @ group_features @ having_features

and analyze_select_complete { select; order; limit = _; select_row_locking } =
  let (core, others) = select in
  let core_features = analyze_select core in
  let others_features = List.concat_map (fun (_, select) -> analyze_select select) others in
  let order_features = List.concat_map (fun (expr, _) -> analyze_expr expr) order in
  let locking_features = match select_row_locking with
    | Some { value = For_share; pos } -> [Dialect.get_lock_in_share_mode pos]
    | Some { value = For_update; pos } -> [Dialect.get_row_locking pos]
    | None -> []
  in
  core_features @ others_features @ order_features @ locking_features

and analyze_select_full { select_complete; cte } =
  let select_features = analyze_select_complete select_complete in
  let cte_features = Option.map_default (fun { cte_items; _ } ->
    List.concat_map (fun { stmt; _ } ->
      match stmt with
      | CteInline select_complete -> analyze_select_complete select_complete
      | CteSharedQuery _ -> []
    ) cte_items
  ) [] cte in
  select_features @ cte_features

and analyze_assignment_expr = function
  | RegularExpr expr -> analyze_expr expr
  | WithDefaultParam (expr, _) -> analyze_expr expr
  | AssignDefault -> []

and analyze_assignments assignments =
  List.concat_map (fun (_, assignment_expr) -> analyze_assignment_expr assignment_expr) assignments

and analyze_column_def_internal (({ kind; extra; _ }: Alter_action_attr.t)) =
  let autoincrement_features = 
    let autoincrement = List.find_opt (fun c ->
      match c.value with 
      | Alter_action_attr.Syntax_constraint Autoincrement -> true 
      | _ -> false
    ) extra in
    match autoincrement with
    | Some { pos; _ } -> [Dialect.get_autoincrement pos]
    | None -> []
  in
  let default_features = extra
    |> List.find_map (function
        | { value = Alter_action_attr.Default { value = expr; pos }; _ } ->
            let col_kind = Option.map (fun k -> k.value.collated) kind in
            Some [Dialect.get_default_expr ~kind:col_kind ~expr pos]
        | _ -> None)
    |> Option.default []
  in
  let collation_features = kind |> Option.map (fun k -> k.value) |> Option.map_default check_collated [] in
  let unsigned_features = match kind with
    | Some { pos; value = { collated; _ } } -> check_unsigned_type pos collated
    | None -> []
  in
  autoincrement_features @ default_features @ collation_features @ unsigned_features

and analyze_alter_action : alter_action -> Dialect.dialect_support list = function
  | `Add (col, _) -> analyze_column_def_internal col
  | `Change (_, col, _) -> analyze_column_def_internal col
  | `Default_or_convert_to collation -> check_collation_opt collation
  | `Drop _ | `RenameTable _ | `RenameColumn _ | `RenameIndex _ | `None -> []

and analyze_insert_action { action; on_conflict_clause; insert_action_kind; _ } =
  let action_features = match action with
  | `Values (_, Some values) ->
      List.concat_map (List.concat_map analyze_assignment_expr) values
  | `Values (_, None) -> []
  | `Param _ -> []
  | `Select (_, select_full) -> analyze_select_full select_full
  | `Set assignments -> Option.map_default analyze_assignments [] assignments
  in
  let conflict_features = match on_conflict_clause with
  | Some ({ value = On_duplicate { assignments; }; pos }) ->
      Dialect.get_on_duplicate_key pos :: analyze_assignments assignments
  | Some ({ value = On_conflict { action = Do_update assignments; _ }; pos }) ->
      Dialect.get_on_conflict pos :: analyze_assignments assignments
  | Some ({ value = On_conflict { action = Do_nothing; _ }; pos }) ->
      [Dialect.get_on_conflict pos]
  | None -> []
  in
  let replace_into_features = match insert_action_kind with
  | Replace_into pos -> [Dialect.get_replace_into pos]
  | Insert_into -> []
  in
  action_features @ conflict_features @ replace_into_features

let analyze_schema_index idx = match idx.value with
  | Regular_idx -> None
  | Fulltext -> Some (Dialect.get_fulltext_index idx.pos)
  | Spatial -> None

let rec analyze = function 
  | Sql.Create (_, Schema { schema; indexes; _ }) ->
    let schema_idx_features = List.filter_map analyze_schema_index indexes in
    schema_idx_features @ List.concat_map analyze_column_def_internal schema
  | Create (_, Select { value = select; pos }) ->
    Dialect.get_create_table_as_select pos :: analyze_select_full select
  | Drop _ -> []
  | Alter (_, actions) ->
      List.concat_map analyze_alter_action actions
  | Rename _ -> []
  | CreateIndex (_, _, cols) -> List.concat_map check_collated cols
  | Insert insert_action ->
      analyze_insert_action insert_action
  | Delete (_, where_opt) ->
      Option.map_default analyze_expr [] where_opt
  | DeleteMulti (_, nested, where_opt) ->
      let nested_features = analyze_nested nested in
      let where_features = Option.map_default analyze_expr [] where_opt in
      nested_features @ where_features
  | Set (assignments, stmt_opt) ->
      let assignments_features = List.concat_map (fun (_, expr) -> analyze_expr expr) assignments in
      let stmt_features = Option.map_default analyze [] stmt_opt in
      assignments_features @ stmt_features
  | Update (_, assignments, where_opt, order, _) ->
      let assignments_features = analyze_assignments assignments in
      let where_features = Option.map_default analyze_expr [] where_opt in
      let order_features = List.concat_map (fun (expr, _) -> analyze_expr expr) order in
      assignments_features @ where_features @ order_features
  | UpdateMulti (nesteds, assignments, where_opt, order, _) ->
      let nesteds_features = List.concat_map analyze_nested nesteds in
      let assignments_features = analyze_assignments assignments in
      let where_features = Option.map_default analyze_expr [] where_opt in
      let order_features = List.concat_map (fun (expr, _) -> analyze_expr expr) order in
      nesteds_features @ assignments_features @ where_features @ order_features
  | Select select_full ->
      analyze_select_full select_full
  | CreateRoutine (_, kind, params) ->
      let kind_collation = kind |> Option.map (fun k -> k.value) |> Option.map_default check_collated [] in
      let kind_unsigned = match kind with
        | Some { pos; value = { collated; _ } } -> check_unsigned_type pos collated
        | None -> []
      in
      let params_features = List.concat_map (fun (_, typ, default_expr_opt) ->
        let typ_collation = check_collated (typ.value) in
        let typ_unsigned = check_unsigned_type typ.pos typ.value.collated in
        typ_collation @ typ_unsigned @ Option.map_default analyze_expr [] default_expr_opt
      ) params in
      kind_collation @ kind_unsigned @ params_features

    