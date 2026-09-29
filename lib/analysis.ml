open Jsonkit.Primitives
open Ppx_deriving_jsonschema_runtime.Primitives.Jsonkit

type diagnostic = {
  message : string;
  pos : Sql.Pos.t option;
}
[@@deriving show, eq, json, jsonschema]

type parsed = {
  sql : string;
  ast : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
}
[@@deriving show, eq, json, jsonschema]

type 'a outcome = ('a, diagnostic list) result

let nonempty ((start, stop) as pos) =
  if Int.compare stop start <= 0 then None else Some pos

let rec expected_error = function
  | Sql_parser.Error -> true
  | Sql_lexer.Error _ -> true
  | Sql.Schema.Error _ -> true
  | Prelude.Sql_error _ -> true
  | Prelude.At (_, exn) -> expected_error exn
  | _ -> false

let diagnostic_of_parser_error exn info =
  let pos =
    match exn with
    | Sql_lexer.Error (_, pos) -> nonempty pos
    | Prelude.At (pos, _) -> nonempty pos
    | _ -> Some info.Parser_utils.pos
  in
  {
    message = Parser_utils.message_of_exn exn;
    pos;
  }

let diagnostic_of_exn exn =
  let pos =
    match exn with
    | Sql_lexer.Error (_, pos) -> nonempty pos
    | Prelude.At (pos, _) -> nonempty pos
    | _ -> None
  in
  {
    message = Parser_utils.message_of_exn exn;
    pos;
  }

let rebase_diagnostic offset diagnostic =
  let pos =
    Option.map
      (fun (start, stop) -> offset + start, offset + stop)
      diagnostic.pos
  in
  { diagnostic with pos }

let protect_state f =
  let previous = Compile.snapshot () in
  Fun.protect ~finally:(fun () -> Compile.restore previous) f

let extension_diagnostic ~pos what =
  { message = "sqlgg extension not allowed: " ^ what; pos = Some pos }

(* Walks the whole stmt: [Sql.expr_exists] does not enter [SelectExpr] bodies. *)
module Extensions = struct
  open Sql

  let rec collect_expr acc = function
    | Param (p, _) ->
      extension_diagnostic ~pos:p.id.pos
        "query parameter (@name / ?)" :: acc
    | Inparam (p, _) ->
      extension_diagnostic ~pos:p.id.pos
        "IN-list parameter" :: acc
    | Choices (id, choices) ->
      let acc =
        extension_diagnostic ~pos:id.pos
          "@choice { ... }" :: acc
      in
      List.fold_left
        (fun acc (c : _ choice) ->
          Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) c.body)
        acc choices
    | InChoice (id, _, e) ->
      let acc =
        extension_diagnostic ~pos:id.pos
          "IN @choice" :: acc
      in
      collect_expr acc e
    | InTupleList { value = { param_id = _; exprs; _ }; pos; _ } ->
      let acc =
        extension_diagnostic ~pos
          "tuple IN @list" :: acc
      in
      List.fold_left collect_expr acc exprs
    | OptionActions { choice; pos = (p1, p2); _ } ->
      let acc =
        extension_diagnostic ~pos:(fst p1, snd p2)
          "optional fragment { ... }?" :: acc
      in
      collect_expr acc choice
    | SelectExpr (sf, _) -> collect_select_full acc sf
    | Fun { kind; parameters; _ } ->
      let acc = List.fold_left collect_expr acc parameters in
      begin match kind with
      | Agg (With_order { order; _ }) -> collect_order acc order
      | _ -> acc
      end
    | Case { value = { case; branches; else_ }; _ } ->
      let acc = Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) case in
      let acc =
        List.fold_left
          (fun acc (b : case_branch) ->
            collect_expr (collect_expr acc b.when_) b.then_)
          acc branches
      in
      Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) else_
    | Value _ | Column _ | Of_values _ -> acc

  and collect_order acc order =
    List.fold_left
      (fun acc (e, dir) ->
        let acc = collect_expr acc e in
        match dir with
        | Some (`Param id) ->
          extension_diagnostic ~pos:id.pos
            "dynamic ORDER BY direction" :: acc
        | Some `Fixed | None -> acc)
      acc order

  and collect_limit acc = function
    | None -> acc
    | Some (params, _) ->
      List.fold_left
        (fun acc (p : _ param) ->
          extension_diagnostic ~pos:p.id.pos
            "LIMIT/OFFSET parameter" :: acc)
        acc params

  and collect_join_condition acc = function
    | Schema.Join.On e -> collect_expr acc e
    | Schema.Join.Default | Natural | Using _ -> acc

  and collect_source_kind acc = function
    | `Select sf -> collect_select_full acc sf
    | `Table _ -> acc
    | `Nested n -> collect_nested acc n
    | `ValueRows rv -> collect_row_values acc rv

  and collect_source acc (kind, _) = collect_source_kind acc kind

  and collect_nested acc (base, joins) =
    let acc = collect_source acc base in
    List.fold_left
      (fun acc { value = (src, _, cond); _ } ->
        collect_join_condition (collect_source acc src) cond)
      acc joins

  and collect_select acc (s : select) =
    let acc =
      List.fold_left
        (fun acc (col : column) ->
          match col.value with
          | All | AllOf _ -> acc
          | Expr (e, _) -> collect_expr acc e.value)
        acc s.columns
    in
    let acc = Stdlib.Option.fold ~none:acc ~some:(collect_nested acc) s.from in
    let acc = Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) s.where in
    let acc = List.fold_left collect_expr acc s.group in
    Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) s.having

  and collect_select_complete acc (sc : select_complete) =
    let first, rest = sc.select in
    let acc = collect_select acc first in
    let acc =
      List.fold_left (fun acc (_, s) -> collect_select acc s) acc rest
    in
    let acc = collect_order acc sc.order in
    collect_limit acc sc.limit

  and collect_select_full acc { select_complete; cte } =
    let acc =
      match cte with
      | None -> acc
      | Some { cte_items; _ } ->
        List.fold_left
          (fun acc item ->
            match item.stmt with
            | CteInline sc -> collect_select_complete acc sc
            | CteSharedQuery _ -> acc)
          acc cte_items
    in
    collect_select_complete acc select_complete

  and collect_row_values acc rv =
    let acc =
      match rv.row_constructor_list with
      | RowExprList rows ->
        List.fold_left (List.fold_left collect_expr) acc rows
      | RowParam { id; _ } ->
        extension_diagnostic ~pos:id.pos
          "VALUES row parameter" :: acc
    in
    let acc = collect_order acc rv.row_order in
    collect_limit acc rv.row_limit

  let collect_assignment_expr acc = function
    | RegularExpr e -> collect_expr acc e
    | AssignDefault -> acc
    | WithDefaultParam (e, (p1, p2)) ->
      let acc =
        extension_diagnostic ~pos:(fst p1, snd p2)
          "default-with-param assignment" :: acc
      in
      collect_expr acc e

  let collect_assignments acc = List.fold_left (fun acc (_, e) -> collect_assignment_expr acc e) acc

  let collect_insert_action acc (ia : insert_action) =
    let acc =
      match ia.action with
      | `Set None -> acc
      | `Set (Some assigns) -> collect_assignments acc assigns
      | `Values (_, None) -> acc
      | `Values (_, Some rows) ->
        List.fold_left (List.fold_left collect_assignment_expr) acc rows
      | `Param (_, id) ->
        extension_diagnostic ~pos:id.pos
          "INSERT ... VALUES @param" :: acc
      | `Select (_, sf) -> collect_select_full acc sf
    in
    match ia.on_conflict_clause with
    | None -> acc
    | Some { value = On_duplicate { assignments }; _ } ->
      collect_assignments acc assignments
    | Some { value = On_conflict { action; _ }; _ } ->
      begin match action with
      | Do_nothing -> acc
      | Do_update assigns -> collect_assignments acc assigns
      end

  let collect_alter_attr acc (attr : Alter_action_attr.t) =
    List.fold_left
      (fun acc (c : Alter_action_attr.constraint_ located) ->
        match c.value with
        | Alter_action_attr.Default { expr; _ } -> collect_expr acc expr.value
        | Syntax_constraint _ -> acc)
      acc attr.extra

  let collect_alter_action acc = function
    | `Add (attr, _) | `Change (_, attr, _) -> collect_alter_attr acc attr
    | `AlterColumnPG (_, { value; _ }) ->
      begin match value with
      | Alter_column_pg.Set_type _ | Set_not_null | Drop_not_null
      | Set_default | Drop_default -> acc
      end
    | `RenameTable _ | `RenameColumn _ | `RenameIndex _ | `Drop _
    | `AddIndex _ | `DropIndex _ | `AddPrimaryKey _ | `DropPrimaryKey
    | `AddConstraint _ | `DropConstraint _ | `Default_or_convert_to _
    | `TtlOptions _ | `RemoveTtl _ | `Cache _ | `NoCache _ -> acc

  let rec collect_stmt acc = function
    | Select sf -> collect_select_full acc sf
    | Insert ia -> collect_insert_action acc ia
    | Delete (_, where) -> Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) where
    | DeleteMulti (_, nested, where) ->
      let acc = collect_nested acc nested in
      Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) where
    | Update (_, assigns, where, order, limit_params) ->
      let acc = collect_assignments acc assigns in
      let acc = Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) where in
      let acc = collect_order acc order in
      collect_limit acc (Some (limit_params, false))
    | UpdateMulti (nested, assigns, where, order, limit_params) ->
      let acc = collect_nested acc nested in
      let acc = collect_assignments acc assigns in
      let acc = Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) where in
      let acc = collect_order acc order in
      collect_limit acc (Some (limit_params, false))
    | Set (assigns, rest) ->
      let acc = List.fold_left (fun acc (_, e) -> collect_expr acc e) acc assigns in
      Stdlib.Option.fold ~none:acc ~some:(collect_stmt acc) rest
    | Create (_, Schema { schema; _ }) ->
      List.fold_left collect_alter_attr acc schema
    | Create (_, Select { value = sf; _ }) -> collect_select_full acc sf
    | Alter { alter_actions; _ } ->
      List.fold_left collect_alter_action acc alter_actions
    | CreateRoutine (_, _, params) ->
      List.fold_left
        (fun acc (_, _, default) ->
          Stdlib.Option.fold ~none:acc ~some:(collect_expr acc) default)
        acc params
    | Drop _ | Rename _ | CreateIndex _ | CreateType _ | DropType _
    | CreateExtension _ | DropExtension _ -> acc

  let of_stmt stmt = List.rev (collect_stmt [] stmt)
end

let check_extensions ~allow_extensions ast =
  if allow_extensions then Ok ()
  else
    match Extensions.of_stmt ast with
    | [] -> Ok ()
    | diags -> Error diags

let parse ?(allow_extensions = true) sql =
  Prelude.with_sql_errors @@ fun () ->
  match Parser.parse_stmt sql with
  | { stmt = ast; dialect_features } ->
    begin match check_extensions ~allow_extensions ast with
    | Ok () -> Ok { sql; ast; dialect_features }
    | Error diags -> Error diags
    end
  | exception Parser_utils.Error (exn, info) when expected_error exn ->
    Error [diagnostic_of_parser_error exn info]

module Schema = struct
  type t = Compile.state

  let current () = Compile.snapshot ()

  let of_sql sql =
    Prelude.with_sql_errors @@ fun () ->
    protect_state @@ fun () ->
    Compile.reset ();
    let statements = Statements.split sql in
    let split_diagnostics =
      List.concat_map
        (fun (statement : Statements.t) ->
          List.map
            (fun (pos, message) -> { message; pos = Some pos })
            statement.errors)
        statements
    in
    match split_diagnostics with
    | _ :: _ -> Error split_diagnostics
    | [] ->
      let rec compile = function
        | [] -> Ok (Compile.snapshot ())
        | (statement : Statements.t) :: rest ->
          let offset = fst statement.pos in
          match Compile.statement ~dynamic_select:Props.Off statement with
          | _ -> compile rest
          | exception Parser_utils.Error (exn, info) when expected_error exn ->
            Error
              [rebase_diagnostic
                 offset
                 (diagnostic_of_parser_error exn info)]
          | exception exn when expected_error exn ->
            Error [rebase_diagnostic offset (diagnostic_of_exn exn)]
      in
      compile statements
end

let analyze ?(allow_extensions = true) ~schema sql =
  Prelude.with_sql_errors @@ fun () ->
  protect_state @@ fun () ->
  Compile.restore schema;
  match Parser.parse_stmt sql with
  | parsed ->
    begin match check_extensions ~allow_extensions parsed.stmt with
    | Error diags -> Error diags
    | Ok () ->
      begin
        match Syntax.eval_parsed sql parsed with
        | result -> Ok result
        | exception exn when expected_error exn ->
          Error [diagnostic_of_exn exn]
      end
    end
  | exception Parser_utils.Error (exn, info) when expected_error exn ->
    Error [diagnostic_of_parser_error exn info]
