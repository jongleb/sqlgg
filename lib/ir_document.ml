open Jsonkit.Primitives
open Ppx_deriving_jsonschema_runtime.Primitives.Jsonkit

type statement =
  | Parsed of {
      sql : string;
      ast : Sql.stmt;
      dialect_features : Dialect.dialect_support list;
    }
  | Checked of {
      sql : string;
      ast : Sql.stmt;
      analysis : Syntax.result;
    }
  | Invalid of {
      sql : string;
      diagnostics : Analysis.diagnostic list;
    }
[@@deriving show, eq, json, jsonschema]

type document = { statements : statement list }
[@@deriving show, eq, json, jsonschema]

let json_schema = lazy (Json_schema.of_deriving document_jsonschema)

let is_invalid = function
  | Invalid _ -> true
  | Parsed _ | Checked _ -> false

let parsed ({ sql; ast; dialect_features } : Analysis.parsed) =
  Parsed { sql; ast; dialect_features }

let invalid ~sql diagnostics = Invalid { sql; diagnostics }

let split_diagnostics (statement : Statements.t) =
  let offset = fst statement.pos in
  List.map
    (fun ((start, stop), message) ->
      { Analysis.message; pos = Some (start - offset, stop - offset) })
    statement.errors

let statement ?(allow_extensions = true) ~schema (statement : Statements.t) =
  let sql = statement.text in
  match split_diagnostics statement with
  | _ :: _ as diagnostics -> invalid ~sql diagnostics
  | [] ->
    begin match Analysis.parse ~allow_extensions sql, schema with
    | Error diagnostics, _ -> invalid ~sql diagnostics
    | Ok parsed_statement, None -> parsed parsed_statement
    | Ok { ast; _ }, Some schema ->
      begin match Analysis.analyze ~allow_extensions ~schema sql with
      | Ok analysis -> Checked { sql; ast; analysis }
      | Error diagnostics -> invalid ~sql diagnostics
      end
    end

let statements_of_sql ?(allow_extensions = true) ~schema sql =
  List.map (statement ~allow_extensions ~schema) (Statements.split sql)

let document_of_sql ?(allow_extensions = true) ~schema sql =
  { statements = statements_of_sql ~allow_extensions ~schema sql }
