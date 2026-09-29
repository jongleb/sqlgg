(** The [sqlgg-ir] output document: one entry per input statement, in input
    order. Positions are relative to each statement's own [sql]. *)

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

val json_schema : Yojson.Basic.t Lazy.t

val is_invalid : statement -> bool

(** Input never extends [~schema]. [~allow_extensions:false] rejects sqlgg
    extensions ([@param], [@choice], [{...}?], tuple lists). *)
val statements_of_sql :
  ?allow_extensions:bool ->
  schema:Analysis.Schema.t option ->
  string ->
  statement list

val document_of_sql :
  ?allow_extensions:bool ->
  schema:Analysis.Schema.t option ->
  string ->
  document
