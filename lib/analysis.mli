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

module Schema : sig
  type t

  val of_sql : string -> t outcome
  val current : unit -> t
end

val parse : ?allow_extensions:bool -> string -> parsed outcome
val analyze : ?allow_extensions:bool -> schema:Schema.t -> string -> Syntax.result outcome
