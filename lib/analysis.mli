type diagnostic = {
  message : string;
  pos : Sql.Pos.t option;
}

type parsed = {
  sql : string;
  ast : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
}

type 'a outcome = ('a, diagnostic list) result

module Schema : sig
  type t

  val of_sql : string -> t outcome
  val current : unit -> t
end

val parse : string -> parsed outcome
val analyze : schema:Schema.t -> string -> Syntax.result outcome
