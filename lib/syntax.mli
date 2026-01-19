
module Config: sig
  val debug : bool ref
  val allow_write_notnull_null : bool ref
  val dynamic_select: bool ref
end

type parse_result = {
  sql: string;
  schema: Sql.Schema.t;
  vars: Sql.var list;
  kind: Stmt.kind;
  dialect_features: Dialect.dialect_support list;
}

val parse : string -> parse_result

val eval_select: Sql.select_full -> Sql.Schema.t * Sql.vars * Stmt.kind
