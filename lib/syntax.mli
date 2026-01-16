
module Config: sig
  val debug : bool ref
  val allow_write_notnull_null : bool ref
end

type parse_result = {
  sql: string;
  schema: Sql.Schema.t;
  vars: Sql.var list;
  kind: Stmt.kind;
  dialect_features: Dialect.dialect_support list;
  from_pos: int option; (** Position of FROM keyword, for dynamic_select *)
}

val parse : string -> parse_result

val eval_select: Sql.select_full -> Sql.Schema.t * Sql.vars * Stmt.kind
