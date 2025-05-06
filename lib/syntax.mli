
val debug : bool ref

val parse : Stmt_elements.t -> string * Sql.Schema.t * Sql.var list * Stmt.kind

val eval_select: Sql.select_full -> Sql.Schema.t * Sql.vars * Stmt.kind
