open Sqlgg

type error = private { pos : Sql.Pos.t; msg : string }

type success = private {
  kind : Stmt.kind;
  schema : Sql.schema;
  params : Params.node list;
  dialect_errors : error list;
  new_table : Symbol.t option;
}

type outcome = private Skip | Error of error | Ok of success

type scope = private {
  symbols : Symbol.t list;
  aliases : Sql.table_alias list;
}

type stmt = private {
  pos : Sql.Pos.t;
  name : string option;
  scope : scope;
  select_scopes : (scope * Sql.Pos.t) list;
  exprs : (Sql.Type.t * Sql.Pos.t) list;
  outcome : outcome;
}

val errors : stmt -> error list
val params : stmt -> Params.node list
val select_scope_at_opt : stmt -> int -> scope option
val scope_at : stmt -> int -> scope

type checked_statement = private {
  block : Statements.t;
  stmt : stmt;
}

type t = private {
  path : string;
  statements : checked_statement list;
  index : Symbol.t Symbol.Index.t;
  snapshot : Compile.state;
}

val find_reusable_opt : t -> string -> (stmt * Symbol.loc) option

module Cache : sig
  type t
  val create : unit -> t
  val forget : t -> string -> unit
end

val analyze : ?cache:Cache.t -> path:string -> string -> t
val check_at : t -> Statements.t -> stmt
