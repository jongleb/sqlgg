type t

val create : Document.t -> offset:int -> t option
val hover : t -> (string * Sqlgg.Sql.Pos.t) option
val definition : t -> Symbol.loc list

type token = { pos : Sqlgg.Sql.Pos.t; typ : Params.token_type }

val semantic_tokens : lines:Line_index.t -> Document.t -> token list
