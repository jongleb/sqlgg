open Sqlgg

type token_type = Parameter | Enum | Enum_member

val all_of_token_type : token_type list
val token_type_to_string : token_type -> string
val equal_token_type : token_type -> token_type -> bool

type kind =
  | Var of Sql.param_id * Sql.var
  | Branch of Sql.param_id * Sql.ctor

type placement = { cursor : Sql.Pos.t; token : Sql.Pos.t option }

type node = private {
  kind : kind;
  placement : placement option;
  children : node list;
}

val of_vars : base:int -> Sql.vars -> node list

val name : Sql.param_id -> string
val label : node -> string
val token_type : node -> token_type

type shape =
  | Scalar of Sql.Type.t
  | Row of Sql.Type.t list
  | Compound

val shape : node -> shape

val all_nodes : node list -> node Seq.t
val outline : node list -> node list
