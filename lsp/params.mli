open Sqlgg

type token_type = Parameter | Enum | Enum_member

val all_of_token_type : token_type list
val token_type_to_string : token_type -> string

type kind =
  | Var of Sql.var
  | Branch of Sql.ctor

type placement = { cursor : Sql.Pos.t; token : Sql.Pos.t option }

type node = private {
  param : Sql.param_id;
  kind : kind;
  placement : placement option;
  children : node list;
}

val of_vars : base:int -> Sql.vars -> node list

val name : Sql.param_id -> string
val label : node -> string
val token_type : node -> token_type

val shape :
  node -> [ `Scalar of Sql.Type.t | `Row of Sql.Type.t list | `Compound ]

val all_nodes : node list -> node Seq.t
val outline : node list -> node list
