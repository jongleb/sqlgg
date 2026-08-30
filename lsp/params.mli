open Sqlgg

type token_type = Parameter | Enum | Enum_member

val all_of_token_type : token_type list
val token_type_to_enum : token_type -> int
val token_type_to_string : token_type -> string

type kind =
  | Var of Sql.param_id * Sql.var
  | Branch of Sql.param_id * Sql.ctor

type node = private {
  base : int;
  kind : kind;
  children : node list;
}

val of_vars : base:int -> Sql.vars -> node list
val token_pos : node -> Sql.Pos.t option

val name : Sql.param_id -> string
val label : node -> string

type shape =
  | Scalar of Sql.Type.t
  | List of Sql.Type.t list
  | Compound

val shape : node -> shape

val all_nodes : node list -> node Seq.t
val outline : node list -> node list
val find_node_opt :
  node list -> int -> f:(node -> 'a option) -> ('a * Sql.Pos.t) option
