open Sqlgg

module Index : Map.S with type key = string

type loc = private {
  file : string;
  pos : Sql.Pos.t;
}

type kind =
  | Table
  | Cte
  | Local

type column = private {
  attr : Sql.attr;
  loc : loc option;
}

type t = private {
  name : string;
  kind : kind;
  loc : loc option;
  columns : column list;
}

val loc : file:string -> Sql.Pos.t -> loc
val column : ?loc:loc -> Sql.attr -> column
val make : name:string -> kind:kind -> ?loc:loc -> column list -> t
val rename : string -> t -> t

val columns : t -> Sql.schema
val find_column_opt : t -> string -> column option
val find_opt : t list -> string -> t option
val unique : t list -> t list
