type metadata = (string * string) list

type t = {
  text : string;
  props : Props.t list;
  pos : Sql.Pos.t;
  metadata : (int * metadata) list;
  comments : Sql.Pos.t list;
  errors : (Sql.Pos.t * string) list;
}

type lexeme = [
  | `Text
  | `Literal
  | `Open_literal
  | `Blank
  | `Semicolon
  | `Comment
  | `Props of (string * string) list
  | `Bad_props
]

val lexemes : string -> (Sql.Pos.t * lexeme) Seq.t
val split : string -> t list
val glue_downs : t list -> t list
