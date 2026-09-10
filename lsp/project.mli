open Sqlgg

type t = {
  schema_files : string list;
  dialect : Dialect.t;
  watch_paths : string list;
}

val default : t

val locate : string -> string option

val load : string -> t
