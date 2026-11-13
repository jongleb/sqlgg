type t

type error = [
  | `Invalid of string
  | `Too_long of int
]

(** The longest id, in bytes: the width of the state table's id column. *)
val max_length : int

val parse : string -> (t, error) result
val to_string : t -> string
val compare : t -> t -> int
val error_to_string : error -> string
