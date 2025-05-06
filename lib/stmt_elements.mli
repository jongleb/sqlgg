type t

val add_char:char -> t -> unit

val add_string: string -> t -> unit

val add_prop: string * string -> t -> unit

val queue_byte_feeder: feeder_queue:t -> bytes -> int -> int

val elements_to_sql: t -> string

val create: unit -> t
