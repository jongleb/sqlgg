(** The deriver embeds a full copy of a recursive group at every use site, as a
    resource with its own ["$id"] and ["$defs"]. That reaches several megabytes
    for the sqlgg AST, and two uses on one source line share an ["$id"] while
    pointing at different entries, which JSON Schema forbids. Merging every
    ["$defs"] into the root and dropping the ["$id"]s resolves both. *)
val bundle : Yojson.Basic.t -> Yojson.Basic.t

val of_deriving : Ppx_deriving_jsonschema_runtime.t -> Yojson.Basic.t

val stmt : Yojson.Basic.t Lazy.t
