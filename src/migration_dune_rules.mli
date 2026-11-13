(** Dune integration of [sqlgg migrations generate], as plain Dune stanzas.

    Every build regenerates the migrations module from a copy of the migration
    log. When the schema changed, the copy differs from the committed log:
    [dune build @sqlgg-migrations] (also part of [@check]) reports the diff
    and [dune promote] accepts the new migrations. *)

val render :
  Migrations_command.generation ->
  generated_file:string ->
  string
