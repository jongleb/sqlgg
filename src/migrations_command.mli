(** Command line of [sqlgg migrations]: migrations for a schema file, with
    the state table, runner and command line provided by sqlgg. *)

(** Host languages that have a migration runtime. *)
type host_language = Ocaml

type generation = {
  dialect : [ `MySQL | `TiDB ];
  host_language : host_language;
  schema_file : string;
  migrations_file : string;
  manual_file : string option;
  module_name : string;
  now : int option;
}

type request =
  | Generate of generation
  | Print_dune_rules of {
      generation : generation;
      generated_file : string;
    }

(** [parse argv] reads the arguments that follow [sqlgg]; [argv.(0)] is
    ["migrations"]. Usage errors and help are printed here, leaving only an
    exit code to the caller. *)
val parse : string array -> [ `Request of request | `Exit of int ]

(** The arguments that make [sqlgg migrations generate] run this generation,
    for callers that write the command into a build file. *)
val generate_arguments : generation -> string list
