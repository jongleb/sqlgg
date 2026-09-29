(** [sqlgg-ir]: emit the full parsed / checked SQL AST as one JSON document. *)

open Sqlgg

let usage =
  "Usage: sqlgg-ir [--schema FILE] [--plain-sql] [FILE ...]\n\
  \       sqlgg-ir --json-schema\n\
   Read SQL from FILE arguments (stdin when none or '-') and print one JSON\n\
   document with the full AST of every statement. With --schema, statements\n\
   are also checked against the schema. --plain-sql rejects sqlgg extensions.\n\
   Exit 1 if any statement is invalid, 2 on usage or I/O errors."

let die fmt =
  Printf.ksprintf
    (fun message ->
      prerr_endline ("sqlgg-ir: " ^ message);
      exit 2)
    fmt

type mode =
  | Json_schema
  | Analyze of {
      schema : string option;
      plain_sql : bool;
      files : string list;
    }

let parse_args args =
  let rec loop ~json_schema ~schema ~plain_sql files = function
    | [] ->
      begin match json_schema, schema, files with
      | true, None, [] when not plain_sql -> Json_schema
      | true, _, _ -> die "--json-schema takes no --schema, --plain-sql, or input files"
      | false, schema, files ->
        Analyze { schema; plain_sql; files = List.rev files }
      end
    | ("--help" | "-h") :: _ ->
      print_endline usage;
      exit 0
    | "--json-schema" :: rest ->
      loop ~json_schema:true ~schema ~plain_sql files rest
    | "--plain-sql" :: rest ->
      loop ~json_schema ~schema ~plain_sql:true files rest
    | "--schema" :: file :: rest when Option.is_none schema ->
      loop ~json_schema ~schema:(Some file) ~plain_sql files rest
    | "--schema" :: _ :: _ -> die "--schema given more than once"
    | [ "--schema" ] -> die "--schema requires a FILE argument"
    | arg :: _ when String.length arg > 1 && Char.equal arg.[0] '-' ->
      die "unknown option: %s\n%s" arg usage
    | file :: rest -> loop ~json_schema ~schema ~plain_sql (file :: files) rest
  in
  loop ~json_schema:false ~schema:None ~plain_sql:false [] args

let read_path = function
  | "-" -> In_channel.input_all stdin
  | path ->
    try In_channel.with_open_bin path In_channel.input_all with
    | Sys_error message -> die "%s" message

let load_schema path =
  match Analysis.Schema.of_sql (read_path path) with
  | Ok schema -> schema
  | Error diagnostics ->
    die "schema %s: %s" path
      (String.concat "; "
         (List.map (fun d -> d.Analysis.message) diagnostics))

let analyze ~schema ~plain_sql files =
  let allow_extensions = not plain_sql in
  let schema = Option.map load_schema schema in
  let sources =
    List.map read_path (match files with [] -> [ "-" ] | files -> files)
  in
  let statements =
    List.concat_map
      (Ir_document.statements_of_sql ~allow_extensions ~schema)
      sources
  in
  print_endline
    (Yojson.Basic.to_string (Ir_document.document_to_json { statements }));
  if List.exists Ir_document.is_invalid statements then exit 1

let () =
  match parse_args (List.tl (Array.to_list Sys.argv)) with
  | Json_schema ->
    print_endline
      (Yojson.Basic.pretty_to_string (Lazy.force Ir_document.json_schema))
  | Analyze { schema; plain_sql; files } ->
    analyze ~schema ~plain_sql files
