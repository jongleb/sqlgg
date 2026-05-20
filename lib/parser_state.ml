
type mode = Normal | Ignore | Ident
let mode = ref Normal
let mode_normal () = mode := Normal
let mode_ignore () = mode := Ignore
let mode_ident () = mode := Ident
module Stmt_metadata = struct
  let stmt_metadata: (int, (string * string) list) Hashtbl.t = Hashtbl.create 16

  let add k v = Hashtbl.add stmt_metadata k v
  let find_all k = Hashtbl.find_all stmt_metadata k
  let reset () = Hashtbl.reset stmt_metadata
end

let current_lexbuf : Lexing.lexbuf option ref = ref None

let with_lexbuf lexbuf f =
  let saved = !current_lexbuf in
  current_lexbuf := Some lexbuf;
  Fun.protect f ~finally:(fun () -> current_lexbuf := saved)

let extract_source (start_, end_) =
  match !current_lexbuf with
  | Some { Lexing.lex_buffer = buf; lex_abs_pos = base; _ }
    when start_ >= base && end_ >= start_ && end_ - base <= Bytes.length buf ->
    Some (String.trim (Bytes.sub_string buf (start_ - base) (end_ - start_)))
  | _ -> None
