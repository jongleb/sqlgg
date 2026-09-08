open Sqlgg_lsp
open Printf

type doc = {
  path : string;
  text : string;
  lines : Line_index.t;
  document : Document.t;
}

let load path =
  let text = In_channel.with_open_bin path In_channel.input_all in
  { path; text; lines = Line_index.make text; document = Document.analyze ~path text }

let position lines offset =
  let { Line_index.line; character } = Line_index.position lines offset in
  sprintf "%d:%d" (line + 1) character

let range lines (start, stop) = position lines start ^ "-" ^ position lines stop

let location doc (loc : Symbol.loc) =
  let lines = if String.equal loc.file doc.path then doc.lines else Line_index.of_file loc.file in
  loc.file ^ " " ^ range lines loc.pos

let offset doc cursor_marker =
  let (cursor_marker, skip) =
    if String.ends_with ~suffix:"^" cursor_marker then
      let cursor_marker = String.sub cursor_marker 0 (String.length cursor_marker - 1) in
      cursor_marker, String.length cursor_marker
    else cursor_marker, 0
  in
  let n = String.length cursor_marker in
  let len = String.length doc.text in
  let matches i =
    let rec loop k = k >= n || (Char.equal doc.text.[i + k] cursor_marker.[k] && loop (k + 1)) in
    loop 1
  in
  let rec search i =
    if i + n > len then None
    else if matches i then Some i
    else Option.bind (String.index_from_opt doc.text (i + 1) cursor_marker.[0]) search
  in
  let found =
    if Int.equal n 0 then Some 0
    else Option.bind (String.index_from_opt doc.text 0 cursor_marker.[0]) search
  in
  match found with
  | Some i -> i + skip
  | None -> failwith ("not in file: " ^ cursor_marker)

let diagnostics doc =
  doc.document.Document.statements
  |> Array.to_seq
  |> Seq.concat_map (fun (statement : Document.checked_statement) ->
    List.to_seq (Document.errors statement.stmt))
  |> Seq.iter (fun (e : Document.error) -> printf "%s %s\n" (range doc.lines e.pos) e.msg)

let tokens doc =
  Ide.semantic_tokens ~lines:doc.lines doc.document
  |> List.iter (fun (token : Ide.token) ->
    printf "%s %s\n" (range doc.lines token.pos) (Params.token_type_to_string token.typ))

let hover doc offset =
  match Option.bind (Ide.create doc.document ~offset) Ide.hover with
  | None -> print_endline "nothing"
  | Some (value, pos) -> printf "%s\n%s" (range doc.lines pos) value

let definition doc offset =
  match Option.fold ~none:[] ~some:Ide.definition (Ide.create doc.document ~offset) with
  | [] -> print_endline "nothing"
  | locs -> List.iter (fun loc -> print_endline (location doc loc)) locs

let complete doc offset =
  let (replace, completions) = Completion.make ~path:doc.path doc.text offset in
  printf "replace %s\n" (range doc.lines replace);
  completions
  |> List.sort (fun (a : Completion.item) b ->
    match Int.compare (Completion.Priority.order a.priority) (Completion.Priority.order b.priority) with
    | 0 -> String.compare a.label b.label
    | order -> order)
  |> List.to_seq
  |> Seq.take 12
  |> Seq.iter (fun (completion : Completion.item) ->
    printf "%s  %s\n" completion.label completion.detail)

type query =
  | Diags [@as "diags"]
  | Tokens [@as "tokens"]
  | Hover [@as "hover"]
  | Definition [@as "def"]
  | Complete [@as "complete"]
[@@deriving of_string]

let parse query =
  let (name, cursor_marker) =
    match String.index_opt query ':' with
    | None -> query, None
    | Some i -> String.sub query 0 i, Some (String.sub query (i + 1) (String.length query - i - 1))
  in
  match query_of_string name with
  | Some query -> query, cursor_marker
  | None -> failwith ("unknown query " ^ query)

let run doc (query, cursor_marker) =
  let cursor = function
    | Some cursor_marker -> offset doc cursor_marker
    | None -> failwith "query needs a :MARKER"
  in
  match query with
  | Diags -> diagnostics doc
  | Tokens -> tokens doc
  | Hover -> hover doc (cursor cursor_marker)
  | Definition -> definition doc (cursor cursor_marker)
  | Complete -> complete doc (cursor cursor_marker)

let () =
  match Array.to_list Sys.argv with
  | _ :: path :: queries ->
    let doc = load path in
    List.iter (fun query -> print_endline ("### " ^ query); run doc (parse query)) queries
  | _ -> failwith "usage: ask FILE QUERY..."
