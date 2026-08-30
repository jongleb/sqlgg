open Sqlgg
open Sqlgg_lsp
open Linol_lwt

let position lines offset =
  let (line, character) = Line_index.position lines offset in
  Position.create ~line ~character

let range lines (start, stop) =
  Range.create ~start:(position lines start) ~end_:(position lines stop)

module Session = struct
  type encoding = [ `UTF8 | `UTF16 ]

  module Line_indexes = Hashtbl.Make (DocumentUri)

  type line_index = {
    encoding : encoding;
    content : string;
    index : Line_index.t;
  }

  type t = {
    documents : Document.Cache.t;
    line_indexes : line_index Line_indexes.t;
  }

  let create () =
    { documents = Document.Cache.create (); line_indexes = Line_indexes.create 16 }

  let document t uri content =
    Document.analyze ~cache:t.documents ~path:(DocumentUri.to_path uri) content

  let lines t ~encoding uri content =
    match Line_indexes.find_opt t.line_indexes uri with
    | Some cached
        when cached.encoding = encoding && String.equal cached.content content ->
      cached.index
    | Some _ | None ->
      let index = Line_index.make ~encoding content in
      Line_indexes.replace t.line_indexes uri { encoding; content; index };
      index

  let offset t ~encoding uri content (pos : Position.t) =
    let lines = lines t ~encoding uri content in
    lines, Line_index.offset lines ~line:pos.line ~character:pos.character

  let cursor t ~encoding uri content pos =
    let (lines, offset) = offset t ~encoding uri content pos in
    lines, Ide.create ~text:content (document t uri content) offset

  let location ~encoding ~here ~lines (loc : Symbol.loc) =
    let lines =
      if String.equal loc.file here then Some lines
      else
        match Line_index.of_file ~encoding loc.file with
        | lines -> Some lines
        | exception Sys_error _ -> None
    in
    lines |> Option.map (fun lines ->
      Location.create ~uri:(DocumentUri.of_path loc.file)
        ~range:(range lines loc.pos))

  let diagnostics t ~encoding uri content =
    let lines = lines t ~encoding uri content in
    (document t uri content).Document.statements
    |> List.concat_map (fun (statement : Document.checked_statement) ->
      Document.errors statement.stmt)
    |> List.map (fun (e : Document.error) ->
      Diagnostic.create ~range:(range lines e.pos) ~severity:DiagnosticSeverity.Error
        ~source:"sqlgg" ~message:(`String e.msg) ())

  let semantic_tokens t ~encoding uri content =
    let lines = lines t ~encoding uri content in
    let delta (prev : Position.t) (token : Ide.token) =
      let pos = position lines (fst token.pos) in
      let stop = position lines (snd token.pos) in
      let delta_line = pos.line - prev.line in
      let delta_char =
        if delta_line = 0 then pos.character - prev.character else pos.character
      in
      pos,
      [ delta_line; delta_char; stop.character - pos.character;
        Params.token_type_to_enum token.typ; 0 ]
    in
    let data =
      Ide.semantic_tokens ~lines (document t uri content)
      |> List.fold_left_map delta (Position.create ~line:0 ~character:0)
      |> snd
      |> List.concat
      |> Array.of_list
    in
    SemanticTokens.create ~data ()

  let hover t ~encoding uri content pos =
    let (lines, cursor) = cursor t ~encoding uri content pos in
    Ide.hover cursor
    |> Option.map (fun (value, pos) ->
      let contents =
        `MarkupContent (MarkupContent.create ~kind:MarkupKind.Markdown ~value)
      in
      Hover.create ~contents ~range:(range lines pos) ())

  let definition t ~encoding uri content pos =
    let (lines, cursor) = cursor t ~encoding uri content pos in
    let here = DocumentUri.to_path uri in
    match
      List.filter_map (location ~encoding ~here ~lines) (Ide.definition cursor)
    with
    | [] -> None
    | locations -> Some (`Location locations)

  let completion t ~encoding uri content pos =
    let (lines, offset) = offset t ~encoding uri content pos in
    let (replace, completions) =
      Completion.at ~cache:t.documents ~path:(DocumentUri.to_path uri)
        content offset
    in
    match completions with
    | [] -> None
    | completions ->
      let range = range lines replace in
      let completion (candidate : Completion.item) =
        CompletionItem.create ~label:candidate.label ~detail:candidate.detail
          ~kind:candidate.kind
          ~sortText:
            (Printf.sprintf "%02d%s"
              (Completion.Priority.order candidate.priority) candidate.label)
          ~textEdit:(`TextEdit
            (TextEdit.create ~range ~newText:candidate.label))
          ()
      in
      Some (`List (List.map completion completions))

  let forget t uri =
    Document.Cache.forget t.documents (DocumentUri.to_path uri);
    Line_indexes.remove t.line_indexes uri
end

let publish_diagnostics
    (notify_back : Linol_lwt.Jsonrpc2.notify_back) ~version uri diagnostics =
  let params = PublishDiagnosticsParams.create ~uri ~version ~diagnostics () in
  notify_back#send_notification
    (Linol.Lsp.Server_notification.PublishDiagnostics params)

class sqlgg_lsp =
  object (self)
    inherit Linol_lwt.Jsonrpc2.server as super

    method spawn_query_handler f = Linol_lwt.spawn f

    method! config_hover = Some (`Bool true)
    method! config_definition = Some (`Bool true)

    method! config_completion =
      Some (CompletionOptions.create ~triggerCharacters:[ "."; "@" ] ())

    method! config_modify_capabilities (capabilities : ServerCapabilities.t) =
      let capabilities = super#config_modify_capabilities capabilities in
      let types = List.map Params.token_type_to_string Params.all_of_token_type in
      let legend = SemanticTokensLegend.create ~tokenTypes:types ~tokenModifiers:[] in
      { capabilities with
        semanticTokensProvider =
          Some (`SemanticTokensOptions (SemanticTokensOptions.create ~legend ~full:(`Bool true) ())) }

    val session = Session.create ()

    method on_notif_doc_did_open ~notify_back doc ~content =
      let diagnostics =
        Session.diagnostics session ~encoding:positionEncoding doc.TextDocumentItem.uri content
      in
      publish_diagnostics notify_back ~version:doc.version doc.uri diagnostics

    method on_notif_doc_did_change ~notify_back doc _changes ~old_content:_ ~new_content =
      let diagnostics =
        Session.diagnostics session ~encoding:positionEncoding
          doc.VersionedTextDocumentIdentifier.uri new_content
      in
      publish_diagnostics notify_back ~version:doc.version doc.uri diagnostics

    method on_notif_doc_did_close ~notify_back doc =
      Session.forget session doc.TextDocumentIdentifier.uri;
      Hashtbl.remove docs doc.uri;
      notify_back#send_diagnostic []

    method! on_request_unhandled : type r.
        notify_back:Linol_lwt.Jsonrpc2.notify_back ->
        id:Linol_lwt.Jsonrpc2.Req_id.t ->
        r Linol.Lsp.Client_request.t ->
        r Linol_lwt.t =
      fun ~notify_back ~id req ->
        match req with
        | Linol.Lsp.Client_request.SemanticTokensFull
            { textDocument = identifier; _ } ->
          begin match self#find_doc identifier.TextDocumentIdentifier.uri with
          | None -> Linol_lwt.return None
          | Some state ->
            Session.semantic_tokens session ~encoding:positionEncoding
              identifier.uri state.Linol_lwt.Jsonrpc2.content
            |> Option.some
            |> Linol_lwt.return
          end
        | _ -> super#on_request_unhandled ~notify_back ~id req

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ doc =
      Session.hover session ~encoding:positionEncoding
        uri doc.Linol_lwt.Jsonrpc2.content pos
      |> Linol_lwt.return

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ doc =
      Session.definition session ~encoding:positionEncoding
        uri doc.Linol_lwt.Jsonrpc2.content pos
      |> Linol_lwt.return

    method! on_req_completion ~notify_back:_ ~id:_ ~uri ~pos ~ctx:_
        ~workDoneToken:_ ~partialResultToken:_ doc =
      Session.completion session ~encoding:positionEncoding
        uri doc.Linol_lwt.Jsonrpc2.content pos
      |> Linol_lwt.return
  end

let () =
  let server = new sqlgg_lsp in
  let rpc = Linol_lwt.Jsonrpc2.create_stdio ~env:() server in
  match
    Linol_lwt.run
      (Linol_lwt.Jsonrpc2.run
        ~shutdown:(fun () -> server#get_status = `ReceivedExit) rpc)
  with
  | () -> ()
  | exception exn ->
    Printf.eprintf "sqlgg-lsp fatal: %s\n%!" (Printexc.to_string exn);
    exit 1
