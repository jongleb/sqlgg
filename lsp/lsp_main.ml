open Sqlgg_lsp
open Linol_lwt

let position lines offset =
  let { Line_index.line; character } = Line_index.position lines offset in
  Position.create ~line ~character

let range lines (start, stop) =
  Range.create ~start:(position lines start) ~end_:(position lines stop)

module Session = struct
  module Line_indexes = Hashtbl.Make (DocumentUri)

  type line_index = {
    encoding : Line_index.position_encoding;
    version : int;
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

  let lines t ~encoding ~version uri content =
    match Line_indexes.find_opt t.line_indexes uri with
    | Some cached
        when Line_index.equal_position_encoding cached.encoding encoding
          && Int.equal cached.version version ->
      cached.index
    | Some _ | None ->
      let index = Line_index.make ~position_encoding:encoding content in
      Line_indexes.replace t.line_indexes uri { encoding; version; index };
      index

  let offset t ~encoding ~version uri content (pos : Position.t) =
    let lines = lines t ~encoding ~version uri content in
    lines, Line_index.offset lines ~line:pos.line ~character:pos.character

  let cursor t ~encoding ~version uri content pos =
    let (lines, offset) = offset t ~encoding ~version uri content pos in
    lines, Ide.create (document t uri content) ~offset

  let locations ~encoding ~here ~lines locs =
    let read file =
      match Line_index.of_file ~position_encoding:encoding file with
      | lines -> Some lines
      | exception Sys_error msg -> prerr_endline ("sqlgg-lsp: " ^ msg); None
    in
    let (_, found) =
      List.fold_left (fun (opened, found) (loc : Symbol.loc) ->
        let (opened, index) =
          if String.equal loc.file here then (opened, Some lines)
          else
            match List.find_opt (fun (file, _) -> String.equal file loc.file) opened with
            | Some (_, index) -> (opened, index)
            | None -> let index = read loc.file in ((loc.file, index) :: opened, index)
        in
        let location index =
          Location.create ~uri:(DocumentUri.of_path loc.file) ~range:(range index loc.pos)
        in
        (opened, match Option.map location index with Some l -> l :: found | None -> found))
        ([], []) locs
    in
    List.rev found

  let diagnostics t ~encoding ~version uri content =
    let lines = lines t ~encoding ~version uri content in
    (document t uri content).Document.statements
    |> Array.to_seq
    |> Seq.concat_map (fun (statement : Document.checked_statement) ->
      List.to_seq (Document.errors statement.stmt))
    |> Seq.map (fun (e : Document.error) ->
      Diagnostic.create ~range:(range lines e.pos) ~severity:DiagnosticSeverity.Error
        ~source:"sqlgg" ~message:(`String e.msg) ())
    |> List.of_seq

  let semantic_tokens t ~encoding ~version uri content =
    let lines = lines t ~encoding ~version uri content in
    let delta (prev : Position.t) (token : Ide.token) =
      let pos = position lines (fst token.pos) in
      let stop = position lines (snd token.pos) in
      let delta_line = pos.line - prev.line in
      let delta_char =
        if Int.equal delta_line 0 then pos.character - prev.character else pos.character
      in
      let type_index =
        match token.typ with
        | Params.Parameter -> 0
        | Enum -> 1
        | Enum_member -> 2
      in
      pos,
      [ delta_line; delta_char; stop.character - pos.character; type_index; 0 ]
    in
    let (_, deltas) =
      Ide.semantic_tokens ~lines (document t uri content)
      |> List.fold_left_map delta (Position.create ~line:0 ~character:0)
    in
    let data = Array.of_list (List.concat deltas) in
    SemanticTokens.create ~data ()

  let hover t ~encoding ~version uri content pos =
    let (lines, cursor) = cursor t ~encoding ~version uri content pos in
    Option.bind cursor Ide.hover
    |> Option.map (fun (value, pos) ->
      let contents =
        `MarkupContent (MarkupContent.create ~kind:MarkupKind.Markdown ~value)
      in
      Hover.create ~contents ~range:(range lines pos) ())

  let definition t ~encoding ~version uri content pos =
    let (lines, cursor) = cursor t ~encoding ~version uri content pos in
    let here = DocumentUri.to_path uri in
    let locs = Option.fold ~none:[] ~some:Ide.definition cursor in
    match locations ~encoding ~here ~lines locs with
    | [] -> None
    | found -> Some (`Location found)

  let completion t ~encoding ~version uri content pos =
    let (lines, offset) = offset t ~encoding ~version uri content pos in
    let (replace, completions) =
      Completion.make ~cache:t.documents ~path:(DocumentUri.to_path uri)
        content offset
    in
    match completions with
    | [] -> None
    | completions ->
      let edit_range = range lines replace in
      let completion (candidate : Completion.item) =
        CompletionItem.create ~label:candidate.label ~detail:candidate.detail
          ~kind:candidate.kind
          ~sortText:
            (Printf.sprintf "%03d%s"
              (Completion.Priority.order candidate.priority) candidate.label)
          ~textEdit:(`TextEdit
            (TextEdit.create ~range:edit_range ~newText:candidate.label))
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
        Session.diagnostics session ~encoding:positionEncoding ~version:doc.TextDocumentItem.version doc.uri content
      in
      publish_diagnostics notify_back ~version:doc.version doc.uri diagnostics

    method on_notif_doc_did_change ~notify_back doc _changes ~old_content:_ ~new_content =
      let diagnostics =
        Session.diagnostics session ~encoding:positionEncoding
          ~version:doc.VersionedTextDocumentIdentifier.version doc.uri new_content
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
              ~version:state.Linol_lwt.Jsonrpc2.version identifier.uri state.content
            |> Option.some
            |> Linol_lwt.return
          end
        | _ -> super#on_request_unhandled ~notify_back ~id req

    method! on_req_hover ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_ doc =
      Session.hover session ~encoding:positionEncoding
        ~version:doc.Linol_lwt.Jsonrpc2.version uri doc.content pos
      |> Linol_lwt.return

    method! on_req_definition ~notify_back:_ ~id:_ ~uri ~pos ~workDoneToken:_
        ~partialResultToken:_ doc =
      Session.definition session ~encoding:positionEncoding
        ~version:doc.Linol_lwt.Jsonrpc2.version uri doc.content pos
      |> Linol_lwt.return

    method! on_req_completion ~notify_back:_ ~id:_ ~uri ~pos ~ctx:_
        ~workDoneToken:_ ~partialResultToken:_ doc =
      Session.completion session ~encoding:positionEncoding
        ~version:doc.Linol_lwt.Jsonrpc2.version uri doc.content pos
      |> Linol_lwt.return
  end

let () =
  Printexc.record_backtrace true;
  let server = new sqlgg_lsp in
  let rpc = Linol_lwt.Jsonrpc2.create_stdio ~env:() server in
  try
    Linol_lwt.run
      (Linol_lwt.Jsonrpc2.run
        ~shutdown:(fun () -> server#get_status = `ReceivedExit) rpc)
  with exn ->
    let backtrace = Printexc.get_backtrace () in
    Printf.eprintf "sqlgg-lsp fatal: %s\n%s%!" (Printexc.to_string exn) backtrace;
    exit 1
