open Printf

exception Failure of string

let failf fmt = ksprintf (fun msg -> raise (Failure msg)) fmt

type server = {
  pid : int;
  input : out_channel;
  output_fd : Unix.file_descr;
  mutable output_buffer : string;
  errors : in_channel;
  mutable reaped : bool;
}

let spawn executable =
  let (stdin_r, stdin_w) = Unix.pipe () in
  let (stdout_r, stdout_w) = Unix.pipe () in
  let (stderr_r, stderr_w) = Unix.pipe () in
  List.iter Unix.set_close_on_exec
    [ stdin_r; stdin_w; stdout_r; stdout_w; stderr_r; stderr_w ];
  let pid =
    Unix.create_process executable [| executable |] stdin_r stdout_w stderr_w
  in
  Unix.close stdin_r;
  Unix.close stdout_w;
  Unix.close stderr_w;
  {
    pid;
    input = Unix.out_channel_of_descr stdin_w;
    output_fd = stdout_r;
    output_buffer = "";
    errors = Unix.in_channel_of_descr stderr_r;
    reaped = false;
  }

let send server json =
  let body = Yojson.Safe.to_string json in
  fprintf server.input "Content-Length: %d\r\n\r\n%s%!" (String.length body) body

let read_more server =
  let (ready, _, _) = Unix.select [ server.output_fd ] [] [] 1. in
  if ready = [] then failf "timed out waiting for an LSP message";
  let bytes = Bytes.create 4096 in
  match Unix.read server.output_fd bytes 0 (Bytes.length bytes) with
  | 0 -> failf "LSP server closed stdout"
  | length ->
    server.output_buffer <-
      server.output_buffer ^ Bytes.sub_string bytes 0 length

let header_separator = Re.compile (Re.str "\r\n\r\n")

let read_message server =
  let rec header () =
    match Re.exec_opt header_separator server.output_buffer with
    | Some groups -> Re.Group.start groups 0
    | None ->
      read_more server;
      header ()
  in
  let header_length = header () in
  let headers = String.sub server.output_buffer 0 header_length in
  let content_length =
    String.split_on_char '\n' headers
    |> List.find_map (fun line ->
      match String.split_on_char ':' (String.trim line) with
      | [ name; value ] when String.equal (String.lowercase_ascii name) "content-length" ->
        Some (int_of_string (String.trim value))
      | _ -> None)
    |> Option.value ~default:(-1)
  in
  if content_length < 0 then failf "LSP response has no Content-Length header";
  let body_offset = header_length + 4 in
  let total_length = body_offset + content_length in
  while String.length server.output_buffer < total_length do
    read_more server
  done;
  let body = String.sub server.output_buffer body_offset content_length in
  server.output_buffer <-
    String.sub server.output_buffer total_length
      (String.length server.output_buffer - total_length);
  Yojson.Safe.from_string body

let rec receive server predicate =
  let json = read_message server in
  if predicate json then json else receive server predicate

let has_id expected = function
  | `Assoc fields ->
    begin match List.assoc_opt "id" fields with
    | Some (`Int id) -> Int.equal id expected
    | Some _ | None -> false
    end
  | _ -> false

let diagnostics satisfy = function
  | `Assoc fields ->
    begin match List.assoc_opt "method" fields, List.assoc_opt "params" fields with
    | Some (`String "textDocument/publishDiagnostics"), Some (`Assoc params) ->
      begin match List.assoc_opt "diagnostics" params with
      | Some (`List diagnostics) -> satisfy diagnostics
      | Some _ | None -> false
      end
    | _ -> false
    end
  | _ -> false

let diagnostics_at_version expected = function
  | `Assoc fields ->
    begin match List.assoc_opt "method" fields, List.assoc_opt "params" fields with
    | Some (`String "textDocument/publishDiagnostics"), Some (`Assoc params) ->
      begin match List.assoc_opt "version" params, List.assoc_opt "diagnostics" params with
      | Some (`Int version), Some (`List (_ :: _)) -> Int.equal version expected
      | _ -> false
      end
    | _ -> false
    end
  | _ -> false

let result = function
  | `Assoc fields -> List.assoc_opt "result" fields
  | _ -> None

let initialize server =
  send server
    (`Assoc [
      "jsonrpc", `String "2.0";
      "id", `Int 1;
      "method", `String "initialize";
      "params", `Assoc [ "capabilities", `Assoc [] ];
    ]);
  ignore (receive server (has_id 1))

let shutdown server id =
  send server
    (`Assoc [
      "jsonrpc", `String "2.0";
      "id", `Int id;
      "method", `String "shutdown";
      "params", `Null;
    ]);
  ignore (receive server (has_id id));
  send server
    (`Assoc [
      "jsonrpc", `String "2.0";
      "method", `String "exit";
      "params", `Assoc [];
    ])

let wait_for_exit server =
  let deadline = Unix.gettimeofday () +. 1. in
  let rec loop () =
    match Unix.waitpid [ Unix.WNOHANG ] server.pid with
    | 0, _ when Unix.gettimeofday () < deadline ->
      Unix.sleepf 0.01;
      loop ()
    | 0, _ -> failf "server did not exit after the exit notification"
    | _, Unix.WEXITED 0 -> server.reaped <- true
    | _, Unix.WEXITED code ->
      server.reaped <- true;
      failf "server exited with code %d" code
    | _, Unix.WSIGNALED signal | _, Unix.WSTOPPED signal ->
      server.reaped <- true;
      failf "server stopped by signal %d" signal
  in
  loop ()

let close server =
  close_out_noerr server.input;
  if not server.reaped then begin
    (try Unix.kill server.pid Sys.sigkill with Unix.Unix_error (Unix.ESRCH, _, _) -> ());
    ignore (Unix.waitpid [] server.pid);
    server.reaped <- true
  end;
  Unix.close server.output_fd;
  close_in_noerr server.errors

let with_server executable f =
  let server = spawn executable in
  match f server with
  | () -> close server
  | exception exn ->
    close server;
    raise exn

let did_open server ~uri ~text =
  send server
    (`Assoc [
      "jsonrpc", `String "2.0";
      "method", `String "textDocument/didOpen";
      "params", `Assoc [
        "textDocument", `Assoc [
          "uri", `String uri;
          "languageId", `String "sql";
          "version", `Int 1;
          "text", `String text;
        ];
      ];
    ])

let did_close server ~uri =
  send server
    (`Assoc [
      "jsonrpc", `String "2.0";
      "method", `String "textDocument/didClose";
      "params", `Assoc [
        "textDocument", `Assoc [ "uri", `String uri ];
      ];
    ])

let did_change server ~uri ~version ~text =
  send server
    (`Assoc [
      "jsonrpc", `String "2.0";
      "method", `String "textDocument/didChange";
      "params", `Assoc [
        "textDocument", `Assoc [
          "uri", `String uri;
          "version", `Int version;
        ];
        "contentChanges", `List [ `Assoc [ "text", `String text ] ];
      ];
    ])

let test_shutdown executable =
  with_server executable (fun server ->
    initialize server;
    shutdown server 2;
    wait_for_exit server)

let test_close_clears_diagnostics executable =
  with_server executable (fun server ->
    let uri = "file:///tmp/sqlgg-lsp-invalid.sql" in
    initialize server;
    did_open server ~uri ~text:"SELECT FROM;";
    ignore (receive server (diagnostics (fun diagnostics -> diagnostics <> [])));
    did_close server ~uri;
    ignore (receive server (diagnostics (fun diagnostics -> diagnostics = [])));
    shutdown server 2;
    close_out_noerr server.input)

let test_close_forgets_document executable =
  with_server executable (fun server ->
    let uri = "file:///tmp/sqlgg-lsp-closed.sql" in
    initialize server;
    did_open server ~uri ~text:"SELECT 1;";
    ignore (receive server (diagnostics (fun _ -> true)));
    did_close server ~uri;
    send server
      (`Assoc [
        "jsonrpc", `String "2.0";
        "id", `Int 2;
        "method", `String "textDocument/semanticTokens/full";
        "params", `Assoc [
          "textDocument", `Assoc [ "uri", `String uri ];
        ];
      ]);
    let response = receive server (has_id 2) in
    if result response <> Some `Null then
      failf "closed document remains requestable: %s" (Yojson.Safe.to_string response);
    shutdown server 3;
    wait_for_exit server)

let test_change_versions_diagnostics executable =
  with_server executable (fun server ->
    let uri = "file:///tmp/sqlgg-lsp-version.sql" in
    initialize server;
    did_open server ~uri ~text:"SELECT FROM;";
    ignore (receive server (diagnostics_at_version 1));
    did_change server ~uri ~version:2 ~text:"SELECT WHERE;";
    ignore (receive server (diagnostics_at_version 2));
    shutdown server 2;
    close_out_noerr server.input)

let () =
  match Array.to_list Sys.argv with
  | [ _; executable; "shutdown" ] -> test_shutdown executable
  | [ _; executable; "close-clears-diagnostics" ] ->
    test_close_clears_diagnostics executable
  | [ _; executable; "close-forgets-document" ] ->
    test_close_forgets_document executable
  | [ _; executable; "change-versions-diagnostics" ] ->
    test_change_versions_diagnostics executable
  | _ ->
    failf
      "usage: protocol LSP \
       {shutdown|close-clears-diagnostics|close-forgets-document|change-versions-diagnostics}"
