type diagnostic = {
  message : string;
  pos : Sql.Pos.t option;
}

type parsed = {
  sql : string;
  ast : Sql.stmt;
  dialect_features : Dialect.dialect_support list;
}

type 'a outcome = ('a, diagnostic list) result

let nonempty ((start, stop) as pos) =
  if stop <= start then None else Some pos

let rec expected_error = function
  | Sql_parser.Error -> true
  | Sql_lexer.Error _ -> true
  | Sql.Schema.Error _ -> true
  | Prelude.Sql_error _ -> true
  | Prelude.At (_, exn) -> expected_error exn
  | _ -> false

let diagnostic_of_parser_error exn info =
  let pos =
    match exn with
    | Sql_lexer.Error (_, pos) -> nonempty pos
    | Prelude.At (pos, _) -> nonempty pos
    | _ -> Some info.Parser_utils.pos
  in
  {
    message = Parser_utils.message_of_exn exn;
    pos;
  }

let diagnostic_of_exn exn =
  let pos =
    match exn with
    | Sql_lexer.Error (_, pos) -> nonempty pos
    | Prelude.At (pos, _) -> nonempty pos
    | _ -> None
  in
  {
    message = Parser_utils.message_of_exn exn;
    pos;
  }

let rebase_diagnostic offset diagnostic =
  let pos =
    Option.map
      (fun (start, stop) -> offset + start, offset + stop)
      diagnostic.pos
  in
  { diagnostic with pos }

let protect_state f =
  let previous = Compile.snapshot () in
  Fun.protect ~finally:(fun () -> Compile.restore previous) f

let parse sql =
  Prelude.with_sql_errors @@ fun () ->
  match Parser.parse_stmt sql with
  | { stmt = ast; dialect_features } ->
    Ok { sql; ast; dialect_features }
  | exception Parser_utils.Error (exn, info) when expected_error exn ->
    Error [diagnostic_of_parser_error exn info]

module Schema = struct
  type t = Compile.state

  let current () = Compile.snapshot ()

  let of_sql sql =
    Prelude.with_sql_errors @@ fun () ->
    protect_state @@ fun () ->
    Compile.reset ();
    let statements = Statements.split sql in
    let split_diagnostics =
      List.concat_map
        (fun (statement : Statements.t) ->
          List.map
            (fun (pos, message) -> { message; pos = Some pos })
            statement.errors)
        statements
    in
    match split_diagnostics with
    | _ :: _ -> Error split_diagnostics
    | [] ->
      let rec compile = function
        | [] -> Ok (Compile.snapshot ())
        | (statement : Statements.t) :: rest ->
          let offset = fst statement.pos in
          match Compile.statement ~dynamic_select:Props.Off statement with
          | _ -> compile rest
          | exception Parser_utils.Error (exn, info) when expected_error exn ->
            Error
              [rebase_diagnostic
                 offset
                 (diagnostic_of_parser_error exn info)]
          | exception exn when expected_error exn ->
            Error [rebase_diagnostic offset (diagnostic_of_exn exn)]
      in
      compile statements
end

let analyze ~schema sql =
  Prelude.with_sql_errors @@ fun () ->
  protect_state @@ fun () ->
  Compile.restore schema;
  match Parser.parse_stmt sql with
  | parsed ->
    begin
      match Syntax.eval_parsed sql parsed with
      | result -> Ok result
      | exception exn when expected_error exn ->
        Error [diagnostic_of_exn exn]
    end
  | exception Parser_utils.Error (exn, info) when expected_error exn ->
    Error [diagnostic_of_parser_error exn info]
