open Sqlgg
open Printf
open Linol_lsp.Types

module Parameter_names = Set.Make (String)

module Priority = struct
  type t =
    | Parameter
    | Cte
    | Qualified_column
    | Column
    | Table of { fits : bool; in_query : bool }
    | Source
    | Function
    | Keyword

  let order = function
    | Parameter | Cte | Qualified_column -> 0
    | Column | Table { fits = true; in_query = false } -> 1
    | Table { fits = false; in_query = false } | Source -> 2
    | Table { fits = true; in_query = true } | Function -> 3
    | Table { fits = false; in_query = true } -> 4
    | Keyword -> 9
end

type item = {
  label : string;
  detail : string;
  kind : CompletionItemKind.t;
  priority : Priority.t;
}

let make ?cache ~path text offset =
  let hole = "sqlgg__completion_hole" in
  let column_items ~priority (source : Symbol.t) =
    Symbol.columns source |> List.map (fun (attr : Sql.attr) ->
      { label = attr.name; detail = sprintf "%s — %s" (Sql.Type.show attr.domain) source.name;
        kind = Field; priority })
  in
  let listing_items ~priority ~kind ~what =
    List.map (fun (symbol : Symbol.t) ->
      { label = symbol.name; kind; priority = priority symbol;
        detail = sprintf "%s — %d columns" what (List.length symbol.columns) })
  in
  let function_item name =
    { label = name; detail = "function"; kind = Function; priority = Priority.Function }
  in
  let offset = Line_index.clamp_offset text offset in
  let document = Document.analyze ?cache ~path text in
  let current_statement = Document.find_statement document offset in
  let stmt =
    Option.map (fun (statement : Document.checked_statement) -> statement.block)
      current_statement
  in
  let ((start, stop) as replace) =
    let range base (lexeme : Recover_parser.lexeme) =
      let ((start, stop) as pos) = Sql.Pos.shift base lexeme.pos in
      if not (Sql.Pos.covers pos offset) then None
      else
        match lexeme.token with
        | PARAM { value = Some _; _ } -> Some (start + 1, stop)
        | IDENT _ | TYPE _ -> Some pos
        | token when Sql_lexer.is_keyword token -> Some pos
        | _ -> None
    in
    Option.bind stmt (fun (stmt : Statements.t) ->
      List.find_map (range (fst stmt.pos)) (Recover_parser.tokens stmt.text))
    |> Option.value ~default:(offset, offset)
  in
  let opaque =
    Statements.lexemes text
    |> Seq.take_while (fun ((start, _), _) -> start <= offset)
    |> Seq.exists (fun (pos, lexeme) ->
      match lexeme with
      | `Literal | `Open_literal | `Comment | `Props _ | `Bad_props ->
        Sql.Pos.contains pos offset
      | `Text | `Blank | `Semicolon -> false)
  in
  if opaque then replace, []
  else
    let stmt : Statements.t =
      match stmt with
      | None ->
        { text = hole;
          pos = (start, start + String.length hole);
          props = [];
          metadata = [];
          comments = [];
          errors = [] }
      | Some stmt ->
        let base = fst stmt.pos in
        let (start, stop) = start - base, stop - base in
        let delta = String.length hole - (stop - start) in
        { stmt with
          text = String.sub stmt.text 0 start ^ hole ^ String.sub stmt.text stop (String.length stmt.text - stop);
          pos = (base, snd stmt.pos + delta);
          metadata =
            List.map (fun (offset, meta) ->
              let adjusted_offset =
                if offset >= stop then offset + delta else offset
              in
              adjusted_offset, meta) stmt.metadata }
    in
    let current = Document.recheck document stmt in
    let other_statement (statement : Document.checked_statement) =
      if Sql.Pos.covers statement.stmt.pos offset
      then None
      else Some statement.stmt
    in
    let statements =
      Seq.cons current
        (Seq.filter_map other_statement (Array.to_seq document.Document.statements))
    in
    let base = fst stmt.pos in
    let hole_start = start - base in
    let run = Recover_parser.run stmt.text hole_start in
    let full = Recover_parser.run stmt.text (String.length stmt.text) in
    let hole_end = hole_start + String.length hole in
    let next =
      List.find_map (fun (lexeme : Recover_parser.lexeme) ->
        if fst lexeme.pos >= hole_end then Some lexeme.token else None)
        full.trace.seen
    in
    let slot = Recover_parser.slot ?next run in
    let replace =
      match slot with
      | Parameter sigil -> base + sigil, stop
      | Name _ | Column_of _ -> replace
    in
    let tables =
      document.Document.index
      |> Symbol.Index.bindings
      |> List.map (fun (_, symbol) -> symbol)
    in
    let source_scope =
      Option.bind current_statement (fun (statement : Document.checked_statement) ->
        match Document.select_scope_opt statement.stmt offset with
        | Some _ as scope -> scope
        | None ->
          match Document.statement_scope statement.stmt with
          | [] -> None
          | _ :: _ as scope -> Some scope)
    in
    let names =
      List.filter_map (fun (lexeme : Recover_parser.lexeme) ->
        Recover_parser.ident_name lexeme.token) full.trace.seen
    in
    let (recovery_sources, sources) =
      let trace_tables =
        List.filter_map (fun name ->
          Symbol.Index.find_opt name document.Document.index)
          full.trace.tables
      in
      let recovery_tables =
        if full.trace.recovery then
          List.filter_map (fun name ->
            Symbol.Index.find_opt name document.Document.index)
            names
        else
          []
      in
      let alias_sources =
        full.trace.sources |> List.filter_map (fun (src, (alias : Sql.source_alias option)) ->
          match src, alias with
          | `Table (table : Sql.table_name), Some alias ->
            Option.map (Symbol.as_alias ~name:alias.table_name.value.tn table)
              (Symbol.Index.find_opt table.tn document.Document.index)
          | `Table _, None | (`Select _ | `Nested _ | `ValueRows _), _ -> None)
      in
      let visible sources =
        List.filter (fun (symbol : Symbol.t) ->
          not (String.equal symbol.name hole)) sources
        |> Symbol.unique
      in
      let recovery_sources =
        visible
          (Document.statement_scope current @ trace_tables @ recovery_tables @ alias_sources)
      in
      let sources =
        Option.fold ~none:recovery_sources ~some:visible source_scope
      in
      recovery_sources, sources
    in
    let functions = Sql.Function.names () in
    let role = function
      | Recover_parser.Table_name ->
        let has_column symbol name =
          Option.is_some (Symbol.find_column_opt symbol name)
        in
        let matching_names =
          List.filter (fun name ->
            List.exists (fun symbol -> has_column symbol name)
              tables)
            names
        in
        let priority (symbol : Symbol.t) =
          Priority.Table {
            fits = List.for_all (has_column symbol) matching_names;
            in_query =
              Option.is_some (Symbol.find_opt recovery_sources symbol.name);
          }
        in
        let ctes =
          sources
          |> List.filter (fun (symbol : Symbol.t) ->
            match symbol.kind with Cte -> true | Table | Derived | Alias _ -> false)
          |> List.map (fun (symbol : Symbol.t) ->
            { label = symbol.name; detail = "CTE in this statement"; kind = Interface; priority = Priority.Cte })
        in
        ctes @ listing_items ~priority ~kind:Struct ~what:"table" tables
      | Column_name -> List.concat_map (column_items ~priority:Priority.Column) sources
      | Qualifier -> listing_items ~priority:(Fun.const Priority.Source) ~kind:Module ~what:"source" sources
      | Function_name ->
        functions |> List.filter (fun name -> not (Sql_lexer.Keywords.mem name Sql_lexer.keywords)) |> List.map function_item
    in
    let completions =
      match slot with
      | Parameter _ ->
        statements
        |> Seq.concat_map (fun stmt -> Params.all_nodes (Document.params stmt))
        |> Seq.filter_map (fun (node : Params.node) ->
          match node.kind with Var (id, _) -> id.value | Branch _ -> None)
        |> Seq.append (List.to_seq full.trace.seen
          |> Seq.filter_map (fun (lexeme : Recover_parser.lexeme) ->
          match lexeme.token with PARAM { value = Some name; _ } -> Some name | _ -> None))
        |> Seq.filter (Fun.negate (String.equal hole))
        |> Parameter_names.of_seq
        |> Parameter_names.elements
        |> List.map (fun name ->
          { label = "@" ^ name; detail = "parameter"; kind = Variable; priority = Priority.Parameter })
      | Column_of q ->
        Option.fold ~none:[]
          ~some:(column_items ~priority:Priority.Qualified_column)
          (Symbol.find_opt sources q)
      | Name roles ->
        let is_type = function Sql_tokens.TYPE _ -> true | _ -> false in
        let keywords =
          Sql_lexer.Keywords.to_seq Sql_lexer.keywords
          |> Seq.filter (fun (_, token) ->
            not (is_type token) && Recover_parser.accepts run token)
          |> Seq.map (fun (keyword, _) ->
            if List.exists (String.equal keyword) functions then function_item keyword
            else
              { label = String.uppercase_ascii keyword; detail = "keyword";
                kind = Keyword; priority = Priority.Keyword })
          |> List.of_seq
        in
        List.concat_map role roles @ keywords
        |> Prelude.unique_by ~key:(fun completion -> completion.label)
    in
    replace, completions
