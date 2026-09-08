open Sqlgg
open Printf

type t = {
  document : Document.t;
  stmt : Document.stmt;
  offset : int;
}

let create document ~offset =
  Document.find_statement document offset
  |> Option.map (fun (statement : Document.checked_statement) ->
    { document; stmt = statement.stmt; offset })

type ident = {
  name : string;
  pos : Sql.Pos.t;
  qualifier : string option;
}

type resolved_column = { source : Symbol.t; column : Symbol.column }

type resolution =
  | Source of { symbol : Symbol.t; declaration : Symbol.t option }
  | Columns of resolved_column * resolved_column list

type target =
  | Shared_query of { stmt : Document.stmt; loc : Symbol.loc }
  | Param of Params.node
  | Name of resolution
  | Branch of { choice : Sql.param_id; node : Params.node }
  | Expr of Sql.Type.t
  | Statement of Document.stmt

let make_target cursor =
  let ( let* ) = Option.bind in
  let located pos target = Some { Sql.value = target; pos } in
  let base = fst cursor.stmt.Document.pos in
  let local = cursor.offset - base in
  let find_node ~f =
    Params.all_nodes (Document.params cursor.stmt)
    |> Seq.filter_map (fun (node : Params.node) ->
      match node.placement, f node with
      | Some { Params.cursor = pos; _ }, Some found -> Some (found, pos)
      | (Some _ | None), _ -> None)
    |> Sql.Pos.find_innermost_opt cursor.offset
  in
  let shared_query () =
    let* (name, pos) =
      List.find_map (fun (lexeme : Recover_parser.lexeme) ->
        match lexeme.token with
        | SHARED_QUERY_REF reference when Sql.Pos.covers reference.pos local ->
          Some (reference.value, Sql.Pos.shift base reference.pos)
        | _ -> None)
        (Document.tokens cursor.stmt)
    in
    let* (stmt, loc) = Document.find_reusable_opt cursor.document name in
    located pos (Shared_query { stmt; loc })
  in
  let param () =
    find_node ~f:(fun (node : Params.node) ->
      match node.kind with Params.Var _ -> Some node | Params.Branch _ -> None)
    |> Option.map (fun (node, pos) -> { Sql.value = Param node; pos })
  in
  let name () =
    let* id =
      let rec loop acc = function
        | [] -> None
        | (lexeme : Recover_parser.lexeme) :: _ when fst lexeme.pos > local -> None
        | (lexeme : Recover_parser.lexeme) :: rest ->
          match Recover_parser.ident_name lexeme.token with
          | Some name when Sql.Pos.covers lexeme.pos local ->
            Some { name; pos = Sql.Pos.shift base lexeme.pos;
              qualifier = Recover_parser.qualifier_before acc }
          | Some _ | None -> loop (lexeme :: acc) rest
      in
      loop [] (Document.tokens cursor.stmt)
    in
    let symbols = Document.scope cursor.stmt cursor.offset in
    let find_columns sources =
      let resolved source =
        Option.map (fun column -> { source; column })
          (Symbol.find_column_opt source id.name)
      in
      match List.filter_map resolved sources with
      | [] -> None
      | first :: rest -> Some (Columns (first, rest))
    in
    let column_in_scope =
      Document.sources cursor.stmt cursor.offset
      |> List.map (Symbol.declared ~schema:cursor.document.Document.index)
      |> Symbol.unique
      |> find_columns
    in
    let in_schema name =
      Symbol.Index.find_opt name cursor.document.Document.index
    in
    let* resolution =
      match id.qualifier with
      | Some qualifier ->
        let owner =
          match Symbol.find_opt symbols qualifier with
          | Some symbol -> Some (Symbol.declared ~schema:cursor.document.Document.index symbol)
          | None -> in_schema qualifier
        in
        Option.fold owner ~none:column_in_scope ~some:(fun owner -> find_columns [ owner ])
      | None ->
        let source_in_scope =
          Symbol.find_opt symbols id.name
          |> Option.map (fun symbol ->
            let declaration = Symbol.declaration ~schema:cursor.document.Document.index symbol in
            Source { symbol; declaration })
        in
        let source_in_schema =
          in_schema id.name
          |> Option.map (fun symbol -> Source { symbol; declaration = Some symbol })
        in
        List.find_map Fun.id [ source_in_scope; column_in_scope; source_in_schema ]
    in
    located id.pos (Name resolution)
  in
  let branch () =
    find_node ~f:(fun (node : Params.node) ->
      match node.kind with
      | Params.Branch (choice, _) -> Some (choice, node)
      | Params.Var _ -> None)
    |> Option.map (fun ((choice, node), pos) ->
      { Sql.value = Branch { choice; node }; pos })
  in
  let expr () =
    Sql.Pos.find_innermost_opt cursor.offset (List.to_seq (Document.exprs cursor.stmt))
    |> Option.map (fun (typ, pos) -> { Sql.value = Expr typ; pos })
  in
  List.find_map (fun candidate -> candidate ())
    [ param; name; branch; expr; shared_query ]
  |> Option.value ~default:{ Sql.value = Statement cursor.stmt; pos = cursor.stmt.pos }

module Markdown = struct
  let with_buffer f = let b = Buffer.create 256 in f b; Buffer.contents b

  let section ?title b = function
    | [] -> ()
    | rows ->
      let width = List.fold_left (fun w (name, _) -> Int.max w (String.length name)) 0 rows in
      Option.iter (bprintf b "\n**%s**\n\n") title;
      bprintf b "```sql\n";
      List.iter (fun (name, typ) -> bprintf b "%-*s  %s\n" width name typ) rows;
      bprintf b "```\n"

  let column_rows = List.map (fun (attr : Sql.attr) -> attr.name, Sql.Type.show attr.domain)

  let param_row depth node =
    let shape =
      match Params.shape node with
      | Params.Scalar typ -> Sql.Type.show typ
      | Row [ typ ] -> Sql.Type.show typ ^ " list"
      | Row types -> sprintf "(%s) list" (String.concat ", " (List.map Sql.Type.show types))
      | Compound -> ""
    in
    String.make (depth * 2) ' ' ^ Params.label node, shape

  let param_rows nodes =
    let rec rows depth (node : Params.node) =
      let shape' = Params.shape node in
      let children = match shape' with Compound -> node.children | Scalar _ | Row _ -> [] in
      Seq.cons
        (param_row depth node)
        (Seq.concat_map (rows (depth + 1)) (List.to_seq children))
    in
    Params.outline nodes |> List.to_seq |> Seq.concat_map (rows 0) |> List.of_seq

  let origin b (source : Symbol.t) =
    match source.kind, source.loc with
    | Table, Some loc -> bprintf b "\nDeclared in `%s`\n" loc.file
    | Table, None -> ()
    | (Cte | Derived | Alias _), _ -> bprintf b "\nAvailable in this statement\n"

  let resolution = function
    | Source { symbol; declaration } ->
      with_buffer @@ fun b ->
        begin match symbol.kind with
        | Table -> bprintf b "**table** `%s`\n\n" symbol.name
        | Cte -> bprintf b "**CTE** `%s`\n\n" symbol.name
        | Derived -> bprintf b "**subquery** `%s`\n\n" symbol.name
        | Alias target ->
          bprintf b "**alias** `%s` of `%s`\n\n" symbol.name (Sql.show_table_name target)
        end;
        section b (column_rows (Symbol.columns symbol));
        origin b (Option.value ~default:symbol declaration)
    | Columns (first, rest) ->
      first :: rest |> List.map (fun { source; column } ->
        with_buffer @@ fun b ->
          section b [ source.name ^ "." ^ column.attr.name, Sql.Type.show column.attr.domain ];
          origin b source)
      |> String.concat "\n---\n"

  let param (node : Params.node) = with_buffer @@ fun b ->
    match Params.shape node with
    | Scalar _ | Row _ -> section b (param_rows [ node ])
    | Compound ->
      section b [ param_row 0 node ];
      section ~title:"Branches" b (param_rows node.children)

  let branch choice (node : Params.node) = with_buffer @@ fun b ->
    bprintf b "branch `%s` of `%s`\n" (Params.label node) (Params.name choice);
    match param_rows node.children with
    | [] -> bprintf b "\nTakes no parameters.\n"
    | rows -> section ~title:"Parameters in this branch" b rows

  let expr typ = with_buffer @@ fun b -> section b [ "expression", Sql.Type.show typ ]

  let kind (kind : Stmt.kind) =
    let tables tables =
      String.concat ", " (List.map Sql.show_table_name tables)
    in
    match kind with
    | Stmt.Select `Zero_one -> "SELECT — at most one row"
    | Stmt.Select `One -> "SELECT — exactly one row"
    | Stmt.Select `Nat -> "SELECT — any number of rows"
    | Stmt.Insert (_, t) -> "INSERT into " ^ Sql.show_table_name t
    | Stmt.Update (Some t) -> "UPDATE " ^ Sql.show_table_name t
    | Stmt.Update None -> "UPDATE"
    | Stmt.Delete l -> "DELETE from " ^ tables l
    | Stmt.Create t -> "CREATE TABLE " ^ Sql.show_table_name t
    | Stmt.CreateIndex name -> "CREATE INDEX " ^ name
    | Stmt.CreateRoutine t -> "CREATE ROUTINE " ^ Sql.show_table_name t
    | Stmt.CreateType name -> "CREATE TYPE " ^ name
    | Stmt.DropType name -> "DROP TYPE " ^ name
    | Stmt.Alter l -> "ALTER " ^ tables l
    | Stmt.Drop t -> "DROP " ^ Sql.show_table_name t
    | Stmt.Other -> "statement"

  let stmt (stmt : Document.stmt) =
    match stmt.outcome, stmt.name with
    | (Verbatim | Rejected _), None -> None
    | (Verbatim | Rejected _), Some name -> Some (sprintf "`%s`\n" name)
    | Checked (checked, _), name ->
      Some (with_buffer @@ fun b ->
        Option.iter (bprintf b "`%s` — ") name;
        bprintf b "%s\n" (kind checked.kind);
        section ~title:"Parameters" b (param_rows checked.params);
        section ~title:"Result" b (column_rows checked.schema))
end

let hover cursor =
  let { Sql.value = target; pos } = make_target cursor in
  let text =
    match target with
    | Param node -> Some (Markdown.param node)
    | Name resolved -> Some (Markdown.resolution resolved)
    | Branch { choice; node } -> Some (Markdown.branch choice node)
    | Expr typ -> Some (Markdown.expr typ)
    | Shared_query { stmt; _ } | Statement stmt -> Markdown.stmt stmt
  in
  Option.map (fun text -> text, pos) text

let definition cursor =
  let locations (source : Symbol.t) (loc : Symbol.loc option) =
    match source.kind, loc with
    | _, None -> []
    | Table, Some loc -> [ loc ]
    | (Cte | Derived | Alias _), Some loc ->
      if Sql.Pos.contains loc.pos cursor.offset then [] else [ loc ]
  in
  let symbol_locations (symbol : Symbol.t) = locations symbol symbol.loc in
  match (make_target cursor).value with
  | Param _ | Branch _ | Expr _ | Statement _ -> []
  | Shared_query { loc; _ } -> [ loc ]
  | Name (Source { symbol; declaration }) ->
    begin match symbol_locations symbol with
    | _ :: _ as found -> found
    | [] -> Option.fold ~none:[] ~some:symbol_locations declaration
    end
  | Name (Columns (first, rest)) ->
    first :: rest |> List.concat_map (fun { source; column } ->
      let loc = match column.loc with None -> source.loc | Some _ -> column.loc in
      locations source loc)

type token = { pos : Sql.Pos.t; typ : Params.token_type }

let semantic_tokens ~lines document =
  let line = Line_index.line lines in
  let single_line token = Int.equal (line (fst token.pos)) (line (snd token.pos)) in
  let rec disjoint acc stop = function
    | [] -> List.rev acc
    | token :: rest when fst token.pos < stop -> disjoint acc stop rest
    | token :: rest -> disjoint (token :: acc) (snd token.pos) rest
  in
  Array.to_seq document.Document.statements
  |> Seq.concat_map (fun (statement : Document.checked_statement) ->
    Params.all_nodes (Document.params statement.stmt))
  |> Seq.filter_map (fun (node : Params.node) ->
    match node.Params.placement with
    | Some { token = Some pos; _ } when not (Sql.Pos.is_empty pos) ->
      Some { pos; typ = Params.token_type node }
    | Some _ | None -> None)
  |> Seq.filter single_line
  |> List.of_seq
  |> List.stable_sort (fun a b -> Int.compare (fst a.pos) (fst b.pos))
  |> disjoint [] 0
