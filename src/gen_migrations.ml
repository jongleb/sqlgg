open Printf
open ExtLib
open Sqlgg

let quote_id name =
  sprintf "`%s`" name

let quote_table_name (name : Sql.table_name) =
  Option.map_default (fun db -> sprintf "`%s`.`%s`" db name.tn) (quote_id name.tn) name.db

let with_len base = function
  | Some n -> sprintf "%s(%d)" base n
  | None -> base

let type_kind_to_sql = function
  | Sql.Type.Int -> "INT"
  | UInt64 -> "BIGINT UNSIGNED"
  | Text -> "TEXT"
  | Blob -> "BLOB"
  | Float -> "FLOAT"
  | Bool -> "BOOLEAN"
  | Datetime -> "DATETIME"
  | Decimal { precision = Some p; scale = Some s } -> sprintf "DECIMAL(%d,%d)" p s
  | Decimal { precision; scale = None } -> with_len "DECIMAL" precision
  | Decimal _ -> "DECIMAL"
  | Json -> "JSON"
  | Union { ctors; _ } ->
    sprintf "ENUM(%s)"
      (String.concat ", " (List.map (sprintf "'%s'") (Sql.Type.Enum_kind.Ctors.elements ctors)))
  | Any | StringLiteral _ | Json_path | One_or_all -> "TEXT"
  | FloatingLiteral _ -> "FLOAT"

let source_type_kind_to_sql = function
  | Sql.Source_type.Infer k -> type_kind_to_sql k
  | Int (size, sign) ->
    let base = match size with
      | None -> "INT"
      | Some Tiny -> "TINYINT" | Some Small -> "SMALLINT"
      | Some Medium -> "MEDIUMINT" | Some Big -> "BIGINT"
    in
    base ^ (match sign with Sql.Signed -> "" | Unsigned -> " UNSIGNED")
  | Float Sql.Single -> "FLOAT"
  | Float Double -> "DOUBLE"
  | Blob (PlainBlob, size, len) ->
    let base = match size with
      | None -> "BLOB"
      | Some Tiny -> "TINYBLOB"
      | Some Medium -> "MEDIUMBLOB"
      | Some Long -> "LONGBLOB"
    in
    with_len base len
  | Blob (Varbinary, _, len) -> with_len "VARBINARY" len
  | Text (PlainText, size, len) ->
    let base = match size with
      | None -> "TEXT"
      | Some Tiny -> "TINYTEXT"
      | Some Medium -> "MEDIUMTEXT"
      | Some Long -> "LONGTEXT"
    in
    with_len base len
  | Text (Char, _, len) -> with_len "CHAR" len
  | Text (Varchar, _, len) -> with_len "VARCHAR" len
  | Text (Varchar2, _, len) -> with_len "VARCHAR2" len

let constraint_to_sql = function
  | Sql.Constraint.PrimaryKey -> Some "PRIMARY KEY"
  | NotNull -> Some "NOT NULL"
  | Null -> Some "NULL"
  | Unique -> Some "UNIQUE"
  | Autoincrement -> Some "AUTO_INCREMENT"
  | WithDefault -> None
  | OnConflict _ -> None
  | Composite _ -> None

type default_sql_lookup = string -> string option

let no_defaults : default_sql_lookup = fun _ -> None

let alter_action_attr_to_sql ?(default_sql_lookup=no_defaults) (col : Sql.Alter_action_attr.t) =
  let name = quote_id col.name in
  let kind = Option.map_default (fun (k : _ Sql.collated Sql.located) ->
    let type_sql = " " ^ source_type_kind_to_sql k.value.collated in
    let collation_sql = Option.map_default (fun (c : string Sql.located) ->
      " COLLATE " ^ c.value) "" k.value.collation in
    type_sql ^ collation_sql) "" col.kind in
  let extras = col.extra |> List.filter_map (fun (c : Sql.Alter_action_attr.constraint_ Sql.located) ->
    match c.value with
    | Syntax_constraint cst -> constraint_to_sql cst
    | Default _ -> Option.map (fun s -> "DEFAULT " ^ s) (default_sql_lookup col.name)
  ) in
  let extras = match extras with [] -> "" | l -> " " ^ String.concat " " l in
  sprintf "%s%s%s" name kind extras

let alter_pos_to_sql = function
  | `Default -> ""
  | `First -> " FIRST"
  | `After col -> sprintf " AFTER %s" (quote_id col)

let enrich_with_source_kind source_kind (col : Sql.Alter_action_attr.t) =
  Option.map_default (fun sk ->
    { col with kind = Some (Sql.make_located ~pos:(0,0) ~value:sk) }) col source_kind

let find_column columns col_name =
  List.find_opt (fun (c : Tables.column) -> c.attr.Sql.name = col_name) columns

let inverse_action table_name (columns : Tables.column list) (action : Sql.alter_action) : Sql.alter_action =
  match action with
  | `Add (col, _pos) -> `Drop col.Sql.Alter_action_attr.name
  | `Drop col_name ->
    (match find_column columns col_name with
     | None -> `None
     | Some entry ->
       let col = Sql.Alter_action_attr.from_attr entry.attr |> enrich_with_source_kind entry.source_kind in
       `Add (col, `Default))
  | `Change (old_name, _new_col, _pos) ->
    (match find_column columns old_name with
     | None -> `None
     | Some entry ->
       let old_col = Sql.Alter_action_attr.from_attr entry.attr |> enrich_with_source_kind entry.source_kind in
       `Change (_new_col.Sql.Alter_action_attr.name, old_col, `Default))
  | `RenameTable _new_name -> `RenameTable table_name
  | `RenameColumn (old_name, new_name) -> `RenameColumn (new_name, old_name)
  | `RenameIndex (old_name, new_name) -> `RenameIndex (new_name, old_name)
  | `AddIndex (Some name, _cols) -> `DropIndex name
  | `AddIndex (None, _) -> `None
  | `DropIndex _name -> `None
  | `DropPrimaryKey ->
    let pk_cols = Tables.get_primary_key_columns columns in
    `AddPrimaryKey pk_cols
  | `AddPrimaryKey _cols -> `DropPrimaryKey
  | `AddConstraint (Some name) -> `DropConstraint name
  | `AddConstraint None -> `None
  | `DropConstraint _name -> `None
  | `Default_or_convert_to _ ->
    let cs, collation = match Tables.get_charset table_name with
      | Some old -> old.charset, Option.map (fun v -> Sql.make_located ~pos:(0,0) ~value:v) old.collation
      | None -> None, None
    in
    (match cs, collation with
     | None, None -> `None
     | _ -> `Default_or_convert_to (cs, collation))
  | `TtlOptions (_, _) -> `RemoveTtl (0, 0)
  | `RemoveTtl _ -> `None
  | `None -> `None

let action_to_sql_fragment ?default_sql_lookup (action : Sql.alter_action) =
  match action with
  | `Add (col, pos) ->
    sprintf "ADD COLUMN %s%s" (alter_action_attr_to_sql ?default_sql_lookup col) (alter_pos_to_sql pos)
  | `Drop col_name ->
    sprintf "DROP COLUMN %s" (quote_id col_name)
  | `Change (old_name, new_col, pos) ->
    sprintf "CHANGE COLUMN %s %s%s" (quote_id old_name)
      (alter_action_attr_to_sql ?default_sql_lookup new_col) (alter_pos_to_sql pos)
  | `RenameTable new_name ->
    sprintf "RENAME TO %s" (quote_table_name new_name)
  | `RenameColumn (old_name, new_name) ->
    sprintf "RENAME COLUMN %s TO %s" (quote_id old_name) (quote_id new_name)
  | `RenameIndex (old_name, new_name) ->
    sprintf "RENAME INDEX %s TO %s" (quote_id old_name) (quote_id new_name)
  | `AddIndex (name, cols) ->
    let name_s = Option.map_default (fun n -> " " ^ quote_id n) "" name in
    sprintf "ADD INDEX%s (%s)" name_s (String.concat ", " (List.map quote_id cols))
  | `DropIndex name ->
    sprintf "DROP INDEX %s" (quote_id name)
  | `AddPrimaryKey cols ->
    sprintf "ADD PRIMARY KEY (%s)" (String.concat ", " (List.map quote_id cols))
  | `DropPrimaryKey ->
    "DROP PRIMARY KEY"
  | `AddConstraint name ->
    sprintf "ADD CONSTRAINT%s" (Option.map_default (fun n -> " " ^ quote_id n) "" name)
  | `DropConstraint name ->
    sprintf "DROP CONSTRAINT %s" (quote_id name)
  | `Default_or_convert_to (cs, collation) ->
    let charset_to_sql = function
      | Sql.Named s -> s
      | Binary -> "binary"
      | Ascii -> "ascii"
      | Unicode -> "unicode"
    in
    let convert cs = sprintf "CONVERT TO CHARACTER SET %s" (charset_to_sql cs) in
    let collate c = sprintf "COLLATE %s" c.Sql.value in
    (match cs, collation with
     | Some cs, Some c -> convert cs ^ " " ^ collate c
     | Some cs, None -> convert cs
     | None, Some c -> collate c
     | None, None -> "")
  | `TtlOptions (opts, _) ->
    let opt_to_sql = function
      | `TtlSet (col, n, unit) ->
        sprintf "TTL = %s + INTERVAL %d %s" (quote_id col) n (String.uppercase_ascii unit)
      | `TtlEnable v -> sprintf "TTL_ENABLE = '%s'" v
    in
    String.concat " " (List.map opt_to_sql opts)
  | `RemoveTtl _ -> "REMOVE TTL"
  | `None -> "(* unsupported: index/constraint operation *)"

let alter_to_sql ?default_sql_lookup table_name actions =
  let fragments = List.map (action_to_sql_fragment ?default_sql_lookup) actions in
  sprintf "ALTER TABLE %s %s" (quote_table_name table_name) (String.concat ", " fragments)

type migration = {
  name : string;
  apply : string;
  revert : string;
}

let inverse table_name (columns : Tables.column list) (actions : Sql.alter_action list) =
  let inverse_actions = List.rev_map (inverse_action table_name columns) actions in
  if List.exists (function `None -> true | _ -> false) inverse_actions then
    None
  else
    let effective_name = List.fold_left (fun name action ->
      match action with `RenameTable new_name -> new_name | _ -> name
    ) table_name actions in
    let default_sql_lookup name =
      match List.find_opt (fun (c : Tables.column) -> c.attr.Sql.name = name) columns with
      | Some c -> c.default_sql
      | None -> None
    in
    Some (alter_to_sql ~default_sql_lookup effective_name inverse_actions)

let drop_index_sql index_name table_name =
  sprintf "DROP INDEX %s ON %s" (quote_id index_name) (quote_table_name table_name)

let rename_inverse_sql pairs =
  let inverse_pairs = List.map (fun (old_name, new_name) ->
    sprintf "%s TO %s" (quote_table_name new_name) (quote_table_name old_name)
  ) pairs in
  sprintf "RENAME TABLE %s" (String.concat ", " inverse_pairs)

