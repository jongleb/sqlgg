open V1

let option f = function
  | None -> `Null
  | Some value -> f value

let string_option = option (fun value -> `String value)

let span { start; stop } =
  `Assoc ["start", `Int start; "stop", `Int stop]

let identifier { name; span = location } =
  `Assoc ["name", string_option name; "span", span location]

let table_name { database; name } =
  `Assoc ["database", string_option database; "name", `String name]

let cardinality = function
  | Unknown_cardinality -> "unknown"
  | Zero_one -> "zeroOrOne"
  | One -> "one"
  | Many -> "many"

let rec operation = function
  | Select value ->
    `Assoc
      [
        "kind", `String "select";
        "cardinality", `String (cardinality value);
      ]
  | Insert table ->
    `Assoc ["kind", `String "insert"; "table", table_name table]
  | Create_table table ->
    `Assoc ["kind", `String "createTable"; "table", table_name table]
  | Create_index name ->
    `Assoc ["kind", `String "createIndex"; "name", `String name]
  | Update table ->
    `Assoc ["kind", `String "update"; "table", option table_name table]
  | Delete tables ->
    `Assoc
      [
        "kind", `String "delete";
        "tables", `List (List.map table_name tables);
      ]
  | Alter tables ->
    `Assoc
      [
        "kind", `String "alter";
        "tables", `List (List.map table_name tables);
      ]
  | Drop_table table ->
    `Assoc ["kind", `String "dropTable"; "table", table_name table]
  | Rename_tables renames ->
    let rename (source, target) =
      `Assoc ["source", table_name source; "target", table_name target]
    in
    `Assoc
      [
        "kind", `String "renameTables";
        "renames", `List (List.map rename renames);
      ]
  | Set nested ->
    `Assoc
      [
        "kind", `String "set";
        "nested", option operation nested;
      ]
  | Create_routine name ->
    `Assoc ["kind", `String "createRoutine"; "name", table_name name]
  | Create_type name ->
    `Assoc ["kind", `String "createType"; "name", `String name]
  | Drop_type name ->
    `Assoc ["kind", `String "dropType"; "name", `String name]
  | Create_extension name ->
    `Assoc ["kind", `String "createExtension"; "name", `String name]
  | Drop_extension names ->
    `Assoc
      [
        "kind", `String "dropExtension";
        "names", `List (List.map (fun name -> `String name) names);
      ]
  | Other -> `Assoc ["kind", `String "other"]

let nullability = function
  | Nullable -> "nullable"
  | Strict -> "strict"
  | Unknown -> "unknown"

let sql_type_kind = function
  | Int -> ["kind", `String "int"]
  | UInt64 -> ["kind", `String "uint64"]
  | Text -> ["kind", `String "text"]
  | Blob -> ["kind", `String "blob"]
  | Float -> ["kind", `String "float"]
  | Bool -> ["kind", `String "bool"]
  | Datetime -> ["kind", `String "datetime"]
  | Decimal { precision; scale } ->
    [
      "kind", `String "decimal";
      "precision", option (fun value -> `Int value) precision;
      "scale", option (fun value -> `Int value) scale;
    ]
  | Union { values; closed } ->
    [
      "kind", `String "union";
      "values", `List (List.map (fun value -> `String value) values);
      "closed", `Bool closed;
    ]
  | String_literal value ->
    ["kind", `String "stringLiteral"; "value", `String value]
  | Floating_literal value ->
    ["kind", `String "floatingLiteral"; "value", `Float value]
  | Json_path -> ["kind", `String "jsonPath"]
  | One_or_all -> ["kind", `String "oneOrAll"]
  | Json -> ["kind", `String "json"]
  | Any -> ["kind", `String "any"]

let sql_type { kind; nullability = nullable } =
  `Assoc
    (sql_type_kind kind
     @ ["nullability", `String (nullability nullable)])

let metadata values =
  let binding (key, value) =
    `Assoc ["key", `String key; "value", `String value]
  in
  `List (List.map binding values)

let membership = function
  | In -> "in"
  | Not_in -> "notIn"

let option_action = function
  | Bool_choices -> "boolChoices"
  | Set_default -> "setDefault"

let field_fields { name; typ; metadata = values } =
  [
    "name", `String name;
    "type", sql_type typ;
    "metadata", metadata values;
  ]

let rec parameter = function
  | Scalar { identifier = id; typ; metadata = values } ->
    `Assoc
      ([
         "kind", `String "scalar";
         "identifier", identifier id;
         "type", sql_type typ;
         "metadata", metadata values;
       ])
  | List { identifier = id; typ; metadata = values } ->
    `Assoc
      [
        "kind", `String "list";
        "identifier", identifier id;
        "type", sql_type typ;
        "metadata", metadata values;
      ]
  | Choice_list { identifier = id; membership = member; parameters } ->
    `Assoc
      [
        "kind", `String "choiceList";
        "identifier", identifier id;
        "membership", `String (membership member);
        "parameters", `List (List.map parameter parameters);
      ]
  | Choice { identifier = id; alternatives } ->
    `Assoc
      [
        "kind", `String "choice";
        "identifier", identifier id;
        "alternatives", `List (List.map alternative alternatives);
      ]
  | Dynamic_select { identifier = id; alternatives } ->
    `Assoc
      [
        "kind", `String "dynamicSelect";
        "identifier", identifier id;
        "alternatives", `List (List.map alternative alternatives);
      ]
  | Dynamic_select_join { identifier = id; span = location; table; alias } ->
    `Assoc
      [
        "kind", `String "dynamicSelectJoin";
        "identifier", identifier id;
        "span", span location;
        "table", table_name table;
        "alias", option table_name alias;
      ]
  | Tuple { identifier = id; tuple } ->
    `Assoc
      [
        "kind", `String "tuple";
        "identifier", identifier id;
        "tuple", tuple_kind tuple;
      ]
  | Optional_action
      {
        identifier = id;
        parameters;
        when_span;
        else_span;
        action;
      } ->
    `Assoc
      [
        "kind", `String "optionalAction";
        "identifier", identifier id;
        "parameters", `List (List.map parameter parameters);
        "whenSpan", span when_span;
        "elseSpan", span else_span;
        "action", `String (option_action action);
      ]
  | Shared { reference; span = location; parameters } ->
    `Assoc
      [
        "kind", `String "shared";
        "reference", `String reference;
        "span", span location;
        "parameters", `List (List.map parameter parameters);
      ]

and alternative = function
  | Simple { name; name_span; body_span; parameters } ->
    `Assoc
      [
        "kind", `String "simple";
        "name", string_option name;
        "nameSpan", span name_span;
        "bodySpan", span body_span;
        "parameters",
        option
          (fun values -> `List (List.map parameter values))
          parameters;
      ]
  | Verbatim { name; sql } ->
    `Assoc
      [
        "kind", `String "verbatim";
        "name", `String name;
        "sql", `String sql;
      ]

and tuple_kind = function
  | Insertion fields ->
    `Assoc
      [
        "kind", `String "insertion";
        "fields", `List (List.map (fun value -> `Assoc (field_fields value)) fields);
      ]
  | Where_in { elements; membership = member; span = location } ->
    let element (typ, values) =
      `Assoc ["type", sql_type typ; "metadata", metadata values]
    in
    `Assoc
      [
        "kind", `String "whereIn";
        "elements", `List (List.map element elements);
        "membership", `String (membership member);
        "span", span location;
      ]
  | Value_rows { types; values_start } ->
    `Assoc
      [
        "kind", `String "valueRows";
        "types", `List (List.map sql_type types);
        "valuesStart", `Int values_start;
      ]

let result_column = function
  | Column value ->
    `Assoc (("kind", `String "column") :: field_fields value)
  | Dynamic_column { identifier = id; fields } ->
    let dynamic_field { identifier = id; field; join_dependencies } =
      `Assoc
        [
          "identifier", identifier id;
          "field", `Assoc (field_fields field);
          "joinDependencies",
          `List (List.map (fun dependency -> `Int dependency) join_dependencies);
        ]
    in
    `Assoc
      [
        "kind", `String "dynamicColumn";
        "identifier", identifier id;
        "fields", `List (List.map dynamic_field fields);
      ]

let feature ({ name; span = location } : feature) =
  `Assoc ["name", `String name; "span", span location]

let diagnostic ({ message; span = location } : diagnostic) =
  `Assoc ["message", `String message; "span", option span location]

let statement = function
  | Parsed { sql; operation = operation_value; features } ->
    `Assoc
      [
        "kind", `String "parsed";
        "sql", `String sql;
        "operation", operation operation_value;
        "features", `List (List.map feature features);
      ]
  | Checked { sql; operation = operation_value; parameters; columns; features } ->
    `Assoc
      [
        "kind", `String "checked";
        "sql", `String sql;
        "operation", operation operation_value;
        "parameters", `List (List.map parameter parameters);
        "columns", `List (List.map result_column columns);
        "features", `List (List.map feature features);
      ]
  | Invalid { sql; diagnostics } ->
    `Assoc
      [
        "kind", `String "invalid";
        "sql", `String sql;
        "diagnostics", `List (List.map diagnostic diagnostics);
      ]

let document { ir_version; statements } =
  if ir_version <> V1.ir_version then
    invalid_arg "sqlgg IR document version must be 1"
  else
    `Assoc
      [
        "irVersion", `Int ir_version;
        "statements", `List (List.map statement statements);
      ]

let to_string value = Yojson.Safe.to_string ~std:true (document value)
