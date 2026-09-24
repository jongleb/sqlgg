open OUnit2
open Sqlgg
open Sqlgg_ir

let get_ok = function
  | Ok value -> value
  | Error diagnostics ->
    assert_failure
      (String.concat
         "; "
         (List.map
            (fun diagnostic -> diagnostic.Analysis.message)
            diagnostics))

let test_lower_parsed_select _ =
  let parsed = get_ok (Analysis.parse "SELECT 1") in
  match Lower.parsed parsed with
  | V1.Parsed
      {
        sql = "SELECT 1";
        operation = Select Unknown_cardinality;
        features = [];
      } ->
    ()
  | _ -> assert_failure "expected a schema-less parsed SELECT"

let test_lower_parsed_set_preserves_nested_statement _ =
  let parsed =
    get_ok
      (Analysis.parse
         "SET STATEMENT max_statement_time = 1 FOR SELECT 1")
  in
  let statement = Lower.parsed parsed in
  begin match statement with
  | V1.Parsed
      {
        operation =
          Set (Some (Select Unknown_cardinality));
        _;
      } ->
    ()
  | _ -> assert_failure "expected SET to preserve its nested SELECT"
  end;
  assert_equal
    ~printer:(fun value -> value)
    "{\"irVersion\":1,\"statements\":[{\"kind\":\"parsed\",\"sql\":\"SET STATEMENT max_statement_time = 1 FOR SELECT 1\",\"operation\":{\"kind\":\"set\",\"nested\":{\"kind\":\"select\",\"cardinality\":\"unknown\"}},\"features\":[]}]}"
    (Json.to_string
       { V1.ir_version = V1.ir_version; statements = [statement] })

let test_lower_checked_select _ =
  let schema =
    get_ok
      (Analysis.Schema.of_sql
         "CREATE TABLE users (id INT NOT NULL)")
  in
  let checked =
    get_ok
      (Analysis.analyze
         ~schema
         "SELECT id FROM users WHERE id = @id")
  in
  match Lower.checked checked with
  | V1.Checked
      {
        operation = Select Many;
        parameters =
          [
            Scalar
              {
                identifier = { name = Some "id"; _ };
                typ = { kind = Int; nullability = Strict };
                _;
              };
          ];
        columns =
          [
            Column
              {
                name = "id";
                typ = { kind = Int; nullability = Strict };
                _;
              };
          ];
        _;
      } ->
    ()
  | _ -> assert_failure "expected a typed checked SELECT"

let test_exact_statement_json _ =
  let int = V1.{ kind = Int; nullability = Strict } in
  let id =
    V1.{ name = Some "id"; span = { start = 7; stop = 10 } }
  in
  let document =
    V1.
      {
        ir_version = 1;
        statements =
          [
            Parsed
              {
                sql = "SELECT 1";
                operation = Select Unknown_cardinality;
                features = [];
              };
            Checked
              {
                sql = "SELECT id";
                operation = Select One;
                parameters =
                  [Scalar { identifier = id; typ = int; metadata = [] }];
                columns =
                  [Column { name = "id"; typ = int; metadata = [] }];
                features =
                  [{ name = "collation"; span = { start = 3; stop = 6 } }];
              };
            Invalid
              {
                sql = "SELECT FROM";
                diagnostics =
                  [
                    {
                      message = "syntax error";
                      span = Some { start = 7; stop = 11 };
                    };
                  ];
              };
          ];
      }
  in
  assert_equal
    ~printer:(fun value -> value)
    "{\"irVersion\":1,\"statements\":[{\"kind\":\"parsed\",\"sql\":\"SELECT 1\",\"operation\":{\"kind\":\"select\",\"cardinality\":\"unknown\"},\"features\":[]},{\"kind\":\"checked\",\"sql\":\"SELECT id\",\"operation\":{\"kind\":\"select\",\"cardinality\":\"one\"},\"parameters\":[{\"kind\":\"scalar\",\"identifier\":{\"name\":\"id\",\"span\":{\"start\":7,\"stop\":10}},\"type\":{\"kind\":\"int\",\"nullability\":\"strict\"},\"metadata\":[]}],\"columns\":[{\"kind\":\"column\",\"name\":\"id\",\"type\":{\"kind\":\"int\",\"nullability\":\"strict\"},\"metadata\":[]}],\"features\":[{\"name\":\"collation\",\"span\":{\"start\":3,\"stop\":6}}]},{\"kind\":\"invalid\",\"sql\":\"SELECT FROM\",\"diagnostics\":[{\"message\":\"syntax error\",\"span\":{\"start\":7,\"stop\":11}}]}]}"
    (Json.to_string document)

let member name = function
  | `Assoc fields -> List.assoc name fields
  | _ -> assert_failure ("expected object containing " ^ name)

let definition_of_ref definitions = function
  | `String reference ->
    let prefix = "#/$defs/" in
    let prefix_length = String.length prefix in
    if String.length reference <= prefix_length
       || String.sub reference 0 prefix_length <> prefix
    then assert_failure ("unsupported schema ref " ^ reference)
    else
      member
        (String.sub
           reference
           prefix_length
           (String.length reference - prefix_length))
        definitions
  | _ -> assert_failure "expected string schema ref"

let rec properties_of definitions = function
  | `Assoc fields ->
    let inherited =
      match List.assoc_opt "$ref" fields with
      | None -> []
      | Some reference ->
        properties_of definitions (definition_of_ref definitions reference)
    in
    let own =
      match List.assoc_opt "properties" fields with
      | Some (`Assoc properties) -> properties
      | Some _ -> assert_failure "expected properties object"
      | None -> []
    in
    own @ inherited
  | _ -> assert_failure "expected schema object"

let one_of_tags definitions definition =
  member "oneOf" definition
  |> function
  | `List alternatives ->
    List.map
      (fun alternative ->
        alternative
        |> properties_of definitions
        |> List.assoc "kind"
        |> member "const"
        |> Yojson.Safe.Util.to_string)
      alternatives
  | _ -> assert_failure "expected oneOf array"

let test_json_schema_tags _ =
  let schema =
    Yojson.Safe.from_file (Sys.getenv "SQLGG_IR_SCHEMA")
  in
  assert_equal
    (`String "https://json-schema.org/draft/2020-12/schema")
    (member "$schema" schema);
  assert_equal (`Int 1) (schema |> member "properties" |> member "irVersion" |> member "const");
  let definitions = member "$defs" schema in
  assert_equal
    ["parsed"; "checked"; "invalid"]
    (definitions |> member "statement" |> one_of_tags definitions);
  assert_equal
    [
      "scalar";
      "list";
      "choiceList";
      "choice";
      "dynamicSelect";
      "dynamicSelectJoin";
      "tuple";
      "optionalAction";
      "shared";
    ]
    (definitions |> member "parameter" |> one_of_tags definitions);
  assert_equal
    ["column"; "dynamicColumn"]
    (definitions |> member "resultColumn" |> one_of_tags definitions);
  assert_equal
    [
      "select";
      "insert";
      "createTable";
      "createIndex";
      "update";
      "delete";
      "alter";
      "dropTable";
      "renameTables";
      "set";
      "createRoutine";
      "createType";
      "dropType";
      "createExtension";
      "dropExtension";
      "other";
    ]
    (definitions |> member "operation" |> one_of_tags definitions);
  assert_equal
    [
      "int";
      "uint64";
      "text";
      "blob";
      "float";
      "bool";
      "datetime";
      "decimal";
      "union";
      "stringLiteral";
      "floatingLiteral";
      "jsonPath";
      "oneOrAll";
      "json";
      "any";
    ]
    (definitions |> member "sqlType" |> one_of_tags definitions);
  assert_equal
    ["simple"; "verbatim"]
    (definitions |> member "alternative" |> one_of_tags definitions);
  assert_equal
    ["insertion"; "whereIn"; "valueRows"]
    (definitions |> member "tupleKind" |> one_of_tags definitions)

let test_json_schema_rejects_extra_properties_structurally _ =
  let schema =
    Yojson.Safe.from_file (Sys.getenv "SQLGG_IR_SCHEMA")
  in
  let definitions = member "$defs" schema in
  assert_equal
    (`Bool false)
    (definitions |> member "sqlType" |> member "unevaluatedProperties");
  assert_equal
    (`Bool false)
    (definitions |> member "operation" |> member "unevaluatedProperties");
  let rename =
    definitions
    |> member "operation"
    |> member "oneOf"
    |> Yojson.Safe.Util.to_list
    |> List.find (fun alternative ->
           List.assoc_opt "kind" (properties_of definitions alternative)
           = Some (`Assoc ["const", `String "renameTables"]))
  in
  assert_equal
    (`Bool false)
    (rename
     |> member "properties"
     |> member "renames"
     |> member "items"
     |> member "additionalProperties")

let located value pos = Sql.{ value; pos }

let parameter_id name pos = located name pos

let native_type ?(nullable = Sql.Type.Depends) kind =
  { Sql.Type.t = kind; nullability = nullable }

let native_field name typ =
  Sql.
    {
      name;
      domain = typ;
      extra = Constraints.empty;
      meta = Meta.empty ();
    }

let empty_annotations : Syntax.stmt_annotations =
  {
    src_tbls = [];
    cte_defs = [];
    table_aliases = [];
    table_defs = [];
    expr_types = [];
    result_aliases = [];
    select_scopes = [];
  }

let test_lower_preserves_complex_parameters_and_columns _ =
  let int = native_type Sql.Type.Int in
  let scalar name pos =
    Sql.Single
      ({ id = parameter_id (Some name) pos; typ = int }, Sql.Meta.empty ())
  in
  let alternative =
    Sql.Simple
      {
        ctor = parameter_id (Some "byId") (18, 40);
        ctor_pos = (20, 25);
        body = Some [scalar "nested" (30, 37)];
      }
  in
  let vars =
    [
      scalar "scalar" (0, 7);
      Sql.SingleIn
        ({ id = parameter_id (Some "list") (8, 13); typ = int }, Sql.Meta.empty ());
      Sql.ChoiceIn
        {
          param = parameter_id (Some "choiceList") (14, 24);
          kind = `NotIn;
          vars = [scalar "choiceItem" (25, 35)];
        };
      Sql.Choice
        (parameter_id (Some "choice") (36, 42),
         [alternative; Verbatim ("all", "TRUE")]);
      Sql.DynamicSelect
        (parameter_id (Some "columns") (43, 50), [alternative]);
      Sql.DynamicSelectJoin
        {
          pid = parameter_id (Some "join") (51, 55);
          pos = (50, 70);
          source =
            {
              table = Sql.make_table_name ~db:"app" "users";
              alias = Some (Sql.make_table_name "u");
            };
        };
      Sql.TupleList
        (parameter_id (Some "insertRows") (71, 81),
         Insertion [native_field "id" int]);
      Sql.TupleList
        (parameter_id (Some "whereRows") (82, 91),
         Where_in
           (located
              ([int, Sql.Meta.of_list ["codec", "id"]], `In)
              (80, 100)));
      Sql.TupleList
        (parameter_id (Some "valueRows") (101, 110),
         ValueRows { types = [int]; values_start_pos = 111 });
      Sql.OptionActionChoice
        (parameter_id (Some "optional") (112, 120),
         [scalar "inside" (121, 127)],
         ((112, 130), (131, 135)),
         Sql.SetDefault);
      Sql.SharedVarsGroup
        ([scalar "sharedValue" (136, 147)], located "sharedQuery" (135, 150));
    ]
  in
  let dynamic_id = parameter_id (Some "result") (151, 157) in
  let native : Syntax.result =
    {
      sql = "SELECT complex";
      schema =
        [
          Sql.Attr (native_field "fixed" int);
          Sql.Dynamic
            (dynamic_id,
             [
               {
                 field_id = parameter_id (Some "dynamic") (158, 165);
                 field_attr = native_field "computed" int;
                 join_deps = [0; 2];
               };
             ]);
        ];
      vars;
      kind = Stmt.Other;
      dialect_features = [];
      annotations = empty_annotations;
    }
  in
  match Lower.checked native with
  | V1.Checked
      {
        parameters =
          [
            Scalar _;
            List _;
            Choice_list { membership = Not_in; _ };
            Choice
              {
                alternatives =
                  [
                    Simple
                      {
                        name = Some "byId";
                        name_span = { start = 20; stop = 25 };
                        body_span = { start = 18; stop = 40 };
                        _;
                      };
                    Verbatim _;
                  ];
                _;
              };
            Dynamic_select _;
            Dynamic_select_join
              {
                table = { database = Some "app"; name = "users" };
                alias = Some { database = None; name = "u" };
                _;
              };
            Tuple { tuple = Insertion [_]; _ };
            Tuple { tuple = Where_in { membership = In; _ }; _ };
            Tuple { tuple = Value_rows { values_start = 111; _ }; _ };
            Optional_action { action = Set_default; _ };
            Shared { reference = "sharedQuery"; _ };
          ];
        columns =
          [
            Column { name = "fixed"; _ };
            Dynamic_column
              {
                fields =
                  [
                    {
                      field = { name = "computed"; _ };
                      join_dependencies = [0; 2];
                      _;
                    };
                  ];
                _;
              };
          ];
        _;
      } ->
    ()
  | _ -> assert_failure "complex parameter or result-column structure was lost"

let test_lower_maps_every_sql_type_and_nullability _ =
  let union =
    Sql.Type.Union
      {
        ctors = Sql.Type.Enum_kind.make ["b"; "a"];
        is_closed = true;
      }
  in
  let native_kinds =
    [
      Sql.Type.Int;
      UInt64;
      Text;
      Blob;
      Float;
      Bool;
      Datetime;
      Decimal { precision = Some 10; scale = Some 2 };
      union;
      StringLiteral "literal";
      FloatingLiteral 1.5;
      Json_path;
      One_or_all;
      Json;
      Any;
    ]
  in
  let vars =
    List.mapi
      (fun index kind ->
        let nullability =
          match index mod 3 with
          | 0 -> Sql.Type.Nullable
          | 1 -> Strict
          | _ -> Depends
        in
        Sql.Single
          ({
             id = parameter_id None (index, index + 1);
             typ = native_type ~nullable:nullability kind;
           },
           Sql.Meta.empty ()))
      native_kinds
  in
  let native : Syntax.result =
    {
      sql = "SELECT types";
      schema = [];
      vars;
      kind = Stmt.Other;
      dialect_features = [];
      annotations = empty_annotations;
    }
  in
  let actual =
    match Lower.checked native with
    | V1.Checked { parameters; _ } ->
      List.map
        (function
          | V1.Scalar { typ; _ } -> typ
          | _ -> assert_failure "expected scalar parameter")
        parameters
    | _ -> assert_failure "expected checked statement"
  in
  let expected_kinds =
    V1.
      [
        Int;
        UInt64;
        Text;
        Blob;
        Float;
        Bool;
        Datetime;
        Decimal { precision = Some 10; scale = Some 2 };
        Union { values = ["a"; "b"]; closed = true };
        String_literal "literal";
        Floating_literal 1.5;
        Json_path;
        One_or_all;
        Json;
        Any;
      ]
  in
  assert_equal expected_kinds (List.map (fun typ -> typ.V1.kind) actual);
  assert_equal
    V1.[Nullable; Strict; Unknown]
    (actual
     |> List.filteri (fun index _ -> index < 3)
     |> List.map (fun typ -> typ.V1.nullability))

let test_lower_invalid_preserves_byte_span _ =
  let diagnostic : Analysis.diagnostic =
    { message = "bad SQL"; pos = Some (2, 9) }
  in
  match Lower.invalid ~sql:"SELECT ?" [diagnostic] with
  | V1.Invalid
      {
        sql = "SELECT ?";
        diagnostics =
          [{ message = "bad SQL"; span = Some { start = 2; stop = 9 } }];
      } ->
    ()
  | _ -> assert_failure "expected invalid statement with byte span"

let test_json_rejects_non_finite_float _ =
  let document value =
    V1.
      {
        ir_version;
        statements =
          [
            Checked
              {
                sql = "SELECT value";
                operation = Select One;
                parameters = [];
                columns =
                  [
                    Column
                      {
                        name = "value";
                        typ =
                          {
                            kind = Floating_literal value;
                            nullability = Strict;
                          };
                        metadata = [];
                      };
                  ];
                features = [];
              };
          ];
      }
  in
  List.iter
    (fun value ->
      match Json.to_string (document value) with
      | exception Yojson.Json_error _ -> ()
      | json ->
        assert_failure
          ("expected non-finite float rejection, got " ^ json))
    [Float.nan; Float.infinity; Float.neg_infinity]

let test_json_rejects_wrong_ir_version _ =
  assert_equal 1 V1.ir_version;
  let invalid : V1.document =
    { ir_version = 2; statements = [] }
  in
  assert_raises
    (Invalid_argument "sqlgg IR document version must be 1")
    (fun () -> ignore (Json.document invalid))

let suite =
  "ir" >::: [
    "lower parsed SELECT" >:: test_lower_parsed_select;
    "lower parsed SET with nested statement" >:: test_lower_parsed_set_preserves_nested_statement;
    "lower checked SELECT" >:: test_lower_checked_select;
    "exact statement JSON" >:: test_exact_statement_json;
    "JSON Schema tags" >:: test_json_schema_tags;
    "JSON Schema strictness" >:: test_json_schema_rejects_extra_properties_structurally;
    "preserve complex parameters and columns" >:: test_lower_preserves_complex_parameters_and_columns;
    "map every SQL type and nullability" >:: test_lower_maps_every_sql_type_and_nullability;
    "lower invalid byte span" >:: test_lower_invalid_preserves_byte_span;
    "reject non-finite JSON floats" >:: test_json_rejects_non_finite_float;
    "reject wrong IR version" >:: test_json_rejects_wrong_ir_version;
  ]

let () = run_test_tt_main suite
