open OUnit2
open Sqlgg

let get_ok = function
  | Ok value -> value
  | Error diagnostics ->
    assert_failure
      (String.concat "; " (List.map (fun d -> d.Analysis.message) diagnostics))

let parse_stmt sql = (get_ok (Analysis.parse sql)).ast

let string_contains haystack needle = ExtLib.String.exists haystack ~sub:needle

let where_is_structurally_present = function
  | Sql.Select { select_complete = { select = (sel, _); _ }; _ } ->
    begin match sel.where with
    | Some _ -> true
    | None -> false
    end
  | _ -> false

let big_select_sql = {|
WITH RECURSIVE hierarchy AS (
  SELECT id, dept_id, name FROM users WHERE id = 1
  UNION ALL
  SELECT u.id, u.dept_id, u.name
  FROM users u
  JOIN hierarchy h ON u.dept_id = h.id
)
SELECT h.id, h.name,
  (SELECT MAX(s.score) FROM scores s WHERE s.user_id = h.id) AS max_score
FROM hierarchy h
LEFT JOIN departments d ON d.id = h.dept_id
JOIN (SELECT id FROM departments WHERE title = 'eng') e ON e.id = d.id
WHERE h.id > 0
  AND (h.name = 'x' OR EXISTS (SELECT 1 FROM scores s2 WHERE s2.user_id = h.id))
  AND LENGTH(h.name) > 0
GROUP BY h.id, h.name, d.id
HAVING COUNT(*) > 0
ORDER BY h.id
LIMIT 10
|}

let test_select_where_roundtrip_structural _ =
  let ast = parse_stmt big_select_sql in
  assert_bool "precondition: WHERE present" (where_is_structurally_present ast);
  let json = Sql.stmt_to_json ast in
  let restored = Sql.stmt_of_json json in
  assert_bool "WHERE must survive JSON roundtrip" (where_is_structurally_present restored);
  begin match restored with
  | Sql.Select
      { select_complete =
          { select =
              ({ where =
                   Some (Sql.Fun { kind = Sql.Logical Sql.And; parameters = _ :: _ :: _; _ });
                 group = _ :: _;
                 having = Some _;
                 from = Some _;
                 _ },
               _);
            order = _ :: _;
            limit = Some _;
            _ };
        cte = Some { is_recursive = true; _ } } ->
    ()
  | _ -> assert_failure "structural match on CTE/JOIN/WHERE/GROUP/HAVING/ORDER/LIMIT failed"
  end

let test_select_where_negative_control _ =
  let ast = parse_stmt big_select_sql in
  let json = Sql.stmt_to_json ast in
  let stripped =
    let open Yojson.Basic.Util in
    match json with
    | `List [`String "Select"; obj] ->
      let sc = obj |> member "select_complete" in
      let sel_pair = sc |> member "select" in
      begin match sel_pair with
      | `List [sel; rest] ->
        let sel' =
          match sel with
          | `Assoc fields ->
            `Assoc (List.map (fun (k, v) ->
              if String.equal k "where" then (k, `Null) else (k, v)) fields)
          | _ -> assert_failure "expected select object"
        in
        let sc' =
          match sc with
          | `Assoc fields ->
            `Assoc (List.map (fun (k, v) ->
              if String.equal k "select" then (k, `List [sel'; rest]) else (k, v)) fields)
          | _ -> assert_failure "expected select_complete object"
        in
        let obj' =
          match obj with
          | `Assoc fields ->
            `Assoc (List.map (fun (k, v) ->
              if String.equal k "select_complete" then (k, sc') else (k, v)) fields)
          | _ -> assert_failure "expected Select payload"
        in
        `List [`String "Select"; obj']
      | _ -> assert_failure "unexpected select pair shape"
      end
    | _ -> assert_failure "unexpected Select JSON shape"
  in
  let restored = Sql.stmt_of_json stripped in
  assert_bool "negative control: stripped WHERE must fail structural check"
    (not (where_is_structurally_present restored))

let test_stmt_jsonschema_builds _ =
  let schema = Ppx_deriving_jsonschema_runtime.json_schema Sql.stmt_jsonschema in
  let bytes = String.length (Yojson.Basic.to_string schema) in
  assert_bool (Printf.sprintf "stmt schema too small: %d" bytes) (Int.compare bytes 1000 > 0);
  Printf.printf "Sql.stmt JSON Schema size: %d bytes\n%!" bytes

let roundtrip_sql sql =
  let ast = parse_stmt sql in
  let restored = Sql.stmt_of_json (Sql.stmt_to_json ast) in
  assert_equal ~cmp:Sql.equal_stmt ~printer:Sql.show_stmt ast restored

let test_corpus_roundtrip _ =
  List.iter roundtrip_sql
    [
      "CREATE TABLE t (id INT NOT NULL, name TEXT NULL)";
      "CREATE INDEX idx_t_id ON t (id)";
      "INSERT INTO t (id, name) VALUES (1, 'a')";
      "INSERT INTO t (id, name) VALUES (1, 'a') ON DUPLICATE KEY UPDATE name = VALUES(name)";
      "UPDATE t SET name = 'b' WHERE id = 1";
      "DELETE FROM t WHERE id = 1";
      "ALTER TABLE t ADD COLUMN x INT NULL";
      "SELECT 1 WHERE @c { A { 1 = 1 } | B { 1 = 2 } }";
      "SELECT 1 WHERE { 1 = 1 }?";
      "SELECT 1 WHERE id IN @ids";
      "SELECT 1 WHERE (1, 2) IN @pairs";
    ]

let test_syntax_result_roundtrip _ =
  let schema =
    get_ok (Analysis.Schema.of_sql "CREATE TABLE t (id INT NOT NULL, name TEXT NULL)")
  in
  let result = get_ok (Analysis.analyze ~schema "SELECT id, name FROM t WHERE id = 1") in
  let json = Syntax.result_to_json result in
  let restored = Syntax.result_of_json json in
  assert_equal ~cmp:Syntax.equal_result ~printer:Syntax.show_result result restored

let test_manual_enum_kind_ctors _ =
  let open Sql.Type.Enum_kind in
  let s = Ctors.of_list ["b"; "a"; "c"] in
  let json = Ctors.to_json s in
  assert_equal ~cmp:Yojson.Basic.equal ~printer:Yojson.Basic.to_string
    (`List [`String "a"; `String "b"; `String "c"]) json;
  assert_bool "ctors roundtrip" (Ctors.equal s (Ctors.of_json json));
  begin match Ctors.of_json (`Int 1) with
  | exception Jsonkit.Of_json_error _ -> ()
  | _ -> assert_failure "Ctors.of_json should reject garbage"
  end

let test_manual_constraints _ =
  let open Sql in
  let s = Constraints.of_list [Constraint.NotNull; Constraint.PrimaryKey] in
  let json = Constraints.to_json s in
  let s' = Constraints.of_json json in
  assert_bool "Constraints roundtrip" (Constraints.equal s s');
  begin match Constraints.of_json (`String "nope") with
  | exception Jsonkit.Of_json_error _ -> ()
  | _ -> assert_failure "Constraints.of_json should reject garbage"
  end

let test_manual_meta _ =
  let open Sql.Meta in
  let m = of_list ["z", "1"; "a", "2"] in
  let json = to_json m in
  begin match json with
  | `Assoc (("a", _) :: _) -> ()
  | `Assoc _ -> assert_failure "Meta.to_json keys must be sorted"
  | _ -> assert_failure "Meta.to_json must be object"
  end;
  assert_bool "Meta roundtrip" (equal m (of_json json));
  begin match of_json (`List []) with
  | exception Jsonkit.Of_json_error _ -> ()
  | _ -> assert_failure "Meta.of_json should reject garbage"
  end

let test_manual_string_set _ =
  let open Sql.Constraint.StringSet in
  let s = of_list ["y"; "x"] in
  let json = to_json s in
  assert_equal ~cmp:Yojson.Basic.equal ~printer:Yojson.Basic.to_string
    (`List [`String "x"; `String "y"]) json;
  assert_bool "StringSet roundtrip" (equal s (of_json json))

let test_fun_shadow_schema_contains_fun _ =
  let schema = Ppx_deriving_jsonschema_runtime.json_schema Sql.stmt_jsonschema in
  let text = Yojson.Basic.to_string schema in
  assert_bool "schema must mention fun_ (shadow for 't still works)"
    (string_contains text "fun_")

let has_extension_diag diagnostics =
  List.exists (fun d ->
    let m = String.lowercase_ascii d.Analysis.message in
    string_contains m "extension" || string_contains m "sqlgg")
    diagnostics

let test_allow_extensions_param_rejected _ =
  match Analysis.parse ~allow_extensions:false "SELECT * FROM t WHERE id = @id" with
  | Error diags ->
    assert_bool "message mentions extension" (has_extension_diag diags);
    begin match diags with
    | { pos = Some (start, stop); _ } :: _ ->
      assert_bool "span non-empty" (Int.compare stop start > 0)
    | _ -> assert_failure "expected diagnostic with byte span"
    end
  | Ok _ -> assert_failure "expected rejection of @id"

let test_allow_extensions_clean_ok _ =
  ignore (get_ok (Analysis.parse ~allow_extensions:false
                    "SELECT id FROM t WHERE id = 1"))

let test_allow_extensions_default_true _ =
  ignore (get_ok (Analysis.parse "SELECT * FROM t WHERE id = @id"));
  ignore (get_ok (Analysis.parse "SELECT id FROM t WHERE id = 1"))

let test_allow_extensions_choices _ =
  match Analysis.parse ~allow_extensions:false
          "SELECT 1 WHERE @c { A { 1 = 1 } | B { 1 = 2 } }" with
  | Error diags -> assert_bool "choices rejected" (has_extension_diag diags)
  | Ok _ -> assert_failure "expected Choices rejection"

let test_allow_extensions_option_actions _ =
  match Analysis.parse ~allow_extensions:false "SELECT 1 WHERE { 1 = 1 }?" with
  | Error diags -> assert_bool "option actions rejected" (has_extension_diag diags)
  | Ok _ -> assert_failure "expected OptionActions rejection"

let test_allow_extensions_create_table_default _ =
  match Analysis.parse ~allow_extensions:false
          "CREATE TABLE t (x INT DEFAULT (@p))" with
  | Error diags -> assert_bool "CREATE DEFAULT rejected" (has_extension_diag diags)
  | Ok _ -> assert_failure "expected @p in CREATE TABLE DEFAULT to be rejected"

let test_allow_extensions_in_tuple_list _ =
  match Analysis.parse ~allow_extensions:false "SELECT 1 WHERE (1, 2) IN @pairs" with
  | Error diags -> assert_bool "tuple list rejected" (has_extension_diag diags)
  | Ok _ -> assert_failure "expected InTupleList rejection"

let test_allow_extensions_analyze _ =
  let schema = get_ok (Analysis.Schema.of_sql "CREATE TABLE t (id INT NOT NULL)") in
  begin match Analysis.analyze ~allow_extensions:false ~schema
                "SELECT id FROM t WHERE id = @id" with
  | Error diags -> assert_bool "analyze rejects param" (has_extension_diag diags)
  | Ok _ -> assert_failure "expected analyze rejection"
  end;
  ignore (get_ok (Analysis.analyze ~allow_extensions:false ~schema
                    "SELECT id FROM t WHERE id = 1"))

let suite =
  "json" >:::
  [
    "select_where_roundtrip_structural" >:: test_select_where_roundtrip_structural;
    "select_where_negative_control" >:: test_select_where_negative_control;
    "stmt_jsonschema_builds" >:: test_stmt_jsonschema_builds;
    "corpus_roundtrip" >:: test_corpus_roundtrip;
    "syntax_result_roundtrip" >:: test_syntax_result_roundtrip;
    "manual_enum_kind_ctors" >:: test_manual_enum_kind_ctors;
    "manual_string_set" >:: test_manual_string_set;
    "manual_constraints" >:: test_manual_constraints;
    "manual_meta" >:: test_manual_meta;
    "fun_shadow_schema_contains_fun" >:: test_fun_shadow_schema_contains_fun;
    "allow_extensions_param_rejected" >:: test_allow_extensions_param_rejected;
    "allow_extensions_clean_ok" >:: test_allow_extensions_clean_ok;
    "allow_extensions_default_true" >:: test_allow_extensions_default_true;
    "allow_extensions_choices" >:: test_allow_extensions_choices;
    "allow_extensions_option_actions" >:: test_allow_extensions_option_actions;
    "allow_extensions_in_tuple_list" >:: test_allow_extensions_in_tuple_list;
    "allow_extensions_create_table_default" >:: test_allow_extensions_create_table_default;
    "allow_extensions_analyze" >:: test_allow_extensions_analyze;
  ]

let () = run_test_tt_main suite
