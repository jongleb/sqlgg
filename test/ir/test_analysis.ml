open OUnit2
open Sqlgg

let test_parse_select _ =
  match Analysis.parse "SELECT 1" with
  | Ok { ast = Sql.Select _; _ } -> ()
  | Ok _ -> assert_failure "expected SELECT AST"
  | Error diagnostics ->
    assert_failure
      (String.concat "; " (List.map (fun diagnostic -> diagnostic.Analysis.message) diagnostics))

let get_ok = function
  | Ok value -> value
  | Error diagnostics ->
    assert_failure
      (String.concat "; " (List.map (fun diagnostic -> diagnostic.Analysis.message) diagnostics))

let test_schema_contexts_are_isolated _ =
  let users =
    get_ok (Analysis.Schema.of_sql "CREATE TABLE users (id INT NOT NULL)")
  in
  let posts =
    get_ok (Analysis.Schema.of_sql "CREATE TABLE posts (id INT NOT NULL)")
  in
  ignore (get_ok (Analysis.analyze ~schema:users "SELECT id FROM users"));
  ignore (get_ok (Analysis.analyze ~schema:posts "SELECT id FROM posts"));
  begin match Analysis.analyze ~schema:posts "SELECT id FROM users" with
  | Error _ -> ()
  | Ok _ -> assert_failure "users table leaked into posts schema"
  end;
  match Analysis.analyze ~schema:users "SELECT id FROM posts" with
  | Error _ -> ()
  | Ok _ -> assert_failure "posts table leaked into users schema"

let test_schema_accepts_multiple_ddl _ =
  let schema =
    get_ok
      (Analysis.Schema.of_sql
         "CREATE TABLE users (id INT); CREATE TABLE posts (id INT)")
  in
  ignore (get_ok (Analysis.analyze ~schema "SELECT id FROM users"));
  ignore (get_ok (Analysis.analyze ~schema "SELECT id FROM posts"))

let test_schema_rebases_second_parser_diagnostic _ =
  let sql = "CREATE TABLE users (id INT);\nSELECT FROM" in
  match Analysis.Schema.of_sql sql with
  | Error [{ pos = Some (36, 40); _ }] -> ()
  | Error [{ pos = Some pos; _ }] ->
    assert_failure
      (Printf.sprintf
         "expected parser span 36-40, got %d-%d"
         (fst pos)
         (snd pos))
  | Error _ -> assert_failure "expected one positioned parser diagnostic"
  | Ok _ -> assert_failure "expected malformed second statement to fail"

let test_schema_rebases_second_semantic_diagnostic _ =
  let sql = "CREATE TABLE users (id INT);\nSELECT missing FROM users" in
  match Analysis.Schema.of_sql sql with
  | Error [{ pos = Some (36, 43); _ }] -> ()
  | Error [{ pos = Some pos; _ }] ->
    assert_failure
      (Printf.sprintf
         "expected semantic span 36-43, got %d-%d"
         (fst pos)
         (snd pos))
  | Error _ -> assert_failure "expected one positioned semantic diagnostic"
  | Ok _ -> assert_failure "expected invalid second statement to fail"

let test_schema_keeps_split_diagnostic_global _ =
  let sql =
    "CREATE TABLE users (id INT);\n-- @foo | broken\nCREATE TABLE posts (id INT)"
  in
  match Analysis.Schema.of_sql sql with
  | Error [{ pos = Some (29, 46); _ }] -> ()
  | Error [{ pos = Some pos; _ }] ->
    assert_failure
      (Printf.sprintf
         "expected split span 29-46, got %d-%d"
         (fst pos)
         (snd pos))
  | Error _ -> assert_failure "expected one positioned split diagnostic"
  | Ok _ -> assert_failure "expected malformed statement properties to fail"

let test_schema_returns_expected_sql_diagnostic _ =
  let sql =
    "CREATE TABLE users (id INT); CREATE TABLE users (id INT)"
  in
  match Analysis.Schema.of_sql sql with
  | Error [{ message = "table users already exists"; pos = None }] -> ()
  | Error [{ message; _ }] ->
    assert_failure ("unexpected schema diagnostic: " ^ message)
  | Error _ -> assert_failure "expected one schema diagnostic"
  | Ok _ -> assert_failure "expected duplicate table to fail"

let test_syntax_diagnostic _ =
  match Analysis.parse "SELECT FROM" with
  | Error [{ message; pos = Some _ }] ->
    assert_bool "diagnostic message is empty" (String.length message > 0)
  | Error _ -> assert_failure "expected one positioned syntax diagnostic"
  | Ok _ -> assert_failure "expected malformed SQL to fail"

let test_semantic_diagnostic _ =
  let schema =
    get_ok (Analysis.Schema.of_sql "CREATE TABLE users (id INT NOT NULL)")
  in
  match Analysis.analyze ~schema "SELECT missing FROM users" with
  | Error [{ message; _ }] ->
    assert_bool
      "diagnostic should identify the missing column"
      (String.length message > 0)
  | Error _ -> assert_failure "expected one semantic diagnostic"
  | Ok _ -> assert_failure "expected unknown column to fail"

let test_calls_restore_ambient_schema _ =
  let previous = Compile.snapshot () in
  Fun.protect
    ~finally:(fun () -> Compile.restore previous)
    (fun () ->
      Compile.reset ();
      List.iter
        (fun statement ->
          ignore (Compile.statement ~dynamic_select:Props.Off statement))
        (Statements.split "CREATE TABLE ambient (id INT NOT NULL)");
      let isolated =
        get_ok (Analysis.Schema.of_sql "CREATE TABLE isolated (id INT NOT NULL)")
      in
      ignore (get_ok (Analysis.analyze ~schema:isolated "SELECT id FROM isolated"));
      let ambient = Analysis.Schema.current () in
      ignore (get_ok (Analysis.analyze ~schema:ambient "SELECT id FROM ambient"));
      match Analysis.analyze ~schema:ambient "SELECT id FROM isolated" with
      | Error _ -> ()
      | Ok _ -> assert_failure "isolated table leaked into ambient schema")

let test_expected_schema_error_restores_ambient_state _ =
  let previous = Compile.snapshot () in
  Fun.protect
    ~finally:(fun () -> Compile.restore previous)
    (fun () ->
      Compile.reset ();
      List.iter
        (fun statement ->
          ignore (Compile.statement ~dynamic_select:Props.Off statement))
        (Statements.split "CREATE TABLE ambient (id INT NOT NULL)");
      begin match Analysis.Schema.of_sql "CREATE TABLE broken (" with
      | Error _ -> ()
      | Ok _ -> assert_failure "expected invalid schema SQL to fail"
      end;
      let ambient = Analysis.Schema.current () in
      ignore (get_ok (Analysis.analyze ~schema:ambient "SELECT id FROM ambient")))

let test_unexpected_failure_is_raised_and_restores_state _ =
  let previous = Compile.snapshot () in
  Fun.protect
    ~finally:(fun () -> Compile.restore previous)
    (fun () ->
      Compile.reset ();
      List.iter
        (fun statement ->
          ignore (Compile.statement ~dynamic_select:Props.Off statement))
        (Statements.split "CREATE TABLE ambient (id INT NOT NULL)");
      assert_raises
        (Failure "not implemented : ensure_res_expr for SELECT")
        (fun () ->
          ignore (Analysis.Schema.of_sql "SET x = (SELECT 1)"));
      assert_raises
        (Failure "outside analysis")
        (fun () -> Prelude.fail "outside analysis");
      let ambient = Analysis.Schema.current () in
      ignore (get_ok (Analysis.analyze ~schema:ambient "SELECT id FROM ambient")))

let test_sql_error_mode_is_nested_and_restored _ =
  let expect_sql_error message f =
    match f () with
    | exception Prelude.Sql_error actual -> assert_equal message actual
    | exception exn ->
      assert_failure
        ("expected Sql_error, got " ^ Printexc.to_string exn)
    | _ -> assert_failure "expected Sql_error"
  in
  Prelude.with_sql_errors (fun () ->
      expect_sql_error "outer before" (fun () -> Prelude.fail "outer before");
      Prelude.with_sql_errors (fun () ->
          expect_sql_error "inner" (fun () -> Prelude.fail "inner"));
      expect_sql_error "outer after" (fun () -> Prelude.fail "outer after"));
  assert_raises
    Exit
    (fun () -> Prelude.with_sql_errors (fun () -> raise Exit));
  assert_raises
    (Failure "outside")
    (fun () -> Prelude.fail "outside")

let suite =
  "analysis" >::: [
    "parse SELECT" >:: test_parse_select;
    "schema contexts are isolated" >:: test_schema_contexts_are_isolated;
    "schema accepts multiple DDL" >:: test_schema_accepts_multiple_ddl;
    "schema rebases second parser diagnostic" >:: test_schema_rebases_second_parser_diagnostic;
    "schema rebases second semantic diagnostic" >:: test_schema_rebases_second_semantic_diagnostic;
    "schema keeps split diagnostic global" >:: test_schema_keeps_split_diagnostic_global;
    "schema returns expected SQL diagnostic" >:: test_schema_returns_expected_sql_diagnostic;
    "syntax diagnostic" >:: test_syntax_diagnostic;
    "semantic diagnostic" >:: test_semantic_diagnostic;
    "calls restore ambient schema" >:: test_calls_restore_ambient_schema;
    "expected schema error restores ambient state" >:: test_expected_schema_error_restores_ambient_state;
    "unexpected failure is raised and restores state" >:: test_unexpected_failure_is_raised_and_restores_state;
    "SQL error mode is nested and restored" >:: test_sql_error_mode_is_nested_and_restored;
  ]

let () = run_test_tt_main suite
