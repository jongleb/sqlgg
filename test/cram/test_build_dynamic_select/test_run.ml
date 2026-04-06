(* test_run.ml - Test dynamic select query generation *)

open Printf

module M (T: Sqlgg_traits.M with 
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  (* === Test 1: Basic select_one_maybe === *)
  module Test1 = struct
    let single_field_name connection =
      printf "[TEST 1.1] Single field: Name\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"]
      ));
      let _ = Sql.select_product connection ~col:(fun s -> s#name) ~id:1L in
      printf "[TEST 1.1] Completed\n\n"

    let single_field_price connection =
      printf "[TEST 1.2] Single field: Price\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_float 99.99]
      ));
      let _ = Sql.select_product connection ~col:(fun s -> s#price) ~id:2L in
      printf "[TEST 1.2] Completed\n\n"

    let combined_name_and_price connection =
      printf "[TEST 1.3] Combined fields: Name and Price using let+/and+\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Gadget";
          Print_ocaml_impl.mock_float 149.99
        ]
      ));
      let _ = Sql.select_product connection ~col:(fun s ->
        let open Dynamic_select in
        let+ n = s#name
        and+ p = s#price in
        (n, p)
      ) ~id:3L in
      printf "[TEST 1.3] Completed\n\n"

    let three_fields connection =
      printf "[TEST 1.4] Three fields: Name, Price, Category\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Phone";
          Print_ocaml_impl.mock_float 599.99;
          Print_ocaml_impl.mock_text "Electronics"
        ]
      ));
      let _ = Sql.select_product connection ~col:(fun s ->
        let open Dynamic_select in
        let+ n = s#name
        and+ p = s#price
        and+ c = s#category in
        (n, p, c)
      ) ~id:4L in
      printf "[TEST 1.4] Completed\n\n"

    let mapped_field connection =
      printf "[TEST 1.5] Mapped field: Price with transformation\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_float 100.0]
      ));
      let _ = Sql.select_product connection ~col:(fun s ->
        let open Dynamic_select in
        let+ p = s#price in
        Option.map (fun x -> x *. 2.0) p
      ) ~id:5L in
      printf "[TEST 1.5] Completed\n\n"

    let with_return connection =
      printf "[TEST 1.6] Return constructor (constant value)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row []
      ));
      let _ = Sql.select_product connection ~col:(fun _s -> Dynamic_select.pure "constant_value") ~id:6L in
      printf "[TEST 1.6] Completed\n\n"

    let run connection =
      single_field_name connection;
      single_field_price connection;
      combined_name_and_price connection;
      three_fields connection;
      mapped_field connection;
      with_return connection
  end

  (* === Test 2: select with callback (multiple rows) === *)
  module Test2 = struct
    let single_field connection =
      printf "[TEST 2.1] List with single field: Name\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_response [
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"];
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Gadget"];
      ];
      Sql.list_products connection ~col:(fun s -> s#name) ~min_stock:10L (fun ~col ->
        printf "  Row: col=%s\n" (match col with Some s -> s | None -> "NULL")
      );
      printf "[TEST 2.1] Completed\n\n"

    let combined_fields connection =
      printf "[TEST 2.2] List with combined fields: Id, Name and Price\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_response [
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 1L; Print_ocaml_impl.mock_text "Widget"; Print_ocaml_impl.mock_float 19.99];
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 2L; Print_ocaml_impl.mock_text "Gadget"; Print_ocaml_impl.mock_float 29.99];
      ];
      Sql.list_products connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ n = s#name
        and+ p = s#price in
        (i, n, p)
      ) ~min_stock:5L (fun ~col ->
        let (i, n, p) = col in
        printf "  Row: id=%Ld, name=%s, price=%s\n" i 
          (match n with Some s -> s | None -> "NULL")
          (match p with Some f -> sprintf "%.2f" f | None -> "NULL")
      );
      printf "[TEST 2.2] Completed\n\n"

    let run connection =
      single_field connection;
      combined_fields connection
  end

  (* === Test 3: Dynamic select with aliased expressions === *)
  module Test3 = struct
    let single_field connection =
      printf "[TEST 3.1] Single field: label\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget - Electronics"
        ]
      ));
      let _ = Sql.multi_dynamic connection ~col:(fun s -> s#label) ~id:1L in
      printf "[TEST 3.1] Completed\n\n"

    let combined connection =
      printf "[TEST 3.2] Combined: label and total_value\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget - Electronics";
          Print_ocaml_impl.mock_float 999.50
        ]
      ));
      let _ = Sql.multi_dynamic connection ~col:(fun s ->
        let open Dynamic_select in
        let+ l = s#label
        and+ tv = s#total_value in
        (l, tv)
      ) ~id:2L in
      printf "[TEST 3.2] Completed\n\n"

    let run connection =
      single_field connection;
      combined connection
  end

  (* === Test 4: Literal value column === *)
  module Test4 = struct
    let single_field connection =
      printf "[TEST 4.1] Fallback literal field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "N/A"]
      ));
      let _ = Sql.with_verbatim connection ~col:(fun s -> s#fallback) ~id:1L in
      printf "[TEST 4.1] Completed\n\n"

    let combined connection =
      printf "[TEST 4.2] Combined: id, name, fallback, category\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_text "N/A";
          Print_ocaml_impl.mock_text "Electronics"
        ]
      ));
      let _ = Sql.with_verbatim connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ n = s#name
        and+ f = s#fallback
        and+ c = s#category in
        (i, n, f, c)
      ) ~id:2L in
      printf "[TEST 4.2] Completed\n\n"

    let run connection =
      single_field connection;
      combined connection
  end

  (* === Test 5: Typed parameter column === *)
  module Test5 = struct
    let name_field connection =
      printf "[TEST 5.1] Name field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"]
      ));
      let _ = Sql.with_param connection ~col:(fun s -> s#name) ~id:1L in
      printf "[TEST 5.1] Completed\n\n"

    let custom_param connection =
      printf "[TEST 5.2] Custom param field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Custom Value"]
      ));
      let _ = Sql.with_param connection ~col:(fun s -> s#custom "Custom Value") ~id:2L in
      printf "[TEST 5.2] Completed\n\n"

    let combined connection =
      printf "[TEST 5.3] Combined: id, name, custom\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_text "Hello"
        ]
      ));
      let _ = Sql.with_param connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ n = s#name
        and+ c = s#custom "Hello" in
        (i, n, c)
      ) ~id:3L in
      printf "[TEST 5.3] Completed\n\n"

    let run connection =
      name_field connection;
      custom_param connection;
      combined connection
  end


  (* === Test 6: All columns dynamic, different order === *)
  module Test6 = struct
    let first_position connection =
      printf "[TEST 6.1] Dynamic select at first position\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget"
        ]
      ));
      let _ = Sql.first_position connection ~col:(fun s -> s#name) ~id:1L in
      printf "[TEST 6.1] Completed\n\n"

    let first_combined connection =
      printf "[TEST 6.2] Dynamic select at first position with combinator\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_float 99.99
        ]
      ));
      let _ = Sql.first_position connection ~col:(fun s ->
        let open Dynamic_select in
        let+ n = s#name
        and+ p = s#price in
        (n, p)
      ) ~id:2L in
      printf "[TEST 6.2] Completed\n\n"

    let run connection =
      first_position connection;
      first_combined connection
  end

  (* === Test 7: select_one (guaranteed row) === *)
  module Test7 = struct
    let select_one_single connection =
      printf "[TEST 7.1] select_one with single field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"]
      ));
      let _ = Sql.select_one_product connection ~col:(fun s -> s#name) ~id:1L in
      printf "[TEST 7.1] Completed\n\n"

    let select_one_combined connection =
      printf "[TEST 7.2] select_one with combined fields\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_float 99.99
        ]
      ));
      let _ = Sql.select_one_product connection ~col:(fun s ->
        let open Dynamic_select in
        let+ n = s#name
        and+ p = s#price in
        (n, p)
      ) ~id:2L in
      printf "[TEST 7.2] Completed\n\n"

    let run connection =
      select_one_single connection;
      select_one_combined connection
  end

  (* === Test 8: module-wrapped column === *)
  module Test8 = struct
    let with_module_id connection =
      printf "[TEST 8.1] Module-wrapped column: Id\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_int 42L]
      ));
      let _ = Sql.with_module connection ~col:(fun s -> s#id) ~id:1L in
      printf "[TEST 8.1] Completed\n\n"

    let with_module_name connection =
      printf "[TEST 8.2] Module-wrapped: regular column Name\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget"]
      ));
      let _ = Sql.with_module connection ~col:(fun s -> s#name) ~id:2L in
      printf "[TEST 8.2] Completed\n\n"

    let run connection =
      with_module_id connection;
      with_module_name connection
  end
  

  (* === Test 9: IN @list inside subquery branch === *)
  module Test9 = struct
    let in_subquery_filtered connection =
      printf "[TEST 9.1] IN list inside subquery branch\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_int 1L
        ]
      ));
      let _ = Sql.with_in_subquery connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ n = s#name
        and+ f = s#filtered [1.0; 2.0] in
        (i, n, f)
      ) ~id:1L in
      printf "[TEST 9.1] Completed\n\n"

    let run connection =
      in_subquery_filtered connection
  end

  (* === Test 10: arithmetic param inside branch === *)
  module Test10 = struct
    let add_tax connection =
      printf "[TEST 10.1] Arithmetic param in branch (price + tax)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_float 120.0
        ]
      ));
      let _ = Sql.with_arith_param connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ at = s#add_tax (Some 20.0) in
        (i, at)
      ) ~id:1L in
      printf "[TEST 10.1] Completed\n\n"

    let run connection =
      add_tax connection
  end

  (* === Test 11: two params inside branch === *)
  module Test11 = struct
    let test_in_range connection =
      printf "[TEST 11.1] Two params in branch (range)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_bool true
        ]
      ));
      let _ = Sql.with_two_params connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ r = s#in_range (Some 10.0) (Some 20.0) in
        (i, r)
      ) ~id:1L in
      printf "[TEST 11.1] Completed\n\n"

    let run connection =
      test_in_range connection
  end

  (* === Test 12: normal param + IN @list inside one branch === *)
  module Test12 = struct
    let match_with_suffix connection =
      printf "[TEST 12.1] Param + IN list in branch\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_bool true
        ]
      ));
      let _ = Sql.with_param_and_in connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ m = s#match_ "_x" ["a_x"; "b_x"] in
        (i, m)
      ) ~id:1L in
      printf "[TEST 12.1] Completed\n\n"

    let run connection =
      match_with_suffix connection
  end

  (* === Test 13: option-actions inside subquery WHERE === *)
  module Test13 = struct
    let opt_none connection =
      printf "[TEST 13.1] Option-actions in subquery (None)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 1L
        ]
      ));
      let _ = Sql.with_option_actions_in_subquery connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ o = s#opt None in
        (i, o)
      ) ~id:1L in
      printf "[TEST 13.1] Completed\n\n"

    let opt_some connection =
      printf "[TEST 13.2] Option-actions in subquery (Some)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 1L
        ]
      ));
      let _ = Sql.with_option_actions_in_subquery connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ o = s#opt (Some 10.0) in
        (i, o)
      ) ~id:2L in
      printf "[TEST 13.2] Completed\n\n"

    let run connection =
      opt_none connection;
      opt_some connection
  end

  (* === Test 14: tuple list IN inside subquery WHERE === *)
  module Test14 = struct
    let test_pairs connection =
      printf "[TEST 14.1] Tuple list IN inside subquery\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 1L
        ]
      ));
      let _ = Sql.with_tuple_list_in_subquery connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ p = s#pairs [ (1L, Some 10L) ] in
        (i, p)
      ) ~id:1L in
      printf "[TEST 14.1] Completed\n\n"

    let run connection =
      test_pairs connection
  end

  (* === Test 15: CASE expression inside branch === *)
  module Test15 = struct
    let test_casey connection =
      printf "[TEST 15.1] CASE expression inside branch\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 123L
        ]
      ));
      let _ = Sql.with_case_expr connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ c = s#casey 2L 123L in
        (i, c)
      ) ~id:1L in
      printf "[TEST 15.1] Completed\n\n"

    let run connection =
      test_casey connection
  end

  (* === Test 16: typed param inside branch === *)
  module Test16 = struct
    let test_typed connection =
      printf "[TEST 16.1] Typed param inside branch\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "hello"
        ]
      ));
      let _ = Sql.with_typed_param connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ t = s#typed "hello" in
        (i, t)
      ) ~id:1L in
      printf "[TEST 16.1] Completed\n\n"

    let run connection =
      test_typed connection
  end

  (* === Test 17: Complex subquery as plain dynamic column === *)
  module Test17 = struct
    let monster_field connection =
      printf "[TEST 17.1] Monster subquery field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 42L
        ]
      ));
      let _ = Sql.monster_nested connection ~col:(fun s ->
        s#monster 2L 1L "then_v" "else_v" (Some 10.0) ["a"; "b"] [(1L, Some 10L)]
      ) ~id:1L in
      printf "[TEST 17.1] Completed\n\n"

    let combined connection =
      printf "[TEST 17.2] Combined: id + monster\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 42L
        ]
      ));
      let _ = Sql.monster_nested connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ m = s#monster 2L 1L "then_v" "else_v" (Some 10.0) ["a"; "b"] [(1L, Some 10L)] in
        (i, m)
      ) ~id:1L in
      printf "[TEST 17.2] Completed\n\n"

    let run connection =
      monster_field connection;
      combined connection
  end

  (* === Test 18: Various SQL constructs as plain dynamic columns === *)
  module Test18 = struct
    let test_plain connection =
      printf "[TEST 18.1] Plain stock field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 100L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:(fun s -> s#plain) ~id:1L in
      printf "[TEST 18.1] Completed\n\n"

    let test_with_in_list connection =
      printf "[TEST 18.2] IN list subquery field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 3L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:(fun s -> s#with_in_list [1L; 2L; 3L]) ~id:1L in
      printf "[TEST 18.2] Completed\n\n"

    let test_with_optional connection =
      printf "[TEST 18.3] Optional subquery field (None)\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 50L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:(fun s -> s#with_optional None) ~id:1L in
      printf "[TEST 18.3] Completed\n\n"

    let test_with_case connection =
      printf "[TEST 18.4] CASE expression field\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 5L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:(fun s -> s#with_case 1L ["foo"; "bar"]) ~id:1L in
      printf "[TEST 18.4] Completed\n\n"

    let test_full_combo connection =
      printf "[TEST 18.5] Full combo: all fields combined\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 100L;
          Print_ocaml_impl.mock_int 3L;
          Print_ocaml_impl.mock_int 50L;
          Print_ocaml_impl.mock_int 5L;
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_int 42L
        ]
      ));
      let _ = Sql.ultimate_combo connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ p = s#plain
        and+ il = s#with_in_list [1L; 2L]
        and+ wo = s#with_optional (Some 5L)
        and+ wc = s#with_case 1L ["foo"]
        and+ wt = s#with_tuple_list [(1L, Some 10L)]
        and+ fc = s#full_combo (Some 5L) ["x"] 100.0 in
        (i, p, il, wo, wc, wt, fc)
      ) ~id:1L in
      printf "[TEST 18.5] Completed\n\n"

    let run connection =
      test_plain connection;
      test_with_in_list connection;
      test_with_optional connection;
      test_with_case connection;
      test_full_combo connection
  end

  (* === Test 19: Mixed columns with arithmetic expression === *)
  module Test19 = struct
    let test_all_fields connection =
      printf "[TEST 19] All fields combined\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_response [
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_text "Electronics";
          Print_ocaml_impl.mock_int 50L;
          Print_ocaml_impl.mock_float 119.99
        ]
      ];
      let _ = Sql.List.ultimate_combo_simple2 connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ n = s#name
        and+ c = s#category
        and+ st = s#stock
        and+ p = s#price_with_tax 10L in
        (i, n, c, st, p)
      ) (fun ~col -> ignore col) in
      printf "[TEST 19] Completed\n\n"

    let run connection = test_all_fields connection
  end

  (* === Test 20: Star expansion and concatenation in runtime mode === *)
  module Test20 = struct
    let star_only connection =
      printf "[TEST 20.1] Star-only dynamic select\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_float 19.99;
          Print_ocaml_impl.mock_text "Electronics";
          Print_ocaml_impl.mock_int 50L
        ]
      ));
      let _ = Sql.all_cols_runtime connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ n = s#name
        and+ p = s#price
        and+ c = s#category
        and+ st = s#stock in
        (i, n, p, c, st)
      ) ~id:1L in
      printf "[TEST 20.1] Completed\n\n"
  end

  module Test21 = struct
    let star_plus_expr connection =
      printf "[TEST 21.1] Star + expr dynamic select\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_float 19.99;
          Print_ocaml_impl.mock_text "Electronics";
          Print_ocaml_impl.mock_int 50L;
          Print_ocaml_impl.mock_int 3L
        ]
      ));
      let _ = Sql.all_cols_plus_expr_runtime connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ n = s#name
        and+ p = s#price
        and+ c = s#category
        and+ st = s#stock
        and+ ip = s#id_plus in
        (i, n, p, c, st, ip)
      ) ~id:1L in
      printf "[TEST 21.1] Completed\n\n"

    let run connection =
      star_plus_expr connection
  end

  module Test22 = struct
    let multiline_select_list connection =
      printf "[TEST 22.1] Multiline select-list formatting\n";
      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [
          Print_ocaml_impl.mock_int 1L;
          Print_ocaml_impl.mock_text "Widget";
          Print_ocaml_impl.mock_float 19.99;
          Print_ocaml_impl.mock_text "Electronics"
        ]
      ));
      let _ = Sql.multiline_cols_runtime connection ~col:(fun s ->
        let open Dynamic_select in
        let+ i = s#id
        and+ n = s#name
        and+ p = s#price
        and+ c = s#category in
        (i, n, p, c)
      ) ~id:1L in
      printf "[TEST 22.1] Completed\n\n"

    let run connection =
      multiline_select_list connection
  end

  (* === Test 23: Cross-query column reuse via scope functions === *)
  module Test23 = struct
    let reuse_name_column_between_queries connection =
      printf "[TEST 23.1] Reuse one col across three queries\n";
      let get_name s = s#name in

      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget-1"]
      ));
      let _ = Sql.select_product connection ~col:get_name ~id:1L in

      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_response [
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget-2"];
      ];
      Sql.list_products connection ~col:get_name ~min_stock:1L (fun ~col ->
        ignore col
      );

      Print_ocaml_impl.clear_mock_responses ();
      Print_ocaml_impl.setup_select_one_response (Some (
        Print_ocaml_impl.make_mock_row [Print_ocaml_impl.mock_text "Widget-3"]
      ));
      let _ = Sql.first_position connection ~col:get_name ~id:2L in

      printf "[TEST 23.1] Completed\n\n"

    let run connection =
      reuse_name_column_between_queries connection
  end
 

  let run_all_tests connection =
    printf "=== Starting Dynamic Select Tests ===\n\n";
    
    try
      printf "--- Test Group 1: Basic select_one_maybe ---\n";
      Test1.run connection;
      
      printf "--- Test Group 2: Select with callback ---\n";
      Test2.run connection;
      
      printf "--- Test Group 3: Multiple dynamic selects ---\n";
      Test3.run connection;
      
      printf "--- Test Group 4: Verbatim branches ---\n";
      Test4.run connection;
      
      printf "--- Test Group 5: Parameter in branch ---\n";
      Test5.run connection;
      
      printf "--- Test Group 6: Dynamic at first position ---\n";
      Test6.run connection;
      
      printf "--- Test Group 7: select_one ---\n";
      Test7.run connection;
      
      printf "--- Test Group 8: module-wrapped column ---\n";
      Test8.run connection;

      printf "--- Test Group 9: IN list inside subquery branch ---\n";
      Test9.run connection;

      printf "--- Test Group 10: arithmetic param inside branch ---\n";
      Test10.run connection;

      printf "--- Test Group 11: two params inside branch ---\n";
      Test11.run connection;

      printf "--- Test Group 12: param + IN list inside one branch ---\n";
      Test12.run connection;

      printf "--- Test Group 13: option-actions inside subquery WHERE ---\n";
      Test13.run connection;

      printf "--- Test Group 14: tuple list IN inside subquery WHERE ---\n";
      Test14.run connection;

      printf "--- Test Group 15: CASE expression inside branch ---\n";
      Test15.run connection;

      printf "--- Test Group 16: typed param inside branch ---\n";
      Test16.run connection;

      printf "--- Test Group 17: monster nested scenario ---\n";
      Test17.run connection;

      printf "--- Test Group 18: ultimate combo (multiple branches) ---\n";
      Test18.run connection;

      printf "--- Test Group 19---\n";
      Test19.run connection;

      printf "--- Test Group 20: star expansion runtime ---\n";
      Test20.star_only connection;

      printf "--- Test Group 21: star + expr runtime ---\n";
      Test21.run connection;

      printf "--- Test Group 22: multiline select-list ---\n";
      Test22.run connection;

      printf "--- Test Group 23: cross-query column reuse ---\n";
      Test23.run connection;
      
      printf "=== All Dynamic Select Tests Passed ===\n"
    with
    | exn -> 
      printf "\n=== Test Failed with Exception: %s ===\n" (Printexc.to_string exn);
      raise exn
end

module Test = M(Print_ocaml_impl)

let () = 
  let con = () in
  
  printf "Dynamic Select Query Generation Tests\n";
  printf "%s\n" (String.make 50 '=');
  
  Test.run_all_tests con;
  
  printf "\n%s\n" (String.make 50 '=');
  printf "All tests executed successfully!\n"
