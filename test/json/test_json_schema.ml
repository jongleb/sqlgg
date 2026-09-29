open OUnit2
open Sqlgg

let string_contains haystack needle = ExtLib.String.exists haystack ~sub:needle

let count_substring haystack needle =
  let hlen = String.length haystack and nlen = String.length needle in
  let rec loop i acc =
    if Int.compare (i + nlen) hlen > 0 then acc
    else if String.equal (String.sub haystack i nlen) needle then loop (i + 1) (acc + 1)
    else loop (i + 1) acc
  in
  loop 0 0

let collect_refs json =
  let refs = ref [] in
  let rec walk = function
    | `Assoc fields ->
      List.iter
        (fun (k, v) ->
          if String.equal k "$ref" then
            match v with
            | `String s -> refs := s :: !refs
            | _ -> ()
          else walk v)
        fields
    | `List items -> List.iter walk items
    | _ -> ()
  in
  walk json;
  List.rev !refs

let def_names schema =
  match schema with
  | `Assoc fields ->
    begin match List.find_opt (fun (k, _) -> String.equal k "$defs") fields with
    | Some (_, `Assoc entries) -> List.map fst entries
    | _ -> []
    end
  | _ -> []

let test_stmt_bundle_shrinks _ =
  let raw =
    (Ppx_deriving_jsonschema_runtime.json_schema Sql.stmt_jsonschema
      :> Yojson.Basic.t)
  in
  let raw_s = Yojson.Basic.to_string raw in
  let bun = Lazy.force Json_schema.stmt in
  let bun_s = Yojson.Basic.to_string bun in
  assert_bool
    (Printf.sprintf "raw too small to be the known bloated schema: %d" (String.length raw_s))
    (Int.compare (String.length raw_s) 1_000_000 > 0);
  assert_bool
    (Printf.sprintf
       "bundled (%d) must be much smaller than raw (%d)"
       (String.length bun_s) (String.length raw_s))
    (Int.compare (String.length bun_s * 5) (String.length raw_s) < 0);
  assert_equal ~cmp:Int.equal ~printer:string_of_int 0 (count_substring bun_s "\"$id\"");
  assert_equal ~cmp:Int.equal ~printer:string_of_int 1 (count_substring bun_s "\"$defs\"")

let test_document_bundle_clean _ =
  let schema = Lazy.force Ir_document.json_schema in
  let text = Yojson.Basic.to_string schema in
  assert_bool "document schema has $schema" (string_contains text "2020-12");
  assert_equal ~cmp:Int.equal ~printer:string_of_int 0 (count_substring text "\"$id\"");
  assert_equal ~cmp:Int.equal ~printer:string_of_int 1 (count_substring text "\"$defs\"");
  List.iter
    (fun name ->
      assert_bool ("variant " ^ name)
        (string_contains text ("\"const\":\"" ^ name ^ "\"")))
    [ "Parsed"; "Checked"; "Invalid" ]

let test_no_dangling_refs _ =
  let schema = Lazy.force Ir_document.json_schema in
  let names = def_names schema in
  let refs = collect_refs schema in
  assert_bool "expected some $ref" (match refs with [] -> false | _ :: _ -> true);
  List.iter
    (fun r ->
      match String.split_on_char '/' r with
      | [ "#"; "$defs"; name ] ->
        assert_bool
          (Printf.sprintf "dangling $ref %s" r)
          (List.mem name names)
      | _ -> assert_failure ("unexpected $ref form: " ^ r))
    refs

let test_conflict_raises _ =
  let schema =
    `Assoc
      [
        "$schema", `String "https://json-schema.org/draft/2020-12/schema";
        "$defs",
        `Assoc
          [
            "expr", `Assoc [ "type", `String "string" ];
          ];
        "properties",
        `Assoc
          [
            "nested",
            `Assoc
              [
                "$id", `String "file://x:1";
                "$defs",
                `Assoc
                  [
                    "expr", `Assoc [ "type", `String "number" ];
                  ];
              ];
          ];
      ]
  in
  begin match Json_schema.bundle schema with
  | exception Invalid_argument msg ->
    assert_bool "mentions conflict" (string_contains msg "conflicting")
  | _ -> assert_failure "expected Invalid_argument on conflicting defs"
  end

let test_identical_nested_ok _ =
  let def = `Assoc [ "type", `String "string" ] in
  let schema =
    `Assoc
      [
        "$schema", `String "https://json-schema.org/draft/2020-12/schema";
        "$defs", `Assoc [ "expr", def ];
        "properties",
        `Assoc
          [
            "nested",
            `Assoc
              [
                "$id", `String "file://x:1";
                "$defs", `Assoc [ "expr", def ];
                "$ref", `String "#/$defs/expr";
              ];
          ];
      ]
  in
  let bundled = Json_schema.bundle schema in
  assert_equal ~cmp:Int.equal ~printer:string_of_int 0
    (count_substring (Yojson.Basic.to_string bundled) "\"$id\"")

let suite =
  "json_schema" >:::
  [
    "stmt_bundle_shrinks" >:: test_stmt_bundle_shrinks;
    "document_bundle_clean" >:: test_document_bundle_clean;
    "no_dangling_refs" >:: test_no_dangling_refs;
    "conflict_raises" >:: test_conflict_raises;
    "identical_nested_ok" >:: test_identical_nested_ok;
  ]

let () = run_test_tt_main suite
