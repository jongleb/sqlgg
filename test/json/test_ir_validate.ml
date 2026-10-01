open Sqlgg

let fail fmt =
  Printf.ksprintf (fun message -> prerr_endline message; exit 1) fmt

let full_select =
  "WITH RECURSIVE nums (n) AS (SELECT 1 UNION ALL SELECT n + 1 FROM nums WHERE n < 5) \
   SELECT u.id, COUNT(*) AS cnt, \
   (SELECT MAX(pp.id) FROM posts pp WHERE pp.user_id = u.id) AS last_post \
   FROM users u \
   LEFT JOIN posts p ON p.user_id = u.id \
   JOIN (SELECT id FROM users WHERE name LIKE @pattern) s ON s.id = u.id \
   WHERE u.id > @min_id AND (u.name = @name OR LOWER(u.name) IN @names) \
   AND EXISTS (SELECT 1 FROM nums WHERE n = u.id) \
   GROUP BY u.id \
   HAVING COUNT(*) > @min_count \
   ORDER BY cnt DESC, u.id \
   LIMIT @lim"

let schema_sql =
  "CREATE TABLE users (id INT NOT NULL PRIMARY KEY, name TEXT, kind ENUM('a','b') NOT NULL);\n\
   CREATE TABLE posts (id INT NOT NULL AUTO_INCREMENT PRIMARY KEY, user_id INT NOT NULL, \
   body TEXT, UNIQUE KEY uk (id, user_id));"

let corpus =
  [
    "INSERT INTO users (id, name) VALUES (@id, @name) ON DUPLICATE KEY UPDATE name = VALUES(name)";
    "INSERT INTO posts (user_id, body) SELECT id, name FROM users WHERE id = @id";
    "UPDATE users SET name = @name WHERE id = @id ORDER BY id LIMIT 1";
    "DELETE FROM posts WHERE user_id IN @ids";
    "CREATE TABLE t (id INT UNSIGNED NOT NULL AUTO_INCREMENT PRIMARY KEY, \
     name VARCHAR(20) NOT NULL DEFAULT 'x', price DECIMAL(10,2), \
     kind ENUM('a','b'), meta TEXT)";
    "CREATE TABLE t2 AS SELECT id, name FROM users";
    "ALTER TABLE users ADD COLUMN z INT NULL AFTER name";
    "DROP TABLE posts";
    "SELECT id FROM users WHERE @choice { A { id = @x } | B { name = 'x' } | C }";
    "SELECT id FROM users WHERE { kind = @k }? AND id IN @ids";
    "SELECT name, COUNT(*) FROM users GROUP BY name UNION SELECT body, 1 FROM posts \
     ORDER BY 2 DESC LIMIT 10 OFFSET @off";
  ]

let () =
  let t0 = Unix.gettimeofday () in
  let schema = Lazy.force Ir_document.json_schema in
  begin match Jsonschema.validate Jsonschema.draft2020_12_validator schema with
  | Ok () -> ()
  | Error _ -> fail "bundled schema fails Draft 2020-12 meta-validation"
  end;
  let validator =
    match Jsonschema.create_validator_from_json ~schema () with
    | Ok v -> v
    | Error e ->
      fail "bundled schema does not compile: %s"
        (match e with
         | Jsonschema.Json_pointer_not_found p -> "pointer " ^ p
         | Jsonschema.Duplicate_id { id; _ } -> "duplicate id " ^ id
         | _ -> "compile_error")
  in
  let users =
    match Analysis.Schema.of_sql schema_sql with
    | Ok s -> s
    | Error _ -> fail "schema fixture"
  in
  let documents =
    List.concat_map
      (fun sql ->
        [
          Ir_document.document_of_sql ~schema:None sql;
          Ir_document.document_of_sql ~schema:(Some users) sql;
        ])
      (full_select :: "SELECT FROM" :: "SET x = (SELECT 1)" :: corpus)
  in
  List.iter
    (fun document ->
      List.iter
        (function
          | Ir_document.Invalid { diagnostics = []; _ } ->
            fail "Invalid statement must carry a diagnostic"
          | _ -> ())
        document.Ir_document.statements)
    documents;
  List.iteri
    (fun i document ->
      match
        Jsonschema.validate validator (Ir_document.document_to_json document)
      with
      | Ok () -> ()
      | Error _ -> fail "document %d does not validate" i)
    documents;
  let broken =
    `Assoc
      [
        "statements",
        `List
          [
            `List
              [
                `String "Parsed";
                `Assoc [ "sql", `String "x" ];
              ];
          ];
      ]
  in
  begin match Jsonschema.validate validator broken with
  | Ok () -> fail "document without ast must be rejected"
  | Error _ -> ()
  end;
  let rec replace_where = function
    | `Assoc fields ->
      `Assoc
        (List.map
           (fun (k, v) ->
             k,
             if String.equal k "where" && not (Yojson.Basic.equal v `Null)
             then `List [ `String "Bogus" ]
             else replace_where v)
           fields)
    | `List items -> `List (List.map replace_where items)
    | json -> json
  in
  let with_where =
    Ir_document.document_to_json
      (Ir_document.document_of_sql ~schema:None full_select)
  in
  begin match Jsonschema.validate validator (replace_where with_where) with
  | Ok () -> fail "document with a malformed WHERE must be rejected"
  | Error _ -> ()
  end;
  let elapsed = Unix.gettimeofday () -. t0 in
  Printf.printf
    "validated %d documents in %.2fs; schema size %d bytes\n"
    (List.length documents)
    elapsed
    (String.length (Yojson.Basic.to_string (Lazy.force Ir_document.json_schema)))
