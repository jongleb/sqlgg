Schema-less stdin: each statement carries its SQL and the full parsed AST.

  $ sqlgg-ir <<'SQL'
  > SELECT 1;
  > SQL
  {"statements":[["Parsed",{"sql":"SELECT 1","ast":["Select",{"select_complete":{"select":[{"source_pos":[0,8],"columns":[{"value":["Expr",{"value":["Value",{"collated":{"t":["Int"],"nullability":["Strict"]},"collation":null}],"pos":[7,8]},null],"pos":[7,8]}],"from":null,"where":null,"group":[],"having":null},[]],"order":[],"limit":null,"select_row_locking":null},"cte":null}],"dialect_features":[]}]]}

WHERE, GROUP BY, HAVING, ORDER BY and LIMIT survive in the parsed AST:

  $ sqlgg-ir - <<'SQL' > parsed.json
  > SELECT id, COUNT(*) FROM users WHERE id > @min AND name LIKE 'a%' GROUP BY id HAVING COUNT(*) > 1 ORDER BY id DESC LIMIT @lim;
  > SQL
  $ grep -o '"where":\["Fun",{"fn_name":"[a-z_]*","kind":\["Logical",\["And"\]\]' parsed.json
  "where":["Fun",{"fn_name":"boolean_bin_op","kind":["Logical",["And"]]
  $ grep -o '"group":\[\["Column",{"collated":{"cname":"id"' parsed.json
  "group":[["Column",{"collated":{"cname":"id"
  $ grep -o '"having":\["Fun"' parsed.json
  "having":["Fun"
  $ grep -o '"order":\[\[\["Column",{"collated":{"cname":"id"' parsed.json
  "order":[[["Column",{"collated":{"cname":"id"
  $ grep -o '"limit":\[\[{"id":{"value":"lim"' parsed.json
  "limit":[[{"id":{"value":"lim"

Schema-aware input is Checked: the same full AST plus the checked analysis.

  $ cat > schema.sql <<'SQL'
  > CREATE TABLE users (id INT NOT NULL, name TEXT);
  > SQL
  $ sqlgg-ir --schema schema.sql - <<'SQL' > checked.json
  > SELECT id FROM users WHERE id = @id;
  > SQL
  $ grep -o '^{"statements":\[\["Checked",{"sql":"SELECT id FROM users WHERE id = @id","ast":\["Select"' checked.json
  {"statements":[["Checked",{"sql":"SELECT id FROM users WHERE id = @id","ast":["Select"
  $ grep -o '"where":\["Fun",{"fn_name":"[a-z_]*","kind":\["Comparison",\["Comp_equal"\]\]' checked.json
  "where":["Fun",{"fn_name":"comparison","kind":["Comparison",["Comp_equal"]]
  $ grep -o '"analysis":{"sql":"SELECT id FROM users WHERE id = @id","schema":\[\["Attr",{"name":"id","domain":{"t":\["Int"\],"nullability":\["Strict"\]}' checked.json
  "analysis":{"sql":"SELECT id FROM users WHERE id = @id","schema":[["Attr",{"name":"id","domain":{"t":["Int"],"nullability":["Strict"]}
  $ grep -o '"vars":\[\["Single",{"id":{"value":"id","pos":\[32,35\]},"typ":{"t":\["Int"\],"nullability":\["Strict"\]}' checked.json
  "vars":[["Single",{"id":{"value":"id","pos":[32,35]},"typ":{"t":["Int"],"nullability":["Strict"]}
  $ grep -o '"kind":\["Select",\["Nat"\]\]' checked.json
  "kind":["Select",["Nat"]]

Inputs never mutate the loaded schema:

  $ sqlgg-ir --schema schema.sql - <<'SQL' > isolated.json
  > CREATE TABLE extra (x INT);
  > SELECT x FROM extra;
  > SQL
  [1]
  $ grep -o '\["Checked",{"sql":"CREATE TABLE extra (x INT)"\|\["Invalid",{"sql":"SELECT x FROM extra"' isolated.json
  ["Checked",{"sql":"CREATE TABLE extra (x INT)"
  ["Invalid",{"sql":"SELECT x FROM extra"

Malformed SQL yields Invalid, exit 1, nothing on stderr:

  $ sqlgg-ir - <<'SQL' 2>err
  > SELECT FROM;
  > SQL
  {"statements":[["Invalid",{"sql":"SELECT FROM","diagnostics":[{"message":"syntax error","pos":[7,11]}]}]]}
  [1]
  $ cat err

Mixed valid and invalid statements keep input order:

  $ sqlgg-ir - <<'SQL' 2>err > mixed.json
  > SELECT 1;
  > SELECT FROM;
  > SELECT 2;
  > SQL
  [1]
  $ grep -o '\["[A-Z][a-z]*",{"sql":"[^"]*"' mixed.json
  ["Parsed",{"sql":"SELECT 1"
  ["Invalid",{"sql":"SELECT FROM"
  ["Parsed",{"sql":"SELECT 2"
  $ cat err

Multiple files are processed in argument order into one compact document:

  $ printf 'SELECT 1;\nSELECT 2;\n' > a.sql
  $ printf 'SELECT 3;\n' > b.sql
  $ sqlgg-ir a.sql b.sql > files.json
  $ grep -o '\["Parsed",{"sql":"[^"]*"' files.json
  ["Parsed",{"sql":"SELECT 1"
  ["Parsed",{"sql":"SELECT 2"
  ["Parsed",{"sql":"SELECT 3"
  $ wc -l < files.json
  1

--plain-sql rejects sqlgg extensions and accepts plain SQL:

  $ sqlgg-ir --plain-sql - <<'SQL' 2>err > plain.json
  > SELECT * FROM t WHERE id = @id;
  > SELECT id FROM t WHERE id = 1;
  > SQL
  [1]
  $ grep -o '\["[A-Z][a-z]*",{"sql":"[^"]*"' plain.json
  ["Invalid",{"sql":"SELECT * FROM t WHERE id = @id"
  ["Parsed",{"sql":"SELECT id FROM t WHERE id = 1"
  $ grep -o 'sqlgg extension not allowed' plain.json
  sqlgg extension not allowed
  $ cat err

--json-schema prints the generated Draft 2020-12 schema:

  $ sqlgg-ir --json-schema > schema.json
  $ grep -F '"$schema": "https://json-schema.org/draft/2020-12/schema"' schema.json
    "$schema": "https://json-schema.org/draft/2020-12/schema",
  $ grep -oE '"const": "(Parsed|Checked|Invalid)"' schema.json | sort -u
  "const": "Checked"
  "const": "Invalid"
  "const": "Parsed"

Missing input file or bad arguments are CLI errors (exit 2), not Invalid:

  $ sqlgg-ir missing.sql 2>err
  [2]
  $ grep -c . err
  1
  $ sqlgg-ir --schema 2>/dev/null
  [2]
  $ sqlgg-ir --bogus 2>/dev/null
  [2]
  $ sqlgg-ir --schema missing.sql a.sql 2>/dev/null
  [2]
