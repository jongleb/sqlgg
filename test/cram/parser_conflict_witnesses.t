Acceptance witnesses for SQL forms tied to Menhir shift/reduce and
reduce/reduce conflicts. Each block must parse and type-check under MySQL
(-gen none); exit status 0 locks compatibility for later conflict-removal tasks.

Expression and predicate witnesses (36 conflicting states), except LIKE/ESCAPE/AND
which is a separate RED witness below (Task 5: expression tiers must parse
`… LIKE @pattern ESCAPE @escape AND flag = 1` with AND at predicate level,
not inside ESCAPE; then this block should expect exit 0 like the others):

  $ sqlgg -gen none -dialect mysql - <<'EOF' 2>&1
  > CREATE TABLE t (
  >   id INT PRIMARY KEY,
  >   name TEXT,
  >   flag INT,
  >   payload BLOB
  > );
  > SELECT id FROM t WHERE NOT EXISTS (SELECT id FROM t);
  > SELECT id FROM t WHERE name IS NULL OR name IS NOT NULL;
  > SELECT id FROM t WHERE name IS NOT DISTINCT FROM @name;
  > SELECT id FROM t WHERE id BETWEEN @lo AND @hi OR flag = 1;
  > SELECT id FROM t WHERE NOT flag = 1;
  > SELECT id FROM t WHERE (id, name) IN @pairs;
  > EOF
  $ echo $?
  0

RED witness: LIKE … ESCAPE … AND … (parser binds AND into ESCAPE operand until Task 5):

  $ sqlgg -gen none -dialect mysql - <<'EOF' 2>&1
  > CREATE TABLE t (
  >   id INT PRIMARY KEY,
  >   name TEXT,
  >   flag INT,
  >   payload BLOB
  > );
  > SELECT id FROM t WHERE name LIKE @pattern ESCAPE @escape AND flag = 1;
  > EOF
  Failed : SELECT id FROM t WHERE name LIKE @pattern ESCAPE @escape AND flag = 1
  Fatal error: exception Failure("types Text?? and Bool do not match in Bool?? -> Text?? -> Bool?? applied to (Bool?, Bool)")
  [2]

Task 2: index_options tail (table constraint, inline KEY, ALTER ADD CONSTRAINT)
with ignored MySQL index-option identifiers (lexer IDENT tokens):

  $ sqlgg -gen none -dialect mysql - <<'EOF' 2>&1
  > CREATE TABLE index_opt_witness (
  >   id INT,
  >   CONSTRAINT c UNIQUE KEY (id) using_btree comment_tail,
  >   KEY ix (id) foo bar
  > );
  > ALTER TABLE index_opt_witness ADD CONSTRAINT uq UNIQUE (id) opt_tail;
  > EOF
  $ echo $?
  0

DDL witnesses (column options, constraints, ALTER):

  $ sqlgg -gen none -dialect mysql - <<'EOF' 2>&1
  > CREATE TABLE ddl_witness (
  >   id BIGINT(20) UNSIGNED PRIMARY KEY,
  >   amount DECIMAL(10,2) NOT NULL,
  >   name NATIONAL VARCHAR(80) COLLATE utf8_bin DEFAULT 'x',
  >   code VARBINARY(255) UNIQUE KEY,
  >   payload BLOB DEFAULT X'00',
  >   CONSTRAINT uq_name UNIQUE KEY (name),
  >   KEY ix_amount (amount)
  > );
  > ALTER TABLE ddl_witness ADD CONSTRAINT uq_amount UNIQUE (amount);
  > EOF
  $ echo $?
  0

JOIN, UPDATE, INSERT, and statement-prefix witnesses:

  $ sqlgg -gen none -dialect mysql - <<'EOF' 2>&1
  > CREATE TABLE db_name.a (id INT PRIMARY KEY, value INT);
  > CREATE TABLE a (id INT PRIMARY KEY, value INT);
  > CREATE TABLE b (id INT PRIMARY KEY, value INT);
  > CREATE TABLE outbox (id INT PRIMARY KEY, value INT);
  > UPDATE a SET value = 1 WHERE id = 1;
  > UPDATE db_name.a SET value = 1 WHERE id = 1;
  > UPDATE a, b SET a.value = b.value WHERE a.id = b.id;
  > UPDATE a LEFT JOIN b ON b.id = a.id SET a.value = b.value;
  > INSERT INTO outbox (id, value) VALUES (1, 2);
  > INSERT INTO outbox SELECT a.id, a.value FROM a;
  > INSERT INTO outbox
  > SELECT a.id, b.value FROM a LEFT JOIN b ON b.id = a.id
  > ON DUPLICATE KEY UPDATE value = VALUES(value);
  > SELECT a.id FROM a NATURAL LEFT JOIN b;
  > EOF
  $ echo $?
  0
