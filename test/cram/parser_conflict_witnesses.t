Acceptance witnesses for SQL forms tied to Menhir shift/reduce and
reduce/reduce conflicts. Each block must parse and type-check under MySQL
(-gen none); exit status 0 locks compatibility for later conflict-removal tasks.

Expression and predicate witnesses (36 conflicting states):

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

LIKE … ESCAPE … AND … keeps AND outside the ESCAPE operand:

  $ sqlgg -gen none -dialect mysql - <<'EOF' 2>&1
  > CREATE TABLE t (
  >   id INT PRIMARY KEY,
  >   name TEXT,
  >   flag INT,
  >   payload BLOB
  > );
  > SELECT id FROM t WHERE name LIKE @pattern ESCAPE @escape AND flag = 1;
  > EOF
  $ echo $?
  0

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
  >   slug TEXT UNIQUE,
  >   payload BLOB DEFAULT X'00',
  >   label TEXT DEFAULT 'x' COLLATE utf8_bin,
  >   bytes BLOB DEFAULT X'01' COLLATE blob_bin,
  >   CONSTRAINT uq_name UNIQUE KEY (name),
  >   KEY ix_amount (amount)
  > );
  > ALTER TABLE ddl_witness ADD CONSTRAINT uq_amount UNIQUE (amount);
  > EOF
  $ echo $?
  0

Focused size witness (Task 3: sizes parsed in flavours only)

  $ sqlgg -gen none -dialect mysql - <<'EOF' 2>&1
  > CREATE TABLE sized (
  >   i INT(11) UNSIGNED,
  >   d DECIMAL(10,2),
  >   b VARBINARY(255),
  >   c CHAR(10),
  >   v VARCHAR(80),
  >   v2 VARCHAR2(40)
  > );
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
