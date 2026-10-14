# Conflict-Free SQL Parser Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Remove all 33 shift/reduce states, 125 arbitrarily resolved shift/reduce conflicts, 3 reduce/reduce states, and 18 arbitrarily resolved reduce/reduce conflicts without changing accepted regression SQL or generated output.

**Architecture:** Keep the existing Menhir parser and AST. Resolve ambiguity at the earliest boundary: tier scalar expressions, give each DDL suffix one owner, parse each table-reference chain once, and left-factor statement prefixes. Where two SQL phrases require more than one token of lookahead, expose the phrase as one lexer token instead of validating a broad parse later.

**Tech Stack:** OCaml, ocamllex, Menhir, dune, OUnit/QCheck, cram tests.

## Global Constraints

- Start from `/home/me/sqlgg-worktrees/parser-conflict-free`.
- Do not add precedence declarations to hide a conflict.
- Do not remove a SQL form currently accepted by the checked corpus.
- Non-empty syntax must remain non-empty in the AST.
- Do not introduce aliases or domain-mode booleans.
- Parse raw tokens once into clause-specific types; do not validate a broad AST downstream.
- Keep all pattern matches exhaustive.
- Use `opam exec -- dune ...`; the system dune is incompatible with this switch.
- After each task, regenerate the Menhir report and require the stated conflict families to disappear.

---

### Task 1: Lock Parser Compatibility With Conflict Witnesses

**Files:**
- Create: `test/cram/parser_conflict_witnesses.t`

**Interfaces:**
- Consumes: the current CLI and all SQL forms represented by the 36 conflicting states.
- Produces: one acceptance fixture used unchanged by every later task.

- [ ] **Step 1: Add the expression witnesses**

Create `test/cram/parser_conflict_witnesses.t` with a heredoc that defines `t`
and checks:

```sql
CREATE TABLE t (
  id INT PRIMARY KEY,
  name TEXT,
  flag INT,
  payload BLOB
);
SELECT id FROM t WHERE NOT EXISTS (SELECT id FROM t);
SELECT id FROM t WHERE name LIKE @pattern ESCAPE @escape AND flag = 1;
SELECT id FROM t WHERE name IS NULL OR name IS NOT NULL;
SELECT id FROM t WHERE name IS NOT DISTINCT FROM @name;
SELECT id FROM t WHERE id BETWEEN @lo AND @hi OR flag = 1;
SELECT id FROM t WHERE NOT flag = 1;
SELECT id FROM t WHERE (id, name) IN @pairs;
```

Invoke:

```sh
sqlgg -gen none -dialect mysql - <<'EOF'
# SQL above
EOF
echo $?
```

Expected cram output: `0`.

- [ ] **Step 2: Add the DDL witnesses**

In the same test, add a second invocation containing:

```sql
CREATE TABLE ddl_witness (
  id BIGINT(20) UNSIGNED PRIMARY KEY,
  amount DECIMAL(10,2) NOT NULL,
  name NATIONAL VARCHAR(80) COLLATE utf8_bin DEFAULT 'x',
  code VARBINARY(255) UNIQUE KEY,
  payload BLOB DEFAULT X'00',
  CONSTRAINT uq_name UNIQUE KEY (name),
  KEY ix_amount (amount)
);
ALTER TABLE ddl_witness ADD CONSTRAINT uq_amount UNIQUE (amount);
```

Expected exit status: `0`.

- [ ] **Step 3: Add the JOIN and statement-prefix witnesses**

Add tables `a`, `b`, and `outbox`, followed by:

```sql
UPDATE a SET value = 1 WHERE id = 1;
UPDATE db_name.a SET value = 1 WHERE id = 1;
UPDATE a, b SET a.value = b.value WHERE a.id = b.id;
UPDATE a LEFT JOIN b ON b.id = a.id SET a.value = b.value;
INSERT INTO outbox (id, value) VALUES (1, 2);
INSERT INTO outbox SELECT a.id, a.value FROM a;
INSERT INTO outbox
SELECT a.id, b.value FROM a LEFT JOIN b ON b.id = a.id
ON DUPLICATE KEY UPDATE value = VALUES(value);
SELECT a.id FROM a NATURAL LEFT JOIN b;
```

Expected exit status: `0`.

- [ ] **Step 4: Verify the fixture and baseline report**

Run:

```bash
opam exec -- dune test test/cram/parser_conflict_witnesses.t
opam exec -- menhir --explain --dump \
  --base /tmp/sqlgg_parser_task1 lib/sql_parser.mly 2>&1 |
  rg "states have|conflicts were"
```

Expected:

```text
Warning: 33 states have shift/reduce conflicts.
Warning: 3 states have reduce/reduce conflicts.
Warning: 125 shift/reduce conflicts were arbitrarily resolved.
Warning: 18 reduce/reduce conflicts were arbitrarily resolved.
```

- [ ] **Step 5: Commit**

```bash
git add test/cram/parser_conflict_witnesses.t
git commit -m "test: cover ambiguous SQL parser forms"
```

---

### Task 2: Remove the Three Empty-List Reduce/Reduce Conflicts

**Files:**
- Modify: `lib/sql_parser.mly:466`
- Test: `test/cram/parser_conflict_witnesses.t`

**Interfaces:**
- Consumes: ignored MySQL index-option identifiers.
- Produces: exactly one empty derivation for `index_options`.

- [ ] **Step 1: Confirm the three failing states**

Run Menhir and verify the report contains reduce/reduce states reached after
table constraints, inline indexes, and `ALTER TABLE ... ADD CONSTRAINT`.

- [ ] **Step 2: Replace the nested optional list**

Replace:

```ocaml
index_options: list(IDENT)? { }
```

with:

```ocaml
index_options:
  | { }
  | IDENT index_options { }
```

This keeps arbitrary ignored identifiers while providing one, and only one,
empty production.

- [ ] **Step 3: Verify the reduction**

Run:

```bash
opam exec -- dune test test/cram/parser_conflict_witnesses.t
opam exec -- menhir --explain --dump \
  --base /tmp/sqlgg_parser_task2 lib/sql_parser.mly 2>&1 |
  rg "states have|conflicts were"
```

Expected: no reduce/reduce warning; shift/reduce remains `33 states / 125`.

- [ ] **Step 4: Run DDL regression tests**

```bash
opam exec -- dune test test/cram/test_migrations
./run_test
```

Expected: both commands exit `0`; `run_test` prints `OK`.

- [ ] **Step 5: Commit**

```bash
git add lib/sql_parser.mly test/cram/parser_conflict_witnesses.t
git commit -m "fix: make index option tail unambiguous"
```

---

### Task 3: Give Every SQL Type Suffix One Grammar Owner

**Files:**
- Modify: `lib/sql_parser.mly:702-776`
- Test: `test/cram/parser_conflict_witnesses.t`
- Test: `test/cram/test.t`

**Interfaces:**
- Consumes: `INT(n) UNSIGNED`, `DECIMAL(p,s)`, `VARBINARY(n)`,
  `CHAR(n)`, `VARCHAR(n)`, and `VARCHAR2(n)`.
- Produces: the unchanged `Sql.Source_type.kind collated` values with sizes
  populated in their flavor-specific productions.

- [ ] **Step 1: Add focused size assertions to the witness fixture**

Generate XML for:

```sql
CREATE TABLE sized (
  i INT(11) UNSIGNED,
  d DECIMAL(10,2),
  b VARBINARY(255),
  c CHAR(10),
  v VARCHAR(80),
  v2 VARCHAR2(40)
);
```

Assert the command exits `0`; checked XML regression tests will verify the
semantic type representation.

- [ ] **Step 2: Remove the duplicate generic size suffix**

Replace:

```ocaml
sql_type:
  | t=collated(sql_type_flavor)
  | t=collated(sql_type_flavor) LPAREN INTEGER RPAREN UNSIGNED?
  | t=collated(sql_type_flavor) LPAREN INTEGER COMMA INTEGER RPAREN
    { t }
```

with:

```ocaml
sql_type:
  | t=collated(sql_type_flavor) { t }
```

Keep size parsing exclusively in `int_type`, `expr_sql_type_flavor`,
`text_var`, and the `T_VARBINARY` flavor.

- [ ] **Step 3: Verify type semantics**

Run:

```bash
opam exec -- dune test test/cram/test.t
./run_test
```

Expected: exit `0`, no changed `test/out/*.xml`.

- [ ] **Step 4: Verify six states disappeared**

Regenerate the Menhir report. Expected: states corresponding to integer,
decimal, varbinary, char, varchar, and varchar2 `LPAREN` conflicts are absent;
shift/reduce count decreases by six.

- [ ] **Step 5: Commit**

```bash
git add lib/sql_parser.mly test/cram/parser_conflict_witnesses.t
git commit -m "fix: parse SQL type dimensions once"
```

---

### Task 4: Make Column Constraints and Collations Clause-Specific

**Files:**
- Modify: `lib/sql_parser.mly:445-505`
- Modify: `lib/sql_parser.mly:650-652`
- Modify: `lib/sql_parser.mly:771-776`
- Test: `test/cram/parser_conflict_witnesses.t`

**Interfaces:**
- Consumes: column `COLLATE`, default literal `COLLATE`, `UNIQUE`, and
  `UNIQUE KEY`.
- Produces: the existing `Alter_action_attr.kind` and syntax constraints with
  no duplicate parser path.

- [ ] **Step 1: Split `UNIQUE` and `UNIQUE KEY`**

Replace:

```ocaml
| UNIQUE KEY? { Some (Alter_action_attr.Syntax_constraint Unique) }
```

with:

```ocaml
| UNIQUE { Some (Alter_action_attr.Syntax_constraint Unique) }
| UNIQUE KEY { Some (Alter_action_attr.Syntax_constraint Unique) }
```

Replace `PRIMARY? KEY` with `PRIMARY KEY`. The checked corpus contains no bare
column-level `KEY`, and MySQL's column constraint is `PRIMARY KEY`; table-level
plain keys remain represented by `inline_idx_kind`.

- [ ] **Step 2: Give DDL `COLLATE` one path**

Remove `COLLATE IDENT` from `column_def_extra`; it currently returns `None` and
is a second, information-losing owner. Keep the existing `collated(sql_type_flavor)`
for type collation and the existing `TEXT/BLOB collate?` literal productions
for expression/default-literal collation. At each column position `COLLATE`
then has exactly one owner: the immediately preceding type or default literal.

- [ ] **Step 3: Verify the four affected states disappeared**

Regenerate the report. Expected: no conflicts for default text/blob
`COLLATE`, type `COLLATE`, or `UNIQUE KEY`.

- [ ] **Step 4: Run focused and migration tests**

```bash
opam exec -- dune test test/cram/parser_conflict_witnesses.t
opam exec -- dune test test/cram/test_migrations
./run_test
```

Expected: all exit `0`, XML corpus remains clean.

- [ ] **Step 5: Commit**

```bash
git add lib/sql_parser.mly test/cram/parser_conflict_witnesses.t
git commit -m "fix: make DDL suffix ownership explicit"
```

---

### Task 5: Replace the Flat Expression Grammar With Precedence Tiers

**Files:**
- Modify: `lib/sql_parser.mly:519-609`
- Modify: `lib/sql_parser.mly:671-694`
- Test: `test/cram/parser_conflict_witnesses.t`
- Test: `src/test.ml`

**Interfaces:**
- Consumes: existing scalar, arithmetic, predicate, and boolean expressions.
- Produces: the same `Sql.expr` constructors and function kinds.

- [ ] **Step 1: Add AST-sensitive unit cases**

In the existing parsing tests in `src/test.ml`, add statements covering:

```sql
SELECT id FROM t WHERE NOT EXISTS (SELECT id FROM t);
SELECT id FROM t WHERE name LIKE @p ESCAPE @e AND flag = 1;
SELECT id FROM t WHERE id BETWEEN @lo AND @hi OR flag = 1;
SELECT id FROM t WHERE id IS NULL OR id IS NOT NULL;
SELECT id FROM t WHERE id IS NOT DISTINCT FROM @id;
SELECT id FROM t WHERE (id, name) IN @pairs;
```

Use the existing `tt`/`parse` helpers to assert inferred parameters and output
schema, proving precedence did not change.

- [ ] **Step 2: Introduce scalar and full-expression tiers**

Replace the single recursive `expr` body with:

```ocaml
b_expr: e=b_expr_ { e }
expr: e=expr_ { e }
c_expr: e=c_expr_ { e }

b_expr_:
  | e=c_expr { e }
  | e=operators(b_expr) { e }

expr_:
  | e=c_expr { e }
  | e=operators(expr) { e }
  | e1=expr AND e2=expr
      { Fun { fn_name = "boolean_bin_op"; kind = Logical And;
              parameters = [e1; e2]; is_over_clause = false } }
  | e1=expr XOR e2=expr
      { Fun { fn_name = "boolean_bin_op"; kind = Logical Xor;
              parameters = [e1; e2]; is_over_clause = false } }
  | e1=expr OR e2=expr
      { Fun { fn_name = "boolean_bin_op"; kind = Logical Or;
              parameters = [e1; e2]; is_over_clause = false } }
```

Move arithmetic, comparison, concatenation, JSON, `EXCL`, `TILDE`, unary
minus, and interval operations into `%inline operators(E)`. Preserve every
existing semantic action exactly.

- [ ] **Step 3: Make predicates explicit**

Add explicit productions for:

```ocaml
| NOT e=expr %prec NOT
| e1=expr mnot(like) e2=expr %prec LIKE
| e1=expr mnot(like) e2=expr ESCAPE esc=c_expr
| e=expr IS NULL
| e=expr IS NOT NULL
| e1=expr IS is_not distinct_from e2=expr
| e=expr mnot(BETWEEN) a=b_expr AND b=expr
```

Remove `NOT` from `unary_op`, remove `like_expr`, remove `escape`, and replace
optional `NOT`/`DISTINCT FROM` chains with explicit `is_not` and
`distinct_from` empty-or-token productions.

- [ ] **Step 4: Make tuple `IN` require the comma structurally**

Replace:

```ocaml
LPAREN exprs=commas(expr) RPAREN k=in_or_not_in p=param
```

with:

```ocaml
LPAREN e=expr COMMA es=commas(expr) RPAREN k=in_or_not_in p=param
```

and construct `exprs = e :: es`. A parenthesized singleton remains a grouped
expression and cannot enter tuple-list parsing.

- [ ] **Step 5: Move atoms into `c_expr_`**

Move literals, columns, grouping, subqueries, parameters, choices, function
calls, `CASE`, and:

```ocaml
EXISTS LPAREN select=select_stmt RPAREN
```

into `c_expr_`. `NOT EXISTS` is then parsed by the one boolean `NOT`
production over an `EXISTS` atom.

- [ ] **Step 6: Verify eight expression states disappeared**

Run:

```bash
opam exec -- dune build
opam exec -- dune test test/cram/parser_conflict_witnesses.t
opam exec -- dune exec src/cli.exe -- -test
```

Regenerate the Menhir report. Expected: no conflicts involving `EXISTS`,
`ESCAPE`, `NULL`, `BETWEEN`, unary operators, or singleton parenthesis.

- [ ] **Step 7: Commit**

```bash
git add lib/sql_parser.mly src/test.ml test/cram/parser_conflict_witnesses.t
git commit -m "refactor: make expression precedence structural"
```

---

### Task 6: Parse UPDATE Targets and JOIN Chains Once

**Files:**
- Modify: `lib/sql_parser.mly:163-170`
- Modify: `lib/sql_parser.mly:266-290`
- Test: `src/test.ml`
- Test: `test/cram/parser_conflict_witnesses.t`
- Test: `test/cram/test_build_dynamic_join/join_kinds.t`

**Interfaces:**
- Consumes: single-table, comma-table, qualified-table, and explicit-join
  UPDATE targets.
- Produces: `Update` for a lone unaliased table and `UpdateMulti` for every
  table-reference chain.

- [ ] **Step 1: Add UPDATE/JOIN regressions**

Add unit cases for:

```sql
UPDATE t SET value = 1 WHERE id = 1;
UPDATE db_name.t SET value = 1 WHERE id = 1;
UPDATE t, u SET t.value = u.value WHERE t.id = u.id;
UPDATE t LEFT JOIN u ON u.id = t.id SET t.value = u.value;
UPDATE t, (SELECT id, value FROM u) AS x
SET t.value = x.value WHERE t.id = x.id;
```

- [ ] **Step 2: Collapse the two UPDATE productions**

Replace both UPDATE statement alternatives with:

```ocaml
| update_cmd tables=table_list SET ss=commas(set_column)
  w=where? o=loption(order) lim=loption(limit)
    {
      match tables with
      | ((`Table table, None), []) -> Update (table, ss, w, o, lim)
      | tables -> UpdateMulti ([tables], ss, w, o, lim)
    }
```

`UpdateMulti` already takes `nested list`, so the multi-table branch uses
`[tables]` and does not change `lib/sql.ml`.

- [ ] **Step 3: Remove the second owner of comma**

Use one `table_list` for the full chain. Do not wrap it in
`commas(table_list)` in UPDATE. A comma is always a `join_source` inside that
chain.

- [ ] **Step 4: Replace `anyorder` for outer joins**

Replace optional/permuted join modifiers with explicit forms:

```ocaml
left_join: LEFT | LEFT OUTER | OUTER LEFT { Schema.Join.Left }
right_join: RIGHT | RIGHT OUTER | OUTER RIGHT { Schema.Join.Right }
full_join: FULL | FULL OUTER | OUTER FULL { Schema.Join.Full }
```

Define NATURAL alternatives explicitly around those join kinds; do not use
`anyorder(NATURAL, join)`.

- [ ] **Step 5: Verify five states disappeared**

Run focused UPDATE and dynamic-join tests, then regenerate conflicts. Expected:
no states for UPDATE `SET`, comma ownership, or NATURAL outer-join ordering.

- [ ] **Step 6: Commit**

```bash
git add lib/sql_parser.mly src/test.ml \
  test/cram/parser_conflict_witnesses.t \
  test/cram/test_build_dynamic_join/join_kinds.t
git commit -m "refactor: parse update table chains once"
```

---

### Task 7: Left-Factor INSERT Parentheses and JOIN Conditions

**Files:**
- Modify: `lib/sql_parser.mly:147-160`
- Modify: `lib/sql_parser.mly:275-320`
- Test: `test/cram/parser_conflict_witnesses.t`
- Test: `src/test.ml`

**Interfaces:**
- Consumes: INSERT column lists, wrapped selects, join conditions,
  `ON DUPLICATE KEY`, and `ON CONFLICT`.
- Produces: the existing `Insert.action`, `join_condition`, and conflict-clause
  values through one parser path each.

- [ ] **Step 1: Left-factor INSERT after the target**

Introduce:

```ocaml
insert_tail:
  | names=insert_columns VALUES values=commas(sequence(set_column_expr))?
      { `Values (names, values) }
  | names=insert_columns VALUES p=param
      { `Param (names, p) }
  | names=insert_columns select=select_stmt
      { `Select (names, make_located ~value:select
                    ~pos:($startofs(select), $endofs(select))) }
  | SET set=commas(set_column)?
      { `Set set }
```

Define `insert_columns` once as empty or `sequence(ident)`. For the accepted
wrapped-select form, add a separate `LPAREN select_stmt RPAREN` tail whose
first inner token is `SELECT` or `WITH`; do not use `maybe_parenth(select_stmt)`
behind an optional column list.

Use one statement production:

```ocaml
insert_action_kind=insert_cmd target=table_name action=insert_tail
ss=located(conflict_clause)?
```

- [ ] **Step 2: Separate explicit and implicit join conditions**

Define:

```ocaml
join_cond_explicit:
  | ON e=expr { On e }
  | USING l=sequence(ident) { Using l }
```

Use it for explicit INNER/LEFT/RIGHT/FULL/STRAIGHT joins. CROSS and NATURAL
joins never consume a condition. Comma joins are represented solely as
`COMMA source` with `Schema.Join.Default`; they cannot consume `ON`.
Consequently, after an explicit join the condition is mandatory, while after a
comma join an `ON DUPLICATE KEY` or `ON CONFLICT` token belongs only to INSERT.
The checked corpus contains no bare INNER/LEFT/RIGHT/FULL/STRAIGHT join and no
comma join with an `ON` clause.

- [ ] **Step 3: Verify eight states disappeared**

Run INSERT and JOIN witnesses plus dialect tests. Regenerate conflicts.
Expected: no `LPAREN` conflicts after INSERT targets and no `ON` conflicts
between joins and INSERT conflict clauses.

- [ ] **Step 4: Commit**

```bash
git add lib/sql_parser.mly src/test.ml \
  test/cram/parser_conflict_witnesses.t
git commit -m "refactor: disambiguate insert and join tails"
```

---

### Task 8: Disambiguate Row-Lock Phrases and Enforce Zero Conflicts

**Files:**
- Modify: `lib/sql_lexer.mll:19-258`
- Modify: `lib/sql_parser.mly:324-335`
- Modify: `lib/dune:1-2`
- Test: `test/cram/parser_conflict_witnesses.t`
- Test: `src/test.ml`

**Interfaces:**
- Consumes: `FOR UPDATE`, `FOR SHARE`, optional `OF`, `NOWAIT`, and
  `WITH LOCK`.
- Produces: the unchanged `select_row_locking_kind located`.

- [ ] **Step 1: Add row-lock witnesses**

Add:

```sql
SELECT id FROM t FOR UPDATE;
SELECT id FROM t FOR UPDATE NOWAIT;
SELECT id FROM t FOR UPDATE WITH LOCK;
SELECT id FROM t FOR SHARE WITH LOCK;
```

Add a direct `Parser.parse_stmt` unit case, avoiding CLI statement splitting:

```sql
CREATE PROCEDURE p() BEGIN
  SELECT 1 FOR UPDATE WITH LOCK
  WITH c AS (SELECT 1 AS x) SELECT x FROM c
END
```

This preserves both interpretations that caused states 391 and 398: the first
`WITH LOCK` is a row-lock phrase, while the second `WITH` starts a statement.

- [ ] **Step 2: Tokenize `WITH LOCK` as one phrase**

Add a `WITH_LOCK` token and an ocamllex phrase rule before generic identifiers.
Use that token only in the row-lock suffix. A following CTE still begins with
`WITH`, so Menhir no longer needs two-token lookahead to distinguish it.

Replace:

```ocaml
with_lock: WITH LOCK { }
```

with the phrase token in `for_update_or_share`. Keep `NOWAIT` and `OF` as
structurally distinct optional suffixes.

- [ ] **Step 3: Run Menhir in strict mode**

Update the Menhir stanza:

```lisp
(menhir
 (modules sql_parser)
 (flags --strict))
```

Run:

```bash
opam exec -- dune clean
opam exec -- dune build @all
```

Expected: exit `0` with no shift/reduce or reduce/reduce warning.

- [ ] **Step 4: Run all verification layers**

```bash
opam exec -- dune runtest
./run_test
git diff --check
```

Expected:

- 487 OUnit/QCheck cases attempted, 0 failures, existing 1 TODO;
- all cram and ppx golden tests pass;
- `run_test` prints `OK`;
- no unreviewed generated-output changes;
- no whitespace errors outside cram output that intentionally preserves SQL.

- [ ] **Step 5: Run the typed-design checklist**

Verify:

- each phrase and clause has one parser owner;
- no new domain boolean or alias exists;
- no non-empty grammar result is represented as an unchecked empty list;
- AST matches remain exhaustive;
- no helper with one consumer remains top-level;
- no suitable existing PPX can replace introduced boilerplate.

- [ ] **Step 6: Commit**

```bash
git add lib/sql_lexer.mll lib/sql_parser.mly lib/dune src/test.ml \
  test/cram/parser_conflict_witnesses.t
git commit -m "build: reject parser conflicts"
```
