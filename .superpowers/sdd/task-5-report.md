# Task 5 report: conflict-free expression parser

## Result

- Base commit: `4435ee81aa2e3ef261d3d749fd9a4c966df0a3f4`
- Implementation commit: `30e1586e` (`refactor: make expression precedence structural`)
- Changed implementation/test files:
  - `lib/sql_parser.mly`
  - `src/test.ml`
  - `test/cram/parser_conflict_witnesses.t`
- Menhir delta: **23 → 15 shift/reduce states**, **115 → 25 individual
  shift/reduce conflicts**, **0 → 0 reduce/reduce conflicts**.

## Grammar ownership before and after

Before Task 5, one recursive `expr` nonterminal owned atoms, arithmetic,
comparison, unary operators, boolean operators, predicates, tuple-IN,
subqueries, and function forms. Precedence declarations and conflict
resolution therefore had to decide where `NOT EXISTS`, `LIKE ... ESCAPE`,
`IS [NOT] NULL`, `BETWEEN ... AND`, unary expressions, and parenthesized
tuple candidates ended.

After Task 5:

- `c_expr` owns atoms: literals, columns, grouping, subqueries, parameters,
  choices, calls, `CASE`, windows, and positive `EXISTS`.
- `%inline operators(E)` owns arithmetic, comparison, concatenation, JSON,
  `EXCL`, `TILDE`, unary minus, and interval operations while preserving the
  previous AST constructors and actions.
- `b_expr` owns scalar/operator expressions.
- `predicate_expr` owns `NOT`, `LIKE`, `ESCAPE`, `IN`, tuple-IN, `IS`, and
  `BETWEEN`.
- `and_expr`, `xor_expr`, and `or_expr` own boolean associativity and
  precedence structurally.
- Tuple-IN now has the shape `LPAREN expr COMMA commas(expr) RPAREN`, so its
  representation contains at least two expressions (`e :: es`).
- The old boolean/predicate precedence declarations, `like_expr`, `escape`,
  `boolean_bin_op`, and `unary_op` were removed rather than supplemented with
  conflict-hiding declarations.

The `IS` tail uses named variants (`Is`/`Is_not`,
`Direct`/`Distinct_from`) and one exhaustive parser-boundary match. This
preserves the checked `expr IS expr`, `IS NULL`, `IS NOT NULL`, and
`IS [NOT] DISTINCT FROM expr` forms without reintroducing the two NULL
conflict states.

## TDD evidence and tests

RED was established before the grammar edit:

- `opam exec -- dune exec src/cli.exe -- -test` failed on
  `name LIKE @p ESCAPE @e AND flag = 1` with the existing
  `Text`/`Bool` mismatch, proving that `AND flag = 1` was consumed by the
  escape operand.
- The updated focused cram expectation failed before implementation (the
  concurrent invocation was rejected by Dune because the unit run held the
  build lock; it was rerun independently after implementation).

Final commands:

- `opam exec -- dune build` — pass; Menhir reports only the expected residual
  15 states / 25 SR conflicts.
- `opam exec -- dune test test/cram/parser_conflict_witnesses.t` — pass.
- `opam exec -- dune exec src/cli.exe -- -test` — pass:
  493 cases, 0 failures, 0 errors, 1 pre-existing todo.
- `opam exec -- dune runtest` — pass:
  493 cases, 0 failures, 0 errors, 1 pre-existing todo; cram/PPX rules pass.
- `./run_test` — pass (`Running regression tests`, `OK`); no XML golden
  changes.
- `git diff --check` — pass.
- IDE diagnostics for the three edited source/test files — no errors.

The final report was generated with:

```text
opam exec -- menhir --strict --external-tokens Sql_tokens --explain \
  --base /tmp/task5-final lib/sql_parser.mly
```

It exits 1 by design because Tasks 6–8 still own conflicts, and reports
exactly:

```text
Error: 15 states have shift/reduce conflicts.
Error: 25 shift/reduce conflicts were arbitrarily resolved.
```

## LIKE / ESCAPE behavior

`LIKE @pattern ESCAPE @escape AND flag = 1` now parses and type-checks.
`ESCAPE` accepts a `c_expr`, while `AND` belongs to `and_expr`; the resulting
shape is `AND (LIKE_ESCAPE (LIKE name @pattern) @escape) (= flag 1)`.
The parameters remain `Text`, and the focused MySQL cram witness exits 0.

## Menhir signatures removed

The following eight baseline expression conflict states disappeared:

1. state 93: `NOT` versus `NOT EXISTS` (`EXISTS`);
2. state 251: optional escape after `like_expr` (`ESCAPE`);
3. state 266: operators/boolean tokens inside the escape operand;
4. state 304: `IS` optional chains versus `IS NULL`;
5. state 306: `IS NOT` optional chains versus `IS NOT NULL`;
6. state 339: boolean `AND` versus the `BETWEEN` delimiter;
7. state 571: unary reduction versus continuing the operand;
8. state 774: grouped singleton versus tuple-IN (`RPAREN`).

The final conflict report contains only the pre-existing `WITH`, JOIN
(`RIGHT`/`NATURAL`/`LEFT`/`FULL`/`COMMA`), DML `ON`, UPDATE `SET`, and INSERT
`LPAREN` families. No expression token family remains and no new family was
introduced.

## Typed-design and PPX checklist

- Illegal/ambiguous parser states are removed structurally at the SQL
  boundary; downstream code receives the existing trusted `Sql.expr`.
- Tuple-IN non-emptiness is represented by the mandatory first expression,
  mandatory comma, and non-empty `commas(expr)` tail.
- No domain booleans were added; parser choices use named variants.
- No module, type, function, value, import, or export aliases were added.
- The new semantic match is exhaustive and has no catch-all branch.
- No single-use top-level OCaml helper was added.
- Existing AST constructors, typed errors, and downstream interfaces remain
  unchanged.
- Existing PPX support (`ppx_deriving`) was checked. It already supplies the
  printers used by tests; no applicable PPX can replace Menhir grammar
  productions or their semantic actions, so no manual PPX-replaceable
  boilerplate was introduced.

## Concerns

No Task 5 concern. The remaining 15 SR states / 25 individual SR conflicts
are unchanged non-expression families intentionally reserved for Tasks 6–8.
