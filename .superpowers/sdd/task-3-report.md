Status: PARTIAL — tests mixed

Commit: e122c904 (branch refactor/conflict-free-parser)

Summary
- GREEN: `opam exec -- dune test test/cram/test.t` -> exit 0 (warnings only)
- RED: `opam exec -- ./run_test` -> exit 2 (FAILED: test/misc.sql)

Exact commands and results
- `opam exec -- dune test test/cram/test.t`
  Output (truncated):
  ```
  Warning: 28 states have shift/reduce conflicts.
  Warning: 120 shift/reduce conflicts were arbitrarily resolved.
  ```
  Exit: 0

- `opam exec -- ./run_test`
  Output (truncated):
  ```
  Running regression tests
  ==> CREATE TABLE IF NOT EXISTS `oauth_tokens` (
    `id` INTEGER UNSIGNED PRIMARY KEY AUTO_INCREMENT,
    `unique_value_google_drive` BINARY(16) AS (
  Position 3:37 Tokens: (16) AS (
  Error: Sqlgg.Sql_parser.MenhirBasics.Error
  Errors encountered, no code generated
  FAILED test/misc.sql
  ```
  Exit: 2

Changed files (this worktree)
- lib/sql_parser.mly (modified)
- test/cram/parser_conflict_witnesses.t (modified)

What I changed
- Removed generic `LPAREN` size suffix from `sql_type` and centralized size parsing in flavor-specific rules per brief.
- Added focused size witness (CREATE TABLE sized ...) to `parser_conflict_witnesses.t`.
- Added `binary int_arg?` handling in `cast_sql_type` to avoid breaking `BINARY(16)` casts; further work may be needed.

Typed-design / PPX check
- No OCaml source files were modified; changes are in parser grammar and cram test fixture.
- Quick grep over modified files shows no module/type/function aliases introduced.
- PPX usage not applicable for these files.

Concerns / next steps
- `./run_test` fails parsing `test/misc.sql` where `BINARY(16) AS (...)` appears. I added `binary int_arg?` to `cast_sql_type` but the generated parser still errors at that position — likely a remaining grammar ambiguity or precedence issue introduced by removing the generic LPAREN branches. Fixing this without reintroducing the six LPAREN conflict states will require a small targeted grammar change around generated columns / column-definition parsing so that `BINARY(16)` is accepted only where appropriate.
- I have not regenerated the Menhir state report yet; next step is to run the menhir report generation and confirm the shift/reduce count decreased by six as requested.
- I attempted to follow TDD: I added the focused failing/verification witness and ran tests. Ideally the flow is: add failing test (RED), implement minimal grammar change (GREEN), then refactor — in this run I applied test+code and observed mixed results; will iterate.

How to reproduce (worktree)
1. cd /home/me/sqlgg-worktrees/parser-conflict-free
2. opam exec -- dune test test/cram/test.t
3. opam exec -- ./run_test

-- End of report

