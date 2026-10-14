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

Final evidence and Menhir delta
--------------------------------

Baseline (checkout a1a27257):

- Command:
  ```
  cd /tmp/sqlgg-base && opam exec -- dune build
  ```
- Output (relevant lines):
  ```
  Warning: 33 states have shift/reduce conflicts.
  Warning: 125 shift/reduce conflicts were arbitrarily resolved.
  ```

After changes (HEAD 7c76d09):

- Command:
  ```
  cd /home/me/sqlgg-worktrees/parser-conflict-free && opam exec -- dune build
  ```
- Output (relevant lines):
  ```
  Warning: 27 states have shift/reduce conflicts.
  Warning: 119 shift/reduce conflicts were arbitrarily resolved.
  ```

Delta:
- States: 33 -> 27 (removed 6 states)
- Shift/reduce conflicts: 125 -> 119 (reduced by 6)

Exact test commands and results (worktree /home/me/sqlgg-worktrees/parser-conflict-free):
- `opam exec -- dune build`
  - Exit: 0, Warnings as above.
- `opam exec -- dune test test/cram/test.t`
  - Exit: 0
- `opam exec -- ./run_test`
  - Output: "Running regression tests\nOK"
  - Exit: 0

Commits (this worktree):
- Base commit referenced: a1a27257
- First edit commit: e122c904 (fix: parse SQL type dimensions once)
- Final series ending HEAD: 7c76d09 (fix(grammar): remove cast LPAREN alternatives; sizes belong to flavors)
- Commit range: e122c904..7c76d09

Changed files:
- lib/sql_parser.mly
- test/cram/parser_conflict_witnesses.t
- .superpowers/sdd/task-3-report.md

Typed-design / PPX check
- No OCaml implementation files were modified other than the grammar (.mly) and cram fixture. No aliases added. PPX not applicable.

Concerns / notes
- Dune/Menhir still report 27 states with shift/reduce conflicts — the remaining conflicts are unrelated to the six LPAREN size-suffix states we removed. The task's scope (remove exactly the six LPAREN conflict states for integer/decimal/varbinary/char/varchar/varchar2) is satisfied: delta = -6.
- There are still general parser conflicts flagged by Menhir; this task intentionally limited scope to the size-suffix family and avoided touching other families.
- I ran TDD-style steps: added focused failing witness, iterated small grammar fixes until tests went green, then recorded evidence and committed.

Status: DONE

-- End of updated report

