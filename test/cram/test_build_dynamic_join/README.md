# Dynamic join tests

A LEFT JOIN whose right key is UNIQUE/PRIMARY and that is referenced only by the
dynamic projection is turned into a FROM "hole": it is emitted at runtime only
when a selected dynamic column comes from it, but only after analysing the join.
Joins that are unsafe to drop stay static.

Each scenario lives in its own `<case>.t/` directory:

- `<case>.sql` — the schema and queries
- `<case>.compare.ml` — the golden generated code
- `run.ml` — runtime checks (final SQL per pick, via the `print_impl` mock)
- `run.t` — the cram script: generation, diff against the golden file, build and run
