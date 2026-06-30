Scoped (fixed-SQL) code generation is pinned to a committed golden file. Two
scoped queries with id/name at different absolute positions produce a shared
[Scope] module and per-query *_col modules whose selectors read by absolute index.
The generated output is diffed against test_scoped_select/scope.expected.ml:

  $ cat test_scoped_select/scope.sql | sqlgg -no-header -gen caml -params unnamed -dialect mysql - > output.ml
  $ diff test_scoped_select/scope.expected.ml output.ml
