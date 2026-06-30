End-to-end scoped (fixed-SQL) pipeline: sqlgg generates the shared [Scope] module
and the scoped *_col modules, and the user writes ONLY the record with
[@@deriving sqlgg] (ppx_sqlgg) to get the reusable field-set constructor. The
derived [who_of_scope] is then aligned BY NAME to two scoped queries:

  $ cp test_scoped_select/scope.sql .
  $ cp test_scoped_select/product_id.ml .
  $ cp test_scoped_select/scope_frag_ppx.ml .
  $ cat scope.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c scope_frag_ppx.ml
  $ echo "derived who_of_scope reused across two scoped queries: OK"
  derived who_of_scope reused across two scoped queries: OK
