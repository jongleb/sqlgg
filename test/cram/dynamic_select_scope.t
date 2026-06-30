A single reusable field-set fragment (scope) works across two different SCOPED
fixed-SQL queries, because [type 'a t] is shared via the generated [Scope] module
and selectors read by absolute column index (aligned BY NAME):

  $ cp test_scoped_select/scope.sql .
  $ cp test_scoped_select/product_id.ml .
  $ cp test_scoped_select/scope_frag.ml .
  $ cat scope.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ grep -c "module Scope = Sqlgg_scope.Make(T)" output.ml
  1
  $ grep -c "(fieldset : _ Scope.t)" output.ml
  4
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c scope_frag.ml
  $ echo "scope fragment reused across two queries: OK"
  scope fragment reused across two queries: OK
