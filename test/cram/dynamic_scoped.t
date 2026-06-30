A single reusable fragment works across two DYNAMIC_SCOPED queries (dynamic_select
+ scoped): columns are chosen at runtime, yet [type 'a t] is shared via the
generated [Dynamic_select] module. This is the third mode, distinct from both pure
scoped (fixed SQL, [Scope]) and pure dynamic (unique per-query [t]):

  $ cp test_scoped_select/dyn_scoped.sql .
  $ cp test_scoped_select/product_id.ml .
  $ cp test_scoped_select/dyn_scoped_frag.ml .
  $ cat dyn_scoped.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ grep -c "module Dynamic_select = struct" output.ml
  1
  $ grep -c "include Dynamic_select" output.ml
  2
  $ grep -c "module Scope = struct" output.ml
  0
  [1]
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c dyn_scoped_frag.ml
  $ echo "dynamic_scoped fragment reused across two queries: OK"
  dynamic_scoped fragment reused across two queries: OK
