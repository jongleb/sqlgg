One derived fragment reused across two dynamic queries:

  $ cat dyn_scoped.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ grep -c "module Dynamic_select = Sqlgg_scope.Dynamic(Row)(Params)" output.ml
  1
  $ grep -c "include Dynamic_select.Ops(Cols)" output.ml
  2
  $ grep -c "module Scope = struct" output.ml
  0
  [1]
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c dyn_scoped_frag.ml
  $ echo "fragment reused across two dynamic queries: OK"
  fragment reused across two dynamic queries: OK
