One record derives BOTH constructors ([mode = both]): [who_of_scope] targets the
fixed-SQL [Scope] applicative and [who_of_dyn] targets [Dynamic_select], so the
same record is reusable across fixed scoped and dynamic scoped queries in one
generated module:

  $ cat cross_mode.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ grep -c "module Scope = Sqlgg_scope.Make(Row)" output.ml
  1
  $ grep -c "module Dynamic_select = Sqlgg_scope.Dynamic(Row)(Params)" output.ml
  1
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c cross_mode_frag.ml
  $ echo "one record, both modes: OK"
  one record, both modes: OK
