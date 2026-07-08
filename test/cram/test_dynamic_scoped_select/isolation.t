Typing guarantees of the dynamic_scoped mode: reuse goes through the
brand-polymorphic [*_of_dyn] functions, while raw selectors are pinned to
their query by the phantom brand [Cols.t].

  $ cat dyn_scoped.sql | sqlgg -no-header -gen caml -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml

Reuse across queries via [who_of_dyn] compiles (positive control):

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c dyn_scoped_frag.ml

A raw selector from q1 passed to q2's select is rejected:

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c raw_mix_bad.ml 2>errors0.log
  [2]
  $ grep -q "Dscope_q1_col.Cols.t" errors0.log && grep -q "Dscope_q2_col" errors0.log && echo "rejected: selector belongs to another query"
  rejected: selector belongs to another query

A fragment cannot demand a column the query does not select — the [Cols]
module fails the signature check:

  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c missing_field_bad.ml 2>errors.log
  [2]
  $ grep -q "price" errors.log && echo "rejected: q2 does not provide price"
  rejected: q2 does not provide price

And fixed-scoped fragments ([Scope.t]) are not interchangeable with dynamic
ones ([Dynamic_select.t]):

  $ cat cross_mode.sql | sqlgg -no-header -gen caml -dialect mysql - > output2.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output2.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c scope_vs_dyn_bad.ml 2>errors2.log
  [2]
  $ grep -q "Scope.t" errors2.log && echo "rejected: fixed-scoped fragment is not a dynamic fragment"
  rejected: fixed-scoped fragment is not a dynamic fragment
