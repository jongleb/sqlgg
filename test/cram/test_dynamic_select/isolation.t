Pure dynamic select: every query module declares its own phantom brand
[Cols.t] and its selectors are pinned to it, so a fragment from one query
cannot be passed to another query's select — it is a compile-time error.

  $ cat isolation.sql | sqlgg -no-header -gen caml -dialect mysql - > output.ml
  $ grep -c "module Cols = struct" output.ml
  2
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml

Correct usage compiles:

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c isolation_ok.ml

A fragment from q1 used with q2 is rejected by the type checker:

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c isolation_bad.ml 2>errors.log
  [2]
  $ grep -q "Q1_col.t" errors.log && grep -q "Q2_col.t" errors.log && echo "rejected: q1 fragment is not a q2 fragment"
  rejected: q1 fragment is not a q2 fragment

Selectors of two queries cannot be combined even through the library-level
brand-polymorphic [Dynamic_select.apply]:

  $ ocamlfind ocamlc -package sqlgg.traits -I . -c mix_apply_bad.ml 2>errors2.log
  [2]
  $ grep -q "Q1_col.Cols.t" errors2.log && grep -q "Q2_col" errors2.log && echo "rejected: apply does not mix brands"
  rejected: apply does not mix brands
