Scoped (fixed-SQL) selectors honor the [sqlgg] module= column meta, so a derived
record can use the wrapper's type (not just int/string). The deriver is
type-agnostic and lines up with the generated selector type:

  $ cat scope_mod.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ grep -c "Scope.read = (fun row -> Product_id.get_column" output.ml
  1
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c product_id.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -I . -c scope_mod_frag.ml
  $ echo "scoped + module= meta + derived record: OK"
  scoped + module= meta + derived record: OK
