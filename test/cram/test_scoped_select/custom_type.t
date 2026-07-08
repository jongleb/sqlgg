Scoped (fixed-SQL) decoding runs custom [module=] conversions end-to-end with a
genuinely custom type (a variant, not an int/string alias). The derived record
uses Color.t, and decoding a mock row actually produces the variant values:

  $ cp ../print_ocaml_impl.ml .
  $ cat items.sql | sqlgg -no-header -gen caml -params unnamed -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c color.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx,yojson -I . -c scope_custom_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o run.exe print_ocaml_impl.cmo color.cmo output.cmo scope_custom_run.cmo
  $ ./run.exe 2>&1 | grep -F 'DECODED'
  DECODED id=7 color=Green
  DECODED id=8 color=Blue
