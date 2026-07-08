One scope (field-set) reused across two DIFFERENT scoped queries. The scope mixes
a plain string (name) with an opaque id type (User_id.t, abstract int64). It is
derived once and applied first to query 1 (users), then to a totally different
query 2 (admins) whose id/name sit at different absolute positions:

  $ cp ../print_ocaml_impl.ml .
  $ cat reuse.sql | sqlgg -no-header -gen caml -params unnamed -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c user_id.mli
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c user_id.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx,yojson -I . -c reuse_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o reuse.exe print_ocaml_impl.cmo user_id.cmo output.cmo reuse_run.cmo
  $ ./reuse.exe 2>&1 | grep -E 'Q[12] who'
  Q1 who: id=1 name=alice
  Q2 who: id=7 name=bob
  Q2 who: id=8 name=carol
