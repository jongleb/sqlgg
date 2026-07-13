Shared choices (same @name used several times in one statement).

  $ cat shared.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > shared.ml
  $ diff shared.ml shared.compare.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c shared.ml
