ON shapes: parameter in ON keeps the join, an extra constant conjunct does not,
inequality keeps it, a table without an alias is matched by its own name.

Generated code matches the golden file:

  $ cat on_shapes.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > on_shapes.ml
  $ diff on_shapes.ml on_shapes.compare.ml

Runtime (print_impl mock):

  $ cp ../../print_impl.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c on_shapes.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run.exe on_shapes.cmo print_impl.cmo run.ml
  $ ./run.exe
  === on_shapes/param_in_on: pick id -> join kept (param in ON) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = ? WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/extra_const_on: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT u.id FROM users u  WHERE u.id = ?
  [SQL] SELECT u.id FROM users u  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/extra_const_on: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/inequality: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id > u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id > u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/no_alias: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT u.id FROM users u  WHERE u.id = ?
  [SQL] SELECT u.id FROM users u  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/no_alias: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT profiles.bio FROM users u  LEFT JOIN profiles ON profiles.user_id = u.id WHERE u.id = ?
  [SQL] SELECT profiles.bio FROM users u  LEFT JOIN profiles ON profiles.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
