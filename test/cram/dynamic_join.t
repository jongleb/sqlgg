A LEFT JOIN whose right key is UNIQUE/PRIMARY and that is referenced only by the
dynamic projection is turned into a FROM "hole": it is emitted at runtime only when a
selected dynamic column comes from it ("не запросили поле — нет джойна"), but only
after analysing the join ("разобрав подноготную джойна"). Joins that are unsafe to drop
stay static. Each scenario lives in test_build_dynamic_join/<case>.sql and the full
generated code is compared against its golden test_build_dynamic_join/<case>.compare.ml.

Basic: droppable PK join / non-unique key / table referenced in WHERE:

  $ cat test_build_dynamic_join/basic.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > basic.ml
  $ diff basic.ml test_build_dynamic_join/basic.compare.ml

Self-joins are matched by their alias key, not the bare table name: non-unique key
keeps the join, PK self-join is droppable:

  $ cat test_build_dynamic_join/self_join.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > self_join.ml
  $ diff self_join.ml test_build_dynamic_join/self_join.compare.ml

Join kinds: only LEFT JOIN ... ON is a candidate (INNER removes rows, USING/NATURAL
have no ON to analyse):

  $ cat test_build_dynamic_join/join_kinds.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > join_kinds.ml
  $ diff join_kinds.ml test_build_dynamic_join/join_kinds.compare.ml

ON shapes: parameter in ON keeps the join, an extra constant conjunct does not,
inequality keeps it, a table without an alias is matched by its own name:

  $ cat test_build_dynamic_join/on_shapes.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > on_shapes.ml
  $ diff on_shapes.ml test_build_dynamic_join/on_shapes.compare.ml

Key shapes: non-PK UNIQUE works, composite PK only when ALL parts are equated:

  $ cat test_build_dynamic_join/key_shapes.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > key_shapes.ml
  $ diff key_shapes.ml test_build_dynamic_join/key_shapes.compare.ml

References outside the projection (GROUP BY / ORDER BY / HAVING / complex projection
expression / subquery in WHERE / unqualified column) pin the join; a droppable join
never referenced by the projection gets no hole and stays static:

  $ cat test_build_dynamic_join/outside_refs.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > outside_refs.ml
  $ diff outside_refs.ml test_build_dynamic_join/outside_refs.compare.ml

Subquery sources: a LEFT JOIN of a subquery is never droppable (its columns inherit
the underlying table's UNIQUE/PRIMARY marks but the subquery may multiply rows); a
subquery as the BASE source does not poison a droppable table join on top of it:

  $ cat test_build_dynamic_join/subquery_sources.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > subquery_sources.ml
  $ diff subquery_sources.ml test_build_dynamic_join/subquery_sources.compare.ml

Transitive chains: a child's ON reference to a droppable parent is a parent edge, not
a blocker; closures are accumulated child first (two-level, three-level, diamond):

  $ cat test_build_dynamic_join/chains.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > chains.ml
  $ diff chains.ml test_build_dynamic_join/chains.compare.ml

If the child join is NOT droppable, its ON reference pins the parent too:

  $ cat test_build_dynamic_join/chain_bad.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > chain_bad.ml
  $ diff chain_bad.ml test_build_dynamic_join/chain_bad.compare.ml

Multiple joins in one FROM: two independent droppables, a subquery in another join's
ON (conservative keep of everything), the same table joined twice (distinct
constructors per alias):

  $ cat test_build_dynamic_join/multi.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > multi.ml
  $ diff multi.ml test_build_dynamic_join/multi.compare.ml

---- Runtime: observe the final SQL per pick (print_impl mock) ----

  $ cp test_build_dynamic_join/run_chains.ml test_build_dynamic_join/run_multi.ml test_build_dynamic_join/run_cases.ml .
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c print_impl.ml
  $ for m in basic self_join join_kinds on_shapes key_shapes outside_refs subquery_sources chains chain_bad multi; do ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c $m.ml; done
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run_chains.exe chains.cmo print_impl.cmo run_chains.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run_multi.exe multi.cmo print_impl.cmo run_multi.ml
  $ ocamlfind ocamlc -package unix,sqlgg.traits -I . -linkpkg -o run_cases.exe basic.cmo self_join.cmo join_kinds.cmo on_shapes.cmo key_shapes.cmo outside_refs.cmo subquery_sources.cmo chain_bad.cmo print_impl.cmo run_cases.ml

Chains: a pick renders only the joins its closure needs, the parent is emitted once:

  $ ./run_chains.exe
  === chain: pick id ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u   WHERE u.id = ?
  [SQL] SELECT u.id FROM users u   WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain: pick bio ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain: pick url (pulls profiles transitively) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = ?
  [SQL] SELECT a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain: pick all ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id, p.bio, a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = ?
  [SQL] SELECT u.id, p.bio, a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain3: pick label (pulls the whole ancestor chain) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT b.label FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id  LEFT JOIN badges b ON b.id = a.badge_id WHERE u.id = ?
  [SQL] SELECT b.label FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id  LEFT JOIN badges b ON b.id = a.badge_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === chain3: pick url (badges not pulled) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id  WHERE u.id = ?
  [SQL] SELECT a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === diamond: pick url (one branch) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[7]: SELECT a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id  WHERE u.id = ?
  [SQL] SELECT a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === diamond: pick label (other branch) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[8]: SELECT b.label FROM users u  LEFT JOIN profiles p ON p.user_id = u.id   LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = ?
  [SQL] SELECT b.label FROM users u  LEFT JOIN profiles p ON p.user_id = u.id   LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === diamond: pick both (parent emitted once) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[9]: SELECT a.url, b.label FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id  LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = ?
  [SQL] SELECT a.url, b.label FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = p.avatar_id  LEFT JOIN badges b ON b.id = p.user_id WHERE u.id = 1
  [MOCK] Returning 0 rows

Independent joins and the same table twice: each pick renders only its own join:

  $ ./run_multi.exe
  === two_indep: pick id (no joins) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u   WHERE u.id = ?
  [SQL] SELECT u.id FROM users u   WHERE u.id = 1
  [MOCK] Returning 0 rows
  === two_indep: pick bio (profiles only) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === two_indep: pick url (avatars only) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT a.url FROM users u   LEFT JOIN avatars a ON a.id = u.id WHERE u.id = ?
  [SQL] SELECT a.url FROM users u   LEFT JOIN avatars a ON a.id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === two_indep: pick bio+url (both) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT p.bio, a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = u.id WHERE u.id = ?
  [SQL] SELECT p.bio, a.url FROM users u  LEFT JOIN profiles p ON p.user_id = u.id  LEFT JOIN avatars a ON a.id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === same_twice: pick bio1 (p1 only) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT p1.bio FROM users u  LEFT JOIN profiles p1 ON p1.user_id = u.id  WHERE u.id = ?
  [SQL] SELECT p1.bio FROM users u  LEFT JOIN profiles p1 ON p1.user_id = u.id  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === same_twice: pick bio2 (p2 only) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT p2.bio FROM users u   LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = ?
  [SQL] SELECT p2.bio FROM users u   LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === same_twice: pick both ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[7]: SELECT p1.bio, p2.bio FROM users u  LEFT JOIN profiles p1 ON p1.user_id = u.id  LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = ?
  [SQL] SELECT p1.bio, p2.bio FROM users u  LEFT JOIN profiles p1 ON p1.user_id = u.id  LEFT JOIN profiles p2 ON p2.user_id = u.mentor_id WHERE u.id = 1
  [MOCK] Returning 0 rows

Every analyzer case end-to-end: pick "id" only (the join disappears ONLY when safely
droppable) and pick the joined column (the join is always present):

  $ ./run_cases.exe
  === basic/ok: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[1]: SELECT u.id FROM users u  WHERE u.id = ?
  [SQL] SELECT u.id FROM users u  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === basic/ok: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[2]: SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === basic/nonuniq: pick id -> join kept (non-unique key) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[3]: SELECT u.id FROM users u LEFT JOIN orders o ON o.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN orders o ON o.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === basic/ref_in_where: pick id -> join kept (WHERE reference) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[4]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE p.bio = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE p.bio = 'x'
  [MOCK] Returning 0 rows
  === self_join/bad: pick id -> join kept (non-unique self key) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[5]: SELECT u1.id FROM users u1 LEFT JOIN users u2 ON u2.manager_id = u1.id
  [SQL] SELECT u1.id FROM users u1 LEFT JOIN users u2 ON u2.manager_id = u1.id
  [MOCK] Returning 0 rows
  === self_join/good: pick id -> join dropped (PK self key) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[6]: SELECT u1.id FROM users u1 
  [SQL] SELECT u1.id FROM users u1 
  [MOCK] Returning 0 rows
  === self_join/good: pick name -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[7]: SELECT u2.name FROM users u1  LEFT JOIN users u2 ON u2.id = u1.manager_id
  [SQL] SELECT u2.name FROM users u1  LEFT JOIN users u2 ON u2.id = u1.manager_id
  [MOCK] Returning 0 rows
  === join_kinds/inner: pick id -> join kept (INNER) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[8]: SELECT u.id FROM users u JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/using: pick id -> join kept (USING) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[9]: SELECT u.id FROM users u LEFT JOIN profiles p USING (user_id) WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p USING (user_id) WHERE u.id = 1
  [MOCK] Returning 0 rows
  === join_kinds/natural: pick id -> join kept (NATURAL) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[10]: SELECT u.id FROM users u NATURAL LEFT JOIN profiles p WHERE u.id = ?
  [SQL] SELECT u.id FROM users u NATURAL LEFT JOIN profiles p WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/param_in_on: pick id -> join kept (param in ON) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[11]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = ? WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/extra_const_on: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[12]: SELECT u.id FROM users u  WHERE u.id = ?
  [SQL] SELECT u.id FROM users u  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/extra_const_on: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[13]: SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = ?
  [SQL] SELECT p.bio FROM users u  LEFT JOIN profiles p ON p.user_id = u.id AND p.bio = 'x' WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/inequality: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[14]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id > u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id > u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/no_alias: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[15]: SELECT u.id FROM users u  WHERE u.id = ?
  [SQL] SELECT u.id FROM users u  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === on_shapes/no_alias: pick bio -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[16]: SELECT profiles.bio FROM users u  LEFT JOIN profiles ON profiles.user_id = u.id WHERE u.id = ?
  [SQL] SELECT profiles.bio FROM users u  LEFT JOIN profiles ON profiles.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/unique: pick id -> join dropped (UNIQUE key) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[17]: SELECT u.id FROM users u  WHERE u.id = ?
  [SQL] SELECT u.id FROM users u  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/unique: pick label -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[18]: SELECT a.label FROM users u  LEFT JOIN accounts a ON a.email = u.email WHERE u.id = ?
  [SQL] SELECT a.label FROM users u  LEFT JOIN accounts a ON a.email = u.email WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/composite_partial: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[19]: SELECT u.id FROM users u LEFT JOIN memberships m ON m.org = u.org WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN memberships m ON m.org = u.org WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/composite_full: pick id -> join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[20]: SELECT u.id FROM users u  WHERE u.id = ?
  [SQL] SELECT u.id FROM users u  WHERE u.id = 1
  [MOCK] Returning 0 rows
  === key_shapes/composite_full: pick title -> join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[21]: SELECT m.title FROM users u  LEFT JOIN memberships m ON m.org = u.org AND m.dept = u.dept WHERE u.id = ?
  [SQL] SELECT m.title FROM users u  LEFT JOIN memberships m ON m.org = u.org AND m.dept = u.dept WHERE u.id = 1
  [MOCK] Returning 0 rows
  === outside_refs/group: pick id -> join kept (GROUP BY) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[22]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio
  [MOCK] Returning 0 rows
  === outside_refs/order: pick id -> join kept (ORDER BY) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[23]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio
  [MOCK] Returning 0 rows
  === outside_refs/having: pick id -> join kept (HAVING) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[24]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0
  [MOCK] Returning 0 rows
  === outside_refs/complex_proj: pick id -> join kept (complex expr) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[25]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === outside_refs/subq_in_where: pick id -> join kept (subquery in WHERE) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[26]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles)
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles)
  [MOCK] Returning 0 rows
  === outside_refs/unqualified: pick id -> join kept (unqualified ref) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[27]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x'
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x'
  [MOCK] Returning 0 rows
  === outside_refs/unreferenced: pick id -> join rendered statically ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[28]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === subquery_sources/plain: pick id -> join kept (subquery source) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[29]: SELECT u.id FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === subquery_sources/cross_dup: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[30]: SELECT u.id FROM users u LEFT JOIN (SELECT p.user_id, p.bio FROM profiles p, users x) s ON s.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN (SELECT p.user_id, p.bio FROM profiles p, users x) s ON s.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === subquery_sources/union_dup: pick id -> join kept ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[31]: SELECT u.id FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles UNION ALL SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN (SELECT user_id, bio FROM profiles UNION ALL SELECT user_id, bio FROM profiles) s ON s.user_id = u.id WHERE u.id = 1
  [MOCK] Returning 0 rows
  === subquery_sources/subq_base: pick id -> table join dropped ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[32]: SELECT s.id FROM (SELECT id FROM users) s 
  [SQL] SELECT s.id FROM (SELECT id FROM users) s 
  [MOCK] Returning 0 rows
  === subquery_sources/subq_base: pick bio -> table join present ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[33]: SELECT p.bio FROM (SELECT id FROM users) s  LEFT JOIN profiles p ON p.user_id = s.id
  [SQL] SELECT p.bio FROM (SELECT id FROM users) s  LEFT JOIN profiles p ON p.user_id = s.id
  [MOCK] Returning 0 rows
  === chain_bad: pick id -> both joins kept (child pins parent) ===
  [MOCK SELECT] Connection type: [> `RO ]
  [MOCK] PREPARE[34]: SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.profile_id = p.id WHERE u.id = ?
  [SQL] SELECT u.id FROM users u LEFT JOIN profiles p ON p.user_id = u.id LEFT JOIN avatars a ON a.profile_id = p.id WHERE u.id = 1
  [MOCK] Returning 0 rows
