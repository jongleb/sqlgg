What [@@deriving sqlgg] expands to:

  $ cat > who.ml <<'EOF'
  > type who = { id : int64; name : string option }
  > [@@deriving sqlgg]
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -dsource -c who.ml 2>&1
  type who = {
    id: int64 ;
    name: string option }[@@deriving sqlgg]
  include
    struct
      [@@@ocaml.warning "-60"]
      let _ = fun (_ : who) -> ()
      module type Sqlgg_who_cols  =
        sig
          type t
          type row
          type params
          type 'a col = ('a, t, row, params) Sqlgg_scope.col
          val id : int64 col
          val name : string option col
        end
      let who_of_cols (type sqlgg__q) (type sqlgg__row) (type sqlgg__params)
        ((module M)  :
          (module Sqlgg_who_cols with type t = sqlgg__q and type row =
            sqlgg__row and type params = sqlgg__params))
        =
        (Sqlgg_scope.apply
           (Sqlgg_scope.apply
              (Sqlgg_scope.pure (fun id -> fun name -> ({ id; name } : who)))
              M.id) M.name : (who, sqlgg__q, sqlgg__row, sqlgg__params)
                               Sqlgg_scope.col)
      let _ = who_of_cols
    end[@@ocaml.doc "@inline"][@@merlin.hide ]

Non-record types are rejected:

  $ cat > bad.ml <<'EOF'
  > type bad = A | B
  > [@@deriving sqlgg]
  > EOF
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg.ppx -c bad.ml 2>&1
  File "bad.ml", lines 1-2, characters 0-18:
  1 | type bad = A | B
  2 | [@@deriving sqlgg]
  Error: deriving sqlgg: only record types are supported
  [2]
