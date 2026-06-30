open Ppxlib

let modtype_name = function "t" -> "Sqlgg_cols" | t -> "Sqlgg_" ^ t ^ "_cols"
let fun_name = function "t" -> "of_cols" | t -> t ^ "_of_cols"

let build ~loc tname (fields : label_declaration list) =
  let (module B) = Ast_builder.make loc in
  let open B in
  let lid = Located.lident in
  let record_ty = ptyp_constr (lid tname) [] in
  let names = List.map (fun ld -> ld.pld_name.txt) fields in
  let ctor =
    eabstract (List.map pvar names)
      [%expr
        ([%e pexp_record (List.map (fun n -> (lid n, evar n)) names) None]
          : [%t record_ty])]
  in
  let package_ty =
    Ast_pattern.(parse (ptyp_package (__ ** __))) loc
      [%type:
        (module Placeholder
          with type t = sqlgg__q
           and type row = sqlgg__row
           and type params = sqlgg__params)]
      (fun name cstrs ->
        ptyp_package ({ name with txt = Lident (modtype_name tname) }, cstrs))
  in
  let sig_val ld =
    psig_value
      (value_description ~name:ld.pld_name
         ~type_:[%type: [%t ld.pld_type] col] ~prim:[])
  in
  [ pstr_modtype
      (module_type_declaration
         ~name:(Located.mk (modtype_name tname))
         ~type_:
           (Some
              (pmty_signature
                 ([%sigi: type t]
                  :: [%sigi: type row]
                  :: [%sigi: type params]
                  :: [%sigi: type 'a col = ('a, t, row, params) Sqlgg_scope.col]
                  :: List.map sig_val fields))))
  ; [%stri
      let [%p pvar (fun_name tname)] =
       fun (type sqlgg__q sqlgg__row sqlgg__params)
         ((module M) : [%t package_ty]) :
           ([%t record_ty], sqlgg__q, sqlgg__row, sqlgg__params) Sqlgg_scope.col ->
        [%e
          List.fold_left
            (fun acc n -> [%expr Sqlgg_scope.apply [%e acc] [%e evar ("M." ^ n)]])
            [%expr Sqlgg_scope.pure [%e ctor]]
            names]]
  ]

let () =
  Deriving.add "sqlgg"
    ~str_type_decl:
      (Deriving.Generator.V2.make Deriving.Args.empty (fun ~ctxt (_, tds) ->
           let loc = Expansion_context.Deriver.derived_item_loc ctxt in
           List.concat_map
             (fun td ->
               Ast_pattern.(parse (ptype_record __)) loc td.ptype_kind
                 ~on_error:(fun () ->
                   [ [%stri
                       [%%ocaml.error
                       "deriving sqlgg: only record types are supported"]] ])
                 (build ~loc td.ptype_name.txt))
             tds))
  |> Deriving.ignore
