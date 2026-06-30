open Ppxlib

let scope_modtype_name tname =
  if tname = "t" then "Sqlgg_scope" else "Sqlgg_" ^ tname ^ "_scope"
let scope_fun_name tname =
  if tname = "t" then "of_scope" else tname ^ "_of_scope"

let selector_type ~loc (ty : core_type) : core_type =
  match ty with
  | [%type: [%t? _] -> [%t? _]] ->
    Location.raise_errorf ~loc
      "deriving sqlgg: a function-typed field cannot be a column"
  | [%type: [%t? _] Scope.t] ->
    Location.raise_errorf ~loc
      "deriving sqlgg: field is already wrapped in Scope.t; \
       declare the plain column type instead"
  | _ -> [%type: [%t ty] Scope.t]

let build_for_record ~loc tname (fields : label_declaration list) =
  let (module B) = Ast_builder.make loc in
  let open B in
  let modtype_name = scope_modtype_name tname in
  let record_ty = ptyp_constr (Located.lident tname) [] in
  let modtype_item =
    pstr_modtype
      (module_type_declaration ~name:(Located.mk modtype_name)
         ~type_:(Some (pmty_signature
           (List.map
              (fun ld ->
                psig_value
                  (value_description ~name:ld.pld_name
                     ~type_:(selector_type ~loc ld.pld_type) ~prim:[]))
              fields))))
  in

  let ctor_lambda =
    eabstract
      (List.map (fun ld -> pvar ld.pld_name.txt) fields)
      [%expr ([%e pexp_record
                    (List.map (fun ld -> (Located.lident ld.pld_name.txt, evar ld.pld_name.txt)) fields)
                    None]
              : [%t record_ty])]
  in

  let applied =
    List.fold_left
      (fun acc ld ->
        [%expr Scope.apply [%e acc] [%e evar ("M." ^ ld.pld_name.txt)]])
      [%expr Scope.pure [%e ctor_lambda]]
      fields
  in

  let m_pat = ppat_constraint [%pat? (module M)] (ptyp_package (Located.lident modtype_name, [])) in
  [ modtype_item
  ; [%stri let [%p pvar (scope_fun_name tname)] =
             fun [%p m_pat] -> ([%e applied] : [%t record_ty] Scope.t)] ]

let generate_impl ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.concat_map
    (fun td -> match td.ptype_kind with
       | Ptype_record fields -> build_for_record ~loc td.ptype_name.txt fields
       | _ ->
         [ Ast_builder.Default.pstr_extension ~loc
             (Location.error_extensionf ~loc
                "deriving sqlgg: only record types are supported") [] ])
    type_decls

let () =
  Deriving.add "sqlgg" ~str_type_decl:(Deriving.Generator.V2.make_noarg generate_impl)
  |> Deriving.ignore
