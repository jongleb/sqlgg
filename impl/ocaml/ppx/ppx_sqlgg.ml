open Ppxlib

let default_applicative = "Scope"

let scope_modtype_name = function
  | "t" -> "Sqlgg_scope"
  | tname -> "Sqlgg_" ^ tname ^ "_scope"

let scope_fun_name = function
  | "t" -> "of_scope"
  | tname -> tname ^ "_of_scope"

let record_kind =
  let open Ast_pattern in
  let field = label_declaration ~name:__' ~mutable_:drop ~type_:__ in
  ptype_record (many (field |> map2 ~f:(fun name ty -> (name, ty))))

let build_for_record ~loc ~app (tname : string) (fields : (string loc * core_type) list) =
  let (module B) = Ast_builder.make loc in
  let open B in
  let app_t ty = ptyp_constr (Located.lident (app ^ ".t")) [ ty ] in

  let selector_type (ty : core_type) : core_type =
    match ty with
    | [%type: [%t? _] -> [%t? _]] ->
      Location.raise_errorf ~loc
        "deriving sqlgg: a function-typed field cannot be a column"
    | [%type: [%t? _] Scope.t] | [%type: [%t? _] Dynamic_select.t] ->
      Location.raise_errorf ~loc
        "deriving sqlgg: field is already wrapped in an applicative; \
         declare the plain column type instead"
    | _ -> app_t ty
  in

  let modtype_name = scope_modtype_name tname in
  let record_ty = ptyp_constr (Located.lident tname) [] in

  let modtype_item =
    pstr_modtype
      (module_type_declaration ~name:(Located.mk modtype_name)
         ~type_:(Some (pmty_signature
           (List.map
              (fun (name, ty) ->
                psig_value (value_description ~name ~type_:(selector_type ty) ~prim:[]))
              fields))))
  in

  let ctor_lambda =
    eabstract
      (List.map (fun (name, _) -> pvar name.txt) fields)
      [%expr ([%e pexp_record
                    (List.map (fun (name, _) -> (Located.lident name.txt, evar name.txt)) fields)
                    None]
              : [%t record_ty])]
  in

  let applied =
    List.fold_left
      (fun acc (name, _) ->
        [%expr [%e evar (app ^ ".apply")] [%e acc] [%e evar ("M." ^ name.txt)]])
      [%expr [%e evar (app ^ ".pure")] [%e ctor_lambda]]
      fields
  in

  let m_pat =
    ppat_constraint [%pat? (module M)] (ptyp_package (Located.lident modtype_name, []))
  in
  [ modtype_item
  ; [%stri
      let [%p pvar (scope_fun_name tname)] =
        fun [%p m_pat] -> ([%e applied] : [%t app_t record_ty])] ]

let generate_impl ~ctxt (_rec_flag, type_decls) mode =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let app =
    match mode with
    | None | Some "scoped" -> default_applicative
    | Some "dynamic" -> "Dynamic_select"
    | Some other ->
      Location.raise_errorf ~loc
        "deriving sqlgg: unknown mode %S (expected scoped or dynamic)" other
  in
  List.concat_map
    (fun td ->
      Ast_pattern.parse_res record_kind loc td.ptype_kind
        ~on_error:(fun () ->
          [ Ast_builder.Default.pstr_extension ~loc
              (Location.error_extensionf ~loc
                 "deriving sqlgg: only record types are supported") [] ])
        (fun fields -> build_for_record ~loc ~app td.ptype_name.txt fields)
      |> Result.fold ~ok:Fun.id ~error:(fun (err, _) ->
           [ Ast_builder.Default.pstr_extension ~loc (Location.Error.to_extension err) [] ]))
    type_decls

let () =
  Deriving.add "sqlgg"
    ~str_type_decl:(Deriving.Generator.V2.make
      Deriving.Args.(empty +> arg "mode" (pexp_ident (lident __)))
      generate_impl)
  |> Deriving.ignore
