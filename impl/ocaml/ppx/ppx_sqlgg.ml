open Ppxlib

type target = { app : string; modtype_suffix : string; fun_suffix : string }

let scoped_target = { app = "Scope"; modtype_suffix = "scope"; fun_suffix = "scope" }
let dynamic_target = { app = "Dynamic_select"; modtype_suffix = "dyn_scope"; fun_suffix = "dyn" }

let modtype_name target = function
  | "t" -> "Sqlgg_" ^ target.modtype_suffix
  | tname -> "Sqlgg_" ^ tname ^ "_" ^ target.modtype_suffix

let fun_name target = function
  | "t" -> "of_" ^ target.fun_suffix
  | tname -> tname ^ "_of_" ^ target.fun_suffix

let record_kind =
  let open Ast_pattern in
  let field = label_declaration ~name:__' ~mutable_:drop ~type_:__ in
  ptype_record (many (field |> map2 ~f:(fun name ty -> (name, ty))))

let build_for_record ~loc ~target (tname : string) (fields : (string loc * core_type) list) =
  let (module B) = Ast_builder.make loc in
  let open B in
  let app = target.app in
  let app_t ~brand ty =
    ptyp_constr (Located.lident (app ^ ".t")) [ ty; ptyp_constr (Located.lident brand) [] ]
  in

  let selector_type (ty : core_type) : core_type =
    match ty with
    | [%type: [%t? _] -> [%t? _]] ->
      Location.raise_errorf ~loc
        "deriving sqlgg: a function-typed field cannot be a column"
    | [%type: [%t? _] Scope.t] | [%type: [%t? _] Dynamic_select.t]
    | [%type: ([%t? _], [%t? _]) Scope.t] | [%type: ([%t? _], [%t? _]) Dynamic_select.t] ->
      Location.raise_errorf ~loc
        "deriving sqlgg: field is already wrapped in an applicative; \
         declare the plain column type instead"
    | _ -> app_t ~brand:"t" ty
  in

  let modtype_name = modtype_name target tname in
  let record_ty = ptyp_constr (Located.lident tname) [] in

  let modtype_item =
    pstr_modtype
      (module_type_declaration ~name:(Located.mk modtype_name)
         ~type_:(Some (pmty_signature
           (psig_type Recursive
              [ type_declaration ~name:(Located.mk "t") ~params:[] ~cstrs:[]
                  ~kind:Ptype_abstract ~private_:Public ~manifest:None ]
            :: List.map
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

  let brand = "sqlgg__q" in
  let m_pat =
    ppat_constraint [%pat? (module M)]
      (ptyp_package
         (Located.lident modtype_name,
          [ (Located.lident "t", ptyp_constr (Located.lident brand) []) ]))
  in
  let body =
    pexp_fun Nolabel None m_pat
      [%expr ([%e applied] : [%t app_t ~brand record_ty])]
  in
  [ modtype_item
  ; pstr_value Nonrecursive
      [ value_binding ~pat:(pvar (fun_name target tname))
          ~expr:(pexp_newtype (Located.mk brand) body) ] ]

let generate_impl ~ctxt (_rec_flag, type_decls) mode =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let targets =
    match mode with
    | None | Some "scoped" -> [ scoped_target ]
    | Some "dynamic" -> [ dynamic_target ]
    | Some "both" -> [ scoped_target; dynamic_target ]
    | Some other ->
      Location.raise_errorf ~loc
        "deriving sqlgg: unknown mode %S (expected scoped, dynamic or both)" other
  in
  List.concat_map
    (fun td ->
      Ast_pattern.parse_res record_kind loc td.ptype_kind
        ~on_error:(fun () ->
          [ Ast_builder.Default.pstr_extension ~loc
              (Location.error_extensionf ~loc
                 "deriving sqlgg: only record types are supported") [] ])
        (fun fields ->
          List.concat_map
            (fun target -> build_for_record ~loc ~target td.ptype_name.txt fields)
            targets)
      |> Result.fold ~ok:Fun.id ~error:(fun (err, _) ->
           [ Ast_builder.Default.pstr_extension ~loc (Location.Error.to_extension err) [] ]))
    type_decls

let () =
  Deriving.add "sqlgg"
    ~str_type_decl:(Deriving.Generator.V2.make
      Deriving.Args.(empty +> arg "mode" (pexp_ident (lident __)))
      generate_impl)
  |> Deriving.ignore
