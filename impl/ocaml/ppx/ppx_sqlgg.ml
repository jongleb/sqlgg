open Ppxlib

module type Target = sig
  val modtype_suffix : string
  val fun_suffix : string
  val wrap : loc:location -> core_type -> brand:core_type -> core_type
  val pure : loc:location -> expression
  val apply : loc:location -> expression
end

module Scoped = struct
  let modtype_suffix = "scope"
  let fun_suffix = "scope"
  let wrap ~loc ty ~brand = [%type: ([%t ty], [%t brand]) Scope.t]
  let pure ~loc = [%expr Scope.pure]
  let apply ~loc = [%expr Scope.apply]
end

module Dynamic = struct
  let modtype_suffix = "dyn_scope"
  let fun_suffix = "dyn"
  let wrap ~loc ty ~brand = [%type: ([%t ty], [%t brand]) Dynamic_select.t]
  let pure ~loc = [%expr Dynamic_select.pure]
  let apply ~loc = [%expr Dynamic_select.apply]
end

let modtype_name (module T : Target) = function
  | "t" -> "Sqlgg_" ^ T.modtype_suffix
  | tname -> "Sqlgg_" ^ tname ^ "_" ^ T.modtype_suffix

let fun_name (module T : Target) = function
  | "t" -> "of_" ^ T.fun_suffix
  | tname -> tname ^ "_of_" ^ T.fun_suffix

let record_kind =
  let open Ast_pattern in
  ptype_record
    (many
       (label_declaration ~name:__' ~mutable_:drop ~type_:__
        |> map2 ~f:(fun name ty -> (name, ty))))

let selector_type ~target:(module T : Target) (ty : core_type) : core_type =
  let loc = ty.ptyp_loc in
  match ty with
  | [%type: [%t? _] -> [%t? _]] ->
    Location.raise_errorf ~loc
      "deriving sqlgg: a function-typed field cannot be a column"
  | [%type: [%t? _] Scope.t]
  | [%type: [%t? _] Dynamic_select.t]
  | [%type: ([%t? _], [%t? _]) Scope.t]
  | [%type: ([%t? _], [%t? _]) Dynamic_select.t] ->
    Location.raise_errorf ~loc
      "deriving sqlgg: field is already wrapped in an applicative; \
       declare the plain column type instead"
  | _ -> T.wrap ~loc ty ~brand:[%type: t]

let build_for_record ~loc ~target (tname : string)
    (fields : (string loc * core_type) list) =
  let (module T : Target) = target in
  let (module B) = Ast_builder.make loc in
  let open B in
  let modtype = modtype_name target tname in
  let record_ty = ptyp_constr (Located.lident tname) [] in

  let modtype_item =
    pstr_modtype
      (module_type_declaration ~name:(Located.mk modtype)
         ~type_:
           (Some
              (pmty_signature
                 ([%sigi: type t]
                  :: List.map
                       (fun (name, ty) ->
                         psig_value
                           (value_description ~name
                              ~type_:(selector_type ~target ty) ~prim:[]))
                       fields))))
  in

  let ctor =
    eabstract
      (List.map (fun (name, _) -> pvar name.txt) fields)
      (pexp_constraint
         (pexp_record
            (List.map
               (fun (name, _) -> (Located.lident name.txt, evar name.txt))
               fields)
            None)
         record_ty)
  in

  let applied =
    List.fold_left
      (fun acc (name, _) ->
        [%expr [%e T.apply ~loc] [%e acc] [%e evar ("M." ^ name.txt)]])
      [%expr [%e T.pure ~loc] [%e ctor]]
      fields
  in

  let m_pat =
    ppat_constraint
      [%pat? (module M)]
      (ptyp_package
         (Located.lident modtype, [ (Located.lident "t", [%type: sqlgg__q]) ]))
  in

  [ modtype_item
  ; [%stri
      let [%p pvar (fun_name target tname)] =
       fun (type sqlgg__q) [%p m_pat] :
           [%t T.wrap ~loc record_ty ~brand:[%type: sqlgg__q]] -> [%e applied]]
  ]

let generate_impl ~ctxt (_rec_flag, type_decls) mode =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  let targets : (module Target) list =
    match mode with
    | None | Some "scoped" -> [ (module Scoped) ]
    | Some "dynamic" -> [ (module Dynamic) ]
    | Some "both" -> [ (module Scoped); (module Dynamic) ]
    | Some other ->
      Location.raise_errorf ~loc
        "deriving sqlgg: unknown mode %S (expected scoped, dynamic or both)"
        other
  in
  let error_item ext = [ Ast_builder.Default.pstr_extension ~loc ext [] ] in
  List.concat_map
    (fun td ->
      Ast_pattern.parse_res record_kind loc td.ptype_kind
        ~on_error:(fun () ->
          error_item
            (Location.error_extensionf ~loc
               "deriving sqlgg: only record types are supported"))
        (fun fields ->
          List.concat_map
            (fun target ->
              build_for_record ~loc ~target td.ptype_name.txt fields)
            targets)
      |> Result.fold ~ok:Fun.id ~error:(fun (err, _) ->
             error_item (Location.Error.to_extension err)))
    type_decls

let () =
  Deriving.add "sqlgg"
    ~str_type_decl:
      (Deriving.Generator.V2.make
         Deriving.Args.(empty +> arg "mode" (pexp_ident (lident __)))
         generate_impl)
  |> Deriving.ignore
