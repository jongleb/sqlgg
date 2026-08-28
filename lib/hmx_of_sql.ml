(** Bridge from the existing type and overload descriptors to the HM(X) core.

    Transitional by design: it exists so the new pipeline can run against the
    function registry that is already there, and so coverage is a test rather
    than a hand transcription of 120 entries. It is deleted together with
    {!Sql.Type} and {!Sql.Function}.

    Everything here is total and returns [result]. What it cannot express is
    reported, not approximated. *)

open Hmx_lattice

(** [None] is [Any], which has no counterpart: its two roles (bottom of the
    lattice, and "not yet known") are a fresh variable in the new world. *)
let of_kind (k : Sql.Type.kind) : Refined.t option =
  match k with
  | Int -> Some (Refined.of_base Base.Int)
  | UInt64 -> Some (Refined.of_base Base.UInt64)
  | Float -> Some (Refined.of_base Base.Float)
  | Bool -> Some (Refined.of_base Base.Bool)
  | Datetime -> Some (Refined.of_base Base.Datetime)
  | Text -> Some (Refined.of_base Base.Text)
  | Blob -> Some (Refined.of_base Base.Blob)
  | Json -> Some (Refined.of_base Base.Json)
  | Json_path -> Some (Refined.of_base Base.Json_path)
  | One_or_all -> Some (Refined.of_base Base.One_or_all)
  | Decimal { precision; scale } -> Some (Refined.make Base.Decimal (Refine.decimal ~precision ~scale))
  | StringLiteral s -> Some (Refined.make Base.Str_lit (Refine.literal s))
  | FloatingLiteral f -> Some (Refined.make Base.Num_lit (Refine.Flt f))
  (* is_closed takes no part in the order — "accepts no further constructors"
     is an upper bound on the variable, not a flag inside the type — but it is
     carried so the declared type can be rebuilt *)
  | Union { ctors; is_closed } ->
    Some (Refined.make Base.Text
            (Refine.enum ~closed:is_closed (Sql.Type.Enum_kind.Ctors.elements ctors)))
  | Any -> None

(** [None] is [Depends], which is a fresh nullability variable *)
let of_nullability (n : Sql.Type.nullability) =
  match n with Strict -> Some false | Nullable -> Some true | Depends -> None

let of_type (t : Sql.Type.t) = of_kind t.t, of_nullability t.nullability

let of_tyvar (v : Sql.Type.tyvar) : Hmx_sig.param_spec =
  match v with
  | Var _ -> Hmx_sig.Same
  | Typ t ->
    (match of_kind t.t with
     | None -> Hmx_sig.Free
     | Some ty -> Hmx_sig.As ty)

(* Ret Any means "the common supertype of the arguments", which is the shared
   variable, not an unconstrained one *)
let ret_of_tyvar (v : Sql.Type.tyvar) : Refined.t option =
  match v with
  | Var _ -> None
  | Typ t -> of_kind t.t

let base b = Refined.of_base b

(* Deliberate divergence from [undepend ret (common_nullability args)]: there,
   a result declared Strict stays Strict whatever the arguments are, so
   LENGTH(nullable_col) claims NOT NULL. Standard scalar functions propagate
   NULL, so a declared Strict means "not null given not-null arguments" and the
   join still applies. Only a declared Nullable is absorbing. *)
let null_of_declared (t : Sql.Type.t) : Hmx_sig.null_rule =
  match t.nullability with
  | Nullable -> Hmx_sig.Const true
  | Strict | Depends -> Hmx_sig.Join

let null_of_ret (v : Sql.Type.tyvar) : Hmx_sig.null_rule =
  match v with Var _ -> Hmx_sig.Join | Typ t -> null_of_declared t

let of_comparison (op : Sql.comparison_op) =
  let open Hmx_sig in
  let ret = base Base.Bool in
  match op with
  (* IS NULL asks about nullability rather than comparing, so a parameter
     under it may perfectly well be null *)
  | Is_null | Is_not_null -> make ~nulls:(Const false) (Args [ Same ]) ~ret
  | Not_distinct_op -> make ~nulls:(Const false) ~compares:true (Args [ Same; Same ]) ~ret
  | Comp_equal | Comp_num_cmp | Comp_text_cmp | Comp_num_eq ->
    make ~compares:true (Args [ Same; Same ]) ~ret

let of_func ~arity (f : Sql.Source_type.t Sql.func) : (Hmx_sig.t, string) result =
  let to_type = Sql.Source_type.to_infer_type in
  let open Hmx_sig in
  let repeat n spec = Args (List.init n (fun _ -> spec)) in
  let bool = base Base.Bool and text = base Base.Text in
  (* Ret/Arith: a concrete result ignores the arguments, Any takes their lub *)
  let ret_like ?(preds = []) t =
    let t = to_type t in
    let nulls = null_of_declared t in
    match of_kind t.t with
    (* Any means "the lub of the arguments", so the shared variable is what
       carries the predicate *)
    | None -> Ok (make ~nulls ~preds (repeat arity Same))
    | Some ty -> Ok (make ~nulls ~ret:ty (repeat arity Free))
  in
  match f with
  | Agg Count ->
    Ok (make ~nulls:(Const false) ~ret:(base Base.Int) (repeat arity Free))
  | Agg Avg ->
    Ok (make ~nulls:(Const true) ~ret:(base Base.Float) (Args [ Same ]))
  | Agg Self -> Ok (make ~nulls:Group_join (Args [ Same ]))
  | Agg (With_order { with_order_kind = Group_concat; _ }) ->
    Ok (make ~nulls:Group_join ~ret:text (repeat arity Free))
  | Agg (With_order { with_order_kind = Json_arrayagg; _ }) ->
    Ok (make ~nulls:Group_join ~ret:(base Base.Json) (Args [ Free ]))
  | Null_handling Null_if -> Ok (make ~nulls:(Const true) (Args [ Same; Same ]))
  | Null_handling If_null -> Ok (make ~nulls:Meet (Args [ Same; Same ]))
  | Null_handling (Coalesce (ret, each)) ->
    Ok (make ~nulls:Meet ?ret:(ret_of_tyvar ret)
          (Varargs { head = [ of_tyvar each ]; tail = [ of_tyvar each ] }))
  | Comparison op | Quantified_comparison { op; _ } -> Ok (of_comparison op)
  | Logical _ -> Ok (make ~ret:bool (Args [ As bool; As bool ]))
  | Negation -> Ok (make ~ret:bool (Args [ As bool ]))
  (* no predicate here on purpose: Arith also carries datetime arithmetic, and
     the descriptors cannot tell the two apart. Num belongs in the hand-written
     table, where + and date_add are separate entries. *)
  | Arith t -> ret_like t
  | Ret t -> ret_like t
  | Membership | Range -> Ok (make ~compares:true ~ret:bool (repeat arity Same))
  | Like { escaped } ->
    Ok (make ~compares:true ~ret:bool (Args [ As (if escaped then bool else text); As text ]))
  | F (ret, args) ->
    Ok (make ~nulls:(null_of_ret ret) ?ret:(ret_of_tyvar ret) (Args (List.map of_tyvar args)))
  | Multi { ret; fixed_args; repeating_pattern } ->
    Ok (make ~nulls:(null_of_ret ret) ?ret:(ret_of_tyvar ret)
          (Varargs { head = List.map of_tyvar fixed_args;
                     tail = List.map of_tyvar repeating_pattern }))
  | Col_assign { ret_t; col_t; arg_t } ->
    Ok (make ~nulls:Assign ?ret:(ret_of_tyvar ret_t) (Args [ of_tyvar col_t; of_tyvar arg_t ]))

(* ------------------------------------------------------------- back *)

(** Rebuild a declared type. Lossy in one direction only: [Num_lit] has no
    counterpart and is settled before it gets here. *)
let to_kind (r : Refined.t) : Sql.Type.kind =
  match r.base, r.refine with
  | Base.Int, _ -> Int
  | UInt64, _ -> UInt64
  | Num_lit, _ | Float, Top | Float, (Enum _ | Dec _) -> Float
  | Float, Flt f -> FloatingLiteral f
  | Decimal, Dec d -> Decimal { precision = Refine.precision_of d; scale = d.scale }
  | Decimal, _ -> Decimal { precision = None; scale = None }
  | Bool, _ -> Bool
  | Datetime, _ -> Datetime
  | Str_lit, Enum { ctors; closed } | Text, Enum { ctors; closed } ->
    (* a lone constructor is a string literal, which is how the old type
       records the same thing *)
    (match Refine.Ctors.elements ctors with
     | [ one ] when not closed -> StringLiteral one
     | l -> Union { ctors = Sql.Type.Enum_kind.make l; is_closed = closed })
  | Str_lit, _ | Text, _ -> Text
  | Blob, _ -> Blob
  | Json, _ -> Json
  | Json_path, _ -> Json_path
  | One_or_all, _ -> One_or_all

let to_nullability = function false -> Sql.Type.Strict | true -> Sql.Type.Nullable

let to_type r null : Sql.Type.t = { t = to_kind r; nullability = to_nullability null }
