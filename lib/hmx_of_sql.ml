
open Hmx_lattice

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

  | Union { ctors; is_closed } ->
    Some (Refined.make Base.Text
            (Refine.enum ~closed:is_closed (Sql.Type.Enum_kind.Ctors.elements ctors)))
  | Any -> None

let of_nullability (n : Sql.Type.nullability) =
  match n with Strict -> Some false | Nullable -> Some true | Depends -> None

let of_type (t : Sql.Type.t) = of_kind t.t, of_nullability t.nullability

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
