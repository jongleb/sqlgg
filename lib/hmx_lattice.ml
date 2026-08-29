
open Printf

exception Conflict of string

let conflict fmt = ksprintf (fun msg -> raise (Conflict msg)) fmt

module Base = struct

  type t =
    | Int
    | UInt64
    | Num_lit
    | Float
    | Decimal
    | Bool
    | Str_lit
    | Datetime
    | Text
    | Blob
    | Json
    | Json_path
    | One_or_all
    [@@deriving eq, ord, show { with_path = false }, enumerate]

  let index = function
    | Int -> 0 | UInt64 -> 1 | Num_lit -> 2 | Float -> 3 | Decimal -> 4 | Bool -> 5
    | Str_lit -> 6 | Datetime -> 7 | Text -> 8 | Blob -> 9 | Json -> 10 | Json_path -> 11
    | One_or_all -> 12

  let count = List.length all

  let declared = [

    Int, Num_lit;
    Num_lit, Float;
    Num_lit, Decimal;
    Int, Float;
    Int, Decimal;
    Int, UInt64;
    Int, Datetime;

    Str_lit, Text;
    Str_lit, Datetime;
    Str_lit, Json;
    Str_lit, Json_path;
    Str_lit, One_or_all;
    Datetime, Text;
    Text, Blob;
    Json, Text;
    Json, Blob;
    Json_path, Text;
    One_or_all, Text;
  ]

  let matrix =
    let m = Array.make_matrix count count false in
    List.iter (fun i -> m.(i).(i) <- true) (List.init count (fun i -> i));
    List.iter (fun (a, b) -> m.(index a).(index b) <- true) declared;

    for k = 0 to count - 1 do
      for i = 0 to count - 1 do
        for j = 0 to count - 1 do
          if m.(i).(k) && m.(k).(j) then m.(i).(j) <- true
        done
      done
    done;
    m

  let leq a b = matrix.(index a).(index b)

  let derived =
    List.filter (fun (a, b) ->
      not (equal a b)
      && leq a b
      && not (List.exists (fun (x, y) -> equal x a && equal y b) declared))
      (List.concat_map (fun a -> List.map (fun b -> a, b) all) all)

  let upper_bounds l = List.filter (fun c -> List.for_all (fun a -> leq a c) l) all
  let lower_bounds l = List.filter (fun c -> List.for_all (fun a -> leq c a) l) all

  let least = function
    | [] -> None
    | l -> (match List.filter (fun m -> List.for_all (leq m) l) l with [ m ] -> Some m | _ -> None)

  let greatest = function
    | [] -> None
    | l -> (match List.filter (fun m -> List.for_all (fun x -> leq x m) l) l with [ m ] -> Some m | _ -> None)

  let lub l = least (upper_bounds l)
  let glb l = greatest (lower_bounds l)

  let join a b = lub [ a; b ]
  let meet a b = glb [ a; b ]

end

module Refine = struct

  module Ctors = struct
    include Set.Make(String)
    let show s = sprintf "{%s}" (String.concat "|" (elements s))
    let pp fmt s = Format.pp_print_string fmt (show s)
  end

  type dec = { int_digits : int option; scale : int option } [@@deriving eq, ord]

  type enum = { ctors : Ctors.t; closed : bool } [@@deriving ord]

  type t =
    | Top
    | Enum of enum
    | Dec of dec
    | Flt of float
    [@@deriving ord]

  let compare_structural = compare

  let equal a b =
    match a, b with
    | Top, Top -> true
    | Enum a, Enum b -> Ctors.equal a.ctors b.ctors
    | Dec a, Dec b -> equal_dec a b
    | Flt a, Flt b -> Float.equal a b
    | (Top | Enum _ | Dec _ | Flt _), _ -> false

  let decimal ~precision ~scale =
    match precision, scale with
    | Some p, Some s -> Dec { int_digits = Some (p - s); scale = Some s }
    | Some p, None -> Dec { int_digits = Some p; scale = Some 0 }
    | None, Some s -> Dec { int_digits = None; scale = Some s }

    | None, None -> Top

  let precision_of d = match d.int_digits, d.scale with Some i, Some s -> Some (i + s) | _ -> None
  let literal s = Enum { ctors = Ctors.singleton s; closed = false }
  let enum ?(closed = false) l = Enum { ctors = Ctors.of_list l; closed }

  let show = function
    | Top -> "_"
    | Enum { ctors; closed } -> Ctors.show ctors ^ (if closed then "" else "..")
    | Dec d ->
      sprintf "(%s,%s)"
        (match precision_of d with Some p -> string_of_int p | None -> "_")
        (match d.scale with Some s -> string_of_int s | None -> "_")
    | Flt f -> sprintf "%g" f

  let pp fmt t = Format.pp_print_string fmt (show t)
  let is_top = function Top -> true | Enum _ | Dec _ | Flt _ -> false

  let is_closed_enum = function Enum { closed; _ } -> closed | Top | Dec _ | Flt _ -> false

  let is_value_set = function Enum _ | Flt _ -> true | Top | Dec _ -> false

  let le_opt x y = match x, y with _, None -> true | None, Some _ -> false | Some a, Some b -> a <= b
  let max_opt x y = match x, y with None, _ | _, None -> None | Some a, Some b -> Some (max a b)
  let min_opt x y = match x, y with None, o | o, None -> o | Some a, Some b -> Some (min a b)

  let leq a b =
    match a, b with
    | _, Top -> true
    | Top, (Enum _ | Dec _ | Flt _) -> false
    | Enum a, Enum b -> Ctors.subset a.ctors b.ctors
    | Dec a, Dec b -> le_opt a.scale b.scale && le_opt a.int_digits b.int_digits
    | Flt a, Flt b -> Float.equal a b

    | Flt f, Dec d ->
      (match d.int_digits, d.scale with
       | Some i, Some s ->
         let max = (10. ** float_of_int i) -. (10. ** (-. float_of_int s)) in
         f >= -. max && f <= max
       | _ -> true)
    | (Enum _ | Dec _ | Flt _), _ -> false

  let join a b =
    match a, b with
    | Top, _ | _, Top -> Top

    | Enum a, Enum b -> Enum { ctors = Ctors.union a.ctors b.ctors; closed = a.closed || b.closed }
    | Dec a, Dec b ->
      Dec { int_digits = max_opt a.int_digits b.int_digits; scale = max_opt a.scale b.scale }
    | Flt a, Flt b -> if Float.equal a b then Flt a else Top
    | (Enum _ | Dec _ | Flt _), _ -> Top

  let meet a b =
    match a, b with
    | Top, x | x, Top -> Some x
    | Enum a, Enum b ->
      let c = Ctors.inter a.ctors b.ctors in
      if Ctors.is_empty c then None else Some (Enum { ctors = c; closed = a.closed || b.closed })
    | Dec a, Dec b ->
      Some (Dec { int_digits = min_opt a.int_digits b.int_digits; scale = min_opt a.scale b.scale })
    | Flt a, Flt b -> if Float.equal a b then Some (Flt a) else None
    | (Enum _ | Dec _ | Flt _), _ -> None

  let join_all = function [] -> None | x :: l -> Some (List.fold_left join x l)

  let meet_all = function
    | [] -> `None
    | x :: l ->
      List.fold_left (fun acc y ->
        match acc with `Some a -> (match meet a y with Some r -> `Some r | None -> `Conflict) | acc -> acc)
        (`Some x) l

  let fits (b : Base.t) = function
    | Top -> true
    | Enum _ -> (match b with Str_lit | Text | Blob | Datetime | Json | Json_path | One_or_all -> true
                            | Int | UInt64 | Num_lit | Float | Decimal | Bool -> false)
    | Dec _ -> Base.equal b Base.Decimal
    | Flt _ -> Base.equal b Base.Float || Base.equal b Base.Num_lit
end

module Pred = struct

  type t = Num | Ord | Stringable
    [@@deriving eq, ord, show { with_path = false }, enumerate]

  let satisfies p (b : Base.t) =
    match p, b with
    | Num, (Int | UInt64 | Num_lit | Float | Decimal) -> true
    | Num, (Bool | Str_lit | Datetime | Text | Blob | Json | Json_path | One_or_all) -> false

    | Ord, _ -> true
    | Stringable, (Str_lit | Text | Blob | Datetime | Json | Json_path | One_or_all) -> true
    | Stringable, (Int | UInt64 | Num_lit | Float | Decimal | Bool) -> false

  let default p : Base.t option =
    match p with
    | Num -> Some Base.Int
    | Ord -> None
    | Stringable -> Some Base.Text

end

module Refined = struct
  type t = { base : Base.t; refine : Refine.t } [@@deriving eq, ord]

  let make base refine = { base; refine }
  let of_base base = { base; refine = Refine.Top }
  let show { base; refine } =
    if Refine.is_top refine then Base.show base else sprintf "%s%s" (Base.show base) (Refine.show refine)
  let pp fmt t = Format.pp_print_string fmt (show t)

  let valid_at (b : Base.t) (r : Refine.t) =
    match b, r with
    | Base.Json_path, Refine.Enum { ctors; _ } ->
      Refine.Ctors.for_all Sqlgg_json_path.Json_path.is_valid ctors
    | Base.One_or_all, Refine.Enum { ctors; _ } ->
      Refine.Ctors.for_all
        (fun s -> List.mem (String.lowercase_ascii s) [ "one"; "all" ]) ctors
    | Base.Json, Refine.Enum { ctors; _ } ->
      Refine.Ctors.for_all
        (fun s -> match Yojson.Safe.from_string s with
           | _ -> true
           | exception Yojson.Json_error _ -> false) ctors
    | _ -> true

  let leq a b =
    Base.leq a.base b.base
    && (if Refine.is_top a.refine then not (Refine.is_value_set b.refine)
        else Refine.leq a.refine b.refine)
    && valid_at b.base a.refine
end
