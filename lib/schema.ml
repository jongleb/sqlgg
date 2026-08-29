
open Printf
open ExtLib
open Prelude

module Type =
struct

  module Enum_kind = struct

    module Ctors =  struct
      include Set.Make(String)

      let pp fmt s =
        Format.fprintf fmt "{%s}"
          (String.concat "; " (elements s))
    end

    type t = Ctors.t [@@deriving eq, show{with_path=false}]

    let make ctors = Ctors.of_list ctors
  end

  type union = { ctors: Enum_kind.t; is_closed: bool } [@@deriving eq, show{with_path=false}]

  type decimal = { precision: int option; scale: int option } [@@deriving eq, show{with_path=false}]

  type kind =
    | Int
    | UInt64
    | Text
    | Blob
    | Float
    | Bool
    | Datetime
    | Decimal of decimal
    | Union of union
    | StringLiteral of string
    | FloatingLiteral of float
    | Json_path
    | One_or_all
    | Json
    | Any (* FIXME - Top and Bottom ? *)
    [@@deriving eq, show{with_path=false}]
    (* TODO NULL is currently typed as Any? which actually is a misnormer *)

    let show_kind = function
      | Union { ctors; _ } -> sprintf "Union (%s)" (String.concat "| " (Enum_kind.Ctors.elements ctors))
      | StringLiteral l -> sprintf "StringLiteral (%s)" l
      | FloatingLiteral f -> sprintf "FloatingLiteral (%g)" f
      | Decimal { precision = Some p; scale = Some s } -> sprintf "Decimal(%d,%d)" p s
      | Decimal { precision = Some p; scale = None } -> sprintf "Decimal(%d)" p
      | Decimal _ -> "Decimal"
      | k -> show_kind k

  type nullability =
  | Nullable (** can be NULL *)
  | Strict (** cannot be NULL *)
  | Depends (** unknown, to be determined *)
  [@@deriving eq, show{with_path=false}]

  type t = { t : kind; nullability : nullability; }[@@deriving eq, show{with_path=false}]

  let nullability nullability = fun t -> { t; nullability }
  let strict = nullability Strict
  let depends = nullability Depends
  let nullable = nullability Nullable
  let make_nullable { t; nullability=_ } = nullable t

  let make_strict { t; nullability=_ } = strict t

  let make_enum_kind ctors = Union { ctors = (Enum_kind.make ctors); is_closed = true }

  let is_nullable { nullability; _ } = nullability = Nullable

  let (=) : t -> t -> bool = equal

  let show { t; nullability; } = show_kind t ^ (match nullability with Nullable -> "?" | Depends -> "??" | Strict -> "")
  let _ = pp
  let pp pf t = Format.pp_print_string pf (show t)

  let type_name t = show_kind t.t

end

module Constraint =
struct
  module StringSet = struct
    include Set.Make(String)
    let show s = [%derive.show: string list] (elements s)
    let pp fmt s = Format.fprintf fmt "%s" (show s)
  end

  type conflict_algo = | Ignore | Replace | Abort | Fail | Rollback
    [@@deriving show{with_path=false}, ord, eq]

  type composite = | CompositePrimary of StringSet.t | CompositeUnique of StringSet.t
    [@@deriving show{with_path=false}, ord, eq]

  type t = | PrimaryKey | NotNull | Null | Unique | Autoincrement | OnConflict of conflict_algo | WithDefault | Composite of composite
    [@@deriving show{with_path=false}, ord, eq]

  let make_composite_primary cols = Composite (CompositePrimary (StringSet.of_list cols))
  let make_composite_unique cols = Composite (CompositeUnique (StringSet.of_list cols))
end

module Constraints = struct
  include Set.Make(Constraint)
  let show s = [%derive.show: Constraint.t list] (elements s)
  let pp fmt s = Format.fprintf fmt "%s" (show s)
end

module Meta = struct

  module StringMap = Map.Make(String)

  type t = string StringMap.t

  let of_list list = List.fold_left (fun map (k, v) -> StringMap.add k v map) StringMap.empty list

  let empty () = StringMap.empty
  let is_empty = StringMap.is_empty
  let find_opt map key = StringMap.find_opt key map

  let pp fmt t =
    Format.fprintf fmt "{%s}"
      (String.concat "; " (List.map (fun (k, v) -> sprintf "%s = %s" k v) (StringMap.bindings t)))

  let equal = StringMap.equal String.equal

  let merge_right a b = StringMap.union (fun _ _ v -> Some v) a b

  let inter a b = StringMap.filter (fun k v -> Option.map_default (String.equal v) false (find_opt b k)) a

  let common x y =
    match x, y with
    | None, m | m, None -> m
    | Some a, Some b -> Some (inter a b)

  let common_all = List.fold_left common None
  let of_option = Option.default (empty ())
  let declared m = if is_empty m then None else Some m
  let shared metas = of_option (common_all (List.map declared metas))
  let internal_keys = [ "non_nullifiable"; "json_null_kind"; "text_as_json" ]

  let of_domain m = List.fold_left (fun m k -> StringMap.remove k m) m internal_keys

  let get_is_non_nullifiable meta = String.equal (Option.default "false" (find_opt meta "non_nullifiable")) "true"
end

type attr = {name : string; domain : Type.t; extra : Constraints.t; meta: Meta.t; }
  [@@deriving eq, show {with_path=false}]

let unique_keys schema =
  let keys_of a =
    Constraints.fold (fun c acc ->
      match c with
      | Constraint.PrimaryKey | Unique -> Constraint.StringSet.singleton a.name :: acc
      | Composite (CompositePrimary s | CompositeUnique s) -> s :: acc
      | NotNull | Null | Autoincrement | OnConflict _ | WithDefault -> acc)
      a.extra []
  in
  List.concat_map keys_of schema |> List.sort_uniq Constraint.StringSet.compare

let make_attribute name kind extra ~meta =
  if Constraints.mem Null extra && Constraints.mem NotNull extra then fail "Column %s can be either NULL or NOT NULL, but not both" name;
  let domain = Type.{ t = Option.default Int kind; nullability = if List.exists (fun cstrt -> Constraints.mem cstrt extra) [NotNull; PrimaryKey]
    then Strict else Nullable } in
  {name;domain;extra;meta=Meta.of_list meta;}

let unnamed_attribute ?(meta = Meta.empty()) domain = {name="";domain;extra=Constraints.empty;meta;}

let make_attribute' ?(extra = Constraints.empty) ?(meta = []) name domain = { name; domain; extra; meta = Meta.of_list meta; }

module Schema =
struct
  type t = attr list
    [@@deriving show]

  exception Error of t * string

  module Source = struct
    module Attr = struct
      type 'a t = { attr: attr; sources: 'a list } [@@deriving show]

      let by_name name sattr = sattr.attr.name = name

      let map_attr f sattr = { sattr with attr = f sattr.attr }
    end

    type 'a t = 'a Attr.t list

    let to_schema list = List.map (fun sattr -> sattr.Attr.attr) list

    let of_schema ?(sources = []) list = List.map (fun attr -> { Attr.attr; sources }) list

    let find_by_name t name = List.find_all (Attr.by_name name) t

    let find t name =
      match find_by_name t name with
      | [x] -> x
      | [] -> raise (Error (to_schema t, "missing attribute : " ^ name))
      | _ -> raise (Error (to_schema t, "duplicate attribute : " ^ name))

    let mem_by_name t a =
      match find_by_name t a.Attr.attr.name with
      | [_] -> true
      | [] -> false
      | _ -> raise (Error (to_schema t, "duplicate attribute : " ^ a.attr.name))

    let sub_by_name l del = List.filter (fun x -> not (mem_by_name del x)) l
  end

  let raise_error t fmt = Printf.ksprintf (fun s -> raise (Error (t,s))) fmt

  (** FIXME attribute case sensitivity? *)
  let by_name name = function attr -> attr.name = name
  let find_by_name t name = List.find_all (by_name name) t

  let find t name =
    match find_by_name t name with
    | [x] -> x
    | [] -> raise (Error (t,"missing attribute : " ^ name))
    | _ -> raise (Error (t,"duplicate attribute : " ^ name))

  let make_unique = List.unique ~cmp:(fun a1 a2 -> a1.name = a2.name && a1.name <> "")
  let is_unique t = List.length (make_unique t) = List.length t
  let check_unique t = is_unique t || raise (Error (t,"duplicate attributes"))

  let project names t = List.map (find t) names

  let change_inplace t before after =
    ignore (find t before);
    List.map (fun attr ->
      match by_name before attr with
      | true -> after
      | false -> attr ) t

  let exists t name =
    match (find t name : attr) with
    | _ -> true
    | exception _ -> false

  let rename t oldname newname =
    if not (exists t oldname) then raise @@ Error (t, "no such column : " ^ oldname);
    if exists t newname then raise @@ Error (t, "column already exists : " ^ newname);
    List.map (fun attr -> if attr.name = oldname then { attr with name = newname } else attr) t

  let to_string v = v |> List.map (fun attr -> sprintf "%s %s" (Type.show attr.domain) attr.name) |>
    String.concat ", " |> sprintf "[%s]"
  let names t = t |> List.map (fun attr -> attr.name) |> String.concat "," |> sprintf "[%s]"

  module Join = struct

    type 'a condition = On of 'a | Default | Natural | Using of string list [@@deriving show]
    type typ = Left | Right | Full | Inner | Straight [@@deriving show]

    let cross t1 t2 = t1 @ t2

    let common_columns cond t1 t2 =
      let with_counterpart a = a, Source.find t2 a.Source.Attr.attr.name in
      match cond with
      | Natural -> t1 |> List.filter (Source.mem_by_name t2) |> List.map with_counterpart
      | Using l -> List.map (fun name -> with_counterpart (Source.find t1 name)) l
      | On _ | Default -> []

    (* TODO check that attribute types match (ignoring nullability)? *)
    let natural t1 t2 =
      let common = List.map fst (common_columns Natural t1 t2) in
      let t1only = Source.sub_by_name t1 common in
      begin match common with
      | _ :: _ -> ()
      | [] ->
        raise (Error (Source.to_schema t1, "no common attributes for natural join of " ^
          names (Source.to_schema t1) ^ " and " ^ names (Source.to_schema t2)))
      end;
      common @ t1only @ Source.sub_by_name t2 common

    let using l t1 t2 =
      let common = List.map fst (common_columns (Using l) t1 t2) in
      common @ Source.sub_by_name t1 common @ Source.sub_by_name t2 common

    let join typ cond a b =
      let nullable = List.map (fun data ->
        Source.Attr.{data with attr={data.attr with domain = Type.make_nullable data.attr.domain}}) in
      let action = match cond with Default | On _ -> cross | Natural -> natural | Using l -> using l in
      match typ with
      | Inner | Straight -> action a b
      | Left -> action a (nullable b)
      | Right -> action (nullable a) b
      | Full -> action (nullable a) (nullable b)

  end

  let cross_all l = List.fold_left Join.cross [] l

  let compound ~merge t1 t2 =
    let open Source in
    let open Attr in
    if List.compare_lengths t1 t2 <> 0 then
      raise (Error (to_schema t1, to_string (to_schema t1)
          ^ " differs in size to " ^ to_string (to_schema t2)));
    let show_name i a =
      match a.name with
      | "" -> sprintf "column %d (of %d)" (i+1) (List.length t1)
      | s -> s
    in
    List.combine t1 t2
    |> List.mapi begin fun i (a1,a2) ->
      match merge a1.attr.domain a2.attr.domain with
      | Some t ->
        let meta = Meta.shared [ a1.attr.meta; a2.attr.meta ] in
        Attr.map_attr (fun attr -> { attr with domain = t; meta }) a1
      | None -> raise (Error (to_schema t1, sprintf "Attributes do not match : %s of type %s and %s of type %s"
        (show_name i a1.attr) (Type.show a1.attr.domain)
        (show_name i a2.attr) (Type.show a2.attr.domain)))
    end

  let add t col pos =
    match find_by_name t col.name with
    | [] ->
      begin
      match pos with
      | `First -> col::t
      | `Default -> t @ [col]
      | `After name ->
        try
          let (i,_) = List.findi (fun _ attr -> by_name name attr) t in
          let (l1,l2) = List.split_nth (i+1) t in
          l1 @ (col :: l2)
        with
          Not_found -> raise (Error (t,"Can't insert column " ^ col.name ^ " after non-existing column " ^ name))
      end
    | _ -> raise (Error (t,"Already has column " ^ col.name))

  let drop t col =
    ignore (find t col);
    List.remove_if (by_name col) t

  let change t oldcol col pos =
    match pos with
    | `Default -> change_inplace t oldcol col
    | `First | `After _ -> add (drop t oldcol) col pos

  let to_string = show
  let print x = prerr_endline (to_string x)

end
