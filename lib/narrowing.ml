open ExtLib
open Sql

module Qualified_attr = struct
  module T = struct
    type t = { sources : table_name list; name : string } [@@deriving eq, ord]
  end
  include T

  let of_attr (a : table_name Schema.Source.Attr.t) = { sources = a.sources; name = a.attr.name }

  let named = function { name = ""; _ } -> None | key -> Some key

  module Map = Map.Make(T)
  module Set = Set.Make(T)
end

module Attr_refinement = struct
  type t = {
    not_null : Qualified_attr.Set.t;
    meta : Meta.t Qualified_attr.Map.t;
  }

  let empty = { not_null = Qualified_attr.Set.empty; meta = Qualified_attr.Map.empty }

  let add a b = {
    not_null = Qualified_attr.Set.union a.not_null b.not_null;
    meta = Qualified_attr.Map.union (fun _ x y -> Some (Meta.merge_right x y)) a.meta b.meta;
  }

  let keep_all = List.fold_left add empty

  let keep_shared = function
    | [] -> empty
    | x :: l ->
      List.fold_left (fun a b -> {
        not_null = Qualified_attr.Set.inter a.not_null b.not_null;
        meta = Qualified_attr.Map.merge (fun _ x y ->
          match x, y with
          | Some x, Some y -> Meta.declared (Meta.inter x y)
          | Some _, None | None, Some _ | None, None -> None) a.meta b.meta;
      }) x l

  let not_null attr = { empty with not_null = Qualified_attr.Set.singleton attr }

  let restrict_not_null keep t = { t with not_null = Qualified_attr.Set.filter keep t.not_null }

  let with_not_null_of ~from t = { t with not_null = from.not_null }


  let inherit_meta ~constrains (col : table_name Schema.Source.Attr.t) ~(referenced : table_name Schema.Source.Attr.t) =
    let inherited = Meta.of_domain referenced.attr.meta in
    let carries =
      match col.attr.domain.t, referenced.attr.domain.t with
      | Union a, Union b -> Type.Enum_kind.Ctors.subset a.ctors b.ctors
      | a, b -> constrains col && Type.equal_kind a b
    in
    match Meta.is_empty inherited, carries with
    | false, true -> { empty with meta = Qualified_attr.Map.singleton (Qualified_attr.of_attr col) inherited }
    | _ -> empty

  let refine_nullability t a =
    if Qualified_attr.Set.mem (Qualified_attr.of_attr a) t.not_null
    then Schema.Source.Attr.map_attr (fun attr -> { attr with domain = Type.make_strict attr.domain }) a
    else a

  let refine_meta t a =
    let uncovered = Option.default (Meta.empty ()) (Qualified_attr.Map.find_opt (Qualified_attr.of_attr a) t.meta) in
    Schema.Source.Attr.map_attr (fun attr -> { attr with meta = Meta.merge_right uncovered attr.meta }) a

  let apply t a = refine_nullability t (refine_meta t a)
end

(** Columns that a satisfied condition proves non-NULL, in three-valued logic.
    N(e) is [req e true], N(NOT e) is [req e false].

                     TRUE            FALSE
       a AND b      N(a) u N(b)     N(a) n N(b)
       a OR  b      N(a) n N(b)     N(a) u N(b)
*)
let narrow_columns ~resolve ~constrains e =
  let open Attr_refinement in
  let strict col =
    match resolve col with
    | Some a when constrains a -> not_null (Qualified_attr.of_attr a)
    | Some _ | None -> empty
  in
  let borrowed = function
    | Sql.Fun { fn_name = ("eq" | "not_distinct"); parameters = [Column a; Column b]; _ } ->
      begin match resolve a.collated, resolve b.collated with
      | Some a, Some b ->
        let borrow = inherit_meta ~constrains in
        add (borrow a ~referenced:b) (borrow b ~referenced:a)
      | None, _ | _, None -> empty
      end
    | _ -> empty
  in
  let rec nn = function
    | Sql.Column col -> strict col.collated
    (* a column is non-null here only if it is non-null in every branch *)
    | Fun { fn_name = ("coalesce" | "ifnull"); parameters; _ } -> keep_shared (List.map nn parameters)
    | Fun { fn_name; parameters; _ } ->
      let proven =
        match Hmx_sig.find fn_name (List.length parameters) with
        | Some { proves_not_null = `All; _ } -> parameters
        | Some { proves_not_null = `First; _ } -> List.take 1 parameters
        | Some { proves_not_null = `None; _ } | None -> []
      in
      keep_all (List.map nn proven)
    | Case c -> paths ~result:nn c
    | Value _ | Param _ | Inparam _ | Choices _ | InChoice _ | InTupleList _
    | SelectExpr _ | OptionActions _ | Of_values _ -> empty

  and req e tv =
    let same e = req e tv in
    let required =
      match (e : Sql.expr) with
      | Fun { fn_name = ("and" | "or") as op; parameters; _ } ->
        let combine = if Bool.equal tv (String.equal op "and") then keep_all else keep_shared in
        combine (List.map same parameters)
      | Fun { fn_name = "xor"; parameters; _ } ->
        keep_all (List.map (fun e -> keep_shared [req e true; req e false]) parameters)
      | Fun { fn_name = ("not" | "excl"); parameters = [e]; _ } -> req e (not tv)
      | Fun { fn_name = ("is_null" | "is_not_null") as op; parameters = [e]; _ } ->
        if Bool.equal tv (String.equal op "is_not_null") then nn e else empty
      | Fun { fn_name = "any_cmp"; parameters = x :: _; _ } when tv -> nn x
      | Fun { fn_name = ("any_cmp" | "all_cmp"); _ } -> empty
      | Case c -> paths ~result:same c
      | Choices (_, alternatives) ->
        keep_shared (List.map (fun (_, e) -> Option.map_default same empty e) alternatives)
      | InChoice _ | OptionActions _ -> empty
      | e -> nn e
    in
    add (if tv then borrowed e else empty) required

  and paths ~result { Sql.case; branches; else_ } =
    let guard { Sql.when_; _ } =
      Option.map_default (fun scrutinee -> add (nn scrutinee) (nn when_)) (req when_ true) case
    in
    let taken = List.map (fun b -> add (guard b) (result b.Sql.then_)) branches in
    keep_shared (taken @ Option.map_default (fun e -> [ result e ]) [] else_)
  in
  req e true

