(** How many rows a query can return, and which tables an expression mentions.

    Takes the column resolver it needs rather than a whole environment: the
    only question it asks about a name is which attribute it denotes. *)

open ExtLib
open Prelude
open Sql

(* what shape a query has: does it group, does it window *)
let list_same l =
  match l with
  | [] -> None
  | x::xs -> if List.for_all (fun y -> x = y) xs then Some x else None

let rec is_grouping = function
| Choices (p,l) ->
  begin match list_same @@ List.map (fun (_,expr) -> Option.map_default is_grouping false expr) l with
  | None -> failed ~at:p.pos "inconsistent grouping in choice branches"
  | Some v -> v
  end
| Fun { fn_name; parameters; _ } ->
  (* grouping function of zero or single parameter or function on grouping result *)
  (Hmx_sig.is_agg fn_name (List.length parameters) && List.length parameters <= 1) || List.exists is_grouping parameters
| e -> List.exists is_grouping (sub_exprs e)

let is_windowing =
  expr_exists (function Sql.Fun { over; _ } -> Option.is_some over | _ -> false)

let exists_grouping columns =
  List.exists (function
    | { value = Expr ({ value; _ }, _); _ } -> is_grouping value
    | { value = (All | AllOf _); _ } -> false
  ) columns

let exists_windowing columns =
  List.exists (function
    | { value = Expr ({ value; _ }, _); _ } -> is_windowing value
    | { value = (All | AllOf _); _ } -> false
  ) columns  

(* all columns from tables, without duplicates *)
(* FIXME check type of duplicates *)
module Table_refs : sig
  type t
  val of_expr : resolve:(Sql.col_name -> Sql.table_name Sql.Schema.Source.Attr.t option) -> Sql.expr -> t
  val of_exprs : resolve:(Sql.col_name -> Sql.table_name Sql.Schema.Source.Attr.t option) -> Sql.expr list -> t
  val may_refer : Sql.join_source -> t -> bool
end = struct
  module Names = Set.Make(String)

  type t = Names.t option

  let anything = None

  let empty = Some Names.empty

  let union a b =
    match a, b with
    | Some x, Some y -> Some (Names.union x y)
    | None, _ | _, None -> anything

  let of_attr attr =
    Names.of_list (List.map (fun (s : table_name) -> s.tn) attr.Schema.Source.Attr.sources)

  let rec of_expr ~resolve = function
    | Sql.Column c -> Option.map of_attr (resolve c.collated)
    | SelectExpr _ -> anything
    | e -> of_exprs ~resolve (sub_exprs e)

  and of_exprs ~resolve l = List.fold_left (fun acc e -> union acc (of_expr ~resolve e)) empty l

  let may_refer source =
    Option.map_default (Names.mem (Sql.join_source_name source).tn) true
end

(* takes what it needs rather than a whole environment *)
let matches_at_most_one_row ~resolve ~schema table expr =
  let module SS = Constraint.StringSet in
  let table_name = Sql.join_source_name table in
  let belongs (a : table_name Schema.Source.Attr.t) =
    List.exists (fun (s : table_name) -> s.tn = table_name.tn) a.sources
  in
  let table_attrs =
    List.filter_map
      (fun a -> if belongs a then Some a.Schema.Source.Attr.attr else None)
      schema
  in
  let keys = unique_keys table_attrs in
  let independent_of_table e =
    not (Table_refs.may_refer table (Table_refs.of_expr ~resolve e))
  in
  let bound1 a b =
    match (match a with Sql.Column col -> resolve col.collated | _ -> None) with
    | Some attr when belongs attr && independent_of_table b ->
      Some attr.Schema.Source.Attr.attr.name
    | _ -> None
  in
  let bound_part a b = match bound1 a b with Some _ as r -> r | None -> bound1 b a in
  let rec bound_parts = function
    | Sql.Fun { fn_name = "and"; parameters; _ } ->
      List.fold_left (fun acc e -> SS.union acc (bound_parts e)) SS.empty parameters
    | Fun { fn_name = "eq"; parameters = [a; b]; _ } ->
      b |> bound_part a |> Option.map_default SS.singleton SS.empty
    | Choices (_, branches) ->
      let of_branch (_, e) = Option.map_default bound_parts SS.empty e in
      (match branches with
       | [] -> SS.empty
       | hd :: tl -> List.fold_left (fun acc b -> SS.inter acc (of_branch b)) (of_branch hd) tl)
    | Fun _ | Value _ | Param _ | Inparam _ | Column _ | Of_values _ | SelectExpr _
    | InChoice _ | OptionActions _ | InTupleList _ | Case _ -> SS.empty
  in
  let bound = bound_parts expr in
  List.exists (fun k -> SS.subset k bound) keys

