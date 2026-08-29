
open Hmx_lattice

let fail fmt = conflict fmt

type column = { name : string; domain : Sql.Type.t; meta : Sql.Meta.t }

type env = {
  column : Sql.col_name -> column;

  grouping : bool;

  allow_aggregates : bool;

  subquery : Sql.select_full -> [ `AsValue | `Exists ] -> Sql.Type.t * Sql.var list * Sql.Meta.t;
  of_values : string -> Sql.Type.t;
}

let column_of_attr (a : Sql.attr) = { name = a.name; domain = a.domain; meta = a.meta }

let apply_json_meta (c : column) : Sql.Type.t =
  let json_null_kind = Sql.Meta.find_opt c.meta "json_null_kind" in
  let text_as_json = Sql.Meta.find_opt c.meta "text_as_json" in
  let is k = Sql.Type.equal_kind c.domain.t k in

  let null () =
    match json_null_kind, c.domain.nullability with
    | Some "false", Strict -> c.domain
    | _ -> Sql.Type.make_nullable c.domain
  in
  match json_null_kind, text_as_json with
  | None, None -> c.domain
  | _, _ when is Json -> null ()
  | _, Some "true" when is Text -> null ()
  | _, Some _ -> fail "column %s has text_as_json meta, but its type is not Text" c.name
  | Some _, None -> fail "column %s has json_null_kind meta, but its type is not Json or Text" c.name

let rec choice_id (e : Sql.expr) =
  match e with
  | Param (p, _) | Inparam (p, _) -> Some p.id
  | Choices (id, _) | InChoice (id, _, _) -> Some id
  | InTupleList { value = { param_id; _ }; _ } -> Some param_id
  | OptionActions _ -> None
  | Value _ | Fun _ | SelectExpr _ | Column _ | Case _ | Of_values _ ->
    List.find_map choice_id (Sql.sub_exprs e)
