(** Dropping joins whose table contributes nothing to the result.

  A rewrite over the resolved FROM clause: pure structure and cardinality, no
  typing. *)

open ExtLib
open Prelude
open Sql


module Id_set = Set.Make(Int)
module Id_map = Map.Make(Int)
module Table_map = Map.Make(String)

type candidate = {
  table : Sql.join_source;
  join : From.join;
}

let join_id c = fst c.join.pos

let eliminate ~resolve ~schema ~from ~columns ~where ~group ~having ~order final_schema from_params =
  let unchanged = final_schema, from_params in
  let joins = Option.map_default (fun f -> f.From.joins) [] from in
  let eliminable ({ From.src; kind; cond; pos = _ } as join) =
    let has_params = expr_exists (function
      | Sql.Param _ | Inparam _ | InTupleList _ | Choices _ | InChoice _
      | OptionActions _ | SelectExpr _ -> true
      | Value _ | Column _ | Of_values _ | Fun _ | Case _ -> false)
    in
    match kind, cond, src.rsrc_physical_table with
    | Schema.Join.Left, Schema.Join.On e, Some table
      when not (has_params e) && Cardinality.matches_at_most_one_row ~resolve ~schema table e ->
      Some { table; join }
    | _ -> None
  in
  let is_implicit j = match j.From.cond with
    | Schema.Join.Natural | Using _ -> true
    | On _ | Default -> false
  in
  let rec after_last_implicit l =
    match List.dropwhile (not $ is_implicit) l with
    | [] -> l
    | _ :: rest -> after_last_implicit rest
  in
  let candidates =
    joins
    |> after_last_implicit
    |> List.filter_map eliminable
    |> List.fold_left (fun m c -> Id_map.add (join_id c) c m) Id_map.empty
  in
  if Id_map.is_empty candidates then unchanged else
  let outside_select_list = option_list where @ group @ option_list having @ List.map fst order in
  let query_exprs =
    List.filter_map (fun c -> match c.Sql.value with
      | All | AllOf _ -> None
      | Expr ({ value = e; _ }, _) -> Some e)
      columns
    @ outside_select_list
  in
  if List.exists Cardinality.is_windowing query_exprs then unchanged else
  let keys_where p m = Id_map.fold (fun k v acc -> if p k v then Id_set.add k acc else acc) m Id_set.empty in
  let used_elsewhere =
    let static_select_list =
      List.concat_map (fun c -> match c.Sql.value with
        | All | AllOf _ -> []
        | Expr ({ value = Choices (_, choices); _ }, _) ->
          List.filter_map (function
            | (_, Some (Sql.Column _)) | (_, None) -> None
            | (_, Some e) -> Some e) choices
        | Expr ({ value = Column _; _ }, _) -> []
        | Expr ({ value = e; _ }, _) -> [e])
        columns
    in
    Cardinality.Table_refs.of_exprs ~resolve (outside_select_list @ static_select_list)
  in
  let condition_refs =
    List.fold_left (fun m { From.cond; pos; _ } ->
      match cond with
      | Schema.Join.On e ->
        let refs = Cardinality.Table_refs.of_expr ~resolve e in
        let j = fst pos in
        let referenced =
          candidates
          |> keys_where (fun _ c -> Cardinality.Table_refs.may_refer c.table refs)
          |> Id_set.remove j
        in
        Id_map.add j referenced m
      | Default | Natural | Using _ -> m)
      Id_map.empty joins
  in
  let condition_refs_of j = condition_refs |> Id_map.find_opt j |> Option.default Id_set.empty in
  let saturate refs set =
    let rec go s =
      let expanded = Id_set.fold (fun j -> Id_set.union (refs j)) s s in
      if Id_set.equal expanded s then s else go expanded
    in
    go set
  in
  let redundant_ids =
    let unreferenced =
      keys_where (fun _ c -> not (Cardinality.Table_refs.may_refer c.table used_elsewhere)) candidates
    in
    let retained = Id_set.diff (keys_where (fun _ _ -> true) condition_refs) unreferenced in
    Id_set.diff unreferenced (saturate condition_refs_of retained)
  in
  let direct i = Id_set.inter (condition_refs_of i) redundant_ids in
  let by_table =
    Id_set.fold (fun j m ->
      let tn = (Sql.join_source_name (Id_map.find j candidates).table).tn in
      Table_map.update tn (fun old -> Some (Id_set.add j (Option.default Id_set.empty old))) m)
      redundant_ids Table_map.empty
    |> Table_map.map (saturate direct)
  in
  let join_of_column a =
    List.find_map (fun s -> Table_map.find_opt s.tn by_table) a.Schema.Source.Attr.sources
  in
  let annotate_column needed field =
    match join_of_column field.Sql.field_attr with
    | None -> needed, field
    | Some pre ->
      Id_set.union needed pre,
      { field with Sql.join_deps = Id_set.elements pre }
  in
  let pid =
    List.find_map (function
      | DynamicWithSources (p, _) -> Some p
      | AttrWithSources _ -> None) final_schema
  in
  let needed, final_schema =
    List.fold_left_map (fun needed -> function
      | DynamicWithSources (p, cols) ->
        let needed, cols = List.fold_left_map annotate_column needed cols in
        needed, DynamicWithSources (p, cols)
      | AttrWithSources _ as x -> needed, x)
      Id_set.empty final_schema
  in
  let holes = match pid with
  | None -> []
  | Some pid ->
    needed
    |> Id_set.elements
    |> List.map (fun j ->
      let c = Id_map.find j candidates in
      Sql.DynamicSelectJoin { pid; pos = c.join.pos; source = c.table })
  in
  let by_position a b = Int.compare (Sql.var_pos a) (Sql.var_pos b) in
  final_schema, List.merge by_position from_params (List.sort ~cmp:by_position holes)
