(* For every analyzer case: pick "id" only (the join must disappear ONLY when it is
   safely droppable) and pick the joined column (the join must always be present). *)

let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

(* ---- basic ---- *)
module Basic = Basic.Sqlgg(Print_impl)

let () =
  let open Basic.Ok_col in
  run "basic/ok: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "basic/ok: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L (fun x -> x)))

let () =
  let open Basic.Nonuniq_col in
  run "basic/nonuniq: pick id -> join kept (non-unique key)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Basic.Ref_in_where_col in
  run "basic/ref_in_where: pick id -> join kept (WHERE reference)" (fun () -> ignore (List.select () id ~b:"x" (fun x -> x)))

(* ---- self joins ---- *)
module Self_join = Self_join.Sqlgg(Print_impl)

let () =
  let open Self_join.Bad_col in
  run "self_join/bad: pick id -> join kept (non-unique self key)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Self_join.Good_col in
  run "self_join/good: pick id -> join dropped (PK self key)" (fun () -> ignore (List.select () id (fun x -> x)));
  run "self_join/good: pick name -> join present" (fun () -> ignore (List.select () name (fun x -> x)))

(* ---- join kinds ---- *)
module Join_kinds = Join_kinds.Sqlgg(Print_impl)

let () =
  let open Join_kinds.Inner_join_col in
  run "join_kinds/inner: pick id -> join kept (INNER)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Join_using_col in
  run "join_kinds/using: pick id -> join kept (USING)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Join_natural_col in
  run "join_kinds/natural: pick id -> join kept (NATURAL)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Using_after_candidate_col in
  run "join_kinds/using_after: pick id -> candidate kept (later USING)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Join_kinds.Natural_after_candidate_col in
  run "join_kinds/natural_after: pick id -> candidate kept (later NATURAL)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

(* ---- ON shapes ---- *)
module On_shapes = On_shapes.Sqlgg(Print_impl)

let () =
  let open On_shapes.Param_in_on_col in
  run "on_shapes/param_in_on: pick id -> join kept (param in ON)" (fun () -> ignore (List.select () id ~b:"x" ~uid:1L (fun x -> x)))

let () =
  let open On_shapes.Extra_const_on_col in
  run "on_shapes/extra_const_on: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "on_shapes/extra_const_on: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L (fun x -> x)))

let () =
  let open On_shapes.On_inequality_col in
  run "on_shapes/inequality: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open On_shapes.No_alias_col in
  run "on_shapes/no_alias: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "on_shapes/no_alias: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L (fun x -> x)))

(* ---- key shapes ---- *)
module Key_shapes = Key_shapes.Sqlgg(Print_impl)

let () =
  let open Key_shapes.Unique_key_col in
  run "key_shapes/unique: pick id -> join dropped (UNIQUE key)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "key_shapes/unique: pick label -> join present" (fun () -> ignore (List.select () label ~uid:1L (fun x -> x)))

let () =
  let open Key_shapes.Composite_partial_col in
  run "key_shapes/composite_partial: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Key_shapes.Composite_full_col in
  run "key_shapes/composite_full: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "key_shapes/composite_full: pick title -> join present" (fun () -> ignore (List.select () title ~uid:1L (fun x -> x)))

(* ---- references outside the projection ---- *)
module Outside_refs = Outside_refs.Sqlgg(Print_impl)

let () =
  let open Outside_refs.Ref_in_group_col in
  run "outside_refs/group: pick id -> join kept (GROUP BY)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Ref_in_order_col in
  run "outside_refs/order: pick id -> join kept (ORDER BY)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Ref_in_having_col in
  run "outside_refs/having: pick id -> join kept (HAVING)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Complex_proj_col in
  run "outside_refs/complex_proj: pick id -> join kept (complex expr)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Outside_refs.Subq_in_where_col in
  run "outside_refs/subq_in_where: pick id -> join kept (subquery in WHERE)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Unqualified_where_col in
  run "outside_refs/unqualified: pick id -> join kept (unqualified ref)" (fun () -> ignore (List.select () id (fun x -> x)))

let () =
  let open Outside_refs.Join_unreferenced_col in
  run "outside_refs/unreferenced: pick id -> join rendered statically" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

(* ---- subquery sources ---- *)
module Subquery_sources = Subquery_sources.Sqlgg(Print_impl)

let () =
  let open Subquery_sources.Join_subq_source_col in
  run "subquery_sources/plain: pick id -> join kept (subquery source)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Subquery_sources.Subq_join_dup_col in
  run "subquery_sources/cross_dup: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Subquery_sources.Subq_union_dup_col in
  run "subquery_sources/union_dup: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open Subquery_sources.Subq_base_join_col in
  run "subquery_sources/subq_base: pick id -> table join dropped" (fun () -> ignore (List.select () id (fun x -> x)));
  run "subquery_sources/subq_base: pick bio -> table join present" (fun () -> ignore (List.select () bio (fun x -> x)))

(* ---- broken chain ---- *)
module Chain_bad = Chain_bad.Sqlgg(Print_impl)

let () =
  let open Chain_bad.Chain_bad_col in
  run "chain_bad: pick id -> both joins kept (child pins parent)" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))
