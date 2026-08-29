
(** SQL syntax and RA *)

open ExtLib
open Prelude
open Sql
open Narrowing

open Env

module Config = Env.Config

let rec resolve_env env = {
  Resolve.column = (fun col ->
    Resolve.column_of_attr (Attr_refinement.apply env.attr_refinement (resolve_column ~env col)).attr);
  grouping = env.query_has_grouping;
  allow_aggregates = env.allow_aggregates;
  of_values = (fun col ->
    match List.assoc_opt col env.of_values_types with
    | Some t -> t
    | None -> fail "VALUES(col) as an expression is only acceptable in ON DUPLICATE KEY UPDATE context");
  subquery = (fun select usage -> subquery_result ~env select usage);
}

and subquery_result ~env select usage =

  let inner = { env with scope = Subquery; allow_aggregates = true; insert_targets = None } in
  let (schema, p, _) = eval_select_full inner select in
  let schema = static_cols "nested select cannot have dynamic attributes" schema in
  let schema' = Schema.Source.to_schema schema in
  let meta = match schema with [ a ] -> a.attr.meta | _ -> Meta.empty () in
  match schema, usage with
  | [ { attr = { domain; _ }; _ } ], `AsValue ->
    let rec with_count = function
      | Case { case = _; branches; else_ } ->
        let then_exprs = List.map (fun b -> b.Sql.then_) branches in
        List.find_map with_count (then_exprs @ option_list else_)
      | Fun { fn_name = "count"; over = None; _ }
      | SelectExpr (_, _) -> Some domain
      | Fun { parameters; over = None; _ } -> List.find_map with_count parameters
      | Choices (_, chs) ->
        List.fold_left (fun acc (_, e) ->
          match acc with None -> None | Some _ -> Stdlib.Option.bind e with_count)
          (Some domain) chs
      | OptionActions { choice; _ } -> with_count choice
      | Fun { over = Some _; _ }
      | Value _ | Param _ | Inparam _ | InChoice _
      | Column _ | InTupleList _ | Of_values _ -> None
    in
    let default_null = Type.make_nullable domain in
    let typ =
      match select.select_complete.select with
      | ({ having = Some _; _ }, _) -> Type.nullable domain.t
      | ({ columns = [ { value = Expr ({ value = c; _ }, _); _ } ]; _ }, _) ->
        c |> with_count |> Option.default default_null
      | ({ columns = [ _ ]; _ }, _) -> default_null
      | _ -> raise (Schema.Error (schema', "nested sub-select used as an expression returns more than one column"))
    in
    typ, p, meta
  | _, `AsValue ->
    raise (Schema.Error (schema', "only one column allowed for SELECT operator in this expression"))
  | _, `Exists -> Type.depends Any, p, meta

and resolve_types env expr =
  match Constrain.solve_expr env.session (resolve_env env) expr with
  | Error msg -> fail "%s" msg
  | Ok (ty, vars, meta) -> vars, ty, meta
  | exception Hmx_lattice.Conflict msg -> fail "%s" msg

and resolve_column_assignments ?(is_update=false) ~env l =
  let open Schema.Source in
  let open Attr in
  let all = all_columns (List.map (fun (a, b) -> Schema.Source.of_schema ~sources:[a] b) env.tables) in
  let env = { env with schema = all } in
  l |> List.map begin fun (col,expr) ->
    let resolved = resolve_column ~env col in
    (* non nullifiable: once a column value is set to non-NULL, it can never be updated back to NULL *)
    let resolved =
      if is_update && Meta.get_is_non_nullifiable resolved.attr.meta then
        Attr.map_attr (fun a ->
          { a with domain = Type.make_strict a.domain; extra = Constraints.add NotNull a.extra }) resolved
      else resolved in
    let attr = resolved.attr in
    (* autoincrement is special - nullable on insert, strict otherwise *)
    let column = if Constraints.mem Autoincrement attr.extra then Type.nullable attr.domain.t else attr.domain in
    if !Config.debug then eprintfn "column assignment %s type %s" col.cname (Type.show column);
    let with_default e = if not @@ Constraints.mem WithDefault attr.extra then fail "Column %s doesn't have default value" col.cname else e in
    let value =
      match expr with
      | RegularExpr e -> Some e
      | WithDefaultParam (e, pos) -> with_default @@ Some (OptionActions { choice = e; pos; kind = SetDefault })
      | AssignDefault -> with_default None
    in
    attr.meta, column, value
  end

and assign_types ?ctx env ~column = function
  | None -> [], column
  | Some e ->
    let lax = !Config.allow_write_notnull_null && Dialect.Semantic.is_non_strict_mode_is_exists () in
    match Constrain.solve_assign ?ctx ~lax env.session (resolve_env env) ~column e with
    | Error msg -> fail "%s" msg
    | Ok (ty, vars, _) -> vars, ty
    | exception Hmx_lattice.Conflict msg -> fail "%s" msg

and infer_schema env columns =
(*   let all = tables |> List.map snd |> List.flatten in *)
  let refine = Attr_refinement.apply env.attr_refinement in
  (match env.insert_targets with
   | Some targets when List.compare_lengths targets columns <> 0 -> failwith "Select cardinality doesn't match Insert"
   | Some _ when List.exists (fun c -> match c.value with All | AllOf _ -> true | Expr _ -> false) columns ->
     failwith "Asterisk not supported"
   | Some _ | None -> ());
  let resolve1 i = function
    | { value = All; _ } -> List.map (fun x -> AttrWithSources (refine x)) env.schema
    | { value = AllOf t; _ } -> List.map (fun x -> AttrWithSources (refine x)) (schema_of ~env t)
    | { value = Expr ({ value = expr; _ }, alias); _ } ->
      let target = Option.map (fun targets -> List.nth targets i) env.insert_targets in
      let apply_alias col =
        Option.map_default
          (fun n -> Schema.Source.Attr.map_attr (fun attr -> { attr with name = n }) col)
          col alias
      in
      let resolve_expr = function
        | Column c -> resolve_column ~env c.collated
        | e ->
          let domain, meta =
            match target with

            | Some a -> snd (assign_types ~ctx:a.meta env ~column:a.domain (Some e)), a.meta
            | None -> let _, domain, meta = resolve_types env e in domain, meta
          in
          { Schema.Source.Attr.attr = unnamed_attribute ~meta domain; sources = [] }
      in
      let col =
        match expr with
        | Choices (p, choices) when dynamic_allowed env ->
          let dynamic = choices |> List.filter_map (fun (choice_p, e_opt) ->
            Option.map (fun choice_e ->
              let field_attr =
                choice_e
                |> resolve_expr |> refine
                |> Schema.Source.Attr.map_attr (fun attr -> unnamed_attribute ~meta:attr.meta attr.domain)
                |> apply_alias
              in
              { Sql.field_id = choice_p; field_attr; join_deps = [] }) e_opt
          ) in
          DynamicWithSources (p, dynamic)
        | e -> AttrWithSources (e |> resolve_expr |> refine |> apply_alias)
      in
      [ col ]
  in
  List.concat (List.mapi resolve1 columns)

and get_params env e = let vars, _, _ = resolve_types env e in vars

(*
let _ =
  let e = Sub [Value Type.Text; Param (Next,None); Sub []; Param (Named "ds", Some Type.Int);] in
  e |> get_params |> to_string |> print_endline
*)

and get_params_of_columns env =
  let get i = function
  | { value = (All | AllOf _); _ } -> []

  | { value = Expr ({ value; _ }, _); _ } when env.insert_targets <> None ->
    let a = List.nth (Option.get env.insert_targets) i in
    fst (assign_types ~ctx:a.meta env ~column:a.domain (Some value))
  | { value = Expr ({ value = Choices (p, choices); _ }, _); _ } when dynamic_allowed env ->
    [DynamicSelect (p, List.map (fun ((n : param_id), e) ->
      match e with
      | Some (Column { collated = { cname; tname }; _ }) when n.pos = dummy_pos ->
        let sql = tname |> Option.map_default (fun t -> Printf.sprintf "%s.%s" (show_table_name t) cname) cname in
        Verbatim (Option.default cname n.value, sql)
      | _ ->
        Simple (n, Option.map (get_params env) e)
    ) choices)]
  | { value = Expr ({ value; _ }, _); _ } -> get_params env value
  in
  fun columns -> List.concat (List.mapi get columns)

and get_params_opt env = function
  | Some x -> get_params env x
  | None -> []

and get_params_l env l = List.concat_map (get_params env) l

and do_join (env,params) { From.src; kind; cond; _ } =
  let joined = Qualified_attr.Set.of_list (List.map Qualified_attr.of_attr src.From.rsrc_schema) in
  let is_joined key = Qualified_attr.Set.mem key joined in
  let is_accumulated key = not (is_joined key) in
  let both = const true in
  let neither = const false in
  let filtered, padded =
    match kind with
    | Inner | Straight -> both, neither
    | Left -> is_joined, is_joined
    | Right -> is_accumulated, is_accumulated
    | Full -> neither, both
  in
  let constrains col = filtered (Qualified_attr.of_attr col) in
  let survives_padding = Attr_refinement.restrict_not_null (fun key -> not (padded key)) in
  let common_columns = match cond with
    | Natural | Using _ -> Schema.Join.common_columns cond env.schema src.From.rsrc_schema
    | On _ | Default -> []
  in
  let schema = Schema.Join.join kind cond env.schema src.From.rsrc_schema in
  let inherited =
    List.map (fun (col, referenced) ->
      Attr_refinement.inherit_meta ~constrains col ~referenced) common_columns
  in
  let env = { env with schema;
    attr_refinement = Attr_refinement.keep_all (survives_padding env.attr_refinement :: inherited) } in
  let params = params @ src.From.rsrc_params in
  match cond with
  | Default | Natural | Using _ -> env, params
  | On e ->
    let env = { env with attr_refinement = Attr_refinement.add env.attr_refinement
      (survives_padding (narrow_columns ~resolve:(resolve_column_opt ~env) ~constrains e)) } in
    (* TODO should use final schema (same as tables)? *)
    env, params @ get_params env e

and join env { From.base; joins } =
  assert (env.schema = []);
  let all_tables = base.From.rsrc_tables @ List.concat_map (fun j -> j.From.src.From.rsrc_tables) joins in
  let env = { env with tables = env.tables @ all_tables; schema = base.From.rsrc_schema } in
  List.fold_left do_join (env, base.From.rsrc_params) joins

and params_of_assigns ?(is_update=false) env ss =
  List.concat_map (fun (ctx, column, e) -> fst (assign_types ~ctx env ~column e))
    (resolve_column_assignments ~is_update ~env ss)

and params_of_order order final_schema env =
  List.concat_map
    (fun (order, direction) ->
       let env = { env with schema = update_schema_with_aliases env.schema final_schema ;  } in
       let p1 = get_params_l env [ order ] in
       let p2 =
         match direction with
         | None | Some `Fixed -> []
         | Some (`Param p) -> [Choice (p,[Verbatim ("ASC","ASC");Verbatim ("DESC","DESC")])]
       in
       p1 @ p2)
    order

and eval_nested env nested =
  (* nested selects generate new fresh schema in scope, cannot refer to outer schema,
    but can refer to attributes of tables through `tables` *)
  let env = { env with schema = []; attr_refinement = Attr_refinement.empty } in
  (* FIXME resolved table schema depends on join (nullability with left), this is resolving too early *)
  match nested with
  | Some (t,l) ->

    let resolve = resolve_source { env with insert_targets = None } in
    let from = {
      From.base = resolve t;
      joins = List.map (fun loc ->
        let (x,jt,jc) = loc.value in
        { From.src = resolve x; kind = jt.value; cond = jc; pos = loc.pos }) l;
    } in
    let env, params = join env from in
    env, params, Some from
  | None -> env, [], None

and eval_select ~order env { columns; from; where; group; having; } =
  let is_passthrough = columns <> [] && List.for_all (fun c -> match c.value with All | AllOf _ -> true | Expr _ -> false) columns in
  let child_scope =
    match env.scope with
    | (Top_level | From_passthrough) when is_passthrough -> From_passthrough
    | Top_level | From_passthrough | Subquery -> Subquery
  in
  let from_env, p2, resolved_from = eval_nested { env with scope = child_scope } from in
  let env = { from_env with scope = env.scope } in
  let env = { env with query_has_grouping = List.length group > 0 } in
  let narrow = Option.map_default (narrow_columns ~resolve:(resolve_column_opt ~env) ~constrains:(const true)) Attr_refinement.empty in
  let narrow_having having =
    let is_grouping_key =
      let keys = Qualified_attr.Set.of_list @@
        List.filter_map (fun e -> Option.map Qualified_attr.of_attr (as_column ~env e)) group
      in
      fun key -> Qualified_attr.Set.mem key keys
    in
    Attr_refinement.restrict_not_null is_grouping_key (narrow having)
  in
  let outer = env.attr_refinement in
  let refined = Attr_refinement.keep_all [ outer; narrow where; narrow_having having ] in
  let where_env = { env with attr_refinement = Attr_refinement.with_not_null_of ~from:outer refined } in
  let env = { env with attr_refinement = refined } in
  let projection = make_dynamic_select ~env columns in
  let final_schema = infer_schema env projection in
  let final_schema =
    match child_scope with
    | From_passthrough -> final_schema @ From.dynamic_columns resolved_from
    | Top_level | Subquery -> final_schema
  in
  let final_schema' = List.concat_map (function
    | AttrWithSources attr -> [attr]
    | DynamicWithSources (_, l) -> List.map (fun f -> f.Sql.field_attr) l
  ) final_schema in
  (* use schema without aliases here *)
  let p1 = get_params_of_columns env projection in
  let env, p3 =
    let per_row env = { env with allow_aggregates = false } in
    let where_params env = get_params_opt (per_row env) where in
    (* Some dialects support aliasing *)
    if Dialect.Semantic.is_where_aliases_dialect () then
      let with_aliases env = { env with schema = make_unique (Schema.Join.cross env.schema final_schema') } in
      with_aliases env, where_params (with_aliases where_env)
    else
      let sourced env = { env with schema = List.filter (fun i -> i.Schema.Source.Attr.sources <> []) env.schema } in
      env, where_params (sourced where_env)
  in
  (* ORDER BY, HAVING, GROUP BY allow have column without explicit referring to source if it's specified in SELECT *)
  let env = { env with schema = update_schema_with_aliases env.schema final_schema' } in
  let cardinality =
    match from, where with
    | None, None ->
      `One
    | None, Some _ ->
      `Zero_one
    | Some _, _ when group = [] && Cardinality.exists_grouping projection && not (Cardinality.exists_windowing projection) ->
      `One
      (* TODO: analyse join types to determine if cardinality optimization can be done *)
    | Some ((`Table t, _), []), Some w when Cardinality.matches_at_most_one_row ~resolve:(resolve_column_opt ~env) ~schema:env.schema { Sql.table = t; alias = None } w ->
      `Zero_one
    | Some _, _ ->
      `Nat
  in
  let p4 = get_params_l { env with allow_aggregates = false } group in
  let p5 = get_params_opt env having in
  let final_schema, p2 =
    Table_elimination.eliminate ~resolve:(resolve_column_opt ~env) ~schema:env.schema ~from:resolved_from ~columns ~where ~group ~having ~order final_schema p2
  in
  (final_schema, p1 @ p2 @ p3 @ p4 @ p5, env, cardinality)

(** @return final schema, params and tables that can be referenced by outside scope *)
and resolve_source env (x, alias) =
  let resolve_schema_with_alias schema = begin match alias with
    | Some { table_name; column_aliases = Some col_schema } ->
      let schema = compound (Schema.Source.of_schema col_schema) schema in
      schema, [table_name, Schema.Source.to_schema schema]
    | Some { table_name; column_aliases = None } ->
      let schema = List.map (fun i -> { i with Schema.Source.Attr.sources = table_name :: i.Schema.Source.Attr.sources }) schema in
      schema, [table_name, Schema.Source.to_schema schema]
    | None -> schema, []
  end in
  match x with
  | `Select select ->
    let (s,p,_) = eval_select_full env select in
    let tbl_alias = Option.map (fun { table_name; _ } -> table_name) alias in
    let add_src i = { i with Schema.Source.Attr.sources = option_list tbl_alias @ i.Schema.Source.Attr.sources } in
    let s, dyn = List.partition_map (function
      | AttrWithSources a -> Left (add_src a)
      | DynamicWithSources (dp, cols) -> Right (DynamicWithSources (dp, List.map (fun f -> { f with Sql.field_attr = add_src f.Sql.field_attr }) cols))
    ) s in
    let s, tables = resolve_schema_with_alias s in
    { From.rsrc_schema = s; From.rsrc_params = p; From.rsrc_tables = tables; From.rsrc_dynamic = dyn; From.rsrc_physical_table = None }
  | `Nested from ->
    let (env,p,resolved_from) = eval_nested env (Some from) in
    let s = infer_schema env [dummy_loc All] in
    if alias <> None then failwith "No alias allowed on nested tables";
    let s = static_cols "Nested source cannot have dynamic columns" s in
    { From.rsrc_schema = s; From.rsrc_params = p; From.rsrc_tables = env.tables; From.rsrc_dynamic = From.dynamic_columns resolved_from; From.rsrc_physical_table = None }
  | `Table s ->
    let (name,s) = Tables.get_from (env.ctes @ Tables.all ()) s in
    let is_cte = List.exists (fun (n, _) -> n = name) env.ctes in
    let alias = Option.map (fun { table_name; _ } -> table_name) alias in
    let sources = (name :: option_list alias) in
    let s3 = List.map (fun attr -> { Schema.Source.Attr.attr; sources }) s  in
    { From.rsrc_schema = s3; From.rsrc_params = []; From.rsrc_tables = List.map (fun name -> name, s) sources; From.rsrc_dynamic = [];
      From.rsrc_physical_table = if is_cte then None else Some { Sql.table = name; alias } }
  | `ValueRows { row_constructor_list; row_order; row_limit; } ->
    (*
      The columns of the table output from VALUES have the implicitly
      named columns column_0, column_1, column_2, and so on
      https://dev.mysql.com/doc/refman/8.4/en/values.html
    *)
    let exprs_to_cols =
      List.mapi (fun idx e ->
        dummy_loc (Expr (dummy_loc e, Some (Printf.sprintf "column_%d" idx)))
      )
    in
    let dummy_select exprs = { columns = exprs_to_cols exprs; from = None; where = None; group = []; having = None } in
    let (s, p, _) = match row_constructor_list with
      | RowExprList [] -> failwith "Each row of a VALUES clause must have at least one column"
      | RowExprList (exprs :: xs) ->
        let unions = List.map (fun exprs -> `Union, dummy_select exprs ) xs in
        let select = dummy_select exprs in
        let select_complete = { select = select, unions; order=row_order; limit=row_limit; select_row_locking = None } in
        let (s, p, v) = eval_select_full env { select_complete; cte = None } in
        let s = static_cols "VALUES cannot have dynamic columns" s in
        (s, p, v)
      | RowParam { id; types; values_start_pos } ->
        Schema.Source.of_schema (List.map (fun t -> make_attribute' "" (Source_type.to_infer_type t)) types),
          [ TupleList (id, ValueRows { types = List.map Source_type.to_infer_type types; values_start_pos }) ], Stmt.Select `Nat
    in
    let s, tables = resolve_schema_with_alias s in
    { From.rsrc_schema = s; From.rsrc_params = p; From.rsrc_tables = tables; From.rsrc_dynamic = []; From.rsrc_physical_table = None }

and eval_select_full env { select_complete; cte } =
  let ctes, p1 = Option.map_default (eval_cte ~session:env.session) ([], []) cte in
  let env = { env with ctes = ctes @ env.ctes } in
  let (s1, p2, env, cardinality) = eval_select ~order:select_complete.order env (fst @@ select_complete.select) in
  eval_compound ~env:{ env with tables = env.tables; } (p1 @ p2, s1, cardinality, select_complete)

and eval_cte ~session { cte_items; is_recursive } =
  let open Schema.Source in
  List.fold_left begin fun (acc_ctes, acc_vars) cte ->
    let env = { (empty_env session) with ctes = acc_ctes; scope = Subquery } in
    let tbl_name = make_table_name cte.cte_name in
    let a1 = Schema.Source.of_schema in
    let s1, p1, _kind =
      if is_recursive then
      begin
        match cte.stmt with
        | CteInline ({ select = select, other; _ } as stmt_) ->
          let other = other |> List.map begin fun cmb ->
            match fst cmb with
            | #cte_supported_compound_op -> cmb
            | `Except | `Intersect ->
              fail "%s: Recursive table reference in EXCEPT or INTERSECT operand is not allowed in CTEs" cte.cte_name
          end
          in
          let stmt = { stmt_ with select = select, other } in
          let s1, p1, env, cardinality = eval_select ~order:[] env (fst stmt.select) in
          let s1' = static_cols "Recursive CTEs cannot have dynamic columns" s1 in
          (* UNIONed fields access by alias to itself cte *)
          let s2 = compound (Option.map_default a1 s1' cte.cols) s1' in
          let a2 = to_schema s2 in
          eval_compound ~env:{ env with ctes = (tbl_name, a2) :: env.ctes } (p1, s1, cardinality, stmt)
        | CteSharedQuery _ -> failwith "Recursive CTEs with shared query currently are not supported"
      end
      else (
        match cte.stmt with
        | CteInline stmt ->
          let s1, p1, env, cardinality = eval_select ~order:[] env (fst stmt.select) in
          eval_compound ~env:{ env with tables = env.tables } (p1, s1, cardinality, stmt)
        | CteSharedQuery shared_query_name ->
          let (_, stmt) = Shared_queries.get shared_query_name.value in
          let s1, p1, kind = eval_select_full env stmt in
          s1, [SharedVarsGroup (p1, shared_query_name)], kind
      )
    in
    let s1 = static_cols "Recursive CTEs cannot have dynamic columns" s1 in
    let s2 = compound (Option.map_default a1 s1 cte.cols) s1 in
    (tbl_name, to_schema s2) :: acc_ctes, acc_vars @ p1 end
  ([], []) cte_items

and eval_compound ~env result =
  let (p1, s1, cardinality, stmt) = result in
  let { select=(_select, other); order; limit; _; } = stmt in
  let other = List.map snd other in
  let (s2l, p2l) = List.split (List.map (fun (s,p,_,_) -> s,p) @@ List.map (eval_select ~order:[] env) other) in
  let cardinality = if other = [] then cardinality else `Nat in
  (* ignoring tables in compound statements - they cannot be used in ORDER BY *)
  let final_schema =
    if other = [] then s1
    else (
      (* TODO: next step is to support it for UNIONS (but if it's possible to control it) *)
      let unwrap = static_cols "Union/Except/Intersect doesn't support dynamic columns" in
      let s1' = unwrap s1 in
      let s2l' = List.map unwrap s2l in
      List.map (fun x -> AttrWithSources x) @@ List.fold_left compound s1' s2l'
    )
  in
  let p3 =
    let schema = List.concat_map (function
      | AttrWithSources attr -> [attr]
      | DynamicWithSources (_, a) -> List.map (fun f -> f.Sql.field_attr) a
    ) final_schema in
    params_of_order order schema env in
  let (p4,limit1) = match limit with Some (p,x) -> List.map (fun p ->
    Single (make_param ~id:p.id ~typ:(Source_type.to_infer_type p.typ), Meta.empty())) p, x | None -> [],false in
  (* Schema.check_unique schema; *)
  let cardinality =
    if limit1 && cardinality = `Nat then `Zero_one
    else cardinality in
  final_schema, ( p1 @ (List.flatten p2l) @ p3 @ p4 : var list), Stmt.Select cardinality

let update_tables ?(is_update=false) ~env sources ss w =
  let schema = Schema.cross_all @@ List.map (fun src -> src.From.rsrc_schema) sources in
  let p0 = List.flatten @@ List.map (fun src -> src.From.rsrc_params) sources in
  let tables = List.flatten @@ List.map (fun src -> src.From.rsrc_tables) sources in (* TODO assert equal duplicates if not unique *)
  let env = { env with tables; schema; } in
  let p1 = params_of_assigns ~is_update env ss in
  let p2 = get_params_opt env w in
  p0 @ p1 @ p2

let resolve_on_conflict_clause ~env tn' = Option.map_default (function
  | {value = On_conflict { action; attrs; }; _ } ->
    let names = List.map (fun attr -> attr.cname) attrs in
    let composite_primary_key = Constraint.make_composite_primary names in
    let composite_unique = Constraint.make_composite_unique names in
    List.iter (fun col ->
      let resolved = resolve_column ~env col in
      if (Constraints.disjoint (Constraints.of_list [
        Unique; PrimaryKey;
        composite_primary_key;
        composite_unique
      ]) resolved.attr.extra ) then
        fail "Schema Error: ON CONFLICT clause (%s) does not match the PRIMARY KEY or UNIQUE constraint for column: %s"
          (names |> String.concat ", ")
          (show_col_name col)
    ) attrs;
    begin match action with
    | Do_nothing -> []
    | Do_update values ->
        let ss = List.map (function
          (*
            The SET and WHERE clauses in ON CONFLICT DO UPDATE have access
            to the existing row using the table's name (or an alias),
            and to rows proposed for insertion using the special excluded table.
            From our perspective, it is the same as accessing the table into which we write.
          *)
         | col, RegularExpr (Column { collated = { cname ; tname = Some { tn = "excluded"; db }; }; collation }) ->
          col, RegularExpr(Column { collated = { cname; tname = Some { tn = tn'; db }; }; collation })
         | e -> e
        ) values in
        ss
    end
  | { value = On_duplicate { assignments; }; _ } -> assignments
) []

let rec eval ~session (stmt:Sql.stmt) =
  let empty_env = empty_env session in
  let open Stmt in
  let open Schema.Source in
  let open Attr in
  match stmt with
  | Create (name, Schema { schema; constraints; indexes }) ->
      Ddl.create name schema constraints indexes;
      ([],[],Create name)
  | Create (name, Select { value=select; _ }) ->
      let (schema,params,_) = eval_select_full empty_env select in
      let schema = static_cols "CREATE TABLE AS SELECT cannot have dynamic columns" schema in
      Tables.add (name, to_schema schema);
      ([],params,Create name)
  | Alter (name,actions) ->
      Ddl.alter name actions;
      ([],[],Alter [name])
  | Rename l ->
    Ddl.rename l;
    ([], [], Alter (List.map fst l)) (* to have sensible target for gen_xml *)
  | Drop name ->
      Ddl.drop name;
      ([],[],Drop name)
  | CreateIndex { ci_name; ci_table; ci_cols; ci_kind } ->
      Ddl.create_index ~name:ci_name ~table:ci_table ~cols:ci_cols ~kind:ci_kind;
      [],[],CreateIndex ci_name
  | Insert { target=table; action=`Values (names, values); on_conflict_clause; _ } ->
    let expect = values_or_all table names in
    let t = Tables.get_schema table in
    let schema = Schema.Source.of_schema ~sources:[table] t in
    let env = { empty_env with tables = [Tables.get table]; schema; } in
    begin match values with
    | None ->
      [], [], Insert(Some (Values, expect), table)
    | Some values ->
      let vl = List.map List.length values in
      let cl = List.length expect in
      if List.exists (fun n -> n <> cl) vl then
        fail "Expecting %u expressions in every VALUES tuple" cl;
      (* pair up columns with inserted values *)
      let assigns = values |> List.map (fun tuple ->
        List.combine
        (List.map (fun a -> {cname=a.name; tname=None}) expect)
        tuple
      ) in
      let resolved = List.concat_map (fun l ->
        let resolved = resolve_column_assignments ~env l in
        List.map2 (fun (ctx, column, e) (c, _) ->
          let (params, t) = assign_types ~ctx env ~column e in
          c, params, t
        ) resolved l
      ) assigns in
      (* a column seen in several VALUES rows is nullable if any row makes it so;
         VALUES(col) later reads this aggregated type *)
      let of_values_types = List.fold_left (fun acc (c, _, t) ->
        match List.assoc_opt c.cname acc with
        | None -> (c.cname, t) :: acc
        | Some t0 ->
          (c.cname, { t with Type.nullability =
            if Type.is_nullable t || Type.is_nullable t0 then Nullable else Strict })
          :: List.remove_assoc c.cname acc)
        [] resolved
      in
      let p1 = List.concat_map (fun (_c, p, _t) -> p) resolved in
      let conflict_assigns = resolve_on_conflict_clause ~env table.tn on_conflict_clause in
      let params2 = params_of_assigns ~is_update:true { env with of_values_types } conflict_assigns in
      [], p1 @ params2, Insert (None, table)
    end
  | Insert { target=table; action=`Param (names, param_id); on_conflict_clause; _ } ->
    let schema = Schema.Source.of_schema ~sources:[table] (Tables.get_schema table) in
    let env = { empty_env with tables = [Tables.get table]; schema; } in
    let conflict_assigns = resolve_on_conflict_clause ~env table.tn on_conflict_clause in
    let expect = values_or_all table names in
    let of_values_types = List.map (fun a -> a.attr.name, a.attr.domain) schema in
    let params2 = params_of_assigns ~is_update:true { env with of_values_types } conflict_assigns in
    let params = [ TupleList (param_id, Insertion expect) ] in
    [], params @ params2, Insert (None, table)
  | Insert { target=table; action=`Select (names, select); on_conflict_clause; _ } ->
    let expect = values_or_all table names in
    let env = { empty_env with tables = [Tables.get table];
      schema = Schema.Source.of_schema ~sources:[table] (Tables.get_schema table);
    } in
    let (schema,params,_) = eval_select_full { env with insert_targets = Some expect } select in
    let schema = static_cols "INSERT ... SELECT cannot have dynamic columns" schema in
    ignore (compound
      (Schema.Source.of_schema expect)
      (Schema.Source.of_schema (Schema.Source.to_schema schema))); (* test equal types once more (not really needed) *)
    let conflict_assigns = resolve_on_conflict_clause ~env table.tn on_conflict_clause in
    let of_values_types = List.map2 (fun a1 a2 -> a2.name, a1.attr.domain) schema expect in
    let params2 = params_of_assigns ~is_update:true { env with of_values_types } conflict_assigns in
    [], params @ params2, Insert (None,table)
  | Insert { target=table; action=`Set ss; on_conflict_clause; _ } ->
    let env = { empty_env with tables = [Tables.get table];
      schema = Schema.Source.of_schema ~sources:[table] (Tables.get_schema table);
    } in
    let (params,inferred) = match ss with
    | None -> [], Some (Assign, Tables.get_schema table)
    | Some ss -> params_of_assigns env ss, None
    in
    let conflict_assigns = resolve_on_conflict_clause ~env table.tn on_conflict_clause in
    let params2 = params_of_assigns ~is_update:true env conflict_assigns in
    [], params @ params2, Insert (inferred,table)
  | Delete (table, where) ->
    let t = Tables.get table in
    let p = get_params_opt { empty_env with tables = [ t ];
      schema = Schema.Source.of_schema ~sources:[fst t] (snd t) } where in
    [], p, Delete [table]
  | DeleteMulti (targets, tables, where) ->
    (* use dummy columns to verify targets match the provided tables  *)
    let select = ({ columns = [dummy_loc All]; from = Some tables; where; group = []; having = None }, []) in
    let select_complete = { select; order = []; limit = None; select_row_locking = None } in
    let _attrs, params, _ = eval_select_full empty_env {select_complete; cte=None } in
    [], params, Delete targets
  | Set (vars, stmt) ->
    let p =
      vars |> List.map (fun (_k,e) ->
        match e with
        | Column _ -> [] (* this is not column but some db-specific identifier *)
        | _ -> get_params empty_env e) |> List.concat
    in
    begin match stmt with
    | None -> [], p, Other
    | Some stmt -> let (schema,p2,kind) = eval ~session stmt in (schema, p @ p2, kind)
    end
  | Update (table,ss,w,o,lim) ->
    let f, s = Tables.get table in
    let env = empty_env in
    let r = Schema.Source.of_schema ~sources:[f] s in
    let params = update_tables ~is_update:true ~env [{ From.rsrc_schema = r; From.rsrc_params = []; From.rsrc_tables = [(f, s)]; From.rsrc_dynamic = [];
      From.rsrc_physical_table = Some { Sql.table = f; alias = None } }] ss w in
    let env = { env with schema = update_schema_with_aliases [] r } in
    let p3 = params_of_order o [] { env with tables = [(f, s)] } in
    let lim = List.map (fun p -> make_param ~id:p.id ~typ:(Source_type.to_infer_type p.typ)) lim in
    [], params @ p3 @ (List.map (fun p -> Single (p, Meta.empty())) lim), Update (Some table)
  | UpdateMulti (tables,ss,w,o,lim) ->
    let env = empty_env in
    let sources = List.map (fun src -> resolve_source { env with scope = Subquery } ((`Nested src), None)) tables in
    let tables = List.map (fun src -> src.From.rsrc_tables) sources |> List.flatten in
    let params = update_tables ~is_update:true ~env sources ss w in
    let p3 = params_of_order o [] { env with schema = Schema.cross_all @@ List.map (fun src -> src.From.rsrc_schema) sources; tables } in
    let lim = List.map (fun p -> make_param ~id:p.id ~typ:(Source_type.to_infer_type p.typ)) lim in
    [], params @ p3 @ (List.map (fun p -> Single (p, Meta.empty())) lim), Update None
  | Select select ->
    let (schema, a, b) = eval_select_full empty_env select in
    List.map drop_sources schema, a, b
  | CreateRoutine (name,ret,params) ->
    Ddl.create_routine name ret params;
    [], [], CreateRoutine name
  | CreateType (name, TypeEnum ctors) ->
     Ddl.create_type name ctors;
     ([], [], CreateType name)
  | DropType (name, if_exists) ->
     Ddl.drop_type ~if_exists name;
     ([], [], DropType name)

let is_alpha = function
| 'a'..'z' -> true
| 'A'..'Z' -> true
| _ -> false

let common_prefix = function
| [] -> 0
| x::_ as l ->
  let rec loop i =
    if String.length x <= i then i
    else
      if List.for_all (fun s -> i < String.length s && s.[i] = x.[i]) l then
        loop (i+1)
      else
        i
  in
  let i = loop 0 in
  (* do not allow empty names or starting not with alpha *)
  if List.exists (fun s -> i = String.length s || not (is_alpha s.[i])) l then 0 else i

(* fill inferred sql for VALUES or SET *)
let complete_sql kind sql =
  match kind with
  | Stmt.Insert (Some (kind,schema), _) ->
    let (pre,each,post) = match kind with
    | Values -> "(", (fun _ -> ""), ")"
    | Assign -> "", (fun name -> name ^" = "), ""
    in
    let module B = Buffer in
    let b = B.create 100 in
    B.add_string b sql;
    B.add_string b " ";
    B.add_string b pre;
    let params = ref [] in
    let first = common_prefix @@ List.map (fun attr -> attr.Sql.name) schema in
    schema |> List.iter (fun attr ->
      if !params <> [] then B.add_string b ",";
      let attr_ref_prefix = each attr.Sql.name in
      let attr_name = String.slice ~first attr.Sql.name in
      let attr_ref = "@" ^ attr_name in
      let pos_start = B.length b + String.length attr_ref_prefix in
      let pos_end = pos_start + String.length attr_ref in
      (* autoincrement is special - nullable on insert, strict otherwise *)
      let typ = if Constraints.mem Autoincrement attr.extra then Type.nullable attr.domain.t else attr.domain in
      let param = Single (make_param ~id:{value=Some attr_name; pos=(pos_start,pos_end)} ~typ, Meta.empty()) in
      B.add_string b attr_ref_prefix;
      B.add_string b attr_ref;
      tuck params param;
    );
    B.add_string b post;
    (B.contents b, List.rev !params)
  | _ -> (sql,[])

let eval_parsed sql ({ Parser.statement; dialect_features } : Parser.parse_result) =
  let session = Constrain.create () in
  let (schema,p1,kind) = eval ~session statement in
  let (sql,p2) = complete_sql kind sql in
  (sql, schema, Params.unify_params session (p1 @ p2), kind, dialect_features)

let parse sql =
  eval_parsed sql (Parser.parse_stmt sql)

let eval_select select_full =
  let session = Constrain.create () in
  let (schema, p1, kind) = eval ~session @@ Select select_full in
  (schema, Params.unify_params session p1, kind)
