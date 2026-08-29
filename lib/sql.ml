(** *)

open Printf
open ExtLib
open Prelude

type pos = int * int [@@deriving show]

type 'a located  = { value : 'a; pos : pos } [@@deriving show, make]
type 'a collated = { collated: 'a; collation: string located option } [@@deriving show, make]

let dummy_pos : pos = (0, 0)
let dummy_loc value = { value; pos = dummy_pos }

(* Schemas and the algebra over them live in {!Schema}; these aliases keep the
   familiar Sql.Type and Sql.Schema spellings everywhere downstream. *)
module Type = Schema.Type
module Constraint = Schema.Constraint
module Constraints = Schema.Constraints
module Meta = Schema.Meta

type attr = Schema.attr = { name : string; domain : Type.t; extra : Constraints.t; meta : Meta.t }
let pp_attr = Schema.pp_attr
let show_attr = Schema.show_attr
let equal_attr = Schema.equal_attr

let unique_keys = Schema.unique_keys
let make_attribute = Schema.make_attribute
let unnamed_attribute = Schema.unnamed_attribute
let make_attribute' = Schema.make_attribute'

module Schema = Schema.Schema

type table_name = { db : string option; tn : string } [@@deriving eq, ord, show]
let show_table_name { db; tn } = match db with Some db -> sprintf "%s.%s" db tn | None -> tn
let make_table_name ?db tn = { db; tn }
type schema = Schema.t [@@deriving show]
type table = table_name * schema [@@deriving show]

type join_source = { table : table_name; alias : table_name option } [@@deriving show]
let join_source_name { table; alias } = Option.default table alias

let print_table out (name,schema) =
  IO.write_line out (show_table_name name);
  schema |> List.iter begin fun {name;domain;extra;_} ->
    IO.printf out "%10s %s %s\n" (Type.show domain) name (Constraints.show extra)
  end;
  IO.write_line out ""

(** optional name and start/end position in string *)
type param_id = string option located [@@deriving show]
type shared_query_ref_id = string located [@@deriving show]

type int_size = Tiny | Small | Medium | Big
  [@@deriving show {with_path=false}, eq]

type lob_size = Tiny | Medium | Long
  [@@deriving show {with_path=false}, eq]

type signedness = Signed | Unsigned
  [@@deriving show {with_path=false}, eq]

type float_precision = Single | Double
  [@@deriving show {with_path=false}, eq]

module Source_type = struct
  type text_flavor =
    | PlainText of lob_size option
    | Char of int option
    | Varchar of int option
    | Varchar2 of int option
    [@@deriving show, eq]

  type blob_flavor =
    | PlainBlob of lob_size option
    | Varbinary of int option
    [@@deriving show, eq]

  type kind = Infer of Type.kind
    | Int of { size : int_size option; sign : signedness; display_width : int option }
    | Float of float_precision
    | Blob of blob_flavor
    | Text of text_flavor
    [@@deriving show, eq]

  type t = { t : kind; nullability : Type.nullability; } [@@deriving eq, show{with_path=false}, make]

  let nullability nullability t = { t = Infer t; nullability }
  let strict = nullability Type.Strict
  let depends = nullability Type.Depends
  let nullable = nullability Type.Nullable

  let kind_to_type_kind = function
    | Infer ty -> ty
    | Int { size = Some Big; sign = Unsigned; _ } -> Type.UInt64
    | Int _ -> Type.Int
    | Float _ -> Type.Float
    | Blob _ -> Type.Blob
    | Text _ -> Type.Text

  let to_infer_type { t; nullability; } = { Type.t = kind_to_type_kind t; nullability }

end

type 't param = { id : param_id; typ : 't; } [@@deriving show, make]
type option_actions_kind = BoolChoices | SetDefault [@@deriving show]
type params = Type.t param list [@@deriving show]
type in_or_not_in = [`In | `NotIn] [@@deriving show]
type ctor =
| Simple of param_id * var list option
| Verbatim of string * string
and var =
| Single of Type.t param * Meta.t
| SingleIn of Type.t param * Meta.t
| ChoiceIn of { param: param_id; kind : in_or_not_in; vars: var list }
| Choice of param_id * ctor list
| DynamicSelect of param_id * ctor list
| DynamicSelectJoin of { pid : param_id; pos : pos; source : join_source }
| TupleList of param_id * tuple_list_kind
(* It differs from Choice that in this case we should generate sql "TRUE", it doesn't seem reusable *)
| OptionActionChoice of param_id * var list * (pos * pos) * option_actions_kind
| SharedVarsGroup of vars * shared_query_ref_id
and tuple_list_kind = 
  | Insertion of schema 
  | Where_in of ((Type.t * Meta.t) list * in_or_not_in) located 
  | ValueRows of { types: Type.t list; values_start_pos: int; }
[@@deriving show]
and vars = var list [@@deriving show]

let ctor_vars = function
  | Simple (_, vars) -> Option.default [] vars
  | Verbatim _ -> []

let sub_vars = function
  | Single _ | SingleIn _ | TupleList _ | DynamicSelectJoin _ -> []
  | ChoiceIn { vars; _ } -> vars
  | OptionActionChoice (_, vars, _, _) -> vars
  | SharedVarsGroup (vars, _) -> vars
  | Choice (_, ctors) | DynamicSelect (_, ctors) -> List.concat_map ctor_vars ctors

let map_sub_vars f =
  let map_ctor = function
    | Simple (n, vars) -> Simple (n, Option.map f vars)
    | Verbatim _ as c -> c
  in
  function
  | Single _ | SingleIn _ | TupleList _ | DynamicSelectJoin _ as v -> v
  | ChoiceIn t -> ChoiceIn { t with vars = f t.vars }
  | OptionActionChoice (p, vars, pos, kind) -> OptionActionChoice (p, f vars, pos, kind)
  | SharedVarsGroup (vars, id) -> SharedVarsGroup (f vars, id)
  | Choice (p, ctors) -> Choice (p, List.map map_ctor ctors)
  | DynamicSelect (p, ctors) -> DynamicSelect (p, List.map map_ctor ctors)

let var_pos = function
  | Single (p, _) | SingleIn (p, _) -> fst p.id.pos
  | Choice (id, _) | DynamicSelect (id, _) | TupleList (id, _)
  | OptionActionChoice (id, _, _, _) -> fst id.pos
  | ChoiceIn { param; _ } -> fst param.pos
  | SharedVarsGroup (_, id) -> fst id.pos
  | DynamicSelectJoin { pos = (j1, _); _ } -> j1

type alter_pos = [ `After of string | `Default | `First ] [@@deriving show {with_path=false}]

type direction = [ `Fixed | `Param of param_id ] [@@deriving show]

type cte_supported_compound_op = [ `Union | `Union_all ] [@@deriving show]

type compound_op = [ cte_supported_compound_op | `Except | `Intersect ] [@@deriving show]

type int_or_param = [`Const of int | `Limit of Source_type.t param]
type limit_t = [ `Limit | `Offset ]
type col_name = {
  cname : string; (** column name *)
  tname : table_name option;
} [@@deriving show]
type source_alias = { table_name : table_name; column_aliases : schema option } [@@deriving show]
type select_row_locking_kind = For_update | For_share [@@deriving show]
and limit = Source_type.t param list * bool
and nested = source * (source * Schema.Join.typ located * join_condition) located list [@@deriving show]
and source_kind = [ `Select of select_full | `Table of table_name | `Nested of nested | `ValueRows of row_values ]
and source = (source_kind * source_alias option) (* alias, position *)
and join_condition = expr Schema.Join.condition
and select = {
  columns : column list;
  from : nested option;
  where : expr option;
  group : expr list;
  having : expr option;
}
and cte_item = { cte_name: string; cols: schema option; stmt: cte_stmt; } [@@deriving show]
and cte_stmt = CteInline of select_complete | CteSharedQuery of shared_query_ref_id [@@deriving show]
and cte = { cte_items: cte_item list; is_recursive: bool; } [@@deriving show]
and select_complete = {
  select : select * (compound_op * select) list;
  order : order;
  limit : limit option;
  select_row_locking: select_row_locking_kind located option;
}
and select_full = { select_complete: select_complete; cte: cte option; }
and row_constructor_list = RowExprList of expr list list | RowParam of { id : param_id; types : Source_type.t list; values_start_pos: int; } 
and row_values = {
  row_constructor_list: row_constructor_list;
  row_order: order;
  row_limit: limit option;
}
and order = (expr * direction option) list
and agg_with_order_kind = 
    | Group_concat
    | Json_arrayagg
(* Almost every function is looked up by name and arity in the signature
   table; only these two carry something the name cannot. *)
and 't func =
  | Named
  | Cast of 't                                    (** the target type is not in the name *)
  | Agg_order of { with_order_kind : agg_with_order_kind; order : order }
      (** GROUP_CONCAT and JSON_ARRAYAGG take an ORDER BY of their own *)
  [@@deriving show]
and 'expr choices = (param_id * 'expr option) list
and 't fun_ = { fn_name: string; kind: 't func; parameters: expr list; over: over option } [@@deriving show]
and over = { frame_may_be_empty: bool } [@@deriving show]
and case_branch = { when_: expr; then_: expr }
and case = {  
  case: expr option;
  branches: case_branch list;
  else_: expr option;
} [@@deriving show]
and in_tuple_list = { exprs: expr list; param_id: param_id; kind_in_tuple_list: in_or_not_in; } [@@deriving show]
and expr =
  | Value of Type.t collated (** literal value *)
  | Param of Source_type.t param * Meta.t
  | Inparam of Source_type.t param * Meta.t
  | Choices of param_id * expr choices
  | InChoice of param_id * in_or_not_in * expr
  | Fun of Source_type.t fun_
  | SelectExpr of select_full * [ `AsValue | `Exists ]
  | Column of col_name collated
  | InTupleList of in_tuple_list located
   (* pos - full syntax pos from {, to }?, pos is only sql, that inside {}?
      to use it during the substitution and to not depend on the magic numbers there.
   *)
  | OptionActions of { choice: expr; pos: (pos * pos); kind: option_actions_kind }
  | Case of case
  | Of_values of string (** VALUES(col_name) *)
and column = column_kind located [@@deriving show {with_path=false}]
and column_kind =
  | All
  | AllOf of table_name
  | Expr of expr located * string option

let fn ?over fn_name kind parameters = Fun { fn_name; kind; parameters; over }

let over_has_a_row = function None -> false | Some o -> not o.frame_may_be_empty

(* where a frame boundary sits relative to the current row *)
type frame_bound = [ `Before | `Current | `After ]

let over_of_frame : (frame_bound * frame_bound) option -> over = function
  | None -> { frame_may_be_empty = false }
  | Some (`After, _) | Some (_, `Before) -> { frame_may_be_empty = true }
  | Some ((`Before | `Current), (`Current | `After)) -> { frame_may_be_empty = false }
let column ?collation collated = Column (make_collated ?collation ~collated ())

let map_kind_exprs f = function
  | Agg_order o -> Agg_order { o with order = List.map (fun (e, dir) -> f e, dir) o.order }
  | Named | Cast _ as kind -> kind

let sub_exprs = function
  | Value _ | Param _ | Inparam _ | Column _ | Of_values _ | SelectExpr _ -> []
  | Choices (_, l) -> List.filter_map snd l
  | InChoice (_, _, e) -> [e]
  | OptionActions { choice; _ } -> [choice]
  | Fun { kind = Agg_order { order; _ }; parameters; _ } -> parameters @ List.map fst order
  | Fun { parameters; _ } -> parameters
  | InTupleList { value = { exprs; _ }; _ } -> exprs
  | Case { case; branches; else_ } ->
    option_list case
    @ List.concat_map (fun (b : case_branch) -> [b.when_; b.then_]) branches
    @ option_list else_

let map_sub_exprs f = function
  | Value _ | Param _ | Inparam _ | Column _ | Of_values _ | SelectExpr _ as e -> e
  | Choices (n, l) -> Choices (n, List.map (fun (n, e) -> n, Option.map f e) l)
  | InChoice (n, k, e) -> InChoice (n, k, f e)
  | OptionActions ({ choice; _ } as o) -> OptionActions { o with choice = f choice }
  | Fun ({ kind; parameters; _ } as fn) ->
    Fun { fn with kind = map_kind_exprs f kind; parameters = List.map f parameters }
  | InTupleList ({ value = { exprs; _ } as tl; _ } as loc) ->
    InTupleList { loc with value = { tl with exprs = List.map f exprs } }
  | Case { case; branches; else_ } ->
    Case {
      case = Option.map f case;
      branches = List.map (fun (b : case_branch) -> { when_ = f b.when_; then_ = f b.then_ }) branches;
      else_ = Option.map f else_;
    }

let rec expr_exists p e = p e || List.exists (expr_exists p) (sub_exprs e)

let make_partition_by = List.iter (function
  | Value _ -> fail "ORDER BY or PARTITION BY uses legacy position indication which is not supported, use expression."
  | _ -> ())

type assignment_expr = 
  | RegularExpr of expr 
  | AssignDefault
  | WithDefaultParam of expr * (pos * pos)
  [@@deriving show {with_path=false}]

type assignments = (col_name * assignment_expr) list [@@deriving show]

type on_conflict = Do_update of assignments | Do_nothing [@@deriving show]

type conflict_clause = 
  | On_duplicate of { assignments: assignments; }
  | On_conflict of { action: on_conflict; attrs: col_name list; }
  [@@deriving show]

type insert_action_kind = Insert_into | Replace_into of pos [@@deriving show]

type insert_action =
{
  insert_action_kind: insert_action_kind;
  target : table_name;
  action : [ `Set of assignments option
           | `Values of (string list option * assignment_expr list list option) (* column names * list of value tuples *)
           | `Param of (string list option * param_id)
           | `Select of (string list option * select_full) ];
  on_conflict_clause : conflict_clause located option;
} [@@deriving show {with_path=false}]

type table_constraints = [ `Ignore | `Primary of string list | `Unique of string option * string list
  | `Foreign of string list * table_name * string list ] [@@deriving show {with_path=false}]

type index_kind  = 
  | Regular_idx
  | Fulltext
  | Spatial
  [@@deriving show {with_path=false}]

module Alter_action_attr = struct

  type default = { expr : expr located; sql : string option }
    [@@deriving show {with_path=false}]

  type constraint_ = Syntax_constraint of Constraint.t | Default of default
    [@@deriving show {with_path=false}]

  type t = {  
    name : string; 
    kind : Source_type.kind collated located option;
    extra : constraint_ located list;
    meta: (string * string) list; 
  }
  [@@deriving show {with_path=false}]

  let constraint_to_syntax_constraint = function
    | Syntax_constraint c -> c
    | Default _ -> WithDefault

  let default_sql (col : t) =
    List.find_map (fun (c : constraint_ located) ->
      match c.value with
      | Default { sql; _ } -> sql
      | Syntax_constraint _ -> None
    ) col.extra

  let to_attr (x: t): attr = make_attribute x.name 
    (Option.map (fun k -> Source_type.kind_to_type_kind k.value.collated) x.kind)
    (Constraints.of_list (List.map (fun c -> constraint_to_syntax_constraint c.value) x.extra))
    ~meta:x.meta

  (* All attributes were already checked for dialect and default expression when writing to Tables,
     we deliberately make the fields dummy to reconstruct
   *)
  let from_attr (attr: attr): t =
    let extra = attr.extra |> Constraints.elements |> List.map (fun c -> 
      let c = match c with
      | Constraint.WithDefault -> Default {
          expr = make_located ~pos:(0,0) ~value:(Value (make_collated ~collated:(Type.depends Any) ()));
          sql = None;
        }
      | x -> Syntax_constraint x
      in
      make_located ~pos:(0,0) ~value:c
    ) in
    let kind = Some (make_located ~pos:(0,0) ~value:(make_collated ~collated:(Source_type.Infer attr.domain.Type.t) ())) in
    let meta = Meta.StringMap.bindings attr.meta in
    { name = attr.name; kind; extra; meta }
end

type index_op_kind =
  | Plain_idx
  | Unique_idx
  | Fulltext_idx
  | Spatial_idx
  [@@deriving show {with_path=false}, eq]

type table_inline_index = {
  idx_kind : index_kind;
  idx_name : string option;
  idx_cols : string list;
  idx_unique : bool;
}
[@@deriving show {with_path=false}]

type add_index = { add_idx_name : string option; add_idx_kind : index_op_kind; add_idx_cols : string list }
  [@@deriving show {with_path=false}]

type create_index_def = {
  ci_name : string;
  ci_table : table_name;
  ci_cols : string collated list;
  ci_kind : index_op_kind;
}
[@@deriving show {with_path=false}]

type create_target_schema = { 
  schema: Alter_action_attr.t list; 
  constraints: table_constraints list; 
  indexes: table_inline_index located list; 
}
[@@deriving show]

type create_target = 
  | Schema of create_target_schema
  | Select of select_full located
[@@deriving show {with_path=false}]

type charset_name = Named of string | Binary | Ascii | Unicode
  [@@deriving show {with_path=false}]

type ttl_option =
  [ `TtlSet of string * int * string
  | `TtlEnable of string ] [@@deriving show {with_path=false}]

module Alter_column_pg = struct
  type t =
    | Set_type of Source_type.kind collated located
    | Set_not_null
    | Drop_not_null
    | Set_default
    | Drop_default
  [@@deriving show {with_path=false}]
end

type alter_action = [
    | `Add of Alter_action_attr.t * alter_pos
    | `RenameTable of table_name
    | `RenameColumn of string * string
    | `RenameIndex of string * string
    | `Drop of string
    | `Change of string * Alter_action_attr.t * alter_pos
    | `AddIndex of add_index
    | `DropIndex of string
    | `AddPrimaryKey of string list
    | `DropPrimaryKey
    | `AddConstraint of string option
    | `DropConstraint of string
    | `Default_or_convert_to of (charset_name * string located option)
    | `TtlOptions of ttl_option list * pos
    | `RemoveTtl of pos
    | `AlterColumnPG of string * Alter_column_pg.t located ] [@@deriving show {with_path=false}]

type create_type_target =
  | TypeEnum of string list
  [@@deriving show {with_path=false}]

type stmt =
  | Create of table_name * create_target
  | Drop of table_name
  | Alter of table_name * alter_action list
  | Rename of (table_name * table_name) list
  | CreateIndex of create_index_def
  | Insert of insert_action
  | Delete of table_name * expr option
  | DeleteMulti of table_name list * nested * expr option
  | Set of (string * expr) list * stmt option
  | Update of table_name * assignments * expr option * order * Source_type.t param list (* where, order, limit *)
  | UpdateMulti of nested list * assignments * expr option * order * Source_type.t param list (* where, order, limit *)
  | Select of select_full
  | CreateRoutine of table_name * Source_type.kind collated located option * (string * Source_type.kind collated located * expr option) list (* table_name represents possibly namespaced function name *)
  | CreateType of string * create_type_target
  | DropType of string * bool
  [@@deriving show {with_path=false}]

(*
open Schema

let test = [{name="a";domain=Type.Int}; {name="b";domain=Type.Int}; {name="c";domain=Type.Text};];;

let () = print test
let () = print (project ["b";"c";"b"] test)
let () = print (project ["b";"d"] test)
let () = print (rename test "a" "new_a")
*)

type 'attr dynamic_field = {
  field_id : param_id;
  field_attr : 'attr;
  join_deps : int list;
}
[@@deriving show]

type schema_column_with_sources =
  | AttrWithSources of table_name Schema.Source.Attr.t
  | DynamicWithSources of param_id * table_name Schema.Source.Attr.t dynamic_field list
  [@@deriving show]

type schema_column =
  | Attr of attr
  | Dynamic of param_id * attr dynamic_field list
  [@@deriving show]

let drop_sources : schema_column_with_sources -> schema_column = function
  | AttrWithSources { attr; _ } -> Attr attr
  | DynamicWithSources (p, l) ->
    Dynamic (p, List.map (fun { field_id; field_attr = { Schema.Source.Attr.attr; _ }; join_deps } ->
      { field_id; field_attr = attr; join_deps }) l)


