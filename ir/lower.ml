open Sqlgg

let table_name ({ Sql.db; tn } : Sql.table_name) : V1.table_name =
  { database = db; name = tn }

let rec operation_of_stmt = function
  | Sql.Select _ -> V1.Select Unknown_cardinality
  | Sql.Insert { target; _ } -> Insert (table_name target)
  | Sql.Create ({ value; _ }, _) -> Create_table (table_name value)
  | Sql.CreateIndex { ci_name; _ } -> Create_index ci_name
  | Sql.Update (target, _, _, _, _) -> Update (Some (table_name target))
  | Sql.UpdateMulti _ -> Update None
  | Sql.Delete (target, _) -> Delete [table_name target]
  | Sql.DeleteMulti (targets, _, _) -> Delete (List.map table_name targets)
  | Sql.Alter { alter_table; _ } -> Alter [table_name alter_table]
  | Sql.Drop target -> Drop_table (table_name target)
  | Sql.Rename renames ->
    Rename_tables
      (List.map
         (fun (source, target) -> table_name source, table_name target)
         renames)
  | Sql.Set (_, nested) -> Set (Option.map operation_of_stmt nested)
  | Sql.CreateRoutine (name, _, _) -> Create_routine (table_name name)
  | Sql.CreateType (name, _) -> Create_type name
  | Sql.DropType (name, _) -> Drop_type name
  | Sql.CreateExtension name -> Create_extension name
  | Sql.DropExtension names -> Drop_extension names

let span (start, stop) : V1.span = { start; stop }

let identifier ({ Sql.value = name; pos } : Sql.param_id) : V1.identifier =
  { name; span = span pos }

let feature ({ Dialect.feature; pos; _ } : Dialect.dialect_support) : V1.feature =
  { name = Dialect.feature_to_string feature; span = span pos }

let nullability = function
  | Sql.Type.Nullable -> V1.Nullable
  | Sql.Type.Strict -> Strict
  | Sql.Type.Depends -> Unknown

let sql_type_kind = function
  | Sql.Type.Int -> V1.Int
  | Sql.Type.UInt64 -> UInt64
  | Sql.Type.Text -> Text
  | Sql.Type.Blob -> Blob
  | Sql.Type.Float -> Float
  | Sql.Type.Bool -> Bool
  | Sql.Type.Datetime -> Datetime
  | Sql.Type.Decimal { precision; scale } -> Decimal { precision; scale }
  | Sql.Type.Union { ctors; is_closed } ->
    Union
      {
        values = Sql.Type.Enum_kind.Ctors.elements ctors;
        closed = is_closed;
      }
  | Sql.Type.StringLiteral value -> String_literal value
  | Sql.Type.FloatingLiteral value -> Floating_literal value
  | Sql.Type.Json_path -> Json_path
  | Sql.Type.One_or_all -> One_or_all
  | Sql.Type.Json -> Json
  | Sql.Type.Any -> Any

let sql_type ({ Sql.Type.t; nullability = nullable } : Sql.Type.t) : V1.sql_type =
  { kind = sql_type_kind t; nullability = nullability nullable }

let metadata meta = Sql.Meta.StringMap.bindings meta

let field ({ Sql.name; domain; meta; _ } : Sql.attr) : V1.field =
  { name; typ = sql_type domain; metadata = metadata meta }

let membership = function
  | `In -> V1.In
  | `NotIn -> Not_in

let option_action = function
  | Sql.BoolChoices -> V1.Bool_choices
  | Sql.SetDefault -> Set_default

let rec parameter = function
  | Sql.Single ({ id; typ }, meta) ->
    V1.Scalar
      {
        identifier = identifier id;
        typ = sql_type typ;
        metadata = metadata meta;
      }
  | Sql.SingleIn ({ id; typ }, meta) ->
    List
      {
        identifier = identifier id;
        typ = sql_type typ;
        metadata = metadata meta;
      }
  | Sql.ChoiceIn { param; kind; vars } ->
    Choice_list
      {
        identifier = identifier param;
        membership = membership kind;
        parameters = List.map parameter vars;
      }
  | Sql.Choice (id, alternatives) ->
    Choice
      {
        identifier = identifier id;
        alternatives = List.map alternative alternatives;
      }
  | Sql.DynamicSelect (id, alternatives) ->
    Dynamic_select
      {
        identifier = identifier id;
        alternatives = List.map alternative alternatives;
      }
  | Sql.DynamicSelectJoin { pid; pos; source = { table; alias } } ->
    Dynamic_select_join
      {
        identifier = identifier pid;
        span = span pos;
        table = table_name table;
        alias = Option.map table_name alias;
      }
  | Sql.TupleList (id, tuple) ->
    Tuple
      {
        identifier = identifier id;
        tuple = tuple_kind tuple;
      }
  | Sql.OptionActionChoice (id, parameters, (when_pos, else_pos), action) ->
    Optional_action
      {
        identifier = identifier id;
        parameters = List.map parameter parameters;
        when_span = span when_pos;
        else_span = span else_pos;
        action = option_action action;
      }
  | Sql.SharedVarsGroup (parameters, reference) ->
    Shared
      {
        reference = reference.value;
        span = span reference.pos;
        parameters = List.map parameter parameters;
      }

and alternative = function
  | Sql.Simple { ctor; ctor_pos; body } ->
    V1.Simple
      {
        name = ctor.value;
        name_span = span ctor_pos;
        body_span = span ctor.pos;
        parameters = Option.map (List.map parameter) body;
      }
  | Sql.Verbatim (name, sql) -> Verbatim { name; sql }

and tuple_kind = function
  | Sql.Insertion schema -> V1.Insertion (List.map field schema)
  | Sql.Where_in { value = (elements, kind); pos } ->
    Where_in
      {
        elements =
          List.map
            (fun (typ, meta) -> sql_type typ, metadata meta)
            elements;
        membership = membership kind;
        span = span pos;
      }
  | Sql.ValueRows { types; values_start_pos } ->
    Value_rows
      {
        types = List.map sql_type types;
        values_start = values_start_pos;
      }

let result_column = function
  | Sql.Attr attr -> V1.Column (field attr)
  | Sql.Dynamic (id, fields) ->
    Dynamic_column
      {
        identifier = identifier id;
        fields =
          List.map
            (fun ({ field_id; field_attr; join_deps } : Sql.attr Sql.dynamic_field) ->
              {
                V1.identifier = identifier field_id;
                field = field field_attr;
                join_dependencies = join_deps;
              })
            fields;
      }

let cardinality = function
  | `Zero_one -> V1.Zero_one
  | `One -> One
  | `Nat -> Many

let operation_of_kind = function
  | Stmt.Select value -> V1.Select (cardinality value)
  | Stmt.Insert (_, target) -> Insert (table_name target)
  | Stmt.Create target -> Create_table (table_name target)
  | Stmt.CreateIndex name -> Create_index name
  | Stmt.Update target -> Update (Option.map table_name target)
  | Stmt.Delete targets -> Delete (List.map table_name targets)
  | Stmt.Alter targets -> Alter (List.map table_name targets)
  | Stmt.Drop target -> Drop_table (table_name target)
  | Stmt.CreateRoutine name -> Create_routine (table_name name)
  | Stmt.CreateType name -> Create_type name
  | Stmt.DropType name -> Drop_type name
  | Stmt.Other -> Other

let parsed ({ Analysis.sql; ast; dialect_features } : Analysis.parsed) =
  V1.Parsed
    {
      sql;
      operation = operation_of_stmt ast;
      features = List.map feature dialect_features;
    }

let checked ({ Syntax.sql; schema; vars; kind; dialect_features; _ } : Syntax.result) =
  V1.Checked
    {
      sql;
      operation = operation_of_kind kind;
      parameters = List.map parameter vars;
      columns = List.map result_column schema;
      features = List.map feature dialect_features;
    }

let invalid ~sql diagnostics =
  V1.Invalid
    {
      sql;
      diagnostics =
        List.map
          (fun ({ Analysis.message; pos } : Analysis.diagnostic) ->
            { V1.message; span = Option.map span pos })
          diagnostics;
    }
