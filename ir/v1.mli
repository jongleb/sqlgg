type span = {
  start : int;
  stop : int;
}

type cardinality =
  | Unknown_cardinality
  | Zero_one
  | One
  | Many

type table_name = {
  database : string option;
  name : string;
}

type operation =
  | Select of cardinality
  | Insert of table_name
  | Create_table of table_name
  | Create_index of string
  | Update of table_name option
  | Delete of table_name list
  | Alter of table_name list
  | Drop_table of table_name
  | Rename_tables of (table_name * table_name) list
  | Set of operation option
  | Create_routine of table_name
  | Create_type of string
  | Drop_type of string
  | Create_extension of string
  | Drop_extension of string list
  | Other

type feature = {
  name : string;
  span : span;
}

type nullability =
  | Nullable
  | Strict
  | Unknown

type sql_type_kind =
  | Int
  | UInt64
  | Text
  | Blob
  | Float
  | Bool
  | Datetime
  | Decimal of {
      precision : int option;
      scale : int option;
    }
  | Union of {
      values : string list;
      closed : bool;
    }
  | String_literal of string
  | Floating_literal of float
  | Json_path
  | One_or_all
  | Json
  | Any

type sql_type = {
  kind : sql_type_kind;
  nullability : nullability;
}

type identifier = {
  name : string option;
  span : span;
}

type metadata = (string * string) list

type membership =
  | In
  | Not_in

type option_action =
  | Bool_choices
  | Set_default

type field = {
  name : string;
  typ : sql_type;
  metadata : metadata;
}

type parameter =
  | Scalar of {
      identifier : identifier;
      typ : sql_type;
      metadata : metadata;
    }
  | List of {
      identifier : identifier;
      typ : sql_type;
      metadata : metadata;
    }
  | Choice_list of {
      identifier : identifier;
      membership : membership;
      parameters : parameter list;
    }
  | Choice of {
      identifier : identifier;
      alternatives : alternative list;
    }
  | Dynamic_select of {
      identifier : identifier;
      alternatives : alternative list;
    }
  | Dynamic_select_join of {
      identifier : identifier;
      span : span;
      table : table_name;
      alias : table_name option;
    }
  | Tuple of {
      identifier : identifier;
      tuple : tuple_kind;
    }
  | Optional_action of {
      identifier : identifier;
      parameters : parameter list;
      when_span : span;
      else_span : span;
      action : option_action;
    }
  | Shared of {
      reference : string;
      span : span;
      parameters : parameter list;
    }

and alternative =
  | Simple of {
      name : string option;
      name_span : span;
      body_span : span;
      parameters : parameter list option;
    }
  | Verbatim of {
      name : string;
      sql : string;
    }

and tuple_kind =
  | Insertion of field list
  | Where_in of {
      elements : (sql_type * metadata) list;
      membership : membership;
      span : span;
    }
  | Value_rows of {
      types : sql_type list;
      values_start : int;
    }

type dynamic_field = {
  identifier : identifier;
  field : field;
  join_dependencies : int list;
}

type result_column =
  | Column of field
  | Dynamic_column of {
      identifier : identifier;
      fields : dynamic_field list;
    }

type diagnostic = {
  message : string;
  span : span option;
}

type statement =
  | Parsed of {
      sql : string;
      operation : operation;
      features : feature list;
    }
  | Checked of {
      sql : string;
      operation : operation;
      parameters : parameter list;
      columns : result_column list;
      features : feature list;
    }
  | Invalid of {
      sql : string;
      diagnostics : diagnostic list;
    }

type document = {
  ir_version : int;
  statements : statement list;
}

val ir_version : int
