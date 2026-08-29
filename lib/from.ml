
open ExtLib
open Sql

type resolved_source = {
  rsrc_schema : table_name Schema.Source.t;
  rsrc_params : vars;
  rsrc_tables : Tables.table list;
  rsrc_dynamic : schema_column_with_sources list;
  rsrc_physical_table : Sql.join_source option;
}

type join = {
  src : resolved_source;
  kind : Schema.Join.typ;
  cond : join_condition;
  pos : pos;
}

type t = {
  base : resolved_source;
  joins : join list;
}

let dynamic_columns from =
  let sources { base; joins } = base :: List.map (fun j -> j.src) joins in
  List.concat_map (fun src -> src.rsrc_dynamic) (Option.map_default sources [] from)
