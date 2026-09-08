open Sqlgg

module Index = Map.Make (String)

type loc = {
  file : string;
  pos : Sql.Pos.t;
}

type kind =
  | Table
  | Cte
  | Derived
  | Alias of Sql.table_name

type column = {
  attr : Sql.attr;
  loc : loc option;
}

type t = {
  name : string;
  kind : kind;
  loc : loc option;
  columns : column list;
}

let loc ~file pos = { file; pos }
let column ?loc attr = { attr; loc }
let make ~name ~kind ?loc columns = { name; kind; loc; columns }

let columns t = List.map (fun col -> col.attr) t.columns

let declaration ~schema t =
  match t.kind with
  | Table -> Index.find_opt t.name schema
  | Alias target -> Index.find_opt target.tn schema
  | Cte | Derived -> None

let declared ~schema t = Option.value ~default:t (declaration ~schema t)

let find_column_opt t name =
  List.find_opt (fun col -> String.equal col.attr.name name) t.columns

let find_opt symbols name =
  List.find_opt (fun sym -> String.equal sym.name name) symbols

let unique = Prelude.unique_by ~key:(fun sym -> sym.name)
