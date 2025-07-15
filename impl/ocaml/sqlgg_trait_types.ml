type json = [
  | `Null
  | `Bool of bool
  | `Float of float
  | `String of string
  | `Assoc of (string * json) list
  | `List of json list
  | `Int of int
  | `Intlit of string
]

type json_path = Sqlgg_json_path.Ast.t
type one_or_all = [ `One | `All ]
