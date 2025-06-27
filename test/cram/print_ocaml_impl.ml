open Printf

module M = struct

type json = [ `Null
  | `String of string
  | `Float of float
  | `Int of int
  | `Bool of bool
  | `List of json list
  | `Assoc of (string * json) list 
]

type json_path = Sqlgg_json_path.Ast.t
type one_or_all = [ `One | `All ]

module Types = struct
  module Bool = struct 
    type t = bool 
    let to_literal = string_of_bool
    let bool_to_literal = to_literal
  end
  module Int = struct 
    include Int64 
    let to_literal = to_string
    let int64_to_literal = to_literal
  end
  module Text = struct
    type t = string
    let to_literal s = sprintf "'%s'" (String.escaped s)
    let string_to_literal = to_literal
  end
  module Blob = struct
    type t = string
    let to_literal s = 
      let hex = String.to_seq s 
               |> Seq.map (fun c -> sprintf "%02X" (Char.code c))
               |> List.of_seq 
               |> String.concat "" in
      sprintf "X'%s'" hex
  end
  module Float = struct 
    type t = float 
    let to_literal = string_of_float
    let float_to_literal = to_literal
  end
  module Decimal = Float
  module Datetime = struct
    type t = float
    let to_literal t = sprintf "'%s'" (string_of_float t)
    let float_to_literal = to_literal
  end
  
  module Json = struct
    type t = Yojson.Basic.t
    let to_literal j = Text.to_literal (Yojson.Basic.to_string j)
    let json_to_literal = to_literal
  end

  module Json_path = struct
    open Sqlgg_json_path
    type t = json_path
    let to_literal j = Text.to_literal (Json_path.string_of_json_path j)
    let json_path_to_literal = to_literal
  end

  module One_or_all = struct
    type t = one_or_all
    let to_literal = function
      | `One -> "one"
      | `All -> "all"
    let one_or_all_to_literal = to_literal
  end

  module Any = Text
end

module type Enum = sig 
  type t
  val inj: string -> t
  val proj: t -> string
end

(* Mock types for testing *)
type statement = string * int  (* sql template, param count *)
type 'a connection = string  (* connection name *)
type params = string * string list ref * int ref * int  (* sql template, collected params, current index, total params *)
type row = unit  (* we don't actually return data *)
type result = unit
type execute_response = { affected_rows: int64; insert_id: int64 option }

type num = int64
type text = string
type any = string
type datetime = float

exception Oops of string

let get_column_ty_generic name default_value =
  let get_fn () index = default_value in
  let get_nullable_fn () index = None in
  (get_fn, get_nullable_fn)

let get_column_Bool, get_column_Bool_nullable = get_column_ty_generic "Bool" false
let get_column_Int, get_column_Int_nullable = get_column_ty_generic "Int" 0L
let get_column_Text, get_column_Text_nullable = get_column_ty_generic "Text" "mock_text"
let get_column_Any, get_column_Any_nullable = get_column_ty_generic "Any" "mock_any"
let get_column_Float, get_column_Float_nullable = get_column_ty_generic "Float" 0.0
let get_column_Decimal, get_column_Decimal_nullable = get_column_ty_generic "Decimal" 0.0
let get_column_Datetime, get_column_Datetime_nullable = get_column_ty_generic "Datetime" 0.0
let get_column_Json, get_column_Json_nullable = get_column_ty_generic "Json" `Null

(* Create a simple mock json_path - use string parsing as fallback *)
let mock_json_path = 
  try 
    Sqlgg_json_path.Json_path.parse_json_path "$.mock"
  with 
  | _ -> Sqlgg_json_path.Json_path.parse_json_path "$"

let get_column_Json_path, get_column_Json_path_nullable = get_column_ty_generic "Json_path" mock_json_path
let get_column_One_or_all, get_column_One_or_all_nullable = get_column_ty_generic "One_or_all" `One

(* Convenience accessors *)
let get_column_bool, get_column_bool_nullable = (get_column_Bool, get_column_Bool_nullable)
let get_column_int64, get_column_int64_nullable = (get_column_Int, get_column_Int_nullable)
let get_column_float, get_column_float_nullable = (get_column_Float, get_column_Float_nullable)
let get_column_decimal, get_column_decimal_nullable = (get_column_Decimal, get_column_Decimal_nullable)
let get_column_string, get_column_string_nullable = (get_column_Text, get_column_Text_nullable)
let get_column_datetime, get_column_datetime_nullable = get_column_ty_generic "datetime" "mock_datetime"
let get_column_json, get_column_json_nullable = (get_column_Json, get_column_Json_nullable)
let get_column_json_path, get_column_json_path_nullable = (get_column_Json_path, get_column_Json_path_nullable)
let get_column_one_or_all, get_column_one_or_all_nullable = (get_column_One_or_all, get_column_One_or_all_nullable)

(* SQL parameter substitution *)
let substitute_params sql params =
  let param_array = Array.of_list params in
  let param_index = ref 0 in
  String.to_seq sql
  |> Seq.map (function
    | '?' when !param_index < Array.length param_array ->
        let param = param_array.(!param_index) in
        incr param_index;
        param
    | c -> String.make 1 c)
  |> Seq.fold_left (^) ""

(* Parameter binding *)
let bind_param value_str (sql, params, index, total) =
  params := value_str :: !params;
  incr index

let start_params (sql, param_count) n = 
  (sql, ref [], ref 0, n)

let finish_params (sql, params, index, total) = 
  let final_sql = substitute_params sql (List.rev !params) in
  printf "[SQL] %s\n" final_sql;
  flush stdout;
  ()

let set_param_null params = bind_param "NULL" params
let set_param_Text params v = bind_param (sprintf "'%s'" (String.escaped v)) params
let set_param_Any = set_param_Text
let set_param_Bool params v = bind_param (if v then "TRUE" else "FALSE") params
let set_param_Int params v = bind_param (Int64.to_string v) params
let set_param_Float params v = bind_param (Float.to_string v) params
let set_param_Decimal = set_param_Float
let set_param_Datetime = set_param_Float
let set_param_Json params v = bind_param (sprintf "'%s'" (String.escaped (Yojson.Basic.to_string v))) params
let set_param_Json_path params v = bind_param (sprintf "'%s'" (String.escaped (Sqlgg_json_path.Json_path.string_of_json_path v))) params
let set_param_One_or_all params v = bind_param (match v with `One -> "'one'" | `All -> "'all'") params

let set_param_bool = set_param_Bool
let set_param_int64 = set_param_Int
let set_param_float = set_param_Float
let set_param_decimal = set_param_Float
let set_param_string = set_param_Text
let set_param_datetime = set_param_Float
let set_param_json = set_param_Json
let set_param_json_path = set_param_Json_path
let set_param_one_or_all = set_param_One_or_all

let no_params (sql, _) = 
  printf "[SQL] %s\n" sql;
  flush stdout;
  ()

module Make_enum (E: Enum) = struct 
  include E

  let get_column () index = E.inj "mock_enum"
  let get_column_nullable () index = None
  let set_param params v = bind_param (sprintf "'%s'" (E.proj v)) params
  let to_literal = E.proj
end

(* Main query functions *)
let select db sql set_params callback =
  printf "[SELECT] Connection: %s\n" db;
  let stmt = (sql, 0) in
  set_params stmt;
  flush stdout

let execute db sql set_params =
  printf "[EXECUTE] Connection: %s\n" db;
  let stmt = (sql, 0) in
  set_params stmt;
  flush stdout;
  { affected_rows = 0L; insert_id = None }

let select_one_maybe db sql set_params convert =
  printf "[SELECT_ONE] Connection: %s\n" db;
  let stmt = (sql, 0) in
  set_params stmt;
  flush stdout;
  Some (convert ())

let select_one db sql set_params convert =
  printf "[SELECT_ONE] Connection: %s\n" db;
  let stmt = (sql, 0) in
  set_params stmt;
  flush stdout;
  convert ()

end (* module M *)

(* Signature checking *)
let () =
  let module Test = (M : Sqlgg_traits.M) in 
  ignore (Test.Oops "Testing implementation ready")

include M
