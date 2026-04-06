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

module Make_dynamic_select (X : sig type params type row end) = struct
  type ('shape, 'a) t = {
    set: X.params -> unit;
    read: X.row -> int -> 'a * int;
    column: string;
    count: int;
    phantom: 'shape option;
  }

  let pure x = {
    set = (fun _p -> ());
    read = (fun _row idx -> (x, idx));
    column = "";
    count = 0;
    phantom = None;
  }

  let apply f a = {
    set = (fun p -> f.set p; a.set p);
    read = (fun row idx ->
      let (vf, i1) = f.read row idx in
      let (va, i2) = a.read row i1 in
      (vf va, i2));
    column = (match f.column, a.column with
      | "", c | c, "" -> c
      | c1, c2 -> c1 ^ ", " ^ c2);
    count = f.count + a.count;
    phantom = None;
  }

  let map f a = apply (pure f) a
  let (let+) t f = map f t
  let (and+) a b = apply (map (fun a b -> (a, b)) a) b
end

 let rec convert_json = function
  | (`Null | `Bool _ | `Int _ | `Intlit _ | `Float _ | `String _) as x -> x
  | `Assoc assoc_list ->
    let convert_pair (key, value) = (key, convert_json value) in
    `Assoc (List.map convert_pair assoc_list)
  | `List json_list | `Tuple json_list ->
    `List (List.map convert_json json_list)
  | `Variant _ -> failwith "Variant type is not supported"
