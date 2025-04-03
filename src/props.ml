(** Association list *)
open ExtLib

type t = (string * string) list [@@deriving show]

let get x n = try Some (List.assoc n x) with Not_found -> None
let get_all x n = List.filter_map (fun (k,v) -> if k = n then Some v else None) x
let set x n v = (n,v)::x
let empty = []

let replace x n v =
  List.map (fun (k, old_v) -> if k = n then (k, v) else (k, old_v)) x
