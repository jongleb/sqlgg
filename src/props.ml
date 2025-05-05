(** Association list *)
open ExtLib

type t = (string * string) list [@@deriving show {with_path=false}]

let get x n = try Some (List.assoc n x) with Not_found -> None
let get_all x n = List.filter_map (fun (k,v) -> if k = n then Some v else None) x
let set x n v = (n,v)::x
let update x n v =
  let rec aux acc = function
    | [] -> List.rev ((n, v) :: acc)
    | (k, _) :: rest when k = n -> List.rev_append acc ((n, v) :: rest)
    | pair :: rest -> aux (pair :: acc) rest
  in
  aux [] x
let set_all l1 l2 = l1 @ l2
let empty = []
