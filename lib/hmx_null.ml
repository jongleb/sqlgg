
open Hmx_lattice

type nullable = bool

let show n = if n then "nullable" else "not null"

module S = struct

  type 'a structure = nullable option

  exception InconsistentConjunction

  let conjunction _equate a b =
    match a, b with
    | None, s | s, None -> s
    | Some x, Some y -> if Bool.equal x y then Some x else raise InconsistentConjunction
end

module U = Inferno.Unifier.Make (S)

type t = U.variable

let fresh () = U.fresh None
let const n = U.fresh (Some n)
let value v = U.get v

type con =
  | Eq of t * t
  | Join of t * t list
  | Meet of t * t list
  | Above of t * t list

type state = { mutable cons : con list }

let create () = { cons = [] }
let add st c = st.cons <- c :: st.cons

let unify a b =
  try U.unify a b with
  | U.Unify _ | S.InconsistentConjunction ->
    conflict "nullability conflict: %s vs %s"
      (match value a with Some n -> show n | None -> "_")
      (match value b with Some n -> show n | None -> "_")

let set v n = unify v (const n)

let step ~top (n, args) =
  let bottom = not top in
  let known = List.filter_map value args in
  if List.exists (Bool.equal top) known then (set n top; `Done)
  else if List.length known = List.length args then begin
    set n (List.fold_left (fun acc x -> if Bool.equal x top then top else acc) bottom known);
    `Done
  end
  else
    match value n with
    | Some m when Bool.equal m bottom -> List.iter (fun a -> set a bottom) args; `Done
    | Some _ | None -> `Defer

let step_above (n, args) =
  if List.exists (fun a -> value a = Some true) args then (set n true; `Done)
  else
    match value n with
    | Some false -> List.iter (fun a -> set a false) args; `Done
    | Some true -> `Done
    | None -> if List.for_all (fun a -> value a <> None) args then `Done else `Defer

let solve st =
  List.iter (function Eq (a, b) -> unify a b | Join _ | Meet _ | Above _ -> ()) st.cons;
  let rec settle pending =
    let still =
      List.filter (function
        | Join (n, args) -> step ~top:true (n, args) = `Defer
        | Meet (n, args) -> step ~top:false (n, args) = `Defer
        | Above (n, args) -> step_above (n, args) = `Defer
        | Eq _ -> false)
        pending
    in
    if List.length still < List.length pending then settle still
  in
  settle st.cons

let get v = match value v with Some n -> n | None -> false
