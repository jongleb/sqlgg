(** Nullability.

    The one part of the constraint language a unifier cannot carry: the result
    of an operator is the {e join} of its operands' nullabilities, and a join of
    three variables is not an equality. Two points and monotone rules, so a
    worklist run to a fixpoint settles it.

    One state per statement, so nothing carries over between them. *)

open Hmx_lattice

type t = N of Null.t | V of int

type con =
  | Eq of t * t
  | Join of t * t list   (** n is the join of ns *)
  | Meet of t * t list   (** n is the meet of ns — COALESCE and relatives *)

type state = {
  parent : (int, int) Hashtbl.t;
  value : (int, Null.t) Hashtbl.t;
  mutable next : int;
  mutable cons : con list;
}

let create () = { parent = Hashtbl.create 32; value = Hashtbl.create 32; next = 0; cons = [] }
let fresh st = st.next <- st.next + 1; V st.next
let add st c = st.cons <- c :: st.cons

let rec find st v =
  match Hashtbl.find_opt st.parent v with
  | None -> v
  | Some p when p = v -> v
  | Some p -> let r = find st p in Hashtbl.replace st.parent v r; r

let get_opt st = function
  | N n -> Some n
  | V v -> Hashtbl.find_opt st.value (find st v)

let clash a b = conflict "nullability conflict: %s vs %s" (Null.show a) (Null.show b)

let set st target n =
  match target with
  | N m -> if not (Null.equal m n) then clash m n
  | V v ->
    let r = find st v in
    (match Hashtbl.find_opt st.value r with
     | None -> Hashtbl.replace st.value r n
     | Some m -> if not (Null.equal m n) then clash m n)

let union st a b =
  match a, b with
  | N x, N y -> if not (Null.equal x y) then clash x y
  | V v, N n | N n, V v -> set st (V v) n
  | V a, V b ->
    let ra = find st a and rb = find st b in
    if ra <> rb then begin
      let va = Hashtbl.find_opt st.value ra and vb = Hashtbl.find_opt st.value rb in
      Hashtbl.replace st.parent ra rb;
      Hashtbl.remove st.value ra;
      match va, vb with
      | None, None -> ()
      | Some v, None | None, Some v -> Hashtbl.replace st.value rb v
      | Some x, Some y -> if not (Null.equal x y) then clash x y
    end

(* Partial knowledge is enough: one [top] argument settles the result, and a
   [bottom] result forces every argument. *)
let step st ~top (n, args) =
  let bottom = match top with Null.Nullable -> Null.NotNull | Null.NotNull -> Null.Nullable in
  let known = List.filter_map (get_opt st) args in
  if List.exists (Null.equal top) known then (set st n top; `Done)
  else if List.length known = List.length args then begin
    set st n (List.fold_left (fun acc x -> if Null.equal x top then top else acc) bottom known);
    `Done
  end
  else
    match get_opt st n with
    | Some m when Null.equal m bottom -> List.iter (fun a -> set st a bottom) args; `Done
    | Some _ | None -> `Defer

let solve st =
  List.iter (function Eq (a, b) -> union st a b | Join _ | Meet _ -> ()) st.cons;
  let rec settle pending =
    let still =
      List.filter (function
        | Join (n, args) -> step st ~top:Null.Nullable (n, args) = `Defer
        | Meet (n, args) -> step st ~top:Null.NotNull (n, args) = `Defer
        | Eq _ -> false)
        pending
    in
    if List.length still < List.length pending then settle still
  in
  settle st.cons

(** [NotNull] is the identity of the join, so "no evidence that this can be
    null" is exactly what an unconstrained variable means. §8 says [Nullable];
    the code has always said otherwise, and the code is right. *)
let default = Null.NotNull

let get st t = match get_opt st t with Some n -> n | None -> default
