(** Nullability.

    The one part of the constraint language a unifier cannot carry: the result
    of an operator is the {e join} of its operands' nullabilities, and a join of
    three variables is not an equality. Equality it can carry, so that half is
    another {!Inferno.Unifier} over a two-point structure; the joins and meets
    are a worklist run to a fixpoint. *)

open Hmx_lattice

module S = struct
  type 'a structure = Null.t option

  exception InconsistentConjunction

  let conjunction _equate a b =
    match a, b with
    | None, s | s, None -> s
    | Some x, Some y -> if Null.equal x y then Some x else raise InconsistentConjunction
end

module U = Inferno.Unifier.Make (S)

type t = U.variable

let fresh () = U.fresh None
let const n = U.fresh (Some n)
let value v = U.get v

type con =
  | Eq of t * t
  | Join of t * t list   (** n is the join of ns *)
  | Meet of t * t list   (** n is the meet of ns — COALESCE and relatives *)

(** only the deferred constraints; the equalities live in the unifier *)
type state = { mutable cons : con list }

let create () = { cons = [] }
let add st c = st.cons <- c :: st.cons

let unify a b =
  match U.unify a b with
  | () -> ()
  | exception (U.Unify _ | S.InconsistentConjunction) ->
    conflict "nullability conflict: %s vs %s"
      (match value a with Some n -> Null.show n | None -> "_")
      (match value b with Some n -> Null.show n | None -> "_")

let set v n = unify v (const n)

(* Partial knowledge is enough: one [top] argument settles the result, and a
   [bottom] result forces every argument. *)
let step ~top (n, args) =
  let bottom = match top with Null.Nullable -> Null.NotNull | Null.NotNull -> Null.Nullable in
  let known = List.filter_map value args in
  if List.exists (Null.equal top) known then (set n top; `Done)
  else if List.length known = List.length args then begin
    set n (List.fold_left (fun acc x -> if Null.equal x top then top else acc) bottom known);
    `Done
  end
  else
    match value n with
    | Some m when Null.equal m bottom -> List.iter (fun a -> set a bottom) args; `Done
    | Some _ | None -> `Defer

let solve st =
  List.iter (function Eq (a, b) -> unify a b | Join _ | Meet _ -> ()) st.cons;
  let rec settle pending =
    let still =
      List.filter (function
        | Join (n, args) -> step ~top:Null.Nullable (n, args) = `Defer
        | Meet (n, args) -> step ~top:Null.NotNull (n, args) = `Defer
        | Eq _ -> false)
        pending
    in
    if List.length still < List.length pending then settle still
  in
  settle st.cons

(** [NotNull] is the identity of the join, so "no evidence that this can be
    null" is exactly what an unconstrained variable means. §8 says [Nullable];
    the code has always said otherwise, and the code is right. *)
let get v = match value v with Some n -> n | None -> Null.NotNull
