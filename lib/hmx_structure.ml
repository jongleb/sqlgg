(** The domain, presented to Inferno as a unification structure.

    The bet that makes this work: SQL base types are nullary. There is no
    [list int] and no [int -> int], so a "shallow type" has no children at all
    and the structure can simply be {!Hmx_domain.info} — the bounds and
    predicates known about one variable. Unification of two variables then
    means merging two such records, which is exactly [conjunction].

    Consequences worth naming:
    - a subtyping bound is stated by unifying with a one-sided structure:
      [Sub (Int, a)] is [a --- { lowers = [Int] }];
    - a predicate rides inside the class, so it still constrains the outcome
      long after the point where it was written — qualified types, with the
      unifier doing the propagation. *)

module S = struct
  type 'a structure = Hmx_domain.info

  exception InconsistentConjunction

  (* Inferno's contract allows no side effect here beyond calling [equate],
     which we never need: with no children there is nothing to equate. Raising
     is the sanctioned way to report a contradiction, and Inferno turns it into
     a [Unify] error carrying both decoded structures. *)
  let conjunction _equate a b =
    let m = Hmx_domain.merge_info a b in
    if Hmx_domain.feasible m then m else raise InconsistentConjunction

  let iter _ _ = ()
  let fold _ _ acc = acc
  let map _ i = i
  let pprint _ i = PPrint.string (Hmx_domain.show_info i)
end

module O = struct
  type 'a structure = Hmx_domain.info
  type tyvar = int
  type ty = Free of int | Bound of Hmx_domain.info

  let inject i = i
  let variable v = Free v
  let structure s = Bound s
  let mu _ t = t

  let show = function
    | Free v -> Printf.sprintf "'%d" v
    | Bound i -> Hmx_domain.show_info i
end
