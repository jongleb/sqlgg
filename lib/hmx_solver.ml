(** Type variables and unification, delegated to Inferno.

    All of it: {!Inferno.Unifier} is union-find with structure merging, and
    {!Hmx_domain.S} says what a merge means, so the whole solver is [fresh] and
    [unify].

    State lives in the variables themselves, not in a module-level table, so
    each statement is independent and nothing has to be reset between them.

    What Inferno cannot own is nullability: [n = n1 join n2] between three
    variables is not an equality, and collapsing it to one would be wrong. That
    lives in {!Hmx_null}. *)

open Hmx_lattice

module U = Inferno.Unifier.Make (Hmx_domain.S)

type var = U.variable
let no_info = Hmx_domain.no_info

let fresh () = U.fresh None
let bounded i = U.fresh (Some i)
let info v = match U.get v with Some i -> i | None -> no_info

(** a value of type [t]: it may still widen, so only a lower bound *)
let at_least t = bounded { no_info with lowers = [ t ] }

(** a value that may not widen — a declared ENUM column, whose type accepts no
    constructor beyond the ones it lists *)
let of_type t = bounded { no_info with lowers = [ t ]; uppers = [ t ] }

(** the right one for a type written down in the schema *)
let declared (t : Refined.t) =
  if Refine.is_closed_enum t.refine then of_type t else at_least t

let unify a b =
  match U.unify a b with
  | () -> ()
  | exception U.Unify (x, y) ->
    conflict "cannot reconcile %s with %s"
      (Hmx_domain.show_info (info x)) (Hmx_domain.show_info (info y))
  | exception Hmx_domain.S.InconsistentConjunction -> conflict "inconsistent constraints"

(** [t] is coercible to the variable *)
let above v t = unify v (bounded { no_info with lowers = [ t ] })

(** the variable is coercible to [t] *)
let below v t = unify v (bounded { no_info with uppers = [ t ] })

(** invariant: exactly [t], as a declared ENUM column is *)
let exactly v t = unify v (bounded { no_info with lowers = [ t ]; uppers = [ t ] })

let has v p = unify v (bounded { no_info with preds = [ p ] })

(* §11.1: a subtyping edge between two variables is solved by unification *)
let same a b = unify a b

(** §8: turn a variable into a concrete type, or say why not *)
let resolve ?fallback v =
  match Hmx_domain.pick ?fallback (info v) with Ok t -> t | Error msg -> conflict "%s" msg

(** Coercions accepted only because the base table was closed transitively —
    [CONCAT(int_col, 'x')] and relatives. §11.4 wants these refused; reporting
    them makes it a dialect decision rather than a silent one. *)
let derived_coercions v =
  match resolve v with
  | exception Conflict _ -> []
  | t ->
    List.filter_map (fun (l : Refined.t) ->
      if Base.is_derived l.base t.Refined.base then Some (l, t) else None)
      (info v).lowers
