
open Hmx_lattice

module U = Inferno.Unifier.Make (Hmx_domain.S)

type var = U.variable
let no_info = Hmx_domain.no_info

let fresh () = U.fresh None
let bounded i = U.fresh (Some i)
let info v = match U.get v with Some i -> i | None -> no_info

let at_least t = bounded { no_info with lowers = [ t ] }

let of_type t = bounded { no_info with lowers = [ t ]; uppers = [ t ] }

let declared (t : Refined.t) =
  if Refine.is_closed_enum t.refine then of_type t else at_least t

let unify a b =
  try U.unify a b with
  | U.Unify (x, y) ->
    conflict "cannot reconcile %s with %s"
      (Hmx_domain.show_info (info x)) (Hmx_domain.show_info (info y))
  | Hmx_domain.S.InconsistentConjunction -> conflict "inconsistent constraints"

let above v t = unify v (bounded { no_info with lowers = [ t ] })

let below v t = unify v (bounded { no_info with uppers = [ t ] })

let has v p = unify v (bounded { no_info with preds = [ p ] })

let same a b = unify a b

let resolve ?fallback v =
  match Hmx_domain.pick ?fallback (info v) with Ok t -> t | Error msg -> conflict "%s" msg

let resolve_opt ?fallback v =
  let i = info v in
  match Hmx_domain.pick ?fallback i with
  | Ok t -> Some t
  | Error _ when Hmx_domain.feasible i -> None
  | Error msg -> conflict "%s" msg
