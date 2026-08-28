(** Constraint language and solver for the HM(X) core.

    Stage 3 of the pipeline: constraints in, a substitution out. Nothing here
    knows about SQL syntax — the generator (stage 2) and the elaborator
    (stage 5) live elsewhere. Every entry point is total and returns [result].

    Two properties the solver is built to have, because later stages and the
    proofs depend on them:

    - {b failure is order independent}. Bounds are accumulated as {e sets} and
      satisfiability is decided by enumerating {!Hmx_lattice.Base.all}. Folding
      a partial join pairwise would not do: on a poset the lub of a three
      element set can exist while the lub of a two element subset does not.
    - {b success is order independent}. Union-find plus set accumulation is
      confluent, and the nullability worklist runs to a fixpoint. *)

open Printf
open Hmx_lattice

type var = int
type pos = int * int

type ty =
  | Var of var
  | Ty of Refined.t

type null_ty =
  | NVar of var
  | N of Null.t

type t =
  | True
  | Eq of ty * ty
  | Sub of ty * ty                    (** [Sub (a, b)]: a is coercible to b *)
  | Has of Pred.t * ty
  | NEq of null_ty * null_ty
  | NJoin of null_ty * null_ty list   (** n = ⊔ ns *)
  | NMeet of null_ty * null_ty list   (** n = ⊓ ns *)
  | Conj of t list
  | At of pos * t

type error = { pos : pos option; msg : string }

let error ?pos msg = Error { pos; msg }
let errorf ?pos fmt = ksprintf (fun msg -> Error { pos; msg }) fmt

let show_ty = function Var v -> sprintf "'%d" v | Ty t -> Refined.show t
let show_null_ty = function NVar v -> sprintf "?%d" v | N n -> Null.show n

let show_error { pos; msg } =
  match pos with None -> msg | Some (a, b) -> sprintf "%s (at %d:%d)" msg a b

(** what is known about a type variable *)
type info = {
  lowers : Refined.t list;  (** set: every l must satisfy l ≤ t *)
  uppers : Refined.t list;  (** set: every u must satisfy t ≤ u *)
  preds : Pred.t list;      (** set *)
}

let no_info = { lowers = []; uppers = []; preds = [] }

let show_info { lowers; uppers; preds } =
  let list f l = String.concat "," (List.map f l) in
  sprintf "[%s <= _ <= %s]%s" (list Refined.show lowers) (list Refined.show uppers)
    (match preds with [] -> "" | l -> sprintf " with %s" (list Pred.show l))

let add_uniq cmp x l = if List.exists (fun y -> cmp x y = 0) l then l else List.sort cmp (x :: l)
let merge_uniq cmp a b = List.fold_left (fun acc x -> add_uniq cmp x acc) a b

let merge_info a b = {
  lowers = merge_uniq Refined.compare a.lowers b.lowers;
  uppers = merge_uniq Refined.compare a.uppers b.uppers;
  preds = merge_uniq Pred.compare a.preds b.preds;
}

(** Refinement the variable must take if its base is resolved to [base].
    A lower bound sitting on a strictly smaller base cannot pass its refinement
    up, so it contributes [Top]; an upper bound sitting on a strictly larger
    base must itself be unrefined, otherwise [base] is not a solution at all. *)
let resolve_refine base info =
  let contributes_top (l : Refined.t) = not (Base.equal l.base base) in
  let lowers = List.map (fun (l : Refined.t) -> if contributes_top l then Refine.Top else l.refine) info.lowers in
  let uppers_here = List.filter_map (fun (u : Refined.t) ->
    if Base.equal u.base base then Some u.refine else None) info.uppers in
  let uppers_above_unrefined = List.for_all (fun (u : Refined.t) ->
    Base.equal u.base base || Refine.is_top u.refine) info.uppers in
  if not uppers_above_unrefined then `Conflict
  else
    match Refine.join_all lowers, Refine.meet_all uppers_here with
    | _, `Conflict -> `Conflict
    | None, `None -> `Ok Refine.Top
    | Some r, `None -> if Refine.fits base r then `Ok r else `Conflict
    | None, `Some r -> if Refine.fits base r then `Ok r else `Conflict
    | Some lo, `Some up ->
      if Refine.leq lo up && Refine.fits base lo then `Ok lo else `Conflict

(** every base the variable could still take *)
let candidates info =
  List.filter (fun b ->
    List.for_all (fun (l : Refined.t) -> Base.leq l.base b) info.lowers
    && List.for_all (fun (u : Refined.t) -> Base.leq b u.base) info.uppers
    && List.for_all (fun p -> Pred.satisfies p b) info.preds
    && (match resolve_refine b info with `Ok _ -> true | `Conflict -> false))
    Base.all

module H = Hashtbl

type state = {
  tparent : (var, var) H.t;
  tinfo : (var, info) H.t;
  nparent : (var, var) H.t;
  nvalue : (var, Null.t) H.t;
  mutable tvars : var list;
  mutable nvars : var list;
  mutable pending : (pos option * t) list;   (** deferred NJoin/NMeet *)
}

let fresh_state () = {
  tparent = H.create 32; tinfo = H.create 32;
  nparent = H.create 32; nvalue = H.create 32;
  tvars = []; nvars = []; pending = [];
}

let rec find tbl v =
  match H.find_opt tbl v with
  | None -> v
  | Some p when p = v -> v
  | Some p -> let r = find tbl p in H.replace tbl v r; r

let seen_tvar st v = if not (List.mem v st.tvars) then st.tvars <- v :: st.tvars
let seen_nvar st v = if not (List.mem v st.nvars) then st.nvars <- v :: st.nvars

let get_info st v = (match H.find_opt st.tinfo (find st.tparent v) with Some i -> i | None -> no_info)

let check_info ?pos st v =
  let r = find st.tparent v in
  let info = get_info st r in
  match candidates info with
  | [] -> errorf ?pos "no type satisfies %s" (show_info info)
  | _ :: _ -> Ok ()

let set_info ?pos st v info =
  let r = find st.tparent v in
  seen_tvar st r;
  H.replace st.tinfo r info;
  check_info ?pos st r

let union_ty ?pos st a b =
  let ra = find st.tparent a and rb = find st.tparent b in
  if ra = rb then Ok () else begin
    let info = merge_info (get_info st ra) (get_info st rb) in
    H.replace st.tparent ra rb;
    seen_tvar st ra; seen_tvar st rb;
    H.remove st.tinfo ra;
    set_info ?pos st rb info
  end

let add_bound ?pos st ~side v t =
  let info = get_info st v in
  let info = match side with
    | `Lower -> { info with lowers = add_uniq Refined.compare t info.lowers }
    | `Upper -> { info with uppers = add_uniq Refined.compare t info.uppers }
  in
  set_info ?pos st v info

let add_pred ?pos st v p =
  let info = get_info st v in
  set_info ?pos st v { info with preds = add_uniq Pred.compare p info.preds }

(* ---- nullability ---- *)

let nfind st v = find st.nparent v

let nget st v = H.find_opt st.nvalue (nfind st v)

let nset ?pos st v n =
  let r = nfind st v in
  seen_nvar st r;
  match H.find_opt st.nvalue r with
  | None -> H.replace st.nvalue r n; Ok ()
  | Some m when Null.equal m n -> Ok ()
  | Some m -> errorf ?pos "nullability conflict: %s vs %s" (Null.show m) (Null.show n)

let union_null ?pos st a b =
  let ra = nfind st a and rb = nfind st b in
  if ra = rb then Ok () else begin
    let va = H.find_opt st.nvalue ra and vb = H.find_opt st.nvalue rb in
    H.replace st.nparent ra rb;
    seen_nvar st ra; seen_nvar st rb;
    H.remove st.nvalue ra;
    match va, vb with
    | None, None -> Ok ()
    | Some v, None | None, Some v -> H.replace st.nvalue rb v; Ok ()
    | Some x, Some y when Null.equal x y -> Ok ()
    | Some x, Some y -> errorf ?pos "nullability conflict: %s vs %s" (Null.show x) (Null.show y)
  end

let null_value st = function
  | N n -> Some n
  | NVar v -> nget st v

let ( let* ) = Result.bind

let rec iter_result f = function
  | [] -> Ok ()
  | x :: l -> let* () = f x in iter_result f l

(** One pass over a deferred [NJoin]/[NMeet]. [`Done] when the constraint
    carries no more information, [`Defer] otherwise. Partial knowledge is used:
    a single [top] argument settles the result, and a [bottom] result forces
    every argument. *)
let step_null ?pos st ~top (n, args) =
  let bottom = match top with Null.Nullable -> Null.NotNull | Null.NotNull -> Null.Nullable in
  let known = List.filter_map (null_value st) args in
  let all_known = List.length known = List.length args in
  let set target value = match target with
    | N m -> if Null.equal m value then Ok () else errorf ?pos "nullability conflict: %s vs %s" (Null.show m) (Null.show value)
    | NVar v -> nset ?pos st v value
  in
  if List.exists (Null.equal top) known then let* () = set n top in Ok `Done
  else if all_known then
    let value = List.fold_left (fun acc x -> if Null.equal x top then top else acc) bottom known in
    let* () = set n value in Ok `Done
  else
    match null_value st n with
    | Some m when Null.equal m bottom -> let* () = iter_result (fun a -> set a bottom) args in Ok `Done
    | Some _ | None -> Ok `Defer

(* ---- main loop ---- *)

let rec walk ?pos st c =
  match c with
  | True -> Ok ()
  | Conj l -> iter_result (walk ?pos st) l
  | At (pos, c) -> walk ~pos st c
  | Eq (Var a, Var b) -> union_ty ?pos st a b
  | Eq (Var a, Ty t) | Eq (Ty t, Var a) ->
    if not (Refined.well_formed t) then errorf ?pos "ill-formed type %s" (Refined.show t)
    else
      let* () = add_bound ?pos st ~side:`Lower a t in
      add_bound ?pos st ~side:`Upper a t
  | Eq (Ty a, Ty b) ->
    if Refined.equal a b then Ok () else errorf ?pos "%s is not %s" (Refined.show a) (Refined.show b)
  (* the documented precision loss of §11.1: a subtyping edge between two
     variables is solved by unification instead of a bounds graph *)
  | Sub (Var a, Var b) -> union_ty ?pos st a b
  | Sub (Var a, Ty t) -> add_bound ?pos st ~side:`Upper a t
  | Sub (Ty t, Var a) -> add_bound ?pos st ~side:`Lower a t
  | Sub (Ty a, Ty b) ->
    if Refined.leq a b then Ok () else errorf ?pos "%s is not coercible to %s" (Refined.show a) (Refined.show b)
  | Has (p, Var a) -> add_pred ?pos st a p
  | Has (p, Ty t) ->
    if Pred.satisfies p t.base then Ok ()
    else errorf ?pos "%s is not %s" (Refined.show t) (Pred.show p)
  | NEq (NVar a, NVar b) -> union_null ?pos st a b
  | NEq (NVar a, N n) | NEq (N n, NVar a) -> nset ?pos st a n
  | NEq (N a, N b) ->
    if Null.equal a b then Ok () else errorf ?pos "%s is not %s" (Null.show a) (Null.show b)
  | NJoin (n, args) ->
    List.iter (function NVar v -> seen_nvar st (nfind st v) | N _ -> ()) (n :: args);
    let* r = step_null ?pos st ~top:Null.Nullable (n, args) in
    if r = `Defer then st.pending <- (pos, c) :: st.pending;
    Ok ()
  | NMeet (n, args) ->
    List.iter (function NVar v -> seen_nvar st (nfind st v) | N _ -> ()) (n :: args);
    let* r = step_null ?pos st ~top:Null.NotNull (n, args) in
    if r = `Defer then st.pending <- (pos, c) :: st.pending;
    Ok ()

(** run the deferred nullability constraints to a fixpoint *)
let rec settle st =
  let pending = st.pending in
  st.pending <- [];
  let* still =
    List.fold_left (fun acc (pos, c) ->
      let* kept = acc in
      match c with
      | NJoin (n, args) ->
        let* r = step_null ?pos st ~top:Null.Nullable (n, args) in
        Ok (if r = `Defer then (pos, c) :: kept else kept)
      | NMeet (n, args) ->
        let* r = step_null ?pos st ~top:Null.NotNull (n, args) in
        Ok (if r = `Defer then (pos, c) :: kept else kept)
      | True | Eq _ | Sub _ | Has _ | NEq _ | Conj _ | At _ -> Ok kept)
      (Ok []) pending
  in
  if List.length still < List.length pending then begin st.pending <- still; settle st end
  else begin st.pending <- still; Ok () end

type policy = { fallback_base : Base.t option; default_null : Null.t }

type solution = { st : state; policy : policy }

(** §8: no fallback base, so an unconstrained parameter is an error rather than
    the old [Any]; an undetermined nullability defaults to [Nullable].
    Both are dialect policy, hence a record and not a constant. *)
let default_policy = { fallback_base = None; default_null = Null.Nullable }

let solve ?(policy = default_policy) c =
  let st = fresh_state () in
  match walk st c with
  | Error e -> Error e
  | Ok () ->
    match settle st with
    | Error e -> Error e
    | Ok () -> Ok { st; policy }

let info_of sol v = get_info sol.st v
let candidates_of sol v = candidates (info_of sol v)

let tvars sol = List.sort_uniq compare (List.map (find sol.st.tparent) sol.st.tvars)
let nvars sol = List.sort_uniq compare (List.map (nfind sol.st) sol.st.nvars)

(** §8, refined: the choice is made over the set of bases that are still
    feasible, so a predicate can never be violated by defaulting. *)
let base_of ?pos sol v =
  let info = info_of sol v in
  match candidates info with
  | [] -> errorf ?pos "no type satisfies %s" (show_info info)
  | cands ->
    let pick =
      let least = Base.lub (List.map (fun (l : Refined.t) -> l.base) info.lowers) in
      let greatest = Base.glb (List.map (fun (u : Refined.t) -> u.base) info.uppers) in
      let mem b = List.exists (Base.equal b) cands in
      let from_preds =
        List.fold_left (fun acc p ->
          match acc with
          | Some _ -> acc
          | None -> (match Pred.default p with Some b when mem b -> Some b | Some _ | None -> None))
          None info.preds
      in
      match least with
      | Some b when mem b -> Some b
      | _ ->
        match greatest with
        | Some b when mem b -> Some b
        | _ ->
          match from_preds with
          | Some b -> Some b
          | None ->
            match cands with
            | [ b ] -> Some b
            | _ -> (match sol.policy.fallback_base with Some b when mem b -> Some b | Some _ | None -> None)
    in
    match pick with
    | None -> errorf ?pos "cannot infer type: %s is satisfied by %s"
                (show_info info) (String.concat ", " (List.map Base.show cands))
    | Some base ->
      match resolve_refine base info with
      | `Conflict -> errorf ?pos "no type satisfies %s" (show_info info)
      | `Ok refine -> Ok (Refined.make base refine)

let null_of sol v = match nget sol.st v with Some n -> n | None -> sol.policy.default_null

let ty_of ?pos sol ~ty ~null =
  let* base = base_of ?pos sol ty in
  Ok (base, null_of sol null)
