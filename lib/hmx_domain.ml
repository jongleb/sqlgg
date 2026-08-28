(** The domain of the constraint language: what is known about one type
    variable, and how two such things combine.

    Kept separate from any solver on purpose. It is what a unifier has to be
    told from the outside — {!Hmx_structure} hands exactly this to Inferno as
    its [conjunction] — and it is also what defaulting reads. Nothing here
    knows about variables, union-find or constraint order. *)

open Printf
open Hmx_lattice

let compare_refined (a : Refined.t) (b : Refined.t) =
  match Base.compare a.base b.base with
  | 0 -> Refine.compare_structural a.refine b.refine
  | n -> n

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
  lowers = merge_uniq compare_refined a.lowers b.lowers;
  uppers = merge_uniq compare_refined a.uppers b.uppers;
  preds = merge_uniq Pred.compare a.preds b.preds;
}

(** The refinement the variable takes once its base is resolved to [base].

    Lower bounds sitting on a strictly smaller base do not pass their own
    refinement up, and they widen a value set to nothing: whatever those values
    are, they are not known to be among the constructors. A capacity is left
    alone. The result is checked against [Refined.leq] before it is returned, so
    this function cannot disagree with the order. *)
let resolve_refine base info =
  let lowers = List.map (fun (l : Refined.t) -> l.base, l.refine) info.lowers in
  let uppers_here = List.filter_map (fun (u : Refined.t) ->
    if Base.equal u.base base then Some u.refine else None) info.uppers in
  (* Value sets and capacities accumulate differently. A lower that says
     nothing about its values makes the result's value set unknown, wherever it
     sits; but it says nothing about capacity either, and an Int landing in a
     Decimal does not make that decimal unbounded. *)
  let value_sets = List.filter_map (fun (_, r) -> if Refine.is_value_set r then Some r else None) lowers in
  let capacities = List.filter_map (fun (_, r) ->
    match r with Refine.Dec _ -> Some r | Top | Enum _ | Flt _ -> None) lowers in
  let unknown_values = List.exists (fun (_, r) -> Refine.is_top r) lowers in
  let unknown_capacity =
    List.exists (fun (b, r) -> Refine.is_top r && Base.equal b base) lowers in
  (* a capacity wins when both are present: a literal feeding a decimal column
     contributes the requirement that it fit, not its own exact value *)
  let lo =
    match capacities, value_sets with
    | [], [] -> None
    | _ :: _, _ -> if unknown_capacity then Some Refine.Top else Refine.join_all capacities
    | [], _ :: _ -> if unknown_values then Some Refine.Top else Refine.join_all value_sets
  in
  let candidate =
    match lo, Refine.meet_all uppers_here with
    | _, `Conflict -> None
    | None, `None -> Some Refine.Top
    | Some r, `None -> Some r
    | None, `Some r -> Some r
    | Some l, `Some u -> if Refine.leq l u then Some l else None
  in
  match candidate with
  | None -> `Conflict
  | Some r when not (Refine.fits base r) -> `Conflict
  | Some r ->
    let t = Refined.make base r in
    if List.for_all (fun l -> Refined.leq l t) info.lowers
       && List.for_all (fun u -> Refined.leq t u) info.uppers
    then `Ok r
    else `Conflict

(** every base the variable could still take *)
let candidates info =
  List.filter (fun b ->
    List.for_all (fun (l : Refined.t) -> Base.leq l.base b) info.lowers
    && List.for_all (fun (u : Refined.t) -> Base.leq b u.base) info.uppers
    && List.for_all (fun p -> Pred.satisfies p b) info.preds
    && (match resolve_refine b info with `Ok _ -> true | `Conflict -> false))
    Base.all

(** [Num_lit] says "an unsuffixed numeric literal", which is a position in the
    lattice, not a type anything can be. Whatever else is decided, the answer
    handed out is the dialect's default for such a literal. *)
let settle (t : Refined.t) =
  (* the literal itself does not survive: it is a value set, and a value set
     does not cross a widening of the base *)
  match t.base with
  | Base.Num_lit -> Refined.of_base Base.Float
  | Base.Str_lit -> Refined.make Base.Text t.refine
  | _ -> t

(** §8, refined: the choice is made over the set of bases that are still
    feasible, so a predicate can never be violated by defaulting. *)
let pick ?fallback info =
  match candidates info with
  | [] -> Error (Printf.sprintf "no type satisfies %s" (show_info info))
  | cands ->
    let mem b = List.exists (Base.equal b) cands in
    let least = Base.lub (List.map (fun (l : Refined.t) -> l.base) info.lowers) in
    let greatest = Base.glb (List.map (fun (u : Refined.t) -> u.base) info.uppers) in
    let from_preds =
      List.fold_left (fun acc p ->
        match acc with
        | Some _ -> acc
        | None -> (match Pred.default p with Some b when mem b -> Some b | Some _ | None -> None))
        None info.preds
    in
    let chosen =
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
            | _ -> (match fallback with Some b when mem b -> Some b | Some _ | None -> None)
    in
    match chosen with
    | None ->
      Error (Printf.sprintf "cannot infer type: %s is satisfied by %s"
               (show_info info) (String.concat ", " (List.map Base.show cands)))
    | Some base ->
      match resolve_refine base info with
      | `Conflict -> Error (Printf.sprintf "no type satisfies %s" (show_info info))
      | `Ok refine -> Ok (settle (Refined.make base refine))

(** is [info] satisfiable at all? *)
let feasible info = candidates info <> []
