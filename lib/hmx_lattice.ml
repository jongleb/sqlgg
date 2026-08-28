(** Domains of the constraint language X for the HM(X) core.

    Four independent lattices:
    - [Base]   — a finite skeleton of SQL base types, ordered by implicit coercion.
    - [Refine] — refinements indexed by a base type (decimal precision, enum
                 constructor sets, string/float literals). Not finite, which is
                 exactly why it is kept out of [Base].
    - [Null]   — the two-point nullability lattice.
    - [Pred]   — qualified-type predicates over [Base].

    The split matters: every argument that needs a finite domain (confluence of
    the solver, termination of the nullability worklist, coherence by
    enumeration) is made about [Base] alone. [Refine] carries the parts of
    {!Sql.Type.kind} that are parameterised and therefore unbounded. *)

open Printf

module Base = struct

  type t =
    | Int
    | UInt64
    | Float
    | Decimal
    | Bool
    | Datetime
    | Text
    | Blob
    | Json
    | Json_path
    | One_or_all
    [@@deriving eq, ord, show { with_path = false }, enumerate]

  (* written out rather than derived: keeps the function total and warning-8 checked *)
  let index = function
    | Int -> 0 | UInt64 -> 1 | Float -> 2 | Decimal -> 3 | Bool -> 4 | Datetime -> 5
    | Text -> 6 | Blob -> 7 | Json -> 8 | Json_path -> 9 | One_or_all -> 10

  let count = List.length all

  (** Immediate coercion edges, transcribed from {!Sql.Type.order_kind}.
      [(a, b)] reads "a is implicitly coercible to b".

      {!Sql.Type.order_kind} is not transitive: it relates [Int] to [Datetime]
      and [Datetime] to [Text], but answers [`No] for [Int] against [Text].
      A subtyping domain needs a preorder, so this table is closed transitively
      below and the edges that only exist in the closure are exposed as
      {!derived} — a dialect may then refuse them, but the solver stays sound. *)
  let declared = [
    Int, Float;
    Int, Decimal;
    Int, UInt64;
    Int, Datetime;
    Datetime, Text;
    Text, Blob;
    Json, Text;
    Json, Blob;
    Json_path, Text;
    One_or_all, Text;
  ]

  let matrix =
    let m = Array.make_matrix count count false in
    List.iter (fun i -> m.(i).(i) <- true) (List.init count (fun i -> i));
    List.iter (fun (a, b) -> m.(index a).(index b) <- true) declared;
    (* Floyd-Warshall over 11 nodes *)
    for k = 0 to count - 1 do
      for i = 0 to count - 1 do
        for j = 0 to count - 1 do
          if m.(i).(k) && m.(k).(j) then m.(i).(j) <- true
        done
      done
    done;
    m

  let leq a b = matrix.(index a).(index b)

  (** pairs related only through the transitive closure *)
  let derived =
    List.filter (fun (a, b) ->
      not (equal a b)
      && leq a b
      && not (List.exists (fun (x, y) -> equal x a && equal y b) declared))
      (List.concat_map (fun a -> List.map (fun b -> a, b) all) all)

  (** did [a <= b] hold only because the table was closed transitively?
      {!Sql.Type.order_kind} answers [`No] for these pairs, so a dialect that
      wants to keep refusing them needs to see them. *)
  let is_derived a b = List.exists (fun (x, y) -> equal x a && equal y b) derived

  let upper_bounds l = List.filter (fun c -> List.for_all (fun a -> leq a c) l) all
  let lower_bounds l = List.filter (fun c -> List.for_all (fun a -> leq c a) l) all

  let least = function
    | [] -> None
    | l -> (match List.filter (fun m -> List.for_all (leq m) l) l with [ m ] -> Some m | _ -> None)

  let greatest = function
    | [] -> None
    | l -> (match List.filter (fun m -> List.for_all (fun x -> leq x m) l) l with [ m ] -> Some m | _ -> None)

  (** lub/glb of a whole set, not a pairwise fold: on a poset with a partial
      join the fold is order dependent (lub {a,b,c} may exist while lub {a,b}
      does not), and the solver must not depend on constraint order. *)
  let lub l = least (upper_bounds l)
  let glb l = greatest (lower_bounds l)

  let join a b = lub [ a; b ]
  let meet a b = glb [ a; b ]

  module Check = struct
    type failure =
      | Not_antisymmetric of t * t
      | Join_not_unique of t * t * t list
      | Meet_not_unique of t * t * t list
      [@@deriving show { with_path = false }]

    let minimal l = List.filter (fun m -> not (List.exists (fun x -> not (equal x m) && leq x m) l)) l
    let maximal l = List.filter (fun m -> not (List.exists (fun x -> not (equal x m) && leq m x) l)) l

    let laws () =
      let pairs = List.concat_map (fun a -> List.map (fun b -> a, b) all) all in
      List.concat_map (fun (a, b) ->
        let anti = if leq a b && leq b a && not (equal a b) then [ Not_antisymmetric (a, b) ] else [] in
        let ubs = minimal (upper_bounds [ a; b ]) in
        let lbs = maximal (lower_bounds [ a; b ]) in
        let j = match ubs with [] | [ _ ] -> [] | l -> [ Join_not_unique (a, b, l) ] in
        let m = match lbs with [] | [ _ ] -> [] | l -> [ Meet_not_unique (a, b, l) ] in
        anti @ j @ m) pairs
  end
end

module Refine = struct

  module Ctors = struct
    include Set.Make(String)
    let show s = sprintf "{%s}" (String.concat "|" (elements s))
    let pp fmt s = Format.pp_print_string fmt (show s)
  end

  (** [None] means unspecified, i.e. the top of that component.
      Precision is stored split, as digits left of the point plus scale: that
      is the pair the order is componentwise in, and [DECIMAL(7)] and
      [DECIMAL(7,0)] then normalise to the same value. *)
  type dec = { int_digits : int option; scale : int option } [@@deriving eq, ord]

  type t =
    | Top
    | Enum of Ctors.t   (** a set of string constructors; a bare literal is a singleton *)
    | Dec of dec
    | Flt of float
    [@@deriving eq, ord]

  let decimal ~precision ~scale =
    match precision, scale with
    | Some p, Some s -> Dec { int_digits = Some (p - s); scale = Some s }
    | Some p, None -> Dec { int_digits = Some p; scale = Some 0 }   (* DECIMAL(p) is DECIMAL(p,0) *)
    | None, s -> Dec { int_digits = None; scale = s }

  let precision_of d = match d.int_digits, d.scale with Some i, Some s -> Some (i + s) | _ -> None
  let literal s = Enum (Ctors.singleton s)
  let enum l = Enum (Ctors.of_list l)

  let show = function
    | Top -> "_"
    | Enum ctors -> Ctors.show ctors
    | Dec d ->
      sprintf "(%s,%s)"
        (match precision_of d with Some p -> string_of_int p | None -> "_")
        (match d.scale with Some s -> string_of_int s | None -> "_")
    | Flt f -> sprintf "%g" f

  let pp fmt t = Format.pp_print_string fmt (show t)
  let is_top = function Top -> true | Enum _ | Dec _ | Flt _ -> false

  (** Two sorts of refinement, and they behave differently when the base widens.
      A {e value set} says which values the type has, so a value arriving from a
      smaller base destroys it: [COALESCE(enum_col, datetime_col)] is plain text.
      A {e capacity} only bounds how much fits, and widening [Int] into
      [Decimal(10,2)] neither needs nor changes it. *)
  let is_value_set = function Enum _ | Flt _ -> true | Top | Dec _ -> false

  let le_opt x y = match x, y with _, None -> true | None, Some _ -> false | Some a, Some b -> a <= b
  let max_opt x y = match x, y with None, _ | _, None -> None | Some a, Some b -> Some (max a b)
  let min_opt x y = match x, y with None, o | o, None -> o | Some a, Some b -> Some (min a b)

  let leq a b =
    match a, b with
    | _, Top -> true
    | Top, (Enum _ | Dec _ | Flt _) -> false
    | Enum a, Enum b -> Ctors.subset a b
    | Dec a, Dec b -> le_opt a.scale b.scale && le_opt a.int_digits b.int_digits
    | Flt a, Flt b -> Float.equal a b
    | (Enum _ | Dec _ | Flt _), _ -> false

  (** Total: refinements of different kinds sit on different base types, so
      their only common upper bound is the absence of a refinement. Keeping
      [join] total is what makes it associative, and the solver relies on that
      when it folds a set of lower bounds. *)
  let join a b =
    match a, b with
    | Top, _ | _, Top -> Top
    | Enum a, Enum b -> Enum (Ctors.union a b)
    | Dec a, Dec b ->
      Dec { int_digits = max_opt a.int_digits b.int_digits; scale = max_opt a.scale b.scale }
    | Flt a, Flt b -> if Float.equal a b then Flt a else Top
    | (Enum _ | Dec _ | Flt _), _ -> Top

  let meet a b =
    match a, b with
    | Top, x | x, Top -> Some x
    | Enum a, Enum b -> let c = Ctors.inter a b in if Ctors.is_empty c then None else Some (Enum c)
    | Dec a, Dec b ->
      Some (Dec { int_digits = min_opt a.int_digits b.int_digits; scale = min_opt a.scale b.scale })
    | Flt a, Flt b -> if Float.equal a b then Some (Flt a) else None
    | (Enum _ | Dec _ | Flt _), _ -> None

  let join_all = function [] -> None | x :: l -> Some (List.fold_left join x l)

  let meet_all = function
    | [] -> `None
    | x :: l ->
      List.fold_left (fun acc y ->
        match acc with `Some a -> (match meet a y with Some r -> `Some r | None -> `Conflict) | acc -> acc)
        (`Some x) l

  (** which base a refinement may sit on; [Top] fits any *)
  let fits (b : Base.t) = function
    | Top -> true
    | Enum _ -> (match b with Text | Blob | Datetime | Json | Json_path | One_or_all -> true
                            | Int | UInt64 | Float | Decimal | Bool -> false)
    | Dec _ -> Base.equal b Base.Decimal
    | Flt _ -> Base.equal b Base.Float
end

module Null = struct
  type t = NotNull | Nullable [@@deriving eq, ord, show { with_path = false }, enumerate]

  let leq a b = match a, b with NotNull, _ -> true | Nullable, Nullable -> true | Nullable, NotNull -> false
  let join a b = match a, b with Nullable, _ | _, Nullable -> Nullable | NotNull, NotNull -> NotNull
  let meet a b = match a, b with NotNull, _ | _, NotNull -> NotNull | Nullable, Nullable -> Nullable
  let join_all = List.fold_left join NotNull
  let meet_all = List.fold_left meet Nullable
end

module Pred = struct
  type t = Num | Ord | Comparable | Stringable | Aggregatable
    [@@deriving eq, ord, show { with_path = false }, enumerate]

  let satisfies p (b : Base.t) =
    match p, b with
    | Num, (Int | UInt64 | Float | Decimal) -> true
    | Num, (Bool | Datetime | Text | Blob | Json | Json_path | One_or_all) -> false
    | Ord, (Int | UInt64 | Float | Decimal | Bool | Datetime | Text | Blob) -> true
    | Ord, (Json | Json_path | One_or_all) -> false
    | Comparable, _ -> true
    | Stringable, (Text | Blob | Datetime | Json | Json_path | One_or_all) -> true
    | Stringable, (Int | UInt64 | Float | Decimal | Bool) -> false
    | Aggregatable, (Int | UInt64 | Float | Decimal | Bool | Datetime | Text | Blob) -> true
    | Aggregatable, (Json | Json_path | One_or_all) -> false

  let members p = List.filter (satisfies p) Base.all

  (** §4.3 asks for convexity, not for an interval: [Num] is not an interval
      ([Float] and [Decimal] are incomparable) yet it is convex, and convexity
      is what makes a predicate decidable from the bounds. *)
  let is_convex p =
    List.for_all (fun x ->
      List.for_all (fun y ->
        List.for_all (fun z ->
          not (satisfies p x && satisfies p y && Base.leq x z && Base.leq z y) || satisfies p z)
          Base.all)
        Base.all)
      Base.all

  (** predicate defaults used when a variable is otherwise unconstrained (§8) *)
  let default p : Base.t option =
    match p with
    | Num -> Some Base.Int
    | Ord | Comparable -> None
    | Stringable -> Some Base.Text
    | Aggregatable -> None
end

(** a base type together with its refinement *)
module Refined = struct
  type t = { base : Base.t; refine : Refine.t } [@@deriving eq, ord]

  let make base refine = { base; refine }
  let of_base base = { base; refine = Refine.Top }
  let show { base; refine } =
    if Refine.is_top refine then Base.show base else sprintf "%s%s" (Base.show base) (Refine.show refine)
  let pp fmt t = Format.pp_print_string fmt (show t)

  (* a refinement does not survive a widening of the base *)
  (* Across a widening of the base only a capacity survives on the right: an
     Int may land in a Decimal(10,2), but a Datetime is not one of an enum's
     constructors. *)
  let leq a b =
    Base.leq a.base b.base
    && (if Base.equal a.base b.base then Refine.leq a.refine b.refine
        else not (Refine.is_value_set b.refine))

  let well_formed { base; refine } = Refine.fits base refine
end
