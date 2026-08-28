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

(** Raised inside a stage and caught once at its boundary. A stage stays a
    total function; threading [(unit, error) result] through every unification
    just to rebuild it at the end is noise. *)
exception Conflict of string

let conflict fmt = ksprintf (fun msg -> raise (Conflict msg)) fmt

module Base = struct

  type t =
    | Int
    | UInt64
    | Num_lit   (** an unsuffixed numeric literal: 1.5 is neither float nor
                    decimal until something says which *)
    | Float
    | Decimal
    | Bool
    | Str_lit  (** a string literal: below every stringable type, and which one
                   it may become is decided by validating its content *)
    | Datetime
    | Text
    | Blob
    | Json
    | Json_path
    | One_or_all
    [@@deriving eq, ord, show { with_path = false }, enumerate]

  (* written out rather than derived: keeps the function total and warning-8 checked *)
  let index = function
    | Int -> 0 | UInt64 -> 1 | Num_lit -> 2 | Float -> 3 | Decimal -> 4 | Bool -> 5
    | Str_lit -> 6 | Datetime -> 7 | Text -> 8 | Blob -> 9 | Json -> 10 | Json_path -> 11
    | One_or_all -> 12

  let count = List.length all

  (** Immediate coercion edges, transcribed from {!Sql.Type.order_kind}.
      [(a, b)] reads "a is implicitly coercible to b".

      {!Sql.Type.order_kind} is not transitive: it relates [Int] to [Datetime]
      and [Datetime] to [Text], but answers [`No] for [Int] against [Text].
      A subtyping domain needs a preorder, so this table is closed transitively
      below and the edges that only exist in the closure are exposed as
      {!derived} — a dialect may then refuse them, but the solver stays sound. *)
  let declared = [
    (* a numeric literal sits below both, which is exactly why Float and
       Decimal can stay incomparable while [decimal_col > 1.5] still types *)
    Int, Num_lit;
    Num_lit, Float;
    Num_lit, Decimal;
    Int, Float;
    Int, Decimal;
    Int, UInt64;
    Int, Datetime;
    (* a literal can become any of these, subject to its content validating *)
    Str_lit, Datetime;
    Str_lit, Json;
    Str_lit, Json_path;
    Str_lit, One_or_all;
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

  (** [closed] records that the constructor set came from a declared ENUM
      rather than from literals. It is a provenance tag, not part of the order:
      it takes no part in [leq] or [equal], because making it an ordering axis
      is what broke antisymmetry. It rides along so the declared type can be
      reconstructed, and codegen ignores it anyway. *)
  type enum = { ctors : Ctors.t; closed : bool } [@@deriving ord]

  type t =
    | Top
    | Enum of enum      (** a set of string constructors; a bare literal is a singleton *)
    | Dec of dec
    | Flt of float
    [@@deriving ord]

  (** structural, including the provenance tag: used to key sets of bounds so
      that accumulation stays order independent *)
  let compare_structural = compare

  let equal a b =
    match a, b with
    | Top, Top -> true
    | Enum a, Enum b -> Ctors.equal a.ctors b.ctors
    | Dec a, Dec b -> equal_dec a b
    | Flt a, Flt b -> Float.equal a b
    | (Top | Enum _ | Dec _ | Flt _), _ -> false

  let decimal ~precision ~scale =
    match precision, scale with
    | Some p, Some s -> Dec { int_digits = Some (p - s); scale = Some s }
    | Some p, None -> Dec { int_digits = Some p; scale = Some 0 }   (* DECIMAL(p) is DECIMAL(p,0) *)
    | None, s -> Dec { int_digits = None; scale = s }

  let precision_of d = match d.int_digits, d.scale with Some i, Some s -> Some (i + s) | _ -> None
  let literal s = Enum { ctors = Ctors.singleton s; closed = false }
  let enum ?(closed = false) l = Enum { ctors = Ctors.of_list l; closed }

  let show = function
    | Top -> "_"
    | Enum { ctors; closed } -> Ctors.show ctors ^ (if closed then "" else "..")
    | Dec d ->
      sprintf "(%s,%s)"
        (match precision_of d with Some p -> string_of_int p | None -> "_")
        (match d.scale with Some s -> string_of_int s | None -> "_")
    | Flt f -> sprintf "%g" f

  let pp fmt t = Format.pp_print_string fmt (show t)
  let is_top = function Top -> true | Enum _ | Dec _ | Flt _ -> false

  (** a declared ENUM: it accepts no constructor beyond the ones it lists *)
  let is_closed_enum = function Enum { closed; _ } -> closed | Top | Dec _ | Flt _ -> false

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
    | Enum a, Enum b -> Ctors.subset a.ctors b.ctors
    | Dec a, Dec b -> le_opt a.scale b.scale && le_opt a.int_digits b.int_digits
    | Flt a, Flt b -> Float.equal a b
    (* the old check_exact_exact_number: an exact numeric literal is below a
       decimal exactly when it fits *)
    | Flt f, Dec d ->
      (match d.int_digits, d.scale with
       | Some i, Some s ->
         let max = (10. ** float_of_int i) -. (10. ** (-. float_of_int s)) in
         f >= -. max && f <= max
       | _ -> true)
    | (Enum _ | Dec _ | Flt _), _ -> false

  (** Total: refinements of different kinds sit on different base types, so
      their only common upper bound is the absence of a refinement. Keeping
      [join] total is what makes it associative, and the solver relies on that
      when it folds a set of lower bounds. *)
  let join a b =
    match a, b with
    | Top, _ | _, Top -> Top
    (* closedness is sticky: a declared enum joined with one of its own
       literals is still that declared enum *)
    | Enum a, Enum b -> Enum { ctors = Ctors.union a.ctors b.ctors; closed = a.closed || b.closed }
    | Dec a, Dec b ->
      Dec { int_digits = max_opt a.int_digits b.int_digits; scale = max_opt a.scale b.scale }
    | Flt a, Flt b -> if Float.equal a b then Flt a else Top
    | (Enum _ | Dec _ | Flt _), _ -> Top

  let meet a b =
    match a, b with
    | Top, x | x, Top -> Some x
    | Enum a, Enum b ->
      let c = Ctors.inter a.ctors b.ctors in
      if Ctors.is_empty c then None else Some (Enum { ctors = c; closed = a.closed || b.closed })
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
    | Enum _ -> (match b with Str_lit | Text | Blob | Datetime | Json | Json_path | One_or_all -> true
                            | Int | UInt64 | Num_lit | Float | Decimal | Bool -> false)
    | Dec _ -> Base.equal b Base.Decimal
    | Flt _ -> Base.equal b Base.Float || Base.equal b Base.Num_lit
end

module Null = struct
  type t = NotNull | Nullable [@@deriving eq, ord, show { with_path = false }, enumerate]

end

module Pred = struct
  type t = Num | Ord | Comparable | Stringable | Aggregatable
    [@@deriving eq, ord, show { with_path = false }, enumerate]

  let satisfies p (b : Base.t) =
    match p, b with
    | Num, (Int | UInt64 | Num_lit | Float | Decimal) -> true
    | Num, (Bool | Str_lit | Datetime | Text | Blob | Json | Json_path | One_or_all) -> false
    | Ord, (Int | UInt64 | Num_lit | Float | Decimal | Bool | Str_lit | Datetime | Text | Blob) -> true
    | Ord, (Json | Json_path | One_or_all) -> false
    | Comparable, _ -> true
    | Stringable, (Str_lit | Text | Blob | Datetime | Json | Json_path | One_or_all) -> true
    | Stringable, (Int | UInt64 | Num_lit | Float | Decimal | Bool) -> false
    | Aggregatable, (Int | UInt64 | Num_lit | Float | Decimal | Bool | Str_lit | Datetime | Text | Blob) -> true
    | Aggregatable, (Json | Json_path | One_or_all) -> false

  (* §4.3 wants these predicates convex — not intervals, [Num] is not one —
     because that is what makes them decidable from the bounds. The tests
     check it. *)

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
  (** Whether a refinement's content is acceptable at a base. This is where
      the old [order_kind]'s scattered literal checks live: a string literal
      may become a JSON path only if it parses as one. *)
  let valid_at (b : Base.t) (r : Refine.t) =
    match b, r with
    | Base.Json_path, Refine.Enum { ctors; _ } ->
      Refine.Ctors.for_all Sqlgg_json_path.Json_path.is_valid ctors
    | Base.One_or_all, Refine.Enum { ctors; _ } ->
      Refine.Ctors.for_all
        (fun s -> List.mem (String.lowercase_ascii s) [ "one"; "all" ]) ctors
    | Base.Json, Refine.Enum { ctors; _ } ->
      Refine.Ctors.for_all
        (fun s -> match Yojson.Safe.from_string s with
           | _ -> true
           | exception Yojson.Json_error _ -> false) ctors
    | _ -> true

  (* An unrefined value says nothing about which values it holds, so it can
     only sit below a capacity, never below a value set: an Int fits a
     Decimal(10,2), but a Datetime is not one of an enum's constructors. A
     refined value carries its own contents up and is compared directly. *)
  let leq a b =
    Base.leq a.base b.base
    && (if Refine.is_top a.refine then not (Refine.is_value_set b.refine)
        else Refine.leq a.refine b.refine)
    && valid_at b.base a.refine
end
