(** Unit and property tests for the HM(X) core (stage 3 of the migration).

    The property tests are the point: the solver is only useful downstream if
    its answer does not depend on the order constraints happen to be generated
    in, and that is not obvious from the code. *)

open Printf
open OUnit
open Sqlgg
open Hmx_lattice

let qcheck (QCheck2.Test.Test cell) =
  QCheck2.Test.get_name cell >:: (fun () ->
    try QCheck2.Test.check_cell_exn ~rand:(Random.State.make [| 42 |]) cell
    with QCheck2.Test.Test_fail (_, msgs) -> assert_failure (String.concat "\n" msgs))

let any_of l = QCheck.Gen.(oneof (List.map return l))

(* ---------------------------------------------------------------- Base *)

let show_pair (a, b) = sprintf "%s<=%s" (Base.show a) (Base.show b)

let test_base_lattice = [

  "declared edges are a partial order" >:: (fun () ->
    assert_equal ~msg:"lattice law violations"
      ~printer:(fun l -> String.concat "\n" (List.map Base.Check.show_failure l))
      [] (Base.Check.laws ()));

  "reflexive and transitive" >:: (fun () ->
    List.iter (fun a -> assert_bool (Base.show a) (Base.leq a a)) Base.all;
    List.iter (fun a -> List.iter (fun b -> List.iter (fun c ->
      if Base.leq a b && Base.leq b c then
        assert_bool (sprintf "%s %s %s" (Base.show a) (Base.show b) (Base.show c)) (Base.leq a c))
      Base.all) Base.all) Base.all);

  (* Frozen on purpose. Sql.Type.order_kind is not transitive, so closing it
     invents relations nobody wrote down; any change to Base.declared must
     re-confirm this list rather than grow it silently. *)
  "edges invented by the transitive closure" >:: (fun () ->
    assert_equal ~msg:"derived edges" ~printer:(fun l -> String.concat " " (List.map show_pair l))
      [ Base.Int, Base.Text; Base.Int, Base.Blob; Base.Datetime, Base.Blob; Base.Json_path, Base.Blob;
        Base.One_or_all, Base.Blob ]
      Base.derived);

  "incomparable pairs stay incomparable" >:: (fun () ->
    let no a b = assert_bool (show_pair (a, b)) (not (Base.leq a b) && not (Base.leq b a)) in
    no Base.Float Base.Decimal;
    no Base.UInt64 Base.Float;
    no Base.Bool Base.Int;
    no Base.Json Base.Datetime);

  "join and meet agree with the order" >:: (fun () ->
    List.iter (fun a -> List.iter (fun b ->
      (match Base.join a b with
       | None -> ()
       | Some j -> assert_bool (show_pair (a, j)) (Base.leq a j && Base.leq b j));
      (match Base.meet a b with
       | None -> ()
       | Some m -> assert_bool (show_pair (m, a)) (Base.leq m a && Base.leq m b)))
      Base.all) Base.all);

  "join of a set is not the pairwise fold" >:: (fun () ->
    (* the reason bounds are kept as sets: folding a partial join is order dependent *)
    assert_equal ~msg:"Int Float" (Some Base.Float) (Base.join Base.Int Base.Float);
    assert_equal ~msg:"Float Decimal" None (Base.join Base.Float Base.Decimal);
    assert_equal ~msg:"lub of the whole set" None (Base.lub [ Base.Int; Base.Float; Base.Decimal ]));
]

let test_pred = [
  "predicates are convex" >:: (fun () ->
    List.iter (fun p ->
      assert_bool (sprintf "%s is not convex over the base lattice" (Pred.show p)) (Pred.is_convex p))
      Pred.all);

  "predicate membership" >:: (fun () ->
    assert_equal ~printer:(fun l -> String.concat "," (List.map Base.show l))
      Base.[ Int; UInt64; Float; Decimal ] (Pred.members Pred.Num));
]

(* -------------------------------------------------------------- Refine *)

let arb_refine =
  let open QCheck.Gen in
  let g = oneof [
    return Refine.Top;
    map Refine.enum (list_size (int_range 1 3) (any_of [ "a"; "b"; "c" ]));
    map2 (fun precision scale -> Refine.decimal ~precision ~scale)
      (option (int_range 4 12)) (option (int_range 0 4));
    map (fun f -> Refine.Flt f) (any_of [ 0.; 1.5 ]);
  ] in
  QCheck.make ~print:Refine.show g

let same_refine a b = match a, b with
  | None, None -> true
  | Some a, Some b -> Refine.equal a b
  | None, Some _ | Some _, None -> false

let test_refine_laws = List.map qcheck [
  QCheck.Test.make ~count:3000 ~name:"refine leq is reflexive" arb_refine
    (fun a -> Refine.leq a a);
  QCheck.Test.make ~count:3000 ~name:"refine leq is antisymmetric"
    (QCheck.pair arb_refine arb_refine)
    (fun (a, b) -> not (Refine.leq a b && Refine.leq b a) || Refine.equal a b);
  QCheck.Test.make ~count:5000 ~name:"refine leq is transitive"
    (QCheck.triple arb_refine arb_refine arb_refine)
    (fun (a, b, c) -> not (Refine.leq a b && Refine.leq b c) || Refine.leq a c);
  QCheck.Test.make ~count:3000 ~name:"refine join is commutative"
    (QCheck.pair arb_refine arb_refine)
    (fun (a, b) -> Refine.equal (Refine.join a b) (Refine.join b a));
  QCheck.Test.make ~count:3000 ~name:"refine join is idempotent" arb_refine
    (fun a -> Refine.equal (Refine.join a a) a);
  QCheck.Test.make ~count:5000 ~name:"refine join is associative"
    (QCheck.triple arb_refine arb_refine arb_refine)
    (fun (a, b, c) -> Refine.equal (Refine.join (Refine.join a b) c) (Refine.join a (Refine.join b c)));
  QCheck.Test.make ~count:3000 ~name:"refine join is the least upper bound"
    (QCheck.pair arb_refine arb_refine)
    (fun (a, b) ->
      let j = Refine.join a b in
      Refine.leq a j && Refine.leq b j);
  QCheck.Test.make ~count:3000 ~name:"refine meet is commutative"
    (QCheck.pair arb_refine arb_refine)
    (fun (a, b) -> same_refine (Refine.meet a b) (Refine.meet b a));
  QCheck.Test.make ~count:5000 ~name:"refine meet is associative"
    (QCheck.triple arb_refine arb_refine arb_refine)
    (fun (a, b, c) ->
      let l = match Refine.meet a b with None -> None | Some ab -> Refine.meet ab c in
      let r = match Refine.meet b c with None -> None | Some bc -> Refine.meet a bc in
      same_refine l r);
  QCheck.Test.make ~count:3000 ~name:"refine meet is a lower bound"
    (QCheck.pair arb_refine arb_refine)
    (fun (a, b) -> match Refine.meet a b with None -> true | Some m -> Refine.leq m a && Refine.leq m b);
  QCheck.Test.make ~count:3000 ~name:"refine absorption"
    (QCheck.pair arb_refine arb_refine)
    (fun (a, b) -> match Refine.meet a (Refine.join a b) with None -> false | Some m -> Refine.equal m a);
]

let test_refine_units = [
  "a literal is a singleton constructor set" >:: (fun () ->
    assert_equal ~printer:Refine.show (Refine.enum [ "a"; "b" ])
      (Refine.join (Refine.literal "a") (Refine.literal "b")));

  "constructor sets join by union" >:: (fun () ->
    assert_equal ~printer:Refine.show (Refine.enum [ "a"; "b"; "z" ])
      (Refine.join (Refine.enum [ "a"; "b" ]) (Refine.literal "z")));

  "constructor sets meet by intersection" >:: (fun () ->
    let show = function None -> "-" | Some r -> Refine.show r in
    assert_equal ~printer:show (Some (Refine.enum [ "b" ]))
      (Refine.meet (Refine.enum [ "a"; "b" ]) (Refine.enum [ "b"; "c" ]));
    assert_equal ~msg:"disjoint" ~printer:show None
      (Refine.meet (Refine.enum [ "a" ]) (Refine.enum [ "b" ])));

  "DECIMAL(p) normalises to DECIMAL(p,0)" >:: (fun () ->
    assert_equal ~printer:Refine.show
      (Refine.decimal ~precision:(Some 7) ~scale:(Some 0))
      (Refine.decimal ~precision:(Some 7) ~scale:None));

  (* Sql.Type.order_kind widens to Decimal(None, max scale), losing the
     precision entirely; keeping the integral digits is both tighter and
     sound, which is the §1 complaint about SUM over a decimal column. *)
  "decimal join keeps the integral digits" >:: (fun () ->
    let d p s = Refine.decimal ~precision:(Some p) ~scale:(Some s) in
    assert_equal ~printer:Refine.show (d 12 4) (Refine.join (d 10 2) (d 10 4));
    assert_bool "join dominates" (Refine.leq (d 10 2) (d 12 4) && Refine.leq (d 10 4) (d 12 4)));
]

(* -------------------------------------------------------------- solver *)

let base b = Hmx.Ty (Refined.of_base b)
let refined b r = Hmx.Ty (Refined.make b r)
let dec p s = Refine.decimal ~precision:(Some p) ~scale:(Some s)

let solve ?policy l = Hmx.solve ?policy (Hmx.Conj l)

let assert_base ?policy ~msg expect l v =
  match solve ?policy l with
  | Error e -> assert_failure (sprintf "%s: %s" msg (Hmx.show_error e))
  | Ok sol ->
    match Hmx.base_of sol v with
    | Error e -> assert_failure (sprintf "%s: %s" msg (Hmx.show_error e))
    | Ok t -> assert_equal ~msg ~printer:Refined.show expect t

let assert_fails ~msg l v =
  match solve l with
  | Error _ -> ()
  | Ok sol ->
    match Hmx.base_of sol v with
    | Error _ -> ()
    | Ok t -> assert_failure (sprintf "%s: expected failure, got %s" msg (Refined.show t))

let test_solver = [

  (* §1: the motivating regression. SUM over a decimal column must keep the
     column's precision instead of collapsing to a join. *)
  "SUM keeps the argument type" >:: (fun () ->
    assert_base ~msg:"sum(decimal(10,2))" (Refined.make Base.Decimal (dec 10 2))
      [ Hmx.Has (Pred.Num, Hmx.Var 0); Hmx.Sub (refined Base.Decimal (dec 10 2), Hmx.Var 0) ] 0);

  "mixed numeric arguments take the lub" >:: (fun () ->
    assert_base ~msg:"int + float" (Refined.of_base Base.Float)
      [ Hmx.Has (Pred.Num, Hmx.Var 0); Hmx.Sub (base Base.Int, Hmx.Var 0); Hmx.Sub (base Base.Float, Hmx.Var 0) ] 0);

  "incomparable numeric arguments are rejected" >:: (fun () ->
    assert_fails ~msg:"float + decimal"
      [ Hmx.Has (Pred.Num, Hmx.Var 0); Hmx.Sub (base Base.Float, Hmx.Var 0); Hmx.Sub (base Base.Decimal, Hmx.Var 0) ] 0);

  (* the predicate, not the bare bounds, is what rules this out: Int <= Datetime
     <= Text holds in the closure, so a lub exists but is not numeric *)
  "a predicate constrains defaulting, not just solving" >:: (fun () ->
    assert_fails ~msg:"num(int, text)"
      [ Hmx.Has (Pred.Num, Hmx.Var 0); Hmx.Sub (base Base.Int, Hmx.Var 0); Hmx.Sub (base Base.Text, Hmx.Var 0) ] 0);

  "an upper bound is taken as the type" >:: (fun () ->
    assert_base ~msg:"concat coerces to text" (Refined.of_base Base.Text)
      [ Hmx.Sub (Hmx.Var 0, base Base.Text); Hmx.Has (Pred.Stringable, Hmx.Var 0) ] 0);

  "a lone predicate defaults" >:: (fun () ->
    assert_base ~msg:"num" (Refined.of_base Base.Int) [ Hmx.Has (Pred.Num, Hmx.Var 0) ] 0;
    assert_base ~msg:"stringable" (Refined.of_base Base.Text) [ Hmx.Has (Pred.Stringable, Hmx.Var 0) ] 0);

  (* §8: Any is gone, so a parameter nothing constrains is an error unless the
     dialect opts into a fallback *)
  "an unconstrained variable cannot be inferred" >:: (fun () ->
    assert_fails ~msg:"bare ?" [ Hmx.Has (Pred.Comparable, Hmx.Var 0) ] 0);

  "a dialect may supply a fallback" >:: (fun () ->
    assert_base ~policy:{ Hmx.fallback_base = Some Base.Text; default_null = Null.Nullable }
      ~msg:"fallback" (Refined.of_base Base.Text) [ Hmx.Has (Pred.Comparable, Hmx.Var 0) ] 0);

  "a parameter takes the type of the column it is compared to" >:: (fun () ->
    let e = Refine.enum [ "a"; "b" ] in
    assert_base ~msg:"status = ?" (Refined.make Base.Text e)
      [ Hmx.Eq (Hmx.Var 0, Hmx.Var 1); Hmx.Eq (Hmx.Var 1, refined Base.Text e) ] 0);

  (* A declared ENUM is an upper bound, not a closedness flag inside the
     lattice: "does not accept more constructors" is exactly what an upper
     bound means, and the rejection then needs no rule of its own. *)
  "a foreign literal against a declared enum is rejected" >:: (fun () ->
    assert_fails ~msg:"status = 'typo'"
      [ Hmx.Sub (Hmx.Var 0, refined Base.Text (Refine.enum [ "a"; "b" ]));
        Hmx.Sub (refined Base.Text (Refine.literal "typo"), Hmx.Var 0) ] 0);

  "a member literal against a declared enum is accepted" >:: (fun () ->
    assert_base ~msg:"status = 'a'" (Refined.make Base.Text (Refine.literal "a"))
      [ Hmx.Sub (Hmx.Var 0, refined Base.Text (Refine.enum [ "a"; "b" ]));
        Hmx.Sub (refined Base.Text (Refine.literal "a"), Hmx.Var 0) ] 0);

  (* how stage 2 should encode a declared ENUM column: invariantly, so that the
     column type is both what a parameter picks up and what a literal is
     checked against *)
  "a declared enum column is invariant" >:: (fun () ->
    let e = Refine.enum [ "a"; "b" ] in
    assert_base ~msg:"status = 'a'" (Refined.make Base.Text e)
      [ Hmx.Eq (Hmx.Var 0, refined Base.Text e);
        Hmx.Sub (refined Base.Text (Refine.literal "a"), Hmx.Var 0) ] 0;
    assert_fails ~msg:"status = 'typo'"
      [ Hmx.Eq (Hmx.Var 0, refined Base.Text e);
        Hmx.Sub (refined Base.Text (Refine.literal "typo"), Hmx.Var 0) ] 0);

  "two literals widen to their union" >:: (fun () ->
    assert_base ~msg:"'a' or 'b'" (Refined.make Base.Text (Refine.enum [ "a"; "b" ]))
      [ Hmx.Sub (refined Base.Text (Refine.literal "a"), Hmx.Var 0);
        Hmx.Sub (refined Base.Text (Refine.literal "b"), Hmx.Var 0) ] 0);

  "a refinement does not survive widening of the base" >:: (fun () ->
    assert_base ~msg:"literal below blob" (Refined.of_base Base.Blob)
      [ Hmx.Sub (refined Base.Text (Refine.literal "a"), Hmx.Var 0); Hmx.Sub (base Base.Blob, Hmx.Var 0) ] 0);
]

let assert_null ~msg expect l v =
  match solve l with
  | Error e -> assert_failure (sprintf "%s: %s" msg (Hmx.show_error e))
  | Ok sol -> assert_equal ~msg ~printer:Null.show expect (Hmx.null_of sol v)

let test_nullability = [
  "join is nullable as soon as one argument is" >:: (fun () ->
    assert_null ~msg:"a + b" Null.Nullable
      [ Hmx.NJoin (Hmx.NVar 0, [ Hmx.N Null.NotNull; Hmx.N Null.Nullable ]) ] 0);

  "join of strict arguments is strict" >:: (fun () ->
    assert_null ~msg:"a + b" Null.NotNull
      [ Hmx.NJoin (Hmx.NVar 0, [ Hmx.N Null.NotNull; Hmx.N Null.NotNull ]) ] 0);

  (* COALESCE: the result is not null as soon as any branch is not null *)
  "meet is strict as soon as one argument is" >:: (fun () ->
    assert_null ~msg:"coalesce" Null.NotNull
      [ Hmx.NMeet (Hmx.NVar 0, [ Hmx.N Null.Nullable; Hmx.N Null.NotNull ]) ] 0);

  "an unknown argument is resolved by the fixpoint, whatever the order" >:: (fun () ->
    let cs = [ Hmx.NJoin (Hmx.NVar 0, [ Hmx.NVar 1; Hmx.N Null.NotNull ]);
               Hmx.NEq (Hmx.NVar 1, Hmx.NVar 2);
               Hmx.NEq (Hmx.NVar 2, Hmx.N Null.Nullable) ] in
    assert_null ~msg:"in order" Null.Nullable cs 0;
    assert_null ~msg:"reversed" Null.Nullable (List.rev cs) 0);

  (* the dual direction: a strict result forces strict arguments *)
  "a strict join result forces its arguments" >:: (fun () ->
    assert_null ~msg:"not null context" Null.NotNull
      [ Hmx.NEq (Hmx.NVar 0, Hmx.N Null.NotNull);
        Hmx.NJoin (Hmx.NVar 0, [ Hmx.NVar 1; Hmx.NVar 2 ]) ] 1);

  "a contradiction is reported" >:: (fun () ->
    match solve [ Hmx.NEq (Hmx.NVar 0, Hmx.N Null.NotNull);
                  Hmx.NJoin (Hmx.NVar 0, [ Hmx.N Null.Nullable ]) ] with
    | Error _ -> ()
    | Ok _ -> assert_failure "expected a nullability conflict");

  "an undetermined nullability defaults by policy" >:: (fun () ->
    assert_null ~msg:"free" Null.Nullable [] 0);
]

(* ------------------------------------------------------ confluence *)

let n_vars = 4

let arb_constraints =
  let open QCheck.Gen in
  let v = int_range 0 (n_vars - 1) in
  let refined =
    oneof [
      map Refined.of_base (any_of Base.all);
      map (fun s -> Refined.make Base.Text (Refine.literal s)) (any_of [ "a"; "b" ]);
      map (fun l -> Refined.make Base.Text (Refine.enum l))
        (list_size (int_range 1 2) (any_of [ "a"; "b"; "c" ]));
      map2 (fun p s -> Refined.make Base.Decimal (Refine.decimal ~precision:(Some p) ~scale:(Some s)))
        (int_range 4 12) (int_range 0 3);
    ] in
  let ty = oneof [ map (fun i -> Hmx.Var i) v; map (fun t -> Hmx.Ty t) refined ] in
  let nty = oneof [ map (fun i -> Hmx.NVar i) v; map (fun n -> Hmx.N n) (any_of Null.all) ] in
  let c = oneof [
    map2 (fun a b -> Hmx.Eq (a, b)) ty ty;
    map2 (fun a b -> Hmx.Sub (a, b)) ty ty;
    map2 (fun p a -> Hmx.Has (p, a)) (any_of Pred.all) ty;
    map2 (fun a b -> Hmx.NEq (a, b)) nty nty;
    map2 (fun a l -> Hmx.NJoin (a, l)) nty (list_size (int_range 1 3) nty);
    map2 (fun a l -> Hmx.NMeet (a, l)) nty (list_size (int_range 1 3) nty);
  ] in
  QCheck.make ~print:(fun l -> sprintf "%d constraints" (List.length l)) (list_size (int_range 1 6) c)

(** the observable substitution: only success/failure and the value, never the
    message, which legitimately depends on which conflict is hit first *)
let snapshot l =
  match solve l with
  | Error _ -> None
  | Ok sol ->
    Some (List.init n_vars (fun v ->
      (match Hmx.base_of sol v with Ok t -> Some (Refined.show t) | Error _ -> None),
      Null.show (Hmx.null_of sol v)))

let same_snapshot a b = match a, b with
  | None, None -> true
  | Some a, Some b -> a = b
  | None, Some _ | Some _, None -> false

let shuffle rand l =
  let a = Array.of_list l in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int rand (i + 1) in
    let t = a.(i) in a.(i) <- a.(j); a.(j) <- t
  done;
  Array.to_list a

let test_confluence = List.map qcheck [
  (* the property the whole design leans on: stage 2 may emit constraints in
     any order and stage 4 must still produce the same sigma *)
  QCheck.Test.make ~count:20000 ~name:"solving is confluent under reordering" arb_constraints
    (fun l ->
      let rand = Random.State.make [| List.length l; 7 |] in
      let a = snapshot l in
      List.for_all (fun _ -> same_snapshot a (snapshot (shuffle rand l))) [ (); (); () ]);

  QCheck.Test.make ~count:5000 ~name:"solving is idempotent under duplication" arb_constraints
    (fun l -> same_snapshot (snapshot l) (snapshot (l @ l)));

  QCheck.Test.make ~count:5000 ~name:"a solved variable satisfies its own constraints" arb_constraints
    (fun l ->
      match solve l with
      | Error _ -> true
      | Ok sol ->
        List.for_all (fun v ->
          match Hmx.base_of sol v with
          | Error _ -> true
          | Ok t ->
            let info = Hmx.info_of sol v in
            List.for_all (fun lo -> Refined.leq lo t) info.Hmx.lowers
            && List.for_all (fun up -> Refined.leq t up) info.Hmx.uppers
            && List.for_all (fun p -> Pred.satisfies p t.Refined.base) info.Hmx.preds)
          (List.init n_vars (fun i -> i)));
]

let tests = [
  "hmx_base_lattice" >::: test_base_lattice;
  "hmx_pred" >::: test_pred;
  "hmx_refine_laws" >::: test_refine_laws;
  "hmx_refine" >::: test_refine_units;
  "hmx_solver" >::: test_solver;
  "hmx_nullability" >::: test_nullability;
  "hmx_confluence" >::: test_confluence;
]
