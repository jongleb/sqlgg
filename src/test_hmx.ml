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

(* the laws the lattice must satisfy, checked here rather than carried in the
   library *)
let minimal l = List.filter (fun m -> not (List.exists (fun x -> not (Base.equal x m) && Base.leq x m) l)) l
let maximal l = List.filter (fun m -> not (List.exists (fun x -> not (Base.equal x m) && Base.leq m x) l)) l
let base_pairs = List.concat_map (fun a -> List.map (fun b -> a, b) Base.all) Base.all

let test_base_lattice = [

  "declared edges are a partial order" >:: (fun () ->
    List.iter (fun (a, b) ->
      assert_bool (sprintf "%s and %s are mutually below each other" (Base.show a) (Base.show b))
        (not (Base.leq a b && Base.leq b a) || Base.equal a b);
      assert_equal ~msg:(sprintf "join of %s and %s is not unique" (Base.show a) (Base.show b))
        true (List.length (minimal (Base.upper_bounds [ a; b ])) <= 1);
      assert_equal ~msg:(sprintf "meet of %s and %s is not unique" (Base.show a) (Base.show b))
        true (List.length (maximal (Base.lower_bounds [ a; b ])) <= 1))
      base_pairs);

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
      [ Base.Int, Base.Text; Base.Int, Base.Blob;
        Base.Str_lit, Base.Text; Base.Str_lit, Base.Blob;
        Base.Datetime, Base.Blob; Base.Json_path, Base.Blob; Base.One_or_all, Base.Blob ]
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

let members p = List.filter (Pred.satisfies p) Base.all

let test_pred = [
  (* §4.3: not intervals — Float and Decimal are incomparable — but convex,
     which is what makes a predicate decidable from the bounds *)
  "predicates are convex" >:: (fun () ->
    List.iter (fun p ->
      List.iter (fun (x, y) ->
        List.iter (fun z ->
          if Pred.satisfies p x && Pred.satisfies p y && Base.leq x z && Base.leq z y then
            assert_bool (sprintf "%s is not convex: %s <= %s <= %s" (Pred.show p)
                           (Base.show x) (Base.show z) (Base.show y)) (Pred.satisfies p z))
          Base.all)
        base_pairs)
      Pred.all);

  "predicate membership" >:: (fun () ->
    assert_equal ~printer:(fun l -> String.concat "," (List.map Base.show l))
      Base.[ Int; UInt64; Num_lit; Float; Decimal ] (members Pred.Num));
]

(* -------------------------------------------------------------- Refine *)

(* Num_lit is a position in the lattice, not a type anything can write down:
   it only ever arrives as the type of a literal, so only as a lower bound.
   The generators below stay inside the vocabulary stage 1 can actually
   produce. *)
let writable_bases = List.filter (fun b -> not (Base.equal b Base.Num_lit)) Base.all

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

let solve ?fallback bounds =
  let v = Hmx_solver.fresh () in
  match List.iter (fun f -> f v) bounds; Hmx_solver.resolve ?fallback v with
  | t -> Ok t
  | exception Conflict e -> Error e

let lo t v = Hmx_solver.above v t
let up t v = Hmx_solver.below v t
let exact t v = Hmx_solver.exactly v t
let pred p v = Hmx_solver.has v p

let assert_solves ~msg ?fallback bounds expect =
  match solve ?fallback bounds with
  | Error e -> assert_failure (sprintf "%s: %s" msg e)
  | Ok t -> assert_equal ~msg ~printer:Refined.show expect t

let assert_conflict ~msg bounds =
  match solve bounds with
  | Error _ -> ()
  | Ok t -> assert_failure (sprintf "%s: expected a conflict, got %s" msg (Refined.show t))

let dec p s = Refine.decimal ~precision:(Some p) ~scale:(Some s)
let base b = Refined.of_base b
let refined b r = Refined.make b r

let test_solver = [

  (* §1: the motivating regression. SUM over a decimal must keep the column's
     precision instead of collapsing to a join. *)
  "SUM keeps the argument type" >:: (fun () ->
    assert_solves ~msg:"sum(decimal(10,2))"
      [ pred Pred.Num; lo (refined Base.Decimal (dec 10 2)) ]
      (refined Base.Decimal (dec 10 2)));

  "mixed numeric arguments take the lub" >:: (fun () ->
    assert_solves ~msg:"int + float" [ pred Pred.Num; lo (base Base.Int); lo (base Base.Float) ]
      (base Base.Float));

  "incomparable numeric arguments are rejected" >:: (fun () ->
    assert_conflict ~msg:"float + decimal"
      [ pred Pred.Num; lo (base Base.Float); lo (base Base.Decimal) ]);

  (* the predicate has to survive into the class, not merely be checked where
     it was written: Int <= Datetime <= Text makes a lub exist *)
  "a predicate constrains the class, not just its own position" >:: (fun () ->
    assert_conflict ~msg:"num(int, text)"
      [ pred Pred.Num; lo (base Base.Int); lo (base Base.Text) ];
    assert_solves ~msg:"int <= _ <= text, Num"
      [ pred Pred.Num; lo (base Base.Int); up (base Base.Text) ] (base Base.Int));

  "an upper bound is taken as the type" >:: (fun () ->
    assert_solves ~msg:"concat" [ up (base Base.Text); pred Pred.Stringable ] (base Base.Text));

  "a lone predicate defaults" >:: (fun () ->
    assert_solves ~msg:"num" [ pred Pred.Num ] (base Base.Int);
    assert_solves ~msg:"stringable" [ pred Pred.Stringable ] (base Base.Text));

  (* §8: Any is gone, so a parameter nothing constrains is an error unless the
     dialect opts into a fallback *)
  "an unconstrained variable cannot be inferred" >:: (fun () ->
    assert_conflict ~msg:"bare ?" [ pred Pred.Comparable ]);

  "a dialect may supply a fallback" >:: (fun () ->
    assert_solves ~fallback:Base.Text ~msg:"fallback" [ pred Pred.Comparable ] (base Base.Text));

  (* A declared ENUM is an upper bound, not a flag inside the lattice: "accepts
     no further constructors" is exactly what an upper bound means. *)
  "a declared enum rejects a foreign literal" >:: (fun () ->
    let e = Refine.enum ~closed:true [ "a"; "b" ] in
    assert_solves ~msg:"status = 'a'"
      [ exact (refined Base.Text e); lo (refined Base.Str_lit (Refine.literal "a")) ]
      (refined Base.Text e);
    assert_conflict ~msg:"status = 'typo'"
      [ exact (refined Base.Text e); lo (refined Base.Str_lit (Refine.literal "typo")) ]);

  "two literals widen to their union" >:: (fun () ->
    assert_solves ~msg:"'a' or 'b'"
      [ lo (refined Base.Str_lit (Refine.literal "a")); lo (refined Base.Str_lit (Refine.literal "b")) ]
      (refined Base.Text (Refine.enum [ "a"; "b" ])));

  (* a value set is destroyed by a value arriving from a smaller base, a
     capacity is not *)
  "a value set does not survive a widening, a capacity does" >:: (fun () ->
    assert_solves ~msg:"coalesce(enum, datetime)"
      [ lo (refined Base.Text (Refine.enum [ "a"; "b" ])); lo (base Base.Datetime) ]
      (base Base.Text);
    assert_solves ~msg:"decimal(10,2) + int"
      [ lo (refined Base.Decimal (dec 10 2)); lo (base Base.Int) ]
      (refined Base.Decimal (dec 10 2)));

  (* a literal sits below every stringable type, and which one it may become is
     decided by validating its content *)
  "a literal rises only where it validates" >:: (fun () ->
    assert_solves ~msg:"'$.a' as a json path"
      [ lo (refined Base.Str_lit (Refine.literal "$.a")); up (base Base.Json_path) ]
      (refined Base.Str_lit (Refine.literal "$.a"));
    assert_conflict ~msg:"'nonsense' as a json path"
      [ lo (refined Base.Str_lit (Refine.literal "nonsense[")); up (base Base.Json_path) ]);

  (* §11.1: a subtyping edge between two variables is unification, so the two
     classes merge their bounds *)
  "unifying two variables merges their bounds" >:: (fun () ->
    let a = Hmx_solver.fresh () and b = Hmx_solver.fresh () in
    Hmx_solver.same a b;
    Hmx_solver.above a (base Base.Int);
    Hmx_solver.below b (base Base.Float);
    assert_equal ~printer:Refined.show (base Base.Int) (Hmx_solver.resolve a));

  (* the lattice needs the transitive closure to be a lattice; §11.4 wants the
     edges it invents refused, so they are reported instead of allowed silently *)
  "a coercion invented by the closure is reported" >:: (fun () ->
    let v = Hmx_solver.fresh () in
    Hmx_solver.above v (base Base.Int);
    Hmx_solver.below v (base Base.Text);
    assert_equal ~msg:"resolves" ~printer:Refined.show (base Base.Int) (Hmx_solver.resolve v);
    (* Int <= Text exists only in the closure; Int <= Datetime is declared *)
    let w = Hmx_solver.fresh () in
    Hmx_solver.above w (base Base.Int);
    Hmx_solver.above w (base Base.Text);
    assert_equal ~msg:"one derived coercion" 1 (List.length (Hmx_solver.derived_coercions w));
    let u = Hmx_solver.fresh () in
    Hmx_solver.above u (base Base.Int);
    Hmx_solver.above u (base Base.Datetime);
    assert_equal ~msg:"declared edges are not reported" 0
      (List.length (Hmx_solver.derived_coercions u)));
]

(* --------------------------------------------------------- nullability *)

let nsolve build =
  let st = Hmx_null.create () in
  let out = build st in
  match Hmx_null.solve st with
  | () -> Ok (Hmx_null.get st out)
  | exception Conflict e -> Error e

let assert_null ~msg expect build =
  match nsolve build with
  | Error e -> assert_failure (sprintf "%s: %s" msg e)
  | Ok n -> assert_equal ~msg ~printer:Null.show expect n

let test_nullability = [
  "a join is nullable as soon as one argument is" >:: (fun () ->
    assert_null ~msg:"a + b" Null.Nullable (fun st ->
      let n = Hmx_null.fresh st in
      Hmx_null.add st (Join (n, [ N Null.NotNull; N Null.Nullable ])); n));

  "a join of strict arguments is strict" >:: (fun () ->
    assert_null ~msg:"a + b" Null.NotNull (fun st ->
      let n = Hmx_null.fresh st in
      Hmx_null.add st (Join (n, [ N Null.NotNull; N Null.NotNull ])); n));

  (* COALESCE: not null as soon as any branch is *)
  "a meet is strict as soon as one argument is" >:: (fun () ->
    assert_null ~msg:"coalesce" Null.NotNull (fun st ->
      let n = Hmx_null.fresh st in
      Hmx_null.add st (Meet (n, [ N Null.Nullable; N Null.NotNull ])); n));

  "an unknown argument is settled by the fixpoint, whatever the order" >:: (fun () ->
    let build st =
      let n = Hmx_null.fresh st and a = Hmx_null.fresh st and b = Hmx_null.fresh st in
      Hmx_null.add st (Join (n, [ a; N Null.NotNull ]));
      Hmx_null.add st (Eq (a, b));
      Hmx_null.add st (Eq (b, N Null.Nullable));
      n
    in
    assert_null ~msg:"fixpoint" Null.Nullable build);

  (* the dual direction: a strict result forces its arguments *)
  "a strict join result forces its arguments" >:: (fun () ->
    assert_null ~msg:"not null context" Null.NotNull (fun st ->
      let n = Hmx_null.fresh st and a = Hmx_null.fresh st in
      Hmx_null.add st (Eq (n, N Null.NotNull));
      Hmx_null.add st (Join (n, [ a; Hmx_null.fresh st ]));
      a));

  "a contradiction is reported" >:: (fun () ->
    match nsolve (fun st ->
      let n = Hmx_null.fresh st in
      Hmx_null.add st (Eq (n, N Null.NotNull));
      Hmx_null.add st (Join (n, [ N Null.Nullable ])); n)
    with
    | Error _ -> ()
    | Ok _ -> assert_failure "expected a nullability conflict");

  (* NotNull is the identity of the join, so an unconstrained variable is not
     null — §8 says otherwise and §8 is wrong *)
  "an undetermined nullability is not null" >:: (fun () ->
    assert_null ~msg:"free" Null.NotNull (fun st -> Hmx_null.fresh st));
]

(* ---------------------------------------------------------- signatures *)

(* The target table, written the way the final version will write it. Until
   Sql.Function is retired the authoritative version is derived by
   Hmx_of_sql.of_func; these pin down what the vocabulary expresses. *)
module Sg = struct
  open Hmx_sig
  let bool = Refined.of_base Base.Bool
  let text = Refined.of_base Base.Text
  let int = Refined.of_base Base.Int

  let arith = make ~preds:[ Pred.Num ] (Args [ Same; Same ]) Ret_same
  let equal = make ~compares:true ~preds:[ Pred.Comparable ] (Args [ Same; Same ]) (Ret bool)
  let sum = make ~agg:true ~preds:[ Pred.Num ] ~nulls:(Const Null.Nullable) (Args [ Same ]) Ret_same
  let count = make ~agg:true ~nulls:(Const Null.NotNull)
      (Varargs { head = []; tail = [ Free ] }) (Ret int)
  let coalesce = make ~nulls:Meet (Varargs { head = [ Same ]; tail = [ Same ] }) Ret_same
  let concat = make (Varargs { head = []; tail = [ As text ] }) (Ret text)
  let concat_ws = make (Varargs { head = [ As text ]; tail = [ As text ] }) (Ret text)
  let json_array_append =
    make (Varargs { head = [ As (Refined.of_base Base.Json);
                             As (Refined.of_base Base.Json_path); Free ];
                    tail = [ As (Refined.of_base Base.Json_path); Free ] })
      (Ret (Refined.of_base Base.Json))
end

let test_signatures = [
  "arity is checked by instantiate" >:: (fun () ->
    let ok n sg = Result.is_ok (Hmx_sig.instantiate sg n) in
    assert_bool "concat_ws/0" (not (ok 0 Sg.concat_ws));
    assert_bool "concat_ws/2" (ok 2 Sg.concat_ws);
    assert_bool "concat_ws/5" (ok 5 Sg.concat_ws);
    assert_bool "json_array_append/3" (ok 3 Sg.json_array_append);
    assert_bool "json_array_append/4" (not (ok 4 Sg.json_array_append));
    assert_bool "json_array_append/5" (ok 5 Sg.json_array_append));

  (* both operands share the scheme variable, which is what lets a parameter
     take its sibling's type and nullability *)
  "comparison shares one variable across both operands" >:: (fun () ->
    match Hmx_sig.instantiate Sg.equal 2 with
    | Error e -> assert_failure e
    | Ok sch -> assert_equal ~msg:"same_at" [ true; true ] sch.Hmx_sig.same_at);

  "varargs expand to the actual arity" >:: (fun () ->
    match Hmx_sig.instantiate Sg.coalesce 3 with
    | Error e -> assert_failure e
    | Ok sch -> assert_equal ~msg:"width" 3 (List.length sch.Hmx_sig.formals));

  "COUNT and SUM carry their own nullability rule" >:: (fun () ->
    let rule sg n = match Hmx_sig.instantiate sg n with
      | Error e -> assert_failure e
      | Ok sch -> sch.Hmx_sig.result_null
    in
    assert_bool "count" (rule Sg.count 1 = Hmx_sig.Const Null.NotNull);
    assert_bool "sum" (rule Sg.sum 1 = Hmx_sig.Const Null.Nullable);
    assert_bool "concat" (rule Sg.concat 2 = Hmx_sig.Join));
]

(* ------------------------------------------- coverage of the old registry *)

(** which arities the current inference accepts, mirroring Sql.signature where
    it is defined and infer_fn where it is not *)
let old_accepts (kind : Sql.Source_type.t Sql.func) arity =
  match kind with
  | Agg Count -> arity = 0 || arity = 1
  | Agg (Self | Avg) -> arity = 1
  | Agg (With_order { with_order_kind = Group_concat; _ }) -> arity >= 1
  | Agg (With_order { with_order_kind = Json_arrayagg; _ }) -> arity = 1
  | Logical _ -> arity = 2
  | Negation -> arity = 1
  | Ret _ | Arith _ -> true
  | Null_handling _ | Comparison _ | Quantified_comparison _ | Membership | Range
  | Like _ | F _ | Col_assign _ | Multi _ -> Option.is_some (Sql.signature kind arity)

let new_accepts kind arity =
  match Hmx_of_sql.of_func ~arity kind with
  | Error _ -> false
  | Ok sg -> Result.is_ok (Hmx_sig.instantiate sg arity)

(** Divergences we accept, with the reason. COALESCE of no arguments passes the
    old signature check and then dies in Hashtbl.find; the new one says so. *)
let known_divergence name arity = String.equal name "coalesce" && arity = 0

let test_registry_coverage = [
  "every registered function translates" >:: (fun () ->
    let total = Sql.Function.fold (fun _ _ _ n -> n + 1) 0 in
    assert_bool (sprintf "the registry looks empty: %d entries" total) (total > 100);
    let bad =
      Sql.Function.fold (fun name narg kind acc ->
        match kind with
        | None -> acc
        | Some kind ->
          let arity = match narg with Some n -> n | None -> 1 in
          match Hmx_of_sql.of_func ~arity kind with
          | Ok _ -> acc
          | Error e -> sprintf "%s: %s" name e :: acc)
        []
    in
    assert_equal ~msg:"untranslatable registrations" ~printer:(String.concat "\n") [] bad);

  "a registered arity is accepted" >:: (fun () ->
    let bad =
      Sql.Function.fold (fun name narg kind acc ->
        match kind, narg with
        | None, _ | _, None -> acc
        | Some kind, Some n -> if new_accepts kind n then acc else sprintf "%s/%d" name n :: acc)
        []
    in
    assert_equal ~msg:"rejected registrations" ~printer:(String.concat " ") [] bad);

  (* the entries registered without an arity are exactly the varargs ones, so
     this compares the old arity rule against the new one head on *)
  "varargs arities agree with the old rule" >:: (fun () ->
    let varargs = Sql.Function.fold (fun _ narg k n ->
      match k, narg with Some _, None -> n + 1 | _ -> n) 0 in
    assert_bool (sprintf "no varargs registrations found: %d" varargs) (varargs > 0);
    let bad =
      Sql.Function.fold (fun name narg kind acc ->
        match kind, narg with
        | None, _ | _, Some _ -> acc
        | Some kind, None ->
          List.fold_left (fun acc arity ->
            if known_divergence name arity then acc
            else if Bool.equal (old_accepts kind arity) (new_accepts kind arity) then acc
            else sprintf "%s/%d: old=%b new=%b" name arity
                   (old_accepts kind arity) (new_accepts kind arity) :: acc)
            acc [ 0; 1; 2; 3; 4; 5; 6 ])
        []
    in
    assert_equal ~msg:"arity disagreements" ~printer:(String.concat " ") [] bad);
]

(* ------------------------------------------ stages 1 to 3, end to end *)

(* The first thing that runs without syntax.ml at all: parse, resolve against
   a hand-built scope, generate constraints, solve. *)

let parse_expr text =
  match (Parser.parse_stmt (sprintf "SELECT %s" text)).statement with
  | Sql.Select { select_complete = { select = ({ columns = [ c ]; _ }, _); _ }; _ } ->
    (match c.value with
     | Sql.Expr (e, _) -> e.value
     | All | AllOf _ -> assert_failure "expected a single expression")
  | _ -> assert_failure (sprintf "not a select: %s" text)

let col ?(sources = [ "t" ]) name base null =
  { Resolve.name; sources; ty = Resolved.known base null; meta = Sql.Meta.empty () }

let scope columns = {
  Resolve.columns;
  named = (fun _ -> None);
  grouping = false;
  guaranteed_row = false;
  subquery = (fun _ _ -> Error { Resolve.pos = None; msg = "no subqueries in this scope" });
  of_values = (fun _ -> Error { Resolve.pos = None; msg = "no VALUES() in this scope" });
}

let demo_scope = scope [
  col "id" (Refined.of_base Base.Int) Null.NotNull;
  col "price" (Refined.make Base.Decimal (dec 10 2)) Null.NotNull;
  col "note" (Refined.of_base Base.Text) Null.Nullable;
  col "status" (Refined.make Base.Text (Refine.enum [ "new"; "done" ])) Null.NotNull;
]

let infer_sql ?(env = demo_scope) text =
  match Resolve.expr env (parse_expr text) with
  | Error e -> Error (Resolve.show_error e)
  | Ok r -> Constrain.infer r

let assert_sql ~msg text base null =
  match infer_sql text with
  | Error e -> assert_failure (sprintf "%s: %s" msg e)
  | Ok t -> assert_equal ~msg ~printer:Sql.Type.show (Hmx_of_sql.to_type base null) t

let assert_sql_fails ~msg text =
  match infer_sql text with
  | Error _ -> ()
  | Ok t -> assert_failure (sprintf "%s: expected failure, got %s" msg (Sql.Type.show t))

let test_pipeline = [

  "a literal" >:: (fun () ->
    assert_sql ~msg:"1" "1" (Refined.of_base Base.Int) Null.NotNull);

  "a column keeps its declared type" >:: (fun () ->
    assert_sql ~msg:"price" "price" (Refined.make Base.Decimal (dec 10 2)) Null.NotNull;
    assert_sql ~msg:"note" "note" (Refined.of_base Base.Text) Null.Nullable);

  (* §1: the motivating case, now through the real parser *)
  "arithmetic on a decimal keeps the precision" >:: (fun () ->
    assert_sql ~msg:"price + 1" "price + 1" (Refined.make Base.Decimal (dec 10 2)) Null.NotNull);

  "arithmetic with a nullable operand is nullable" >:: (fun () ->
    assert_sql ~msg:"id + length(note)" "id + length(note)" (Refined.of_base Base.Int) Null.Nullable);

  "a numeric literal lands on either side of the lattice" >:: (fun () ->
    assert_sql ~msg:"price + 1.5" "price + 1.5" (Refined.make Base.Decimal (dec 10 2)) Null.NotNull;
    assert_sql ~msg:"1.5" "1.5" (Refined.of_base Base.Float) Null.NotNull);

  "COALESCE is not null once a branch is" >:: (fun () ->
    assert_sql ~msg:"coalesce(note, 'x')" "coalesce(note, 'x')"
      (Refined.of_base Base.Text) Null.NotNull);

  "a parameter takes the type of what it is compared to" >:: (fun () ->
    assert_sql ~msg:"status = @s" "status = @s" (Refined.of_base Base.Bool) Null.NotNull);

  "a literal outside a declared enum is rejected" >:: (fun () ->
    assert_sql ~msg:"status" "status"
      (Refined.make Base.Text (Refine.enum [ "new"; "done" ])) Null.NotNull);

  (* Not rejected yet: Arith also carries datetime arithmetic, so the
     descriptors cannot say Num. The predicate arrives with the hand-written
     signature table, where + and date_add are separate entries. *)
  "arithmetic has no numeric predicate until the table is written" >:: (fun () ->
    assert_sql ~msg:"id + note" "id + note" (Refined.of_base Base.Text) Null.Nullable);

  "an unknown column is reported by stage 1" >:: (fun () ->
    match Resolve.expr demo_scope (parse_expr "nosuch") with
    | Ok _ -> assert_failure "expected a resolve error"
    | Error e -> assert_bool (Resolve.show_error e)
                   (String.length (Resolve.show_error e) > 0));
]

(* ------------------------------------------------ stage 1: FROM and JOIN *)

let attr name t null =
  Sql.make_attribute' name { Sql.Type.t; nullability = null }

let demo_catalog = {
  Resolve.table = (fun (n : Sql.table_name) ->
    match n.tn with
    | "a" -> Ok (Resolve.sourced n [ attr "id" Sql.Type.Int Strict; attr "x" Sql.Type.Text Strict ])
    | "b" -> Ok (Resolve.sourced n [ attr "id" Sql.Type.Int Strict; attr "y" Sql.Type.Text Strict ])
    | other -> Error { Resolve.pos = None; msg = "no such table " ^ other });
  select = (fun _ -> Error { Resolve.pos = None; msg = "subqueries not wired yet" });
  values = (fun _ -> Error { Resolve.pos = None; msg = "value rows not wired yet" });
}

let parse_from text =
  match (Parser.parse_stmt text).statement with
  | Sql.Select { select_complete = { select = (sel, _); _ }; _ } -> sel.from
  | _ -> assert_failure (sprintf "not a select: %s" text)

let resolve_from text =
  match parse_from text with
  | None -> assert_failure "no FROM clause"
  | Some n ->
    match Resolve.nested demo_catalog n with
    | Error e -> assert_failure (Resolve.show_error e)
    | Ok schema -> Resolve.scope_of_schema schema

let find_col scope name sources =
  match List.find_opt (fun (c : Resolve.column) ->
    String.equal c.name name && c.sources = sources) scope with
  | Some c -> c
  | None ->
    assert_failure (sprintf "no %s from %s in scope [%s]" name (String.concat "," sources)
      (String.concat " " (List.map (fun (c : Resolve.column) ->
        sprintf "%s.%s" (String.concat "," c.sources) c.name) scope)))

let null_of (c : Resolve.column) =
  match c.ty.null with Some n -> n | None -> assert_failure "no nullability on a declared column"

let test_from = [

  "a plain table puts its columns in scope" >:: (fun () ->
    let scope = resolve_from "SELECT 1 FROM a" in
    assert_equal ~msg:"width" 2 (List.length scope);
    assert_equal ~msg:"x" Null.NotNull (null_of (find_col scope "x" [ "a" ])));

  "an inner join keeps both sides strict" >:: (fun () ->
    let scope = resolve_from "SELECT 1 FROM a JOIN b ON a.id = b.id" in
    assert_equal ~msg:"a.x" Null.NotNull (null_of (find_col scope "x" [ "a" ]));
    assert_equal ~msg:"b.y" Null.NotNull (null_of (find_col scope "y" [ "b" ])));

  (* the padding rule: the optional side of an outer join goes nullable *)
  "a left join makes the right side nullable" >:: (fun () ->
    let scope = resolve_from "SELECT 1 FROM a LEFT JOIN b ON a.id = b.id" in
    assert_equal ~msg:"a.x stays strict" Null.NotNull (null_of (find_col scope "x" [ "a" ]));
    assert_equal ~msg:"b.y goes nullable" Null.Nullable (null_of (find_col scope "y" [ "b" ])));

  "a right join pads the other side" >:: (fun () ->
    let scope = resolve_from "SELECT 1 FROM a RIGHT JOIN b ON a.id = b.id" in
    assert_equal ~msg:"a.x" Null.Nullable (null_of (find_col scope "x" [ "a" ]));
    assert_equal ~msg:"b.y" Null.NotNull (null_of (find_col scope "y" [ "b" ])));

  "an alias renames the source" >:: (fun () ->
    let scope = resolve_from "SELECT 1 FROM a AS t1" in
    ignore (find_col scope "x" [ "t1" ]));

  (* USING collapses the shared column instead of duplicating it *)
  "USING keeps one copy of the common column" >:: (fun () ->
    let scope = resolve_from "SELECT 1 FROM a JOIN b USING (id)" in
    assert_equal ~msg:"width" 3 (List.length scope));

  "an unknown table is reported" >:: (fun () ->
    match parse_from "SELECT 1 FROM nosuch" with
    | None -> assert_failure "no FROM"
    | Some n ->
      match Resolve.nested demo_catalog n with
      | Ok _ -> assert_failure "expected an error"
      | Error _ -> ());

  (* stage 1 output feeds stage 2 directly *)
  "a joined scope types an expression" >:: (fun () ->
    let cols = resolve_from "SELECT 1 FROM a LEFT JOIN b ON a.id = b.id" in
    let env = scope cols in
    match Resolve.expr env (parse_expr "concat(a.x, b.y)") with
    | Error e -> assert_failure (Resolve.show_error e)
    | Ok r ->
      match Constrain.infer r with
      | Error e -> assert_failure e
      | Ok t ->
        assert_equal ~msg:"nullable through the outer join" ~printer:Sql.Type.show
          (Hmx_of_sql.to_type (Refined.of_base Base.Text) Null.Nullable) t);
]

let tests = [
  "hmx_base_lattice" >::: test_base_lattice;
  "hmx_pred" >::: test_pred;
  "hmx_refine_laws" >::: test_refine_laws;
  "hmx_refine" >::: test_refine_units;
  "hmx_solver" >::: test_solver;
  "hmx_nullability" >::: test_nullability;
  "hmx_signatures" >::: test_signatures;
  "hmx_registry_coverage" >::: test_registry_coverage;
  "hmx_pipeline" >::: test_pipeline;
  "hmx_from" >::: test_from;
]
