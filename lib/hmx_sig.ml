(** Function signatures for the HM(X) core.

    A signature is not a type: it is a function from arity to a scheme, so
    varargs are expanded at constraint-generation time and never reach the
    solver. {!instantiate} is the only place that knows about arity.

    The vocabulary is deliberately smaller than the [Sql.func] constructors it
    replaces. Once every argument is related to its formal by [Sub] rather than
    [Eq], "all arguments have the same type" and "all arguments are below a
    common result" are the same constraint, so they are one case here; what
    actually varies is the shape of the formals, the shape of the result, the
    predicates, and the nullability rule. *)

open Hmx_lattice

(** what one formal position is *)
type param_spec =
  | Same          (** the scheme's shared variable *)
  | As of Hmx.ty  (** a fixed formal type; the argument is coerced to it *)
  | Free          (** a fresh variable, otherwise unconstrained *)

type params =
  | Args of param_spec list
  | Varargs of { head : param_spec list; tail : param_spec list }
      (** [head] followed by [tail] repeated zero or more times *)

type ret =
  | Ret_same       (** the shared variable *)
  | Ret of Hmx.ty

type null_rule =
  | Join           (** null if any argument is *)
  | Meet           (** not null if any argument is not — COALESCE and relatives *)
  | Const of Null.t

(** which argument positions force a parameter to be NOT NULL.
    This is [Sql.strict_args] made part of the signature instead of a special
    case in the generator: comparing to NULL is never true, so a parameter in
    such a position is never usefully nullable. *)
type strict_args = No_strict | All_strict | First_strict

type t = {
  params : params;
  ret : ret;
  preds : Pred.t list;      (** predicates on the shared variable *)
  nulls : null_rule;
  strict : strict_args;
  agg : bool;               (** may only appear in an aggregate context *)
}

let make ?(preds = []) ?(nulls = Join) ?(strict = No_strict) ?(agg = false) params ret =
  { params; ret; preds; nulls; strict; agg }

(** the result of applying a signature to a concrete arity *)
type scheme = {
  formals : Hmx.ty list;    (** one per actual argument; emit [Sub (arg, formal)] *)
  result : Hmx.ty;
  result_null : null_rule;
  strict_at : bool list;    (** per argument: does this position force NOT NULL *)
  side : Hmx.t;             (** predicates on the scheme's own variables *)
}

let expand_params params arity =
  match params with
  | Args l -> if List.length l = arity then Ok l else Error (List.length l)
  | Varargs { head; tail } ->
    let h = List.length head and t = List.length tail in
    let rest = arity - h in
    if rest < 0 then Error h
    else if rest = 0 then Ok head
    else if t = 0 then Error h
    else if rest mod t <> 0 then Error h
    else
      let rec repeat acc n = if n = 0 then acc else repeat (acc @ tail) (n - 1) in
      Ok (repeat head (rest / t))

let instantiate ~fresh sg arity =
  match expand_params sg.params arity with
  | Error expected ->
    Error (Printf.sprintf "wrong number of arguments: got %d, expected %s%d" arity
             (match sg.params with Args _ -> "" | Varargs _ -> "at least ") expected)
  | Ok specs ->
    (* the shared variable is allocated only when something actually mentions it *)
    let shared = lazy (Hmx.Var (fresh ())) in
    let formal = function
      | Same -> Lazy.force shared
      | As t -> t
      | Free -> Hmx.Var (fresh ())
    in
    let formals = List.map formal specs in
    let result = match sg.ret with Ret_same -> Lazy.force shared | Ret t -> t in
    let side =
      match sg.preds with
      | [] -> Hmx.True
      | preds -> Hmx.Conj (List.map (fun p -> Hmx.Has (p, Lazy.force shared)) preds)
    in
    let strict_at =
      List.mapi (fun i _ ->
        match sg.strict with
        | No_strict -> false
        | All_strict -> true
        | First_strict -> i = 0)
        specs
    in
    Ok { formals; result; result_null = sg.nulls; strict_at; side }

(* ---------------------------------------------------------------- table *)

let ty b = Hmx.Ty (Refined.of_base b)
let bool = ty Base.Bool
let text = ty Base.Text
let int = ty Base.Int

let arith = make ~preds:[ Pred.Num ] ~strict:All_strict (Args [ Same; Same ]) Ret_same
let comparison = make ~strict:All_strict ~preds:[ Pred.Comparable ] (Args [ Same; Same ]) (Ret bool)
let ordering = make ~strict:All_strict ~preds:[ Pred.Ord ] (Args [ Same; Same ]) (Ret bool)

(** a fragment of the table; the shape is what matters, the remaining entries
    are a transcription of the [Sql.Function] registrations *)
let table : (string * int option * t) list = [
  "+", None, arith;
  "-", None, arith;
  "*", None, arith;
  "=", None, comparison;
  "<", None, ordering;
  "is null", Some 1, make ~nulls:(Const Null.NotNull) (Args [ Free ]) (Ret bool);
  "is not null", Some 1, make ~nulls:(Const Null.NotNull) (Args [ Free ]) (Ret bool);
  "sum", Some 1, make ~agg:true ~preds:[ Pred.Num ] ~nulls:(Const Null.Nullable) (Args [ Same ]) Ret_same;
  "max", Some 1, make ~agg:true ~preds:[ Pred.Ord ] ~nulls:(Const Null.Nullable) (Args [ Same ]) Ret_same;
  "min", Some 1, make ~agg:true ~preds:[ Pred.Ord ] ~nulls:(Const Null.Nullable) (Args [ Same ]) Ret_same;
  "avg", Some 1, make ~agg:true ~preds:[ Pred.Num ] ~nulls:(Const Null.Nullable) (Args [ Same ]) (Ret (ty Base.Float));
  "count", None, make ~agg:true ~nulls:(Const Null.NotNull) (Varargs { head = []; tail = [ Free ] }) (Ret int);
  "coalesce", None, make ~nulls:Meet (Varargs { head = [ Same ]; tail = [ Same ] }) Ret_same;
  "ifnull", Some 2, make ~nulls:Meet (Args [ Same; Same ]) Ret_same;
  "nullif", Some 2, make ~nulls:(Const Null.Nullable) (Args [ Same; Same ]) Ret_same;
  "greatest", None, make ~preds:[ Pred.Ord ] (Varargs { head = [ Same; Same ]; tail = [ Same ] }) Ret_same;
  "least", None, make ~preds:[ Pred.Ord ] (Varargs { head = [ Same; Same ]; tail = [ Same ] }) Ret_same;
  "concat", None, make (Varargs { head = []; tail = [ As text ] }) (Ret text);
  "concat_ws", None, make (Varargs { head = [ As text ]; tail = [ As text ] }) (Ret text);
  "lower", Some 1, make (Args [ As text ]) (Ret text);
  "upper", Some 1, make (Args [ As text ]) (Ret text);
  "length", Some 1, make (Args [ As text ]) (Ret int);
  "json_array_append", None,
    make (Varargs { head = [ As (ty Base.Json); As (ty Base.Json_path); Free ];
                    tail = [ As (ty Base.Json_path); Free ] }) (Ret (ty Base.Json));
]

let find name arity =
  let name = String.lowercase_ascii name in
  let matches (n, a, _) = String.equal n name && (match a with None -> true | Some a -> a = arity) in
  match List.filter matches table with
  | (_, Some _, sg) :: _ -> Some sg
  | (_, None, sg) :: _ -> Some sg
  | [] -> None
