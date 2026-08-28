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
  | As of Refined.t  (** a fixed formal type; the argument is coerced to it *)
  | Free          (** a fresh variable, otherwise unconstrained *)

type params =
  | Args of param_spec list
  | Varargs of { head : param_spec list; tail : param_spec list }
      (** [head] followed by [tail] repeated zero or more times *)

type ret =
  | Ret_same       (** the shared variable *)
  | Ret of Refined.t

type null_rule =
  | Join           (** null if any argument is *)
  | Meet           (** not null if any argument is not — COALESCE and relatives *)
  | Const of Null.t
  | Group_join     (** [Join], but nullable unless the group is guaranteed a row *)
  | Assign         (** SET col = e: the argument's nullability must sit below the column's *)

type t = {
  params : params;
  ret : ret;
  preds : Pred.t list;      (** predicates on the shared variable *)
  nulls : null_rule;
  agg : bool;               (** may only appear in an aggregate context *)
  compares : bool;
      (** the operands are compared rather than combined. A comparison against
          NULL is never true, so a parameter here is not offered as nullable —
          it is the one exception to inheriting a sibling's nullability. *)
}

let make ?(preds = []) ?(nulls = Join) ?(agg = false) ?(compares = false) params ret =
  { params; ret; preds; nulls; agg; compares }

(** the result of applying a signature to a concrete arity *)
type scheme = {
  formals : param_spec list;   (** one per actual argument *)
  result : ret;
  result_null : null_rule;
  same_at : bool list;         (** per argument: does it share the scheme variable *)
  preds : Pred.t list;         (** predicates on the shared variable *)
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

let instantiate sg arity =
  match expand_params sg.params arity with
  | Error expected ->
    Error (Printf.sprintf "wrong number of arguments: got %d, expected %s%d" arity
             (match sg.params with Args _ -> "" | Varargs _ -> "at least ") expected)
  | Ok formals ->
    Ok { formals;
         result = sg.ret;
         result_null = sg.nulls;
         same_at = List.map (function Same -> true | As _ | Free -> false) formals;
         preds = sg.preds }
