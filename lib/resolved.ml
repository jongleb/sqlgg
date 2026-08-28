(** Output of stage 1.

    Names are resolved to references, overloads to signatures, and every node
    carries whether it sits in a scalar, aggregate or window position. What it
    deliberately does {e not} carry is inferred types: a type that was never
    written down is simply absent here and becomes a variable in stage 2.

    That absence is the whole point. Today [Sql.Type.Any] plays two roles at
    once — the bottom of the lattice and "not known yet" — and the second role
    is what this [option] replaces. *)

open Hmx_lattice

(** what the source says about a type; [None] means nothing was written *)
type ty = { base : Refined.t option; null : Null.t option }

let unknown = { base = None; null = None }
let of_base b = { base = Some b; null = None }
let known base null = { base = Some base; null = Some null }

type col_ref = { table : string option; column : string }

type mode = Scalar | Agg | Window

type expr =
  | Lit of ty
  | Col of col_ref * ty
  | Param of param
  | Call of call
  | Case of { scrutinee : expr option; branches : branch list; else_ : expr option }
  | Choices of Sql.param_id * (Sql.param_id * expr option) list
      (** {n}? alternatives: exactly one branch survives into the emitted SQL *)
  | InChoice of { id : Sql.param_id; kind : Sql.in_or_not_in; expr : expr }
  | InTupleList of {
      id : Sql.param_id;
      items : (expr * Sql.Meta.t) list;
      kind : Sql.in_or_not_in;
      pos : Sql.pos;
    }
  | OptionActions of {
      id : Sql.param_id;
      choice : expr;
      pos : Sql.pos * Sql.pos;
      kind : Sql.option_actions_kind;
    }
  (* a subquery brings its own parameters, already built by the stage that
     resolved it *)
  | Subquery of { ty : ty; kind : [ `AsValue | `Exists ]; vars : Sql.var list }

and branch = { when_ : expr; then_ : expr }

and param = { id : Sql.param_id; ty : ty; meta : Sql.Meta.t; in_list : bool }

and call = {
  name : string;
  sg : Hmx_sig.t;
  args : expr list;
  mode : mode;
  guaranteed_row : bool;
      (** the aggregate is known to see at least one row, so a strict argument
          keeps a strict result — [Sql.Syntax]'s [aggregates_a_row] *)
  order : (expr * Sql.direction option) list;
      (** GROUP_CONCAT and friends take an ORDER BY of their own, which carries
          parameters like any other expression *)
}

let show_ty { base; null } =
  Printf.sprintf "%s%s"
    (match base with Some b -> Refined.show b | None -> "_")
    (match null with Some Null.Nullable -> "?" | Some Null.NotNull -> "!" | None -> "")

let rec show = function
  | Lit t -> show_ty t
  | Col ({ table; column }, t) ->
    Printf.sprintf "%s%s:%s" (match table with Some x -> x ^ "." | None -> "") column (show_ty t)
  | Param { id; ty; _ } ->
    Printf.sprintf "?%s:%s" (match id.value with Some n -> n | None -> "") (show_ty ty)
  | Call { name; args; _ } ->
    Printf.sprintf "%s(%s)" name (String.concat ", " (List.map show args))
  | Case { scrutinee; branches; else_ } ->
    Printf.sprintf "case%s %s%s end"
      (match scrutinee with Some e -> " " ^ show e | None -> "")
      (String.concat " " (List.map (fun { when_; then_ } ->
        Printf.sprintf "when %s then %s" (show when_) (show then_)) branches))
      (match else_ with Some e -> " else " ^ show e | None -> "")
  | Choices (_, l) ->
    Printf.sprintf "{%s}" (String.concat "|" (List.map (function
      | _, Some e -> show e | _, None -> "-") l))
  | InChoice { expr; _ } -> Printf.sprintf "in{%s}" (show expr)
  | InTupleList { items; _ } ->
    Printf.sprintf "(%s) in ..." (String.concat "," (List.map (fun (e, _) -> show e) items))
  | OptionActions { choice; _ } -> Printf.sprintf "opt{%s}" (show choice)
  | Subquery { kind = `AsValue; ty } -> Printf.sprintf "(select):%s" (show_ty ty)
  | Subquery { kind = `Exists; _ } -> "exists(select)"
