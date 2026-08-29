(** Scope: what names mean to constraint generation.

    Name resolution itself — tables, CTEs, FROM clauses, aliases — is
    {!Syntax}'s business, since that is where the catalog lives. This is only
    the interface {!Constrain} sees: a column lookup, and the few facts about
    the surrounding query that change how an expression types.

    Everything here speaks declared types, {!Sql.Type.t}: [Any] and [Depends]
    mean "nothing written", and {!Constrain} turns those into fresh variables. *)

open Hmx_lattice

let fail fmt = conflict fmt

(** a column in scope *)
type column = { name : string; domain : Sql.Type.t; meta : Sql.Meta.t }

type env = {
  column : Sql.col_name -> column;
  (** an aggregate here is guaranteed to see a row, so a strict argument keeps
      a strict result *)
  grouping : bool;
  (** §6: an aggregate is a function of a group, so it has no meaning where
      rows are still being filtered — in WHERE, in GROUP BY, in a join
      condition, or inside another aggregate *)
  allow_aggregates : bool;
  (** the type, parameters and metadata of a nested SELECT *)
  subquery : Sql.select_full -> [ `AsValue | `Exists ] -> Sql.Type.t * Sql.var list * Sql.Meta.t;
  of_values : string -> Sql.Type.t;
}

let column_of_attr (a : Sql.attr) = { name = a.name; domain = a.domain; meta = a.meta }

(** JSON null is not SQL NULL, and DDL has no way to say which a column allows,
    so sqlgg carries it as metadata. It is a property of the column, decided
    before any inference, which is why it belongs here and not in the solver. *)
let apply_json_meta (c : column) : Sql.Type.t =
  let json_null_kind = Sql.Meta.find_opt c.meta "json_null_kind" in
  let text_as_json = Sql.Meta.find_opt c.meta "text_as_json" in
  let is k = Sql.Type.equal_kind c.domain.t k in
  (* a JSON column may hold a JSON null unless told otherwise *)
  let null () =
    match json_null_kind, c.domain.nullability with
    | Some "false", Strict -> c.domain
    | _ -> Sql.Type.make_nullable c.domain
  in
  match json_null_kind, text_as_json with
  | None, None -> c.domain
  | _, _ when is Json -> null ()
  | _, Some "true" when is Text -> null ()
  | _, Some _ -> fail "column %s has text_as_json meta, but its type is not Text" c.name
  | Some _, None -> fail "column %s has json_null_kind meta, but its type is not Json or Text" c.name

(* the parameter an option-actions block switches on *)
let rec choice_id (e : Sql.expr) =
  match e with
  | Choices (id, _) -> Some id
  | InChoice (id, _, _) -> Some id
  | OptionActions { choice; _ } -> choice_id choice
  | Value _ | Param _ | Inparam _ | Fun _ | SelectExpr _ | Column _
  | InTupleList _ | Case _ | Of_values _ -> None
