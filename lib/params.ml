(** The parameter tree after compilation: its shape, and unifying parameters
    that appear under the same name in several places.

    A phase over {!Sql.var}: no expressions, no schemas, no catalog. *)

open ExtLib
open Prelude
open Sql

(* the synthetic name a dynamic-select column parameter carries *)
let dynamic_col_param_name = "col"

type var_shape =
  | Shape_param
  | Shape_in_param
  | Shape_tuple of string option
  | Shape_choice_in of { param : string option; kind : in_or_not_in; vars : var_shape list }
  | Shape_opt_choice of string option * var_shape list
  | Shape_shared_group of string * var_shape list
  | Shape_choice of string option * ctor_shape list
  | Shape_dyn_select of string option * ctor_shape list
  | Shape_dyn_join of string option
and ctor_shape =
  | Shape_simple of string option * var_shape list
  | Shape_verbatim of string

let rec var_shape = function
  | Single _ -> Shape_param
  | SingleIn _ -> Shape_in_param
  | TupleList (id, _) -> Shape_tuple id.value
  | ChoiceIn { param; kind; vars } -> Shape_choice_in { param = param.value; kind; vars = List.map var_shape vars }
  | OptionActionChoice (id, vars, _, _) -> Shape_opt_choice (id.value, List.map var_shape vars)
  | SharedVarsGroup (vars, id) -> Shape_shared_group (id.value, List.map var_shape vars)
  | Choice (id, cs) -> Shape_choice (id.value, List.map ctor_shape cs)
  | DynamicSelect (id, cs) -> Shape_dyn_select (id.value, List.map ctor_shape cs)
  | DynamicSelectJoin { pid; _ } -> Shape_dyn_join pid.value
and ctor_shape = function
  | Simple (p, args) -> Shape_simple (p.value, List.map var_shape (Option.default [] args))
  | Verbatim (n, _) -> Shape_verbatim n

(* FIXME unify each choice separately *)
let unify_params l =
  (* A parameter standing in several places is one value, so it is one solver
     variable: sharing a name is unification, and nullability joins over the
     sightings rather than unifying. *)
  let vars : (string, Hmx_solver.var) Hashtbl.t = Hashtbl.create 10 in
  let nulls : (string, bool) Hashtbl.t = Hashtbl.create 10 in
  let var name =
    match Hashtbl.find_opt vars name with
    | Some v -> v
    | None -> let v = Hmx_solver.fresh () in Hashtbl.add vars name v; v
  in
  let nullable name = Option.default false (Hashtbl.find_opt nulls name) in
  (* the value must be acceptable everywhere it stands, so each sighting is an
     upper bound *)
  let note name (ty : Type.t) =
    (match Hmx_of_sql.of_kind ty.t with
     | Some b ->
       (try Hmx_solver.below (var name) b with
        | Hmx_lattice.Conflict _ ->
          fail "incompatible types for parameter %S : %s" name (Type.show ty))
     | None -> ());
    Hashtbl.replace nulls name (Type.is_nullable ty || nullable name)
  in
  let alias a b =
    Hmx_solver.same (var a) (var b);
    let n = nullable a || nullable b in
    Hashtbl.replace nulls a n; Hashtbl.replace nulls b n
  in
  let solved name ~default =
    match Hashtbl.find_opt vars name with
    | None -> default
    | Some v ->
      match Hmx_solver.resolve v with
      | r -> Hmx_of_sql.to_type r (nullable name)
      | exception Hmx_lattice.Conflict _ -> default
  in
  let choices = Hashtbl.create 10 in
  let rec bound_names vars =
    vars |> List.concat_map (function
      | Single (p, _) | SingleIn (p, _) -> [p.id.value]
      | v -> bound_names (sub_vars v))
  in
  let register p signature =
    match p.value with
    | None -> () (* anonymous ie non-shared *)
    | Some n ->
    match Hashtbl.find_opt choices n, signature with
    | None, _ -> Hashtbl.add choices n signature
    | Some (`Branches (s1, names1)), `Branches (s2, names2) when s1 = s2 ->
      List.iter2 (fun n1 n2 -> match n1, n2 with Some n1, Some n2 -> alias n1 n2 | _ -> ()) names1 names2
    | Some (`Branches _), `Branches _ -> failed ~at:p.pos "choice %s is used several times with different branches" n
    | Some `Dynamic, `Dynamic -> failed ~at:p.pos "dynamic select %s occurs several times in one statement (not supported)" n
    | Some `Dynamic, `Branches _ | Some (`Branches _), `Dynamic ->
      if n = dynamic_col_param_name then
        failed ~at:p.pos "dynamic_select reserves the name %s for the column picker, rename choice %s" n n
      else
        failed ~at:p.pos "parameter %s is ambiguous : used as both choice and dynamic select" n
  in
  let rec collect var =
    begin match var with
    | Single ({ id; typ; _ }, _) | SingleIn ({ id; typ; _ }, _) ->
      Option.may (fun name -> note name typ) id.value
    | Choice (p, ctors) ->
      register p (`Branches (List.map ctor_shape ctors, bound_names (List.concat_map ctor_vars ctors)))
    | DynamicSelect (p, _) -> register p `Dynamic
    | TupleList _ | ChoiceIn _ | OptionActionChoice _ | SharedVarsGroup _ | DynamicSelectJoin _ -> ()
    end;
    List.iter collect (sub_vars var)
  in
  (* if no other clues - input parameters are strict *)
  let final { id; typ; _ } =
    let typ = Option.map_default (solved ~default:typ) typ id.value in
    (* if nothing else said so, an input parameter is not null *)
    let typ = match typ.Type.nullability with Depends -> Type.strict typ.t | _ -> typ in
    make_param ~id ~typ
  in
  let rec rewrite = function
    | Single (p, m) -> Single (final p, m)
    | SingleIn (p, m) -> SingleIn (final p, m)
    | v -> map_sub_vars (List.map rewrite) v
  in
  List.iter collect l;
  List.map rewrite l
