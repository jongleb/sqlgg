
open ExtLib
open Prelude
open Sql

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

let unify_params session l =
  let choices = Hashtbl.create 10 in
  let rec bound_names vars =
    vars |> List.concat_map (function
      | Single (p, _) | SingleIn (p, _) -> [p.id.value]
      | v -> bound_names (sub_vars v))
  in
  let alias a b = Constrain.alias session a b in
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
    | Single _ | SingleIn _ -> ()
    | Choice (p, ctors) ->
      register p (`Branches (List.map ctor_shape ctors, bound_names (List.concat_map ctor_vars ctors)))
    | DynamicSelect (p, _) -> register p `Dynamic
    | TupleList _ | ChoiceIn _ | OptionActionChoice _ | SharedVarsGroup _ | DynamicSelectJoin _ -> ()
    end;
    List.iter collect (sub_vars var)
  in
  let final p =
    let p = Constrain.read_param session p in

    let typ = match p.typ.Type.nullability with Depends -> Type.strict p.typ.t | _ -> p.typ in
    make_param ~id:p.id ~typ
  in
  let rec rewrite = function
    | Single (p, m) -> Single (final p, m)
    | SingleIn (p, m) -> SingleIn (final p, m)
    | v -> map_sub_vars (List.map rewrite) v
  in
  List.iter collect l;
  List.map rewrite l
