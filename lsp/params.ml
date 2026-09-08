open Sqlgg

type token_type =
  | Parameter [@as "parameter"]
  | Enum [@as "enum"]
  | Enum_member [@as "enumMember"]
[@@deriving enumerate, to_string, eq]

type kind =
  | Var of Sql.param_id * Sql.var
  | Branch of Sql.param_id * Sql.ctor

type placement = { cursor : Sql.Pos.t; token : Sql.Pos.t option }

type node = {
  kind : kind;
  placement : placement option;
  children : node list;
}

let name (id : Sql.param_id) =
  match id.value with Some name -> "@" ^ name | None -> "?"

let token_type node =
  match node.kind with
  | Var (_, (Sql.Single _ | SingleIn _ | ChoiceIn _ | TupleList _ | SharedVarsGroup _)) -> Parameter
  | Var (_, (Choice _ | DynamicSelect _ | DynamicSelectJoin _ | OptionActionChoice _)) -> Enum
  | Branch _ -> Enum_member

let label node =
  match node.kind with
  | Var (id, var) ->
    let suffix =
      match var with
      | ChoiceIn { kind = `In; _ } -> " — IN"
      | ChoiceIn { kind = `NotIn; _ } -> " — NOT IN"
      | DynamicSelect _ -> " — dynamic select"
      | DynamicSelectJoin _ -> " — dynamic join"
      | TupleList _ -> " — tuple list"
      | Single _ | SingleIn _ | Choice _ | OptionActionChoice _ | SharedVarsGroup _ -> ""
    in
    name id ^ suffix
  | Branch (_, Simple { ctor; _ }) -> Option.value ~default:"_" ctor.value
  | Branch (_, Verbatim (name, _)) -> name

let of_vars ~base vars =
  let sigil_pos (id : Sql.param_id) =
    Option.map
      (fun name -> Sql.Pos.span id.pos (fst id.pos, fst id.pos + 1 + String.length name))
      id.value
  in
  let placement_of_kind = function
    | Var (_, Sql.DynamicSelect _) -> None
    | Var (id, (Sql.Single _ | SingleIn _)) ->
      Some { cursor = Option.value (sigil_pos id) ~default:id.pos; token = Some id.pos }
    | Var (id, (Choice _ | ChoiceIn _ | DynamicSelectJoin _ | TupleList _
               | OptionActionChoice _ | SharedVarsGroup _)) ->
      Some { cursor = Option.value (sigil_pos id) ~default:id.pos; token = sigil_pos id }
    | Branch (_, Sql.Simple { ctor; ctor_pos; _ }) ->
      if Sql.Pos.is_empty ctor_pos
      then Some { cursor = ctor.pos; token = None }
      else Some { cursor = Sql.Pos.span ctor_pos ctor.pos; token = Some ctor_pos }
    | Branch (_, Verbatim _) -> None
  in
  let rec map_placement f node =
    { node with
      placement = f node.placement;
      children = List.map (map_placement f) node.children }
  in
  let hide = map_placement (Fun.const None) in
  let shift =
    let shift_pos = Sql.Pos.shift base in
    map_placement
      (Option.map (fun { cursor; token } ->
        { cursor = shift_pos cursor; token = Option.map shift_pos token }))
  in
  let create ?(children = []) kind =
    { kind; placement = placement_of_kind kind; children }
  in
  let rec of_var (var : Sql.var) =
    let param_node ?children id = create ?children (Var (id, var)) in
    match var with
    | Sql.Single (p, _) | Sql.SingleIn (p, _) -> [ param_node p.id ]
    | Sql.ChoiceIn { param; vars; _ } ->
      let pos = match vars with [ Sql.SingleIn (p, _) ] -> p.id.pos | _ -> param.pos in
      [ param_node ~children:(of_list vars) { param with pos } ]
    | Sql.Choice (id, ctors) | Sql.DynamicSelect (id, ctors) ->
      [ param_node ~children:(List.map (of_ctor id) ctors) id ]
    | Sql.DynamicSelectJoin { pid; _ } -> [ param_node pid ]
    | Sql.TupleList (id, _) -> [ param_node id ]
    | Sql.OptionActionChoice (id, vars, _, _) -> [ param_node ~children:(of_list vars) id ]
    | Sql.SharedVarsGroup (vars, _) -> List.map hide (of_list vars)
  and of_ctor choice (c : Sql.ctor) =
    create ~children:(of_list (Sql.ctor_vars c)) (Branch (choice, c))
  and of_list vars = List.concat_map of_var vars in
  List.map shift (of_list vars)

type shape =
  | Scalar of Sql.Type.t
  | Row of Sql.Type.t list
  | Compound

let rec shape node =
  match node.kind with
  | Var (_, (Sql.Single (p, _) | SingleIn (p, _))) -> Scalar p.typ
  | Var (_, ChoiceIn _) ->
    let rec scalars acc = function
      | [] -> Row (List.rev acc)
      | child :: rest ->
        match shape child with
        | Scalar t -> scalars (t :: acc) rest
        | Row _ | Compound -> Compound
    in
    scalars [] node.children
  | Var (_, TupleList (_, Where_in { value = (types, _); _ })) -> Row (List.map fst types)
  | Var (_, TupleList (_, ValueRows { types; _ })) -> Row types
  | Var (_, TupleList (_, Insertion schema)) -> Row (List.map (fun (attr : Sql.attr) -> attr.domain) schema)
  | Var (_, (Choice _ | DynamicSelect _ | DynamicSelectJoin _ | OptionActionChoice _ | SharedVarsGroup _))
  | Branch _ -> Compound

type key =
  | At_var of Sql.Pos.t * string option
  | At_branch of Sql.Pos.t * string option

let outline nodes =
  let key_of_node node =
    match node.kind with
    | Var (id, _) -> At_var (id.pos, id.value)
    | Branch (_, Simple { ctor; _ }) -> At_branch (ctor.pos, ctor.value)
    | Branch (choice, Verbatim (name, _)) -> At_branch (choice.pos, Some name)
  in
  let equal_key a b =
    match a, b with
    | At_var (pos, name), At_var (pos', name')
    | At_branch (pos, name), At_branch (pos', name') ->
      Sql.Pos.equal pos pos' && Option.equal String.equal name name'
    | (At_var _ | At_branch _), _ -> false
  in
  let rec loop seen = function
    | [] -> []
    | node :: rest ->
      let key = key_of_node node in
      if List.exists (equal_key key) seen then loop seen rest
      else { node with children = loop [] node.children } :: loop (key :: seen) rest
  in
  loop [] nodes

let all_nodes nodes =
  let rec preorder node = Seq.cons node (Seq.concat_map preorder (List.to_seq node.children)) in
  Seq.concat_map preorder (List.to_seq nodes)
