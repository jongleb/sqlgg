let rec strip hoisted (json : Yojson.Basic.t) =
  match json with
  | `Assoc fields ->
    let hoisted =
      match List.find_opt (fun (k, _) -> String.equal k "$defs") fields with
      | None -> hoisted
      | Some (_, `Assoc entries) -> List.fold_left collect hoisted entries
      | Some _ -> invalid_arg "Json_schema.bundle: $defs is not an object"
    in
    let hoisted, fields =
      List.fold_left
        (fun (hoisted, kept) (key, value) ->
          match key with
          | "$id" | "$defs" -> hoisted, kept
          | _ ->
            let hoisted, value = strip hoisted value in
            hoisted, (key, value) :: kept)
        (hoisted, []) fields
    in
    hoisted, `Assoc (List.rev fields)
  | `List items ->
    let hoisted, items =
      List.fold_left
        (fun (hoisted, kept) item ->
          let hoisted, item = strip hoisted item in
          hoisted, item :: kept)
        (hoisted, []) items
    in
    hoisted, `List (List.rev items)
  | json -> hoisted, json

and collect hoisted (name, def) =
  let hoisted, def = strip hoisted def in
  match List.find_opt (fun (k, _) -> String.equal k name) hoisted with
  | None -> (name, def) :: hoisted
  | Some (_, existing) when Yojson.Basic.equal existing def -> hoisted
  | Some _ ->
    invalid_arg ("Json_schema.bundle: conflicting definitions for " ^ name)

let bundle (schema : Yojson.Basic.t) : Yojson.Basic.t =
  match strip [] schema with
  | hoisted, `Assoc fields ->
    let header, rest =
      List.partition (fun (key, _) -> String.equal key "$schema") fields
    in
    `Assoc (header @ [ "$defs", `Assoc (List.rev hoisted) ] @ rest)
  | _, _ -> invalid_arg "Json_schema.bundle: schema is not an object"

let of_deriving schema =
  bundle (Ppx_deriving_jsonschema_runtime.json_schema schema :> Yojson.Basic.t)

let stmt = lazy (of_deriving Sql.stmt_jsonschema)
