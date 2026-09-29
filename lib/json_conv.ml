(* [Jsonkit.Primitives] has no primitive for a JSON object with arbitrary
   keys, so map codecs are spelled out here. *)

let object_to_json value_to_json bindings =
  `Assoc (List.map (fun (key, value) -> key, value_to_json value) bindings)

let object_of_json value_of_json ~what = function
  | `Assoc fields -> List.map (fun (key, value) -> key, value_of_json value) fields
  | _ -> Jsonkit.of_json_msg_error (what ^ ": expected object")

let object_jsonschema value_jsonschema =
  `Assoc [ "type", `String "object"; "additionalProperties", value_jsonschema ]
