
type mode = Normal | Ignore | Ident
let mode = ref Normal
let mode_normal () = mode := Normal
let mode_ignore () = mode := Ignore
let mode_ident () = mode := Ident
let is_statement = ref false

type metadata_item = {
  pos : Lexing.position;
  name : string;
  value : string;
}

let collected_metadata : metadata_item list ref = ref []

let add_metadata pos name value =
  collected_metadata := { pos; name; value } :: !collected_metadata

let get_metadata_before parser_pos_cnum =
  let sorted_list = List.sort (fun m1 m2 -> compare m1.pos.pos_cnum m2.pos.pos_cnum) !collected_metadata in
  let rec filter_before acc = function
    | [] -> List.rev acc
    | item :: rest ->
      if item.pos.pos_cnum < parser_pos_cnum then
        filter_before (item :: acc) rest
      else
        List.rev acc
  in
  filter_before [] sorted_list

let clear_metadata () =
  collected_metadata := []
