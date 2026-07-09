open Print_ocaml_impl
module Db = Output.Sqlgg(Print_ocaml_impl)
open Db

type who = { id : User_id.t; name : string } [@@deriving sqlgg]

let show tag w =
  Printf.printf "%s who: id=%Ld name=%s\n" tag (User_id.to_int64 w.id) w.name

let () =
  clear_mock_responses ();
  setup_select_one_response
    (Some (make_mock_row [ mock_int 1L; mock_text "alice"; mock_text "a@x" ]));
  begin match Get_user_col.(select () (who_of_scope (module Cols))
          ~id:(User_id.get_column 1L)) with
  | Some w -> show "Q1" w
  | None -> print_endline "Q1 none"
  end;
  setup_select_response
    [ make_mock_row [ mock_text "root"; mock_int 7L; mock_text "bob" ];
      make_mock_row [ mock_text "ops"; mock_int 8L; mock_text "carol" ] ];
  List_admins_col.(select () (who_of_scope (module Cols)) (show "Q2"))
