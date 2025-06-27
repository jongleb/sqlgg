module M (T: Sqlgg_traits.M with type Types.Json.t = Yojson.Basic.t and
  type Types.Json_path.t = Sqlgg_json_path.Ast.t and
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.One_or_all.t = [`One | `All]) = struct

  module Sql = Output.Sqlgg(T)

  let test_call connection = 
    Sql.test1 ~json:
      (`Assoc
         [ "users",
           `List
             [ `Assoc
                 [ "id", `Int 1;
                   "settings",
                   `Assoc [ "themes", `List [ `String "dark" ] ]
                 ]
             ]
         ])
    ~one:`One connection

  let test_call2 connection =
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let from_combinator = root / ~."data" / any / ~."users" / last / ~."settings" in (* $.data[*].users[last].settings *)
    Sql.test2 ~path:from_combinator connection

  let test_call3 connection =
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let name_path = root / ~."name" in (* $.name *)
    Sql.test3 ~id:1L ~name_path connection (fun ~r -> ())

  let test_call4 connection =
    let open Sqlgg_json_path in
    let open Ast.Syntax in  
    let email_path = root / ~."user" / ~."email" in (* $.user.email *)
    Sql.test4 ~id:2L ~email_path connection (fun ~r -> ())

  let test_call5 connection =
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let login_path = root / ~."last_login" in (* $.last_login *)
    Sql.test5 ~id:3L ~login_path connection

  let test_call6 connection =
    Sql.test6 ~id:4L connection (fun ~r -> ())

  let test_call7 connection =
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let category_path = root / ~."metadata" / ~."category" in (* $.metadata.category *)
    Sql.test7 ~category_path ~category:(`String "electronics") connection (fun ~id ~name -> ())

  let test_call8 connection =
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let name_path = root / ~."profile" / ~."name" in (* $.profile.name *)
    let theme_path = root / ~."settings" / ~."theme" in (* $.settings.theme *)
    let active_path = root / ~."profile" / ~."active" in (* $.profile.active *)
    Sql.test8 ~name_path ~theme_path ~active_path connection (fun ~id ~user_name ~theme -> ())

  let test_call9 connection =
    let open Sqlgg_json_path in
    let open Ast.Syntax in
    let update_path = root / ~."timestamps" / ~."last_update" in (* $.timestamps.last_update *)
    let role_path = root / ~."user" / ~."role" in (* $.user.role *)
    Sql.test9 ~update_path ~role_path ~role:(`String "admin") connection

  let test_call10 connection =
    Sql.test10 ~search_value:"admin" ~id:5L connection (fun ~r -> ())
end

module Test = M(Print_ocaml_impl)

let () = 
  let con = "test_connection" in
  let _ = Test.test_call con 
  and _ = Test.test_call2 con 
  and _ = Test.test_call3 con 
  and _ = Test.test_call4 con
  and _ = Test.test_call5 con
  and _ = Test.test_call6 con
  and _ = Test.test_call7 con
  and _ = Test.test_call8 con
  and _ = Test.test_call9 con in
  ignore (Test.test_call10 con);
  Printf.printf "All JSON tests executed successfully.\n"
