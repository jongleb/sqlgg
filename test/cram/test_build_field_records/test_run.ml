open Printf

module M (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Datetime.t = float) = struct

  module Sql = Output.Sqlgg(T)

  let test_list_users connection =
    printf "[TEST 1] list_users: callback receives records, access fields\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1L;
        Print_ocaml_impl.mock_text "Alice";
        Print_ocaml_impl.mock_text "Smith";
        Print_ocaml_impl.mock_text "alice@example.com";
        Print_ocaml_impl.mock_text "+1234567890";
        Print_ocaml_impl.mock_float 1700000000.0
      ];
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 2L;
        Print_ocaml_impl.mock_text "Bob";
        Print_ocaml_impl.mock_text "Jones";
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_float 1700000001.0
      ];
    ];
    Sql.list_users connection (fun ~id ~name ~contact ~created_at ->
      printf "  id=%Ld given=%s family=%s" id name.Sql.Name.given name.Sql.Name.family;
      printf " email=%s phone=%s"
        (match contact.Sql.Contact.email with Some e -> e | None -> "NULL")
        (match contact.Sql.Contact.phone with Some p -> p | None -> "NULL");
      printf " created_at=%.0f\n" created_at
    );
    printf "[TEST 1] OK\n\n"

  let test_get_user connection =
    printf "[TEST 2] get_user: select_one_maybe returns tuple with records\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1L;
        Print_ocaml_impl.mock_text "Alice";
        Print_ocaml_impl.mock_text "Smith";
        Print_ocaml_impl.mock_text "alice@example.com";
        Print_ocaml_impl.mock_text "+1234567890";
        Print_ocaml_impl.mock_float 1700000000.0
      ]
    ));
    (match Sql.get_user connection ~id:1L with
     | Some (id, name, contact, created_at) ->
       printf "  id=%Ld given=%s family=%s" id name.Sql.Name.given name.Sql.Name.family;
       printf " email=%s phone=%s"
         (match contact.Sql.Contact.email with Some e -> e | None -> "NULL")
         (match contact.Sql.Contact.phone with Some p -> p | None -> "NULL");
       printf " created_at=%.0f\n" created_at
     | None ->
       printf "  UNEXPECTED: no row\n");
    printf "[TEST 2] OK\n\n"

  let test_get_user_none connection =
    printf "[TEST 3] get_user: select_one_maybe returns None\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response None;
    (match Sql.get_user connection ~id:999L with
     | Some _ -> printf "  UNEXPECTED: got a row\n"
     | None -> printf "  correctly returned None\n");
    printf "[TEST 3] OK\n\n"

  let test_get_name connection =
    printf "[TEST 4] get_name: select_one_maybe returns single record\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_text "Alice";
        Print_ocaml_impl.mock_text "Smith"
      ]
    ));
    (match Sql.get_name connection ~id:1L with
     | Some name ->
       printf "  given=%s family=%s\n" name.Sql.Name.given name.Sql.Name.family
     | None ->
       printf "  UNEXPECTED: no row\n");
    printf "[TEST 4] OK\n\n"

  let test_list_names connection =
    printf "[TEST 5] list_names: callback with single record arg\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_text "Alice";
        Print_ocaml_impl.mock_text "Smith"
      ];
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_text "Bob";
        Print_ocaml_impl.mock_text "Jones"
      ];
    ];
    Sql.list_names connection (fun ~name ->
      printf "  given=%s family=%s\n" name.Sql.Name.given name.Sql.Name.family
    );
    printf "[TEST 5] OK\n\n"

  let test_insert_user connection =
    printf "[TEST 6] insert_user: INSERT with record-annotated columns\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
    let result = Sql.insert_user connection
      ~id:10L
      ~given_name:"Charlie"
      ~family_name:"Brown"
      ~email:(Some "charlie@example.com")
      ~phone:None
      ~created_at:1700000099.0
    in
    printf "  affected_rows=%Ld\n" result.affected_rows;
    printf "[TEST 6] OK\n\n"

  let test_update_name connection =
    printf "[TEST 7] update_name: UPDATE record-annotated columns\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
    let result = Sql.update_name connection
      ~given_name:"Charles"
      ~family_name:"Schulz"
      ~id:10L
    in
    printf "  affected_rows=%Ld\n" result.affected_rows;
    printf "[TEST 7] OK\n\n"

  let test_update_contact connection =
    printf "[TEST 8] update_contact: UPDATE nullable record-annotated columns\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
    let result = Sql.update_contact connection
      ~email:(Some "new@example.com")
      ~phone:(Some "+9876543210")
      ~id:10L
    in
    printf "  affected_rows=%Ld\n" result.affected_rows;
    printf "[TEST 8] OK\n\n"

  let test_roundtrip connection =
    printf "[TEST 9] roundtrip: INSERT then SELECT, access record fields\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
    let _ = Sql.insert_user connection
      ~id:20L
      ~given_name:"Diana"
      ~family_name:"Prince"
      ~email:(Some "diana@example.com")
      ~phone:(Some "+1111111111")
      ~created_at:1700000200.0
    in
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 20L;
        Print_ocaml_impl.mock_text "Diana";
        Print_ocaml_impl.mock_text "Prince";
        Print_ocaml_impl.mock_text "diana@example.com";
        Print_ocaml_impl.mock_text "+1111111111";
        Print_ocaml_impl.mock_float 1700000200.0
      ]
    ));
    (match Sql.get_user connection ~id:20L with
     | Some (id, name, contact, _created_at) ->
       printf "  inserted id=%Ld\n" id;
       printf "  name.given=%s name.family=%s\n" name.Sql.Name.given name.Sql.Name.family;
       printf "  contact.email=%s contact.phone=%s\n"
         (match contact.Sql.Contact.email with Some e -> e | None -> "NULL")
         (match contact.Sql.Contact.phone with Some p -> p | None -> "NULL")
     | None ->
       printf "  UNEXPECTED: no row after insert\n");
    printf "[TEST 9] OK\n\n"

  let test_get_account connection =
    printf "[TEST 10] get_account: module= on id + module= inside record\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 42L;
        Print_ocaml_impl.mock_text "Bruce";
        Print_ocaml_impl.mock_text "Wayne";
        Print_ocaml_impl.mock_text "bruce@wayne.enterprises";
        Print_ocaml_impl.mock_int 1000000L
      ]
    ));
    let uid = User_id.get_column 42L in
    (match Sql.get_account connection ~id:uid with
     | Some (id, owner, balance) ->
       printf "  id=%s\n" (User_id.to_string id);
       printf "  owner.given=%s owner.family=%s\n" owner.Sql.Owner.given owner.Sql.Owner.family;
       printf "  owner.email=%s (domain=%s)\n"
         (Email_addr.to_string owner.Sql.Owner.email)
         (Email_addr.domain owner.Sql.Owner.email);
       printf "  balance=%Ld\n" balance
     | None ->
       printf "  UNEXPECTED: no row\n");
    printf "[TEST 10] OK\n\n"

  let test_list_accounts connection =
    printf "[TEST 11] list_accounts: callback with User_id.t + owner record with Email_addr.t\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 42L;
        Print_ocaml_impl.mock_text "Bruce";
        Print_ocaml_impl.mock_text "Wayne";
        Print_ocaml_impl.mock_text "bruce@wayne.enterprises";
        Print_ocaml_impl.mock_int 1000000L
      ];
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 43L;
        Print_ocaml_impl.mock_text "Clark";
        Print_ocaml_impl.mock_text "Kent";
        Print_ocaml_impl.mock_text "clark@dailyplanet.com";
        Print_ocaml_impl.mock_int 50000L
      ];
    ];
    Sql.list_accounts connection (fun ~id ~owner ~balance ->
      printf "  id=%s owner=%s %s email=%s (domain=%s) balance=%Ld\n"
        (User_id.to_string id)
        owner.Sql.Owner.given owner.Sql.Owner.family
        (Email_addr.to_string owner.Sql.Owner.email)
        (Email_addr.domain owner.Sql.Owner.email)
        balance
    );
    printf "[TEST 11] OK\n\n"

  let test_insert_account connection =
    printf "[TEST 12] insert_account: INSERT with User_id.t param + Email_addr.t param\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
    let uid = User_id.get_column 99L in
    let email = Email_addr.get_column "test@example.org" in
    let result = Sql.insert_account connection
      ~id:uid
      ~given_name:"Peter"
      ~family_name:"Parker"
      ~email
      ~balance:500L
    in
    printf "  affected_rows=%Ld\n" result.affected_rows;
    printf "[TEST 12] OK\n\n"

  let test_account_roundtrip connection =
    printf "[TEST 13] account roundtrip: INSERT then SELECT, access module-wrapped record fields\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
    let uid = User_id.get_column 77L in
    let email = Email_addr.get_column "tony@stark.industries" in
    let _ = Sql.insert_account connection
      ~id:uid ~given_name:"Tony" ~family_name:"Stark" ~email ~balance:999999L
    in
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 77L;
        Print_ocaml_impl.mock_text "Tony";
        Print_ocaml_impl.mock_text "Stark";
        Print_ocaml_impl.mock_text "tony@stark.industries";
        Print_ocaml_impl.mock_int 999999L
      ]
    ));
    (match Sql.get_account connection ~id:uid with
     | Some (id, owner, balance) ->
       printf "  id=%s\n" (User_id.to_string id);
       printf "  owner.given=%s owner.family=%s\n" owner.Sql.Owner.given owner.Sql.Owner.family;
       printf "  owner.email=%s domain=%s\n"
         (Email_addr.to_string owner.Sql.Owner.email)
         (Email_addr.domain owner.Sql.Owner.email);
       printf "  balance=%Ld\n" balance
     | None ->
       printf "  UNEXPECTED: no row\n");
    printf "[TEST 13] OK\n\n"

  let test_get_order connection =
    printf "[TEST 14] get_order: module= outside record + nullable module= inside record + Money.t\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1001L;
        Print_ocaml_impl.mock_int 42L;
        Print_ocaml_impl.mock_text "Gotham";
        Print_ocaml_impl.mock_text "10001";
        Print_ocaml_impl.mock_text "bruce@wayne.enterprises";
        Print_ocaml_impl.mock_int 9999L
      ]
    ));
    (match Sql.get_order connection ~id:1001L with
     | Some (id, buyer_id, shipping, amount) ->
       printf "  id=%Ld buyer=%s\n" id (User_id.to_string buyer_id);
       printf "  shipping.city=%s shipping.zip=%s\n" shipping.Sql.Shipping.city shipping.Sql.Shipping.zip;
       printf "  shipping.notify_email=%s\n"
         (match shipping.Sql.Shipping.notify_email with
          | Some e -> Email_addr.to_string e ^ " (domain=" ^ Email_addr.domain e ^ ")"
          | None -> "NULL");
       printf "  amount=%s\n" (Money.to_string amount)
     | None ->
       printf "  UNEXPECTED: no row\n");
    printf "[TEST 14] OK\n\n"

  let test_get_order_null_email connection =
    printf "[TEST 15] get_order: nullable module= Email_addr inside record is None\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1002L;
        Print_ocaml_impl.mock_int 43L;
        Print_ocaml_impl.mock_text "Metropolis";
        Print_ocaml_impl.mock_text "20002";
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_int 4500L
      ]
    ));
    (match Sql.get_order connection ~id:1002L with
     | Some (id, buyer_id, shipping, amount) ->
       printf "  id=%Ld buyer=%s\n" id (User_id.to_string buyer_id);
       printf "  shipping.city=%s shipping.zip=%s\n" shipping.Sql.Shipping.city shipping.Sql.Shipping.zip;
       printf "  shipping.notify_email=%s\n"
         (match shipping.Sql.Shipping.notify_email with
          | Some e -> Email_addr.to_string e
          | None -> "NULL");
       printf "  amount=%s\n" (Money.to_string amount)
     | None ->
       printf "  UNEXPECTED: no row\n");
    printf "[TEST 15] OK\n\n"

  let test_list_orders connection =
    printf "[TEST 16] list_orders_by_buyer: callback with mixed modules and record\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1001L;
        Print_ocaml_impl.mock_int 42L;
        Print_ocaml_impl.mock_text "Gotham";
        Print_ocaml_impl.mock_text "10001";
        Print_ocaml_impl.mock_text "alfred@wayne.enterprises";
        Print_ocaml_impl.mock_int 25000L
      ];
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1003L;
        Print_ocaml_impl.mock_int 42L;
        Print_ocaml_impl.mock_text "Bludhaven";
        Print_ocaml_impl.mock_text "10099";
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_int 750L
      ];
    ];
    let buyer = User_id.get_column 42L in
    Sql.list_orders_by_buyer connection ~buyer_id:buyer (fun ~id ~buyer_id ~shipping ~amount ->
      printf "  order=%Ld buyer=%s city=%s zip=%s notify=%s amount=%s\n"
        id (User_id.to_string buyer_id)
        shipping.Sql.Shipping.city shipping.Sql.Shipping.zip
        (match shipping.Sql.Shipping.notify_email with Some e -> Email_addr.to_string e | None -> "NULL")
        (Money.to_string amount)
    );
    printf "[TEST 16] OK\n\n"

  let test_insert_order connection =
    printf "[TEST 17] insert_order: INSERT with User_id + Email_addr option + Money params\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
    let buyer = User_id.get_column 42L in
    let amount = Money.of_cents 15099L in
    let result = Sql.insert_order connection
      ~id:2001L
      ~buyer_id:buyer
      ~ship_city:"Star City"
      ~ship_zip:"30003"
      ~notify_email:(Some (Email_addr.get_column "oliver@queen.industries"))
      ~amount
    in
    printf "  affected_rows=%Ld\n" result.affected_rows;
    printf "[TEST 17] OK\n\n"

  let test_order_roundtrip connection =
    printf "[TEST 18] order roundtrip: INSERT then SELECT, verify all module-wrapped fields\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_execute_response ~affected_rows:1L ();
    let buyer = User_id.get_column 43L in
    let amount = Money.of_cents 3300L in
    let _ = Sql.insert_order connection
      ~id:2002L ~buyer_id:buyer
      ~ship_city:"Central City" ~ship_zip:"40004"
      ~notify_email:None ~amount
    in
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 2002L;
        Print_ocaml_impl.mock_int 43L;
        Print_ocaml_impl.mock_text "Central City";
        Print_ocaml_impl.mock_text "40004";
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_int 3300L
      ]
    ));
    (match Sql.get_order connection ~id:2002L with
     | Some (id, buyer_id, shipping, amount) ->
       printf "  id=%Ld buyer=%s\n" id (User_id.to_string buyer_id);
       printf "  shipping.city=%s zip=%s\n" shipping.Sql.Shipping.city shipping.Sql.Shipping.zip;
       printf "  shipping.notify_email=%s\n"
         (match shipping.Sql.Shipping.notify_email with Some e -> Email_addr.to_string e | None -> "NULL");
       printf "  amount=%s\n" (Money.to_string amount)
     | None ->
       printf "  UNEXPECTED: no row\n");
    printf "[TEST 18] OK\n\n"

  let test_join_get connection =
    printf "[TEST 19] JOIN: get_order_with_buyer, records from two tables\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1001L;
        Print_ocaml_impl.mock_int 42L;
        Print_ocaml_impl.mock_text "Bruce";
        Print_ocaml_impl.mock_text "Wayne";
        Print_ocaml_impl.mock_text "Gotham";
        Print_ocaml_impl.mock_text "10001";
        Print_ocaml_impl.mock_text "alfred@wayne.enterprises";
        Print_ocaml_impl.mock_int 9999L
      ]
    ));
    (match Sql.get_order_with_buyer connection ~order_id:1001L with
     | Some (id, buyer_id, name, shipping, amount) ->
       printf "  order_id=%Ld buyer=%s\n" id (User_id.to_string buyer_id);
       printf "  name.given=%s name.family=%s\n" name.Sql.Name.given name.Sql.Name.family;
       printf "  shipping.city=%s shipping.zip=%s\n" shipping.Sql.Shipping.city shipping.Sql.Shipping.zip;
       printf "  shipping.notify=%s\n"
         (match shipping.Sql.Shipping.notify_email with
          | Some e -> Email_addr.to_string e ^ " (domain=" ^ Email_addr.domain e ^ ")"
          | None -> "NULL");
       printf "  amount=%s\n" (Money.to_string amount)
     | None ->
       printf "  UNEXPECTED: no row\n");
    printf "[TEST 19] OK\n\n"

  let test_join_list connection =
    printf "[TEST 20] JOIN: list_orders_with_buyers, name record from users + shipping from orders\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1001L;
        Print_ocaml_impl.mock_int 42L;
        Print_ocaml_impl.mock_text "Bruce";
        Print_ocaml_impl.mock_text "Wayne";
        Print_ocaml_impl.mock_text "Gotham";
        Print_ocaml_impl.mock_text "10001";
        Print_ocaml_impl.mock_text "alfred@wayne.enterprises";
        Print_ocaml_impl.mock_int 25000L
      ];
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1002L;
        Print_ocaml_impl.mock_int 43L;
        Print_ocaml_impl.mock_text "Clark";
        Print_ocaml_impl.mock_text "Kent";
        Print_ocaml_impl.mock_text "Metropolis";
        Print_ocaml_impl.mock_text "20002";
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_int 4500L
      ];
    ];
    Sql.list_orders_with_buyers connection (fun ~id ~buyer_id ~name ~shipping ~amount ->
      printf "  order=%Ld buyer=%s name=%s %s city=%s zip=%s notify=%s amount=%s\n"
        id (User_id.to_string buyer_id)
        name.Sql.Name.given name.Sql.Name.family
        shipping.Sql.Shipping.city shipping.Sql.Shipping.zip
        (match shipping.Sql.Shipping.notify_email with Some e -> Email_addr.to_string e | None -> "NULL")
        (Money.to_string amount)
    );
    printf "[TEST 20] OK\n\n"

  let test_left_join connection =
    printf "[TEST 21] LEFT JOIN: name record + shipping record option\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1L;
        Print_ocaml_impl.mock_text "Alice";
        Print_ocaml_impl.mock_text "Smith";
        Print_ocaml_impl.mock_int 1001L;
        Print_ocaml_impl.mock_text "Gotham";
        Print_ocaml_impl.mock_text "10001";
        Print_ocaml_impl.mock_text "alfred@wayne.enterprises";
        Print_ocaml_impl.mock_int 9999L
      ];
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 2L;
        Print_ocaml_impl.mock_text "Bob";
        Print_ocaml_impl.mock_text "Jones";
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null
      ];
    ];
    Sql.left_join_orders connection (fun ~id ~name ~id0 ~shipping ~amount ->
      printf "  user=%Ld name=%s %s order=%s"
        id name.Sql.Name.given name.Sql.Name.family
        (match id0 with Some oid -> Int64.to_string oid | None -> "NULL");
      (match shipping with
       | Some s ->
         printf " shipping=%s/%s/%s"
           s.Sql.Shipping.city s.Sql.Shipping.zip
           (match s.Sql.Shipping.notify_email with Some e -> Email_addr.to_string e | None -> "NULL")
       | None -> printf " shipping=NULL");
      printf " amount=%s\n"
        (match amount with Some m -> Money.to_string m | None -> "NULL")
    );
    printf "[TEST 21] OK\n\n"

  let test_right_join connection =
    printf "[TEST 22] RIGHT JOIN: shipping record option + name record\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1001L;
        Print_ocaml_impl.mock_text "Gotham";
        Print_ocaml_impl.mock_text "10001";
        Print_ocaml_impl.mock_text "alfred@wayne.enterprises";
        Print_ocaml_impl.mock_int 9999L;
        Print_ocaml_impl.mock_int 1L;
        Print_ocaml_impl.mock_text "Alice";
        Print_ocaml_impl.mock_text "Smith"
      ];
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_int 2L;
        Print_ocaml_impl.mock_text "Bob";
        Print_ocaml_impl.mock_text "Jones"
      ];
    ];
    Sql.right_join_users connection (fun ~id ~shipping ~amount ~id0 ~name ->
      printf "  order=%s"
        (match id with Some oid -> Int64.to_string oid | None -> "NULL");
      (match shipping with
       | Some s ->
         printf " shipping=%s/%s/%s"
           s.Sql.Shipping.city s.Sql.Shipping.zip
           (match s.Sql.Shipping.notify_email with Some e -> Email_addr.to_string e | None -> "NULL")
       | None -> printf " shipping=NULL");
      printf " amount=%s user=%Ld name=%s %s\n"
        (match amount with Some m -> Money.to_string m | None -> "NULL")
        id0 name.Sql.Name.given name.Sql.Name.family
    );
    printf "[TEST 22] OK\n\n"

  let test_union connection =
    printf "[TEST 23] UNION ALL: name records from two queries\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_response [
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_text "Alice";
        Print_ocaml_impl.mock_text "Smith"
      ];
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_text "Bob";
        Print_ocaml_impl.mock_text "Jones"
      ];
    ];
    Sql.union_names connection ~id1:1L ~id2:2L (fun ~name ->
      printf "  name=%s %s\n" name.Sql.Name.given name.Sql.Name.family
    );
    printf "[TEST 23] OK\n\n"

  let test_left_join_some connection =
    printf "[TEST 24] LEFT JOIN select_one: user with order -> Shipping.t option = Some\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 1L;
        Print_ocaml_impl.mock_text "Alice";
        Print_ocaml_impl.mock_text "Smith";
        Print_ocaml_impl.mock_int 1001L;
        Print_ocaml_impl.mock_text "Gotham";
        Print_ocaml_impl.mock_text "10001";
        Print_ocaml_impl.mock_text "bruce@wayne.enterprises";
        Print_ocaml_impl.mock_int 9999L
      ]
    ));
    (match Sql.left_join_order_one connection ~user_id:1L with
     | Some (uid, name, order_id, shipping, amount) ->
       printf "  uid=%Ld name=%s %s\n" uid name.Sql.Name.given name.Sql.Name.family;
       assert (name.Sql.Name.given = "Alice");
       assert (name.Sql.Name.family = "Smith");
       printf "  order_id=%s\n" (match order_id with Some oid -> Int64.to_string oid | None -> "NULL");
       assert (order_id = Some 1001L);
       (match shipping with
        | Some s ->
          printf "  shipping.city=%s\n" s.Sql.Shipping.city;
          printf "  shipping.zip=%s\n" s.Sql.Shipping.zip;
          printf "  shipping.notify=%s\n"
            (match s.Sql.Shipping.notify_email with
             | Some e -> Email_addr.to_string e
             | None -> "NULL");
          assert (s.Sql.Shipping.city = "Gotham");
          assert (s.Sql.Shipping.zip = "10001");
          assert (s.Sql.Shipping.notify_email <> None);
          let email = Option.get s.Sql.Shipping.notify_email in
          printf "  email domain=%s\n" (Email_addr.domain email);
          assert (Email_addr.domain email = "wayne.enterprises")
        | None ->
          printf "  UNEXPECTED: shipping is None\n"; assert false);
       printf "  amount=%s\n" (match amount with Some m -> Money.to_string m | None -> "NULL");
       assert (amount <> None);
       let m = Option.get amount in
       assert (Money.to_string m = "99.99")
     | None ->
       printf "  UNEXPECTED: no row\n"; assert false);
    printf "[TEST 24] OK\n\n"

  let test_left_join_none connection =
    printf "[TEST 25] LEFT JOIN select_one: user without order -> Shipping.t option = None\n";
    Print_ocaml_impl.clear_mock_responses ();
    Print_ocaml_impl.setup_select_one_response (Some (
      Print_ocaml_impl.make_mock_row [
        Print_ocaml_impl.mock_int 2L;
        Print_ocaml_impl.mock_text "Bob";
        Print_ocaml_impl.mock_text "Jones";
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null;
        Print_ocaml_impl.mock_null
      ]
    ));
    (match Sql.left_join_order_one connection ~user_id:2L with
     | Some (uid, name, order_id, shipping, amount) ->
       printf "  uid=%Ld name=%s %s\n" uid name.Sql.Name.given name.Sql.Name.family;
       assert (name.Sql.Name.given = "Bob");
       assert (name.Sql.Name.family = "Jones");
       printf "  order_id=%s\n" (match order_id with Some _ -> "UNEXPECTED" | None -> "NULL");
       assert (order_id = None);
       printf "  shipping=%s\n" (match shipping with Some _ -> "UNEXPECTED" | None -> "None");
       assert (shipping = None);
       printf "  amount=%s\n" (match amount with Some _ -> "UNEXPECTED" | None -> "None");
       assert (amount = None)
     | None ->
       printf "  UNEXPECTED: no row\n"; assert false);
    printf "[TEST 25] OK\n\n"

  let run_all connection =
    printf "=== Field Records Tests ===\n\n";
    test_list_users connection;
    test_get_user connection;
    test_get_user_none connection;
    test_get_name connection;
    test_list_names connection;
    test_insert_user connection;
    test_update_name connection;
    test_update_contact connection;
    test_roundtrip connection;
    test_get_account connection;
    test_list_accounts connection;
    test_insert_account connection;
    test_account_roundtrip connection;
    test_get_order connection;
    test_get_order_null_email connection;
    test_list_orders connection;
    test_insert_order connection;
    test_order_roundtrip connection;
    test_join_get connection;
    test_join_list connection;
    test_left_join connection;
    test_right_join connection;
    test_union connection;
    test_left_join_some connection;
    test_left_join_none connection;
    printf "=== All Field Records Tests Passed ===\n"
end

module Test = M(Print_ocaml_impl)

let () =
  let con = () in
  printf "Field Records Compile-and-Run Tests\n";
  printf "%s\n" (String.make 50 '=');
  Test.run_all con;
  printf "\n%s\n" (String.make 50 '=');
  printf "All tests executed successfully!\n"
