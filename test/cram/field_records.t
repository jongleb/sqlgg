Record field grouping: basic case with two records and plain columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF'
  > CREATE TABLE users (
  >   id          BIGINT PRIMARY KEY,
  >   -- [sqlgg] record=name.given
  >   given_name  TEXT NOT NULL,
  >   -- [sqlgg] record=name.family
  >   family_name TEXT NOT NULL,
  >   -- [sqlgg] record=contact.email
  >   email       TEXT,
  >   -- [sqlgg] record=contact.phone
  >   phone       TEXT,
  >   created_at  TIMESTAMP NOT NULL
  > );
  > SELECT id, given_name, family_name, email, phone, created_at FROM users;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    module Name = struct
      type t = { given : T.Types.Text.t; family : T.Types.Text.t }
    end
    module Contact = struct
      type t = { email : T.Types.Text.t option; phone : T.Types.Text.t option }
    end
  
    let create_users db  =
      T.execute db ("CREATE TABLE users (\n\
    id          BIGINT PRIMARY KEY,\n\
      given_name  TEXT NOT NULL,\n\
      family_name TEXT NOT NULL,\n\
      email       TEXT,\n\
      phone       TEXT,\n\
    created_at  TIMESTAMP NOT NULL\n\
  )") T.no_params
  
    let select_1 db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:{ Name.given = (T.get_column_Text stmt 1); Name.family = (T.get_column_Text stmt 2) }
          ~contact:{ Contact.email = (T.get_column_Text_nullable stmt 3); Contact.phone = (T.get_column_Text_nullable stmt 4) }
          ~created_at:(T.get_column_Datetime stmt 5)
      in
      T.select db ("SELECT id, given_name, family_name, email, phone, created_at FROM users") T.no_params invoke_callback
  
    module Fold = struct
      let select_1 db  callback acc =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
            ~name:{ Name.given = (T.get_column_Text stmt 1); Name.family = (T.get_column_Text stmt 2) }
            ~contact:{ Contact.email = (T.get_column_Text_nullable stmt 3); Contact.phone = (T.get_column_Text_nullable stmt 4) }
            ~created_at:(T.get_column_Datetime stmt 5)
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT id, given_name, family_name, email, phone, created_at FROM users") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_1 db  callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
            ~name:{ Name.given = (T.get_column_Text stmt 1); Name.family = (T.get_column_Text stmt 2) }
            ~contact:{ Contact.email = (T.get_column_Text_nullable stmt 3); Contact.phone = (T.get_column_Text_nullable stmt 4) }
            ~created_at:(T.get_column_Datetime stmt 5)
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT id, given_name, family_name, email, phone, created_at FROM users") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Record field grouping: select_one_maybe with record:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF'
  > CREATE TABLE users (
  >   id          BIGINT PRIMARY KEY,
  >   -- [sqlgg] record=name.given
  >   given_name  TEXT NOT NULL,
  >   -- [sqlgg] record=name.family
  >   family_name TEXT NOT NULL,
  >   created_at  TIMESTAMP NOT NULL
  > );
  > -- @get_user
  > SELECT id, given_name, family_name, created_at FROM users WHERE id = @id LIMIT 1;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    module Name = struct
      type t = { given : T.Types.Text.t; family : T.Types.Text.t }
    end
  
    let create_users db  =
      T.execute db ("CREATE TABLE users (\n\
    id          BIGINT PRIMARY KEY,\n\
      given_name  TEXT NOT NULL,\n\
      family_name TEXT NOT NULL,\n\
    created_at  TIMESTAMP NOT NULL\n\
  )") T.no_params
  
    let get_user db ~id =
      let get_row stmt =
        (T.get_column_Int stmt 0), { Name.given = (T.get_column_Text stmt 1); Name.family = (T.get_column_Text stmt 2) }, (T.get_column_Datetime stmt 3)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select_one_maybe db ("SELECT id, given_name, family_name, created_at FROM users WHERE id = ? LIMIT 1") set_params get_row
  
    module Single = struct
      let get_user db ~id callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
            ~name:{ Name.given = (T.get_column_Text stmt 1); Name.family = (T.get_column_Text stmt 2) }
            ~created_at:(T.get_column_Datetime stmt 3)
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select_one_maybe db ("SELECT id, given_name, family_name, created_at FROM users WHERE id = ? LIMIT 1") set_params invoke_callback
  
    end (* module Single *)
  end (* module Sqlgg *)

Record field grouping: record with module= on a field inside:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF'
  > CREATE TABLE users (
  >   id          BIGINT PRIMARY KEY,
  >   -- [sqlgg] record=name.given
  >   given_name  TEXT NOT NULL,
  >   -- [sqlgg] record=name.family
  >   -- [sqlgg] module=Custom
  >   family_name TEXT NOT NULL,
  >   created_at  TIMESTAMP NOT NULL
  > );
  > SELECT id, given_name, family_name, created_at FROM users;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    module Name = struct
      type t = { given : T.Types.Text.t; family : Custom.t }
    end
  
    let create_users db  =
      T.execute db ("CREATE TABLE users (\n\
    id          BIGINT PRIMARY KEY,\n\
      given_name  TEXT NOT NULL,\n\
        family_name TEXT NOT NULL,\n\
    created_at  TIMESTAMP NOT NULL\n\
  )") T.no_params
  
    let select_1 db  callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:{ Name.given = (T.get_column_Text stmt 1); Name.family = (Custom.get_column (T.get_column_string stmt 2)) }
          ~created_at:(T.get_column_Datetime stmt 3)
      in
      T.select db ("SELECT id, given_name, family_name, created_at FROM users") T.no_params invoke_callback
  
    module Fold = struct
      let select_1 db  callback acc =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
            ~name:{ Name.given = (T.get_column_Text stmt 1); Name.family = (Custom.get_column (T.get_column_string stmt 2)) }
            ~created_at:(T.get_column_Datetime stmt 3)
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT id, given_name, family_name, created_at FROM users") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_1 db  callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int stmt 0)
            ~name:{ Name.given = (T.get_column_Text stmt 1); Name.family = (Custom.get_column (T.get_column_string stmt 2)) }
            ~created_at:(T.get_column_Datetime stmt 3)
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT id, given_name, family_name, created_at FROM users") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Record field grouping: single record as only result:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF'
  > CREATE TABLE users (
  >   id          BIGINT PRIMARY KEY,
  >   -- [sqlgg] record=name.given
  >   given_name  TEXT NOT NULL,
  >   -- [sqlgg] record=name.family
  >   family_name TEXT NOT NULL
  > );
  > SELECT given_name, family_name FROM users;
  > -- @get_name
  > SELECT given_name, family_name FROM users WHERE id = @id LIMIT 1;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    module Name = struct
      type t = { given : T.Types.Text.t; family : T.Types.Text.t }
    end
  
    let create_users db  =
      T.execute db ("CREATE TABLE users (\n\
    id          BIGINT PRIMARY KEY,\n\
      given_name  TEXT NOT NULL,\n\
      family_name TEXT NOT NULL\n\
  )") T.no_params
  
    let select_1 db  callback =
      let invoke_callback stmt =
        callback
          ~name:{ Name.given = (T.get_column_Text stmt 0); Name.family = (T.get_column_Text stmt 1) }
      in
      T.select db ("SELECT given_name, family_name FROM users") T.no_params invoke_callback
  
    let get_name db ~id =
      let get_row stmt =
        { Name.given = (T.get_column_Text stmt 0); Name.family = (T.get_column_Text stmt 1) }
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select_one_maybe db ("SELECT given_name, family_name FROM users WHERE id = ? LIMIT 1") set_params get_row
  
    module Single = struct
      let get_name db ~id callback =
        let invoke_callback stmt =
          callback
            ~name:{ Name.given = (T.get_column_Text stmt 0); Name.family = (T.get_column_Text stmt 1) }
        in
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select_one_maybe db ("SELECT given_name, family_name FROM users WHERE id = ? LIMIT 1") set_params invoke_callback
  
    end (* module Single *)
    
    module Fold = struct
      let select_1 db  callback acc =
        let invoke_callback stmt =
          callback
            ~name:{ Name.given = (T.get_column_Text stmt 0); Name.family = (T.get_column_Text stmt 1) }
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT given_name, family_name FROM users") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_1 db  callback =
        let invoke_callback stmt =
          callback
            ~name:{ Name.given = (T.get_column_Text stmt 0); Name.family = (T.get_column_Text stmt 1) }
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT given_name, family_name FROM users") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Record field grouping: compile and run with mock trait:
  $ cp test_build_field_records/field_records.sql .
  $ cp test_build_field_records/test_run.ml .
  $ cp test_build_field_records/user_id.ml .
  $ cp test_build_field_records/email_addr.ml .
  $ cp test_build_field_records/money.ml .
  $ cat field_records.sql | sqlgg -no-header -gen caml -dialect mysql - > output.ml 2>/dev/null
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c user_id.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c email_addr.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c money.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c output.ml
  $ ocamlfind ocamlc -package sqlgg.traits -I . -c test_run.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o test_run.exe print_ocaml_impl.cmo user_id.cmo email_addr.cmo money.cmo output.cmo test_run.ml
  $ ./test_run.exe
  Field Records Compile-and-Run Tests
  ==================================================
  === Field Records Tests ===
  
  [TEST 1] list_users: callback receives records, access fields
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id, given_name, family_name, email, phone, created_at FROM users
  [MOCK] Returning 2 rows
    Row 0: col0=1 col1=Alice col2=Smith col3=alice@example.com col4=+1234567890 col5=1700000000. 
  [MOCK] get_column_Datetime[5] = 1700000000.000000
  [MOCK] get_column_Text_nullable[4] = Some "+1234567890"
  [MOCK] get_column_Text_nullable[3] = Some "alice@example.com"
  [MOCK] get_column_Text[2] = "Smith"
  [MOCK] get_column_Text[1] = "Alice"
  [MOCK] get_column_Int[0] = 1
    id=1 given=Alice family=Smith email=alice@example.com phone=+1234567890 created_at=1700000000
    Row 1: col0=2 col1=Bob col2=Jones col3=NULL col4=NULL col5=1700000001. 
  [MOCK] get_column_Datetime[5] = 1700000001.000000
  [MOCK] get_column_Text_nullable[4] = None
  [MOCK] get_column_Text_nullable[3] = None
  [MOCK] get_column_Text[2] = "Jones"
  [MOCK] get_column_Text[1] = "Bob"
  [MOCK] get_column_Int[0] = 2
    id=2 given=Bob family=Jones email=NULL phone=NULL created_at=1700000001
  [TEST 1] OK
  
  [TEST 2] get_user: select_one_maybe returns tuple with records
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, given_name, family_name, email, phone, created_at FROM users WHERE id = 1 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Datetime[5] = 1700000000.000000
  [MOCK] get_column_Text_nullable[4] = Some "+1234567890"
  [MOCK] get_column_Text_nullable[3] = Some "alice@example.com"
  [MOCK] get_column_Text[2] = "Smith"
  [MOCK] get_column_Text[1] = "Alice"
  [MOCK] get_column_Int[0] = 1
    id=1 given=Alice family=Smith email=alice@example.com phone=+1234567890 created_at=1700000000
  [TEST 2] OK
  
  [TEST 3] get_user: select_one_maybe returns None
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, given_name, family_name, email, phone, created_at FROM users WHERE id = 999 LIMIT 1
  [MOCK] Returning no rows
    correctly returned None
  [TEST 3] OK
  
  [TEST 4] get_name: select_one_maybe returns single record
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT given_name, family_name FROM users WHERE id = 1 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Text[1] = "Smith"
  [MOCK] get_column_Text[0] = "Alice"
    given=Alice family=Smith
  [TEST 4] OK
  
  [TEST 5] list_names: callback with single record arg
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT given_name, family_name FROM users
  [MOCK] Returning 2 rows
    Row 0: col0=Alice col1=Smith 
  [MOCK] get_column_Text[1] = "Smith"
  [MOCK] get_column_Text[0] = "Alice"
    given=Alice family=Smith
    Row 1: col0=Bob col1=Jones 
  [MOCK] get_column_Text[1] = "Jones"
  [MOCK] get_column_Text[0] = "Bob"
    given=Bob family=Jones
  [TEST 5] OK
  
  [TEST 6] insert_user: INSERT with record-annotated columns
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] INSERT INTO users (id, given_name, family_name, email, phone, created_at) VALUES (10, 'Charlie', 'Brown', 'charlie@example.com', NULL, 1700000099.)
  [MOCK] Execute result: affected_rows=1, insert_id=None
    affected_rows=1
  [TEST 6] OK
  
  [TEST 7] update_name: UPDATE record-annotated columns
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] UPDATE users SET given_name = 'Charles', family_name = 'Schulz' WHERE id = 10
  [MOCK] Execute result: affected_rows=1, insert_id=None
    affected_rows=1
  [TEST 7] OK
  
  [TEST 8] update_contact: UPDATE nullable record-annotated columns
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] UPDATE users SET email = 'new@example.com', phone = '+9876543210' WHERE id = 10
  [MOCK] Execute result: affected_rows=1, insert_id=None
    affected_rows=1
  [TEST 8] OK
  
  [TEST 9] roundtrip: INSERT then SELECT, access record fields
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] INSERT INTO users (id, given_name, family_name, email, phone, created_at) VALUES (20, 'Diana', 'Prince', 'diana@example.com', '+1111111111', 1700000200.)
  [MOCK] Execute result: affected_rows=1, insert_id=None
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, given_name, family_name, email, phone, created_at FROM users WHERE id = 20 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Datetime[5] = 1700000200.000000
  [MOCK] get_column_Text_nullable[4] = Some "+1111111111"
  [MOCK] get_column_Text_nullable[3] = Some "diana@example.com"
  [MOCK] get_column_Text[2] = "Prince"
  [MOCK] get_column_Text[1] = "Diana"
  [MOCK] get_column_Int[0] = 20
    inserted id=20
    name.given=Diana name.family=Prince
    contact.email=diana@example.com contact.phone=+1111111111
  [TEST 9] OK
  
  [TEST 10] get_account: module= on id + module= inside record
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, given_name, family_name, email, balance FROM accounts WHERE id = 42 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[4] = 1000000
  [MOCK] get_column_Text[3] = "bruce@wayne.enterprises"
  [MOCK] get_column_Text[2] = "Wayne"
  [MOCK] get_column_Text[1] = "Bruce"
  [MOCK] get_column_Int[0] = 42
    id=42
    owner.given=Bruce owner.family=Wayne
    owner.email=bruce@wayne.enterprises (domain=wayne.enterprises)
    balance=1000000
  [TEST 10] OK
  
  [TEST 11] list_accounts: callback with User_id.t + owner record with Email_addr.t
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id, given_name, family_name, email, balance FROM accounts
  [MOCK] Returning 2 rows
    Row 0: col0=42 col1=Bruce col2=Wayne col3=bruce@wayne.enterprises col4=1000000 
  [MOCK] get_column_Int[4] = 1000000
  [MOCK] get_column_Text[3] = "bruce@wayne.enterprises"
  [MOCK] get_column_Text[2] = "Wayne"
  [MOCK] get_column_Text[1] = "Bruce"
  [MOCK] get_column_Int[0] = 42
    id=42 owner=Bruce Wayne email=bruce@wayne.enterprises (domain=wayne.enterprises) balance=1000000
    Row 1: col0=43 col1=Clark col2=Kent col3=clark@dailyplanet.com col4=50000 
  [MOCK] get_column_Int[4] = 50000
  [MOCK] get_column_Text[3] = "clark@dailyplanet.com"
  [MOCK] get_column_Text[2] = "Kent"
  [MOCK] get_column_Text[1] = "Clark"
  [MOCK] get_column_Int[0] = 43
    id=43 owner=Clark Kent email=clark@dailyplanet.com (domain=dailyplanet.com) balance=50000
  [TEST 11] OK
  
  [TEST 12] insert_account: INSERT with User_id.t param + Email_addr.t param
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] INSERT INTO accounts (id, given_name, family_name, email, balance) VALUES (99, 'Peter', 'Parker', 'test@example.org', 500)
  [MOCK] Execute result: affected_rows=1, insert_id=None
    affected_rows=1
  [TEST 12] OK
  
  [TEST 13] account roundtrip: INSERT then SELECT, access module-wrapped record fields
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] INSERT INTO accounts (id, given_name, family_name, email, balance) VALUES (77, 'Tony', 'Stark', 'tony@stark.industries', 999999)
  [MOCK] Execute result: affected_rows=1, insert_id=None
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, given_name, family_name, email, balance FROM accounts WHERE id = 77 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[4] = 999999
  [MOCK] get_column_Text[3] = "tony@stark.industries"
  [MOCK] get_column_Text[2] = "Stark"
  [MOCK] get_column_Text[1] = "Tony"
  [MOCK] get_column_Int[0] = 77
    id=77
    owner.given=Tony owner.family=Stark
    owner.email=tony@stark.industries domain=stark.industries
    balance=999999
  [TEST 13] OK
  
  [TEST 14] get_order: module= outside record + nullable module= inside record + Money.t
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, buyer_id, ship_city, ship_zip, notify_email, amount FROM orders WHERE id = 1001 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[5] = 9999
  [MOCK] get_column_Text_nullable[4] = Some "bruce@wayne.enterprises"
  [MOCK] get_column_Text[3] = "10001"
  [MOCK] get_column_Text[2] = "Gotham"
  [MOCK] get_column_Int[1] = 42
  [MOCK] get_column_Int[0] = 1001
    id=1001 buyer=42
    shipping.city=Gotham shipping.zip=10001
    shipping.notify_email=bruce@wayne.enterprises (domain=wayne.enterprises)
    amount=99.99
  [TEST 14] OK
  
  [TEST 15] get_order: nullable module= Email_addr inside record is None
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, buyer_id, ship_city, ship_zip, notify_email, amount FROM orders WHERE id = 1002 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[5] = 4500
  [MOCK] get_column_Text_nullable[4] = None
  [MOCK] get_column_Text[3] = "20002"
  [MOCK] get_column_Text[2] = "Metropolis"
  [MOCK] get_column_Int[1] = 43
  [MOCK] get_column_Int[0] = 1002
    id=1002 buyer=43
    shipping.city=Metropolis shipping.zip=20002
    shipping.notify_email=NULL
    amount=45.00
  [TEST 15] OK
  
  [TEST 16] list_orders_by_buyer: callback with mixed modules and record
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT id, buyer_id, ship_city, ship_zip, notify_email, amount FROM orders WHERE buyer_id = 42
  [MOCK] Returning 2 rows
    Row 0: col0=1001 col1=42 col2=Gotham col3=10001 col4=alfred@wayne.enterprises col5=25000 
  [MOCK] get_column_Int[5] = 25000
  [MOCK] get_column_Text_nullable[4] = Some "alfred@wayne.enterprises"
  [MOCK] get_column_Text[3] = "10001"
  [MOCK] get_column_Text[2] = "Gotham"
  [MOCK] get_column_Int[1] = 42
  [MOCK] get_column_Int[0] = 1001
    order=1001 buyer=42 city=Gotham zip=10001 notify=alfred@wayne.enterprises amount=250.00
    Row 1: col0=1003 col1=42 col2=Bludhaven col3=10099 col4=NULL col5=750 
  [MOCK] get_column_Int[5] = 750
  [MOCK] get_column_Text_nullable[4] = None
  [MOCK] get_column_Text[3] = "10099"
  [MOCK] get_column_Text[2] = "Bludhaven"
  [MOCK] get_column_Int[1] = 42
  [MOCK] get_column_Int[0] = 1003
    order=1003 buyer=42 city=Bludhaven zip=10099 notify=NULL amount=7.50
  [TEST 16] OK
  
  [TEST 17] insert_order: INSERT with User_id + Email_addr option + Money params
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] INSERT INTO orders (id, buyer_id, ship_city, ship_zip, notify_email, amount) VALUES (2001, 42, 'Star City', '30003', 'oliver@queen.industries', 15099)
  [MOCK] Execute result: affected_rows=1, insert_id=None
    affected_rows=1
  [TEST 17] OK
  
  [TEST 18] order roundtrip: INSERT then SELECT, verify all module-wrapped fields
  [MOCK EXECUTE] Connection type: [> `WR ]
  [SQL] INSERT INTO orders (id, buyer_id, ship_city, ship_zip, notify_email, amount) VALUES (2002, 43, 'Central City', '40004', NULL, 3300)
  [MOCK] Execute result: affected_rows=1, insert_id=None
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT id, buyer_id, ship_city, ship_zip, notify_email, amount FROM orders WHERE id = 2002 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[5] = 3300
  [MOCK] get_column_Text_nullable[4] = None
  [MOCK] get_column_Text[3] = "40004"
  [MOCK] get_column_Text[2] = "Central City"
  [MOCK] get_column_Int[1] = 43
  [MOCK] get_column_Int[0] = 2002
    id=2002 buyer=43
    shipping.city=Central City zip=40004
    shipping.notify_email=NULL
    amount=33.00
  [TEST 18] OK
  
  [TEST 19] JOIN: get_order_with_buyer, records from two tables
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT o.id, o.buyer_id, u.given_name, u.family_name, o.ship_city, o.ship_zip, o.notify_email, o.amount
  FROM orders o
  JOIN users u ON u.id = o.buyer_id
  WHERE o.id = 1001 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Int[7] = 9999
  [MOCK] get_column_Text_nullable[6] = Some "alfred@wayne.enterprises"
  [MOCK] get_column_Text[5] = "10001"
  [MOCK] get_column_Text[4] = "Gotham"
  [MOCK] get_column_Text[3] = "Wayne"
  [MOCK] get_column_Text[2] = "Bruce"
  [MOCK] get_column_Int[1] = 42
  [MOCK] get_column_Int[0] = 1001
    order_id=1001 buyer=42
    name.given=Bruce name.family=Wayne
    shipping.city=Gotham shipping.zip=10001
    shipping.notify=alfred@wayne.enterprises (domain=wayne.enterprises)
    amount=99.99
  [TEST 19] OK
  
  [TEST 20] JOIN: list_orders_with_buyers, name record from users + shipping from orders
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT o.id, o.buyer_id, u.given_name, u.family_name, o.ship_city, o.ship_zip, o.notify_email, o.amount
  FROM orders o
  JOIN users u ON u.id = o.buyer_id
  [MOCK] Returning 2 rows
    Row 0: col0=1001 col1=42 col2=Bruce col3=Wayne col4=Gotham col5=10001 col6=alfred@wayne.enterprises col7=25000 
  [MOCK] get_column_Int[7] = 25000
  [MOCK] get_column_Text_nullable[6] = Some "alfred@wayne.enterprises"
  [MOCK] get_column_Text[5] = "10001"
  [MOCK] get_column_Text[4] = "Gotham"
  [MOCK] get_column_Text[3] = "Wayne"
  [MOCK] get_column_Text[2] = "Bruce"
  [MOCK] get_column_Int[1] = 42
  [MOCK] get_column_Int[0] = 1001
    order=1001 buyer=42 name=Bruce Wayne city=Gotham zip=10001 notify=alfred@wayne.enterprises amount=250.00
    Row 1: col0=1002 col1=43 col2=Clark col3=Kent col4=Metropolis col5=20002 col6=NULL col7=4500 
  [MOCK] get_column_Int[7] = 4500
  [MOCK] get_column_Text_nullable[6] = None
  [MOCK] get_column_Text[5] = "20002"
  [MOCK] get_column_Text[4] = "Metropolis"
  [MOCK] get_column_Text[3] = "Kent"
  [MOCK] get_column_Text[2] = "Clark"
  [MOCK] get_column_Int[1] = 43
  [MOCK] get_column_Int[0] = 1002
    order=1002 buyer=43 name=Clark Kent city=Metropolis zip=20002 notify=NULL amount=45.00
  [TEST 20] OK
  
  [TEST 21] LEFT JOIN: name record + shipping record option
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT u.id, u.given_name, u.family_name, o.id, o.ship_city, o.ship_zip, o.notify_email, o.amount
  FROM users u
  LEFT JOIN orders o ON o.buyer_id = u.id
  [MOCK] Returning 2 rows
    Row 0: col0=1 col1=Alice col2=Smith col3=1001 col4=Gotham col5=10001 col6=alfred@wayne.enterprises col7=9999 
  [MOCK] get_column_Int_nullable[7] = Some 9999
  [MOCK] get_column_Text_nullable[4] = Some "Gotham"
  [MOCK] get_column_Text_nullable[6] = Some "alfred@wayne.enterprises"
  [MOCK] get_column_Text[5] = "10001"
  [MOCK] get_column_Text[4] = "Gotham"
  [MOCK] get_column_Int_nullable[3] = Some 1001
  [MOCK] get_column_Text[2] = "Smith"
  [MOCK] get_column_Text[1] = "Alice"
  [MOCK] get_column_Int[0] = 1
    user=1 name=Alice Smith order=1001 shipping=Gotham/10001/alfred@wayne.enterprises amount=99.99
    Row 1: col0=2 col1=Bob col2=Jones col3=NULL col4=NULL col5=NULL col6=NULL col7=NULL 
  [MOCK] get_column_Int_nullable[7] = None
  [MOCK] get_column_Text_nullable[4] = None
  [MOCK] get_column_Int_nullable[3] = None
  [MOCK] get_column_Text[2] = "Jones"
  [MOCK] get_column_Text[1] = "Bob"
  [MOCK] get_column_Int[0] = 2
    user=2 name=Bob Jones order=NULL shipping=NULL amount=NULL
  [TEST 21] OK
  
  [TEST 22] RIGHT JOIN: shipping record option + name record
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT o.id, o.ship_city, o.ship_zip, o.notify_email, o.amount, u.id, u.given_name, u.family_name
  FROM orders o
  RIGHT JOIN users u ON o.buyer_id = u.id
  [MOCK] Returning 2 rows
    Row 0: col0=1001 col1=Gotham col2=10001 col3=alfred@wayne.enterprises col4=9999 col5=1 col6=Alice col7=Smith 
  [MOCK] get_column_Text[7] = "Smith"
  [MOCK] get_column_Text[6] = "Alice"
  [MOCK] get_column_Int[5] = 1
  [MOCK] get_column_Int_nullable[4] = Some 9999
  [MOCK] get_column_Text_nullable[1] = Some "Gotham"
  [MOCK] get_column_Text_nullable[3] = Some "alfred@wayne.enterprises"
  [MOCK] get_column_Text[2] = "10001"
  [MOCK] get_column_Text[1] = "Gotham"
  [MOCK] get_column_Int_nullable[0] = Some 1001
    order=1001 shipping=Gotham/10001/alfred@wayne.enterprises amount=99.99 user=1 name=Alice Smith
    Row 1: col0=NULL col1=NULL col2=NULL col3=NULL col4=NULL col5=2 col6=Bob col7=Jones 
  [MOCK] get_column_Text[7] = "Jones"
  [MOCK] get_column_Text[6] = "Bob"
  [MOCK] get_column_Int[5] = 2
  [MOCK] get_column_Int_nullable[4] = None
  [MOCK] get_column_Text_nullable[1] = None
  [MOCK] get_column_Int_nullable[0] = None
    order=NULL shipping=NULL amount=NULL user=2 name=Bob Jones
  [TEST 22] OK
  
  [TEST 23] UNION ALL: name records from two queries
  [MOCK SELECT] Connection type: [> `RO ]
  [SQL] SELECT given_name, family_name FROM users WHERE id = 1
  UNION ALL
  SELECT given_name, family_name FROM users WHERE id = 2
  [MOCK] Returning 2 rows
    Row 0: col0=Alice col1=Smith 
  [MOCK] get_column_Text[1] = "Smith"
  [MOCK] get_column_Text[0] = "Alice"
    name=Alice Smith
    Row 1: col0=Bob col1=Jones 
  [MOCK] get_column_Text[1] = "Jones"
  [MOCK] get_column_Text[0] = "Bob"
    name=Bob Jones
  [TEST 23] OK
  
  [TEST 24] LEFT JOIN select_one: user with order -> Shipping.t option = Some
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT u.id, u.given_name, u.family_name, o.id, o.ship_city, o.ship_zip, o.notify_email, o.amount
  FROM users u
  LEFT JOIN orders o ON o.buyer_id = u.id
  WHERE u.id = 1 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[7] = Some 9999
  [MOCK] get_column_Text_nullable[4] = Some "Gotham"
  [MOCK] get_column_Text_nullable[6] = Some "bruce@wayne.enterprises"
  [MOCK] get_column_Text[5] = "10001"
  [MOCK] get_column_Text[4] = "Gotham"
  [MOCK] get_column_Int_nullable[3] = Some 1001
  [MOCK] get_column_Text[2] = "Smith"
  [MOCK] get_column_Text[1] = "Alice"
  [MOCK] get_column_Int[0] = 1
    uid=1 name=Alice Smith
    order_id=1001
    shipping.city=Gotham
    shipping.zip=10001
    shipping.notify=bruce@wayne.enterprises
    email domain=wayne.enterprises
    amount=99.99
  [TEST 24] OK
  
  [TEST 25] LEFT JOIN select_one: user without order -> Shipping.t option = None
  [MOCK SELECT_ONE_MAYBE] Connection type: [> `RO ]
  [SQL] SELECT u.id, u.given_name, u.family_name, o.id, o.ship_city, o.ship_zip, o.notify_email, o.amount
  FROM users u
  LEFT JOIN orders o ON o.buyer_id = u.id
  WHERE u.id = 2 LIMIT 1
  [MOCK] Returning one row
  [MOCK] get_column_Int_nullable[7] = None
  [MOCK] get_column_Text_nullable[4] = None
  [MOCK] get_column_Int_nullable[3] = None
  [MOCK] get_column_Text[2] = "Jones"
  [MOCK] get_column_Text[1] = "Bob"
  [MOCK] get_column_Int[0] = 2
    uid=2 name=Bob Jones
    order_id=NULL
    shipping=None
    amount=None
  [TEST 25] OK
  
  === All Field Records Tests Passed ===
  
  ==================================================
  All tests executed successfully!

Record field grouping: invalid format error:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t1 (
  >   -- [sqlgg] record=bad_format
  >   col TEXT NOT NULL
  > );
  > SELECT col FROM t1;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  Fatal error: exception Failure("invalid record annotation, expected record=name.field: bad_format")
  [2]
