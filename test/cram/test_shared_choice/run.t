Shared choices (same @name used several times in one statement).

  $ cat shared.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > shared.ml
  $ diff shared.ml shared.compare.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c shared.ml

Different branches are rejected:

  $ sqlgg -gen caml -no-header - <<'EOF'
  > CREATE TABLE t (a INT, b INT);
  > SELECT * FROM t WHERE a = @x { A { 1 } | B { 2 } } AND b = @x { C { 10 } | D { 20 } };
  > EOF
  Failed : SELECT * FROM t WHERE a = @x { A { 1 } | B { 2 } } AND b = @x { C { 10 } | D { 20 } }
  At : @x { C { 10 } | D { 20 } }
  Fatal error: exception Failure("choice x is used several times with different branches")
  [2]

Different number of branches is rejected:

  $ sqlgg -gen caml -no-header - <<'EOF'
  > CREATE TABLE t (a INT, b INT);
  > SELECT * FROM t WHERE a = @x { A { 1 } | B { 2 } } AND b = @x { A { 1 } | B { 2 } | C { 3 } };
  > EOF
  Failed : SELECT * FROM t WHERE a = @x { A { 1 } | B { 2 } } AND b = @x { A { 1 } | B { 2 } | C { 3 } }
  At : @x { A { 1 } | B { 2 } | C { 3 } }
  Fatal error: exception Failure("choice x is used several times with different branches")
  [2]

Param in one occurrence, constant in the other is rejected:

  $ sqlgg -gen caml -no-header - <<'EOF'
  > CREATE TABLE t (a INT, b INT);
  > SELECT * FROM t WHERE a = @x { A { @p } | B { 2 } } AND b = @x { A { 1 } | B { 2 } };
  > EOF
  Failed : SELECT * FROM t WHERE a = @x { A { @p } | B { 2 } } AND b = @x { A { 1 } | B { 2 } }
  At : @x { A { 1 } | B { 2 } }
  Fatal error: exception Failure("choice x is used several times with different branches")
  [2]

Scalar param in one occurrence, IN-list in the other is rejected:

  $ sqlgg -gen caml -no-header - <<'EOF'
  > CREATE TABLE t (a INT, b INT);
  > SELECT * FROM t WHERE @x { P { a = @v } | N { TRUE } } AND @x { P { b IN @v } | N { TRUE } };
  > EOF
  Failed : SELECT * FROM t WHERE @x { P { a = @v } | N { TRUE } } AND @x { P { b IN @v } | N { TRUE } }
  At : @x { P { b IN @v } | N { TRUE } }
  Fatal error: exception Failure("choice x is used several times with different branches")
  [2]

Differently named params of incompatible types are rejected:

  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF'
  > CREATE TABLE t (id INT, name TEXT);
  > SELECT id FROM t WHERE @f { P { id = @a } | N { TRUE } } AND @f { P { name = @b } | N { TRUE } };
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_t db  =
      T.execute db (Sqlgg_traits.Query.make ~sql:("CREATE TABLE t (id INT, name TEXT)") ~name:"create_t" ~kind:Sqlgg_traits.Query.(Create "t") ()) T.no_params
  
    let select_1 db ~f callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int_nullable stmt 0)
      in
      let set_params stmt =
        let p = T.start_params stmt (0 + (match f with `P _ -> 1 | `N -> 0) + (match f with `P _ -> 1 | `N -> 0)) in
        begin match f with
        | `N -> ()
        | `P (a) ->
          T.set_param_Text p a;
        end;
        begin match f with
        | `N -> ()
        | `P (b) ->
          T.set_param_Text p b;
        end;
        T.finish_params p
      in
      T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM t WHERE " ^ (match f with `P _ -> " ( id = ? ) " | `N -> " ( TRUE ) ") ^ " AND " ^ (match f with `P _ -> " ( name = ? ) " | `N -> " ( TRUE ) ")) ~name:"select_1" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params invoke_callback
  
    module Fold = struct
      let select_1 db ~f callback acc =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int_nullable stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `P _ -> 1 | `N -> 0) + (match f with `P _ -> 1 | `N -> 0)) in
          begin match f with
          | `N -> ()
          | `P (a) ->
            T.set_param_Text p a;
          end;
          begin match f with
          | `N -> ()
          | `P (b) ->
            T.set_param_Text p b;
          end;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM t WHERE " ^ (match f with `P _ -> " ( id = ? ) " | `N -> " ( TRUE ) ") ^ " AND " ^ (match f with `P _ -> " ( name = ? ) " | `N -> " ( TRUE ) ")) ~name:"select_1" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_1 db ~f callback =
        let invoke_callback stmt =
          callback
            ~id:(T.get_column_Int_nullable stmt 0)
        in
        let set_params stmt =
          let p = T.start_params stmt (0 + (match f with `P _ -> 1 | `N -> 0) + (match f with `P _ -> 1 | `N -> 0)) in
          begin match f with
          | `N -> ()
          | `P (a) ->
            T.set_param_Text p a;
          end;
          begin match f with
          | `N -> ()
          | `P (b) ->
            T.set_param_Text p b;
          end;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db (Sqlgg_traits.Query.make ~sql:("SELECT id FROM t WHERE " ^ (match f with `P _ -> " ( id = ? ) " | `N -> " ( TRUE ) ") ^ " AND " ^ (match f with `P _ -> " ( name = ? ) " | `N -> " ( TRUE ) ")) ~name:"select_1" ~kind:Sqlgg_traits.Query.(Select Nat) ()) set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)
