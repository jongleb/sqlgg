Test DynamicSelect with applicative combinators generates proper SQL:
  $ cp test_build_dynamic_select/dynamic_select.sql .
  $ cp test_build_dynamic_select/product_id.ml .
  $ cp test_build_dynamic_select/test_run.ml .
  $ cat dynamic_select.sql | sqlgg -no-header -gen caml_io -params unnamed -gen caml -dialect mysql - > output.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c print_ocaml_impl.ml
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c product_id.ml
  $ ocamlfind ocamlc -package sqlgg.traits,sqlgg -I . -c output.ml
  File "output.ml", line 930, characters 43-48:
  930 |       let p = T.start_params stmt (1 + col.count) in
                                                   ^^^^^
  Error: Unbound record field count
  [2]
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -c test_run.ml
  File "test_run.ml", line 11, characters 15-27:
  11 |   module Sql = Output.Sqlgg(T)
                      ^^^^^^^^^^^^
  Error: Unbound module Output
  [2]
  $ ocamlfind ocamlc -package sqlgg.traits,yojson -I . -linkpkg -o test_run.exe print_ocaml_impl.cmo product_id.cmo output.cmo test_run.ml
  File "test_run.ml", line 11, characters 15-27:
  11 |   module Sql = Output.Sqlgg(T)
                      ^^^^^^^^^^^^
  Error: Unbound module Output
  [2]
  $ ./test_run.exe
  /tmp/dune_cram_99ea24_.cram.sh/main.sh: 1: /tmp/dune_cram_99ea24_.cram.sh/10.sh: ./test_run.exe: not found
  [127]

Test DynamicSelect edge: single column:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @single_col
  > SELECT id FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Single_col_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let single_col db ~col callback =
      let col = col Single_col_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let single_col db ~col callback acc =
        let col = col Single_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let single_col db ~col callback =
        let col = col Single_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

DynamicSelect: SELECT * remains static select:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @all_cols
  > SELECT * FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module All_cols_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; name : 'a1; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = "t.id";
          count = 0;
          phantom = None;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = "t.name";
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method name = name
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let all_cols db ~col callback =
      let col = col All_cols_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let all_cols db ~col callback acc =
        let col = col All_cols_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let all_cols db ~col callback =
        let col = col All_cols_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

DynamicSelect: SELECT * with expression in same list:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @all_cols_plus_expr
  > SELECT *, id + 2 AS id_plus FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module All_cols_plus_expr_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; name : 'a1; id_plus : 'a2; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = "t.id";
          count = 0;
          phantom = None;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = "t.name";
          count = 0;
          phantom = None;
        }
      let id_plus =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id + 2");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method name = name
        method id_plus = id_plus
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let all_cols_plus_expr db ~col callback =
      let col = col All_cols_plus_expr_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let all_cols_plus_expr db ~col callback acc =
        let col = col All_cols_plus_expr_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let all_cols_plus_expr db ~col callback =
        let col = col All_cols_plus_expr_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

DynamicSelect: auto names for expressions without alias:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @auto_names
  > SELECT id + 1, id * 2, name FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Auto_names_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < col1 : 'a0; col2 : 'a1; name : 'a2; .. >
  
      let col1 =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id + 1");
          count = 0;
          phantom = None;
        }
      let col2 =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id * 2");
          count = 0;
          phantom = None;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          phantom = None;
        }
      let all = object
        method col1 = col1
        method col2 = col2
        method name = name
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let auto_names db ~col callback =
      let col = col Auto_names_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let auto_names db ~col callback acc =
        let col = col Auto_names_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let auto_names db ~col callback =
        let col = col Auto_names_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: expression at first position:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @expr_first
  > SELECT id + 1 AS id_plus FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Expr_first_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id_plus : 'a0; .. >
  
      let id_plus =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id + 1");
          count = 0;
          phantom = None;
        }
      let all = object
        method id_plus = id_plus
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let expr_first db ~col callback =
      let col = col Expr_first_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let expr_first db ~col callback acc =
        let col = col Expr_first_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let expr_first db ~col callback =
        let col = col Expr_first_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: literal only:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @literal_only
  > SELECT 'hello' AS greeting, 42 AS answer FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Literal_only_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < greeting : 'a0; answer : 'a1; .. >
  
      let greeting =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("'hello'");
          count = 0;
          phantom = None;
        }
      let answer =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("42");
          count = 0;
          phantom = None;
        }
      let all = object
        method greeting = greeting
        method answer = answer
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT)") T.no_params
  
    let literal_only db ~col callback =
      let col = col Literal_only_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let literal_only db ~col callback acc =
        let col = col Literal_only_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let literal_only db ~col callback =
        let col = col Literal_only_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: many columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (a INT, b TEXT, c DECIMAL(10,2), d INT, e TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @many_cols
  > SELECT a, b, c, d, e FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Many_cols_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < a : 'a0; b : 'a1; c : 'a2; d : 'a3; e : 'a4; .. >
  
      let a =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("a");
          count = 0;
          phantom = None;
        }
      let b =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("b");
          count = 0;
          phantom = None;
        }
      let c =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("c");
          count = 0;
          phantom = None;
        }
      let d =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("d");
          count = 0;
          phantom = None;
        }
      let e =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("e");
          count = 0;
          phantom = None;
        }
      let all = object
        method a = a
        method b = b
        method c = c
        method d = d
        method e = e
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b TEXT, c DECIMAL(10,2), d INT, e TEXT)") T.no_params
  
    let many_cols db ~col callback =
      let col = col Many_cols_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let many_cols db ~col callback acc =
        let col = col Many_cols_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let many_cols db ~col callback =
        let col = col Many_cols_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: no space after commas:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @no_space
  > SELECT id,name,price FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module No_space_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; name : 'a1; price : 'a2; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          phantom = None;
        }
      let price =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method name = name
        method price = price
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    let no_space db ~col callback =
      let col = col No_space_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let no_space db ~col callback acc =
        let col = col No_space_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let no_space db ~col callback =
        let col = col No_space_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: minimal spacing:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (a INT, b INT);
  > -- [sqlgg] dynamic_select=true
  > -- @tight
  > SELECT a,b FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Tight_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < a : 'a0; b : 'a1; .. >
  
      let a =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("a");
          count = 0;
          phantom = None;
        }
      let b =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("b");
          count = 0;
          phantom = None;
        }
      let all = object
        method a = a
        method b = b
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b INT)") T.no_params
  
    let tight db ~col callback =
      let col = col Tight_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let tight db ~col callback acc =
        let col = col Tight_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let tight db ~col callback =
        let col = col Tight_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect edge: column without alias gets auto name:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @no_alias
  > SELECT id + 1 FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module No_alias_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < col1 : 'a0; .. >
  
      let col1 =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id + 1");
          count = 0;
          phantom = None;
        }
      let all = object
        method col1 = col1
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT)") T.no_params
  
    let no_alias db ~col callback =
      let col = col No_alias_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let no_alias db ~col callback acc =
        let col = col No_alias_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let no_alias db ~col callback =
        let col = col No_alias_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect with dynamic_select flag:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE accounts (id INT PRIMARY KEY, balance DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @select_ids2
  > SELECT id, balance, @t + 1 AS t_plus_one, (SELECT 6 + @seven LIMIT 1) AS sub_result FROM accounts WHERE id > @t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Select_ids2_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; balance : 'a1; t_plus_one : 'a2; sub_result : 'a3; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let balance =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("balance");
          count = 0;
          phantom = None;
        }
      let t_plus_one t =
        let _set_t_plus_one p =
          T.set_param_Int p t;
          ()
        in
        {
          set = _set_t_plus_one;
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("" ^ "?" ^ " + 1");
          count = 1;
          phantom = None;
        }
      let sub_result seven =
        let _set_sub_result p =
          T.set_param_Int p seven;
          ()
        in
        {
          set = _set_sub_result;
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("(SELECT 6 + " ^ "?" ^ " LIMIT 1)");
          count = 1;
          phantom = None;
        }
      let all = object
        method id = id
        method balance = balance
        method t_plus_one = t_plus_one
        method sub_result = sub_result
      end
    end
  
  
    let create_accounts db  =
      T.execute db ("CREATE TABLE accounts (id INT PRIMARY KEY, balance DECIMAL(10,2))") T.no_params
  
    let select_ids2 db ~col ~t callback =
      let col = col Select_ids2_col.all in
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p t;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM accounts WHERE id > ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let select_ids2 db ~col ~t callback acc =
        let col = col Select_ids2_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p t;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM accounts WHERE id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let select_ids2 db ~col ~t callback =
        let col = col Select_ids2_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p t;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM accounts WHERE id > ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect with two dynamic columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE items (id INT, name TEXT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @multi_dynamic
  > SELECT id, name, price, price * 2 AS doubled_price FROM items;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Multi_dynamic_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; name : 'a1; price : 'a2; doubled_price : 'a3; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          phantom = None;
        }
      let price =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
          phantom = None;
        }
      let doubled_price =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price * 2");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method name = name
        method price = price
        method doubled_price = doubled_price
      end
    end
  
  
    let create_items db  =
      T.execute db ("CREATE TABLE items (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    let multi_dynamic db ~col callback =
      let col = col Multi_dynamic_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM items")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let multi_dynamic db ~col callback acc =
        let col = col Multi_dynamic_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM items")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let multi_dynamic db ~col callback =
        let col = col Multi_dynamic_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM items")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect with Verbatim branches:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE users (id INT, status TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @with_verbatim
  > SELECT id, status, 'active' AS literal_status FROM users;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module With_verbatim_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; status : 'a1; literal_status : 'a2; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let status =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("status");
          count = 0;
          phantom = None;
        }
      let literal_status =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("'active'");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method status = status
        method literal_status = literal_status
      end
    end
  
  
    let create_users db  =
      T.execute db ("CREATE TABLE users (id INT, status TEXT)") T.no_params
  
    let with_verbatim db ~col callback =
      let col = col With_verbatim_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM users")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let with_verbatim db ~col callback acc =
        let col = col With_verbatim_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let with_verbatim db ~col callback =
        let col = col With_verbatim_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM users")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect at beginning of SELECT:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE data (a INT, b TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @first_col
  > SELECT a, b FROM data;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module First_col_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < a : 'a0; b : 'a1; .. >
  
      let a =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("a");
          count = 0;
          phantom = None;
        }
      let b =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("b");
          count = 0;
          phantom = None;
        }
      let all = object
        method a = a
        method b = b
      end
    end
  
  
    let create_data db  =
      T.execute db ("CREATE TABLE data (a INT, b TEXT)") T.no_params
  
    let first_col db ~col callback =
      let col = col First_col_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM data")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let first_col db ~col callback acc =
        let col = col First_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM data")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let first_col db ~col callback =
        let col = col First_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM data")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect disabled in subquery (fallback to Choice):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1 
  > CREATE TABLE t1 (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @with_subquery
  > SELECT id, (SELECT @x { A { 1 } | B { 2 } } LIMIT 1) as sub FROM t1;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module With_subquery_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; sub : 'a1; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let sub x =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("(SELECT " ^ (match x with `A -> " 1 " | `B -> " 2 ") ^ " LIMIT 1)");
          count = 0 + (match x with `A -> 0 | `B -> 0);
          phantom = None;
        }
      let all = object
        method id = id
        method sub = sub
      end
    end
  
  
    let create_t1 db  =
      T.execute db ("CREATE TABLE t1 (id INT)") T.no_params
  
    let with_subquery db ~col callback =
      let col = col With_subquery_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t1")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let with_subquery db ~col callback acc =
        let col = col With_subquery_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t1")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let with_subquery db ~col callback =
        let col = col With_subquery_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t1")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Test DynamicSelect with module annotation:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE wrapped (
  >     -- [sqlgg] module=Product_id
  >     id INT PRIMARY KEY,
  >     name TEXT,
  >     price DECIMAL(10,2)
  > );
  > -- [sqlgg] dynamic_select=true
  > -- @with_module
  > SELECT id, name, price FROM wrapped WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module With_module_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; name : 'a1; price : 'a2; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (Product_id.get_column (T.get_column_int64 row idx), idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          phantom = None;
        }
      let price =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method name = name
        method price = price
      end
    end
  
  
    let create_wrapped db  =
      T.execute db ("CREATE TABLE wrapped (\n\
          id INT PRIMARY KEY,\n\
      name TEXT,\n\
      price DECIMAL(10,2)\n\
  )") T.no_params
  
    let with_module db ~col ~id =
      let col = col With_module_col.all in
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_int64 p (Product_id.set_param id);
        T.finish_params p
      in
      T.select_one_maybe db
      ("SELECT " ^ col.column ^ " FROM wrapped WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
    module Single = struct
      let with_module db ~col ~id =
        let col = col With_module_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_int64 p (Product_id.set_param id);
          T.finish_params p
        in
        T.select_one_maybe db
        ("SELECT " ^ col.column ^ " FROM wrapped WHERE id = ?")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
    end (* module Single *)
  end (* module Sqlgg *)

Test DynamicSelect with LIMIT 1 (select_one):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @select_one_product
  > SELECT name, price FROM products WHERE id = @id LIMIT 1;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Select_one_product_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < name : 'a0; price : 'a1; .. >
  
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          phantom = None;
        }
      let price =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
          phantom = None;
        }
      let all = object
        method name = name
        method price = price
      end
    end
  
  
    let create_products db  =
      T.execute db ("CREATE TABLE products (id INT PRIMARY KEY, name TEXT, price DECIMAL(10,2))") T.no_params
  
    let select_one_product db ~col ~id =
      let col = col Select_one_product_col.all in
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select_one_maybe db
      ("SELECT " ^ col.column ^ " FROM products WHERE id = ? LIMIT 1")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
    module Single = struct
      let select_one_product db ~col ~id =
        let col = col Select_one_product_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        T.select_one_maybe db
        ("SELECT " ^ col.column ^ " FROM products WHERE id = ? LIMIT 1")
        set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in (__sqlgg_r_col))
  
    end (* module Single *)
  end (* module Sqlgg *)

Test DynamicSelect comprehensive list:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE products (
  >  id INT PRIMARY KEY,
  >  name TEXT,
  >  price DECIMAL(10,2),
  >  category TEXT,
  >  stock INT
  > );
  > -- [sqlgg] dynamic_select=true
  > -- @ultimate_combo_simple2
  > SELECT
  >    id,
  >    name,
  >    category, stock,
  >    price * (1 + @tax_rate) AS price_with_tax
  > FROM products;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Ultimate_combo_simple2_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; name : 'a1; category : 'a2; stock : 'a3; price_with_tax : 'a4; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          phantom = None;
        }
      let category =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("category");
          count = 0;
          phantom = None;
        }
      let stock =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("stock");
          count = 0;
          phantom = None;
        }
      let price_with_tax tax_rate =
        let _set_price_with_tax p =
          T.set_param_Int p tax_rate;
          ()
        in
        {
          set = _set_price_with_tax;
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price * (1 + " ^ "?" ^ ")");
          count = 1;
          phantom = None;
        }
      let all = object
        method id = id
        method name = name
        method category = category
        method stock = stock
        method price_with_tax = price_with_tax
      end
    end
  
  
    let create_products db  =
      T.execute db ("CREATE TABLE products (\n\
   id INT PRIMARY KEY,\n\
   name TEXT,\n\
   price DECIMAL(10,2),\n\
   category TEXT,\n\
   stock INT\n\
  )") T.no_params
  
    let ultimate_combo_simple2 db ~col callback =
      let col = col Ultimate_combo_simple2_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT\n\
     " ^ col.column ^ "\n\
  FROM products")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let ultimate_combo_simple2 db ~col callback acc =
        let col = col Ultimate_combo_simple2_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT\n\
     " ^ col.column ^ "\n\
  FROM products")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let ultimate_combo_simple2 db ~col callback =
        let col = col Ultimate_combo_simple2_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT\n\
     " ^ col.column ^ "\n\
  FROM products")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: param as bare column expression (spacing at ctor boundary):
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, val TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @bare_param
  > SELECT id, @custom_val :: Text AS custom FROM t WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Bare_param_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; custom : 'a1; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let custom custom_val =
        let _set_custom p =
          T.set_param_Text p custom_val;
          ()
        in
        {
          set = _set_custom;
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("" ^ "?");
          count = 1;
          phantom = None;
        }
      let all = object
        method id = id
        method custom = custom
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, val TEXT)") T.no_params
  
    let bare_param db ~col ~id callback =
      let col = col Bare_param_col.all in
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let bare_param db ~col ~id callback acc =
        let col = col Bare_param_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let bare_param db ~col ~id callback =
        let col = col Bare_param_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: consecutive params as columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT);
  > -- [sqlgg] dynamic_select=true
  > -- @multi_param
  > SELECT @a :: Int AS col_a, @b :: Text AS col_b FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Multi_param_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < col_a : 'a0; col_b : 'a1; .. >
  
      let col_a a =
        let _set_col_a p =
          T.set_param_Int p a;
          ()
        in
        {
          set = _set_col_a;
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("" ^ "?");
          count = 1;
          phantom = None;
        }
      let col_b b =
        let _set_col_b p =
          T.set_param_Text p b;
          ()
        in
        {
          set = _set_col_b;
          read = (fun row idx -> (T.get_column_Text row idx, idx + 1));
          column = ("" ^ "?");
          count = 1;
          phantom = None;
        }
      let all = object
        method col_a = col_a
        method col_b = col_b
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT)") T.no_params
  
    let multi_param db ~col callback =
      let col = col Multi_param_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let multi_param db ~col callback acc =
        let col = col Multi_param_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let multi_param db ~col callback =
        let col = col Multi_param_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: mixed columns and params without spaces after commas:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @tight_commas
  > SELECT id,name,price,@extra :: Int AS bonus FROM t WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Tight_commas_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; name : 'a1; price : 'a2; bonus : 'a3; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("name");
          count = 0;
          phantom = None;
        }
      let price =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("price");
          count = 0;
          phantom = None;
        }
      let bonus extra =
        let _set_bonus p =
          T.set_param_Int p extra;
          ()
        in
        {
          set = _set_bonus;
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("" ^ "?");
          count = 1;
          phantom = None;
        }
      let all = object
        method id = id
        method name = name
        method price = price
        method bonus = bonus
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT, price DECIMAL(10,2))") T.no_params
  
    let tight_commas db ~col ~id callback =
      let col = col Tight_commas_col.all in
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let tight_commas db ~col ~id callback acc =
        let col = col Tight_commas_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let tight_commas db ~col ~id callback =
        let col = col Tight_commas_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: subquery expression as dynamic column:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @subquery_col
  > SELECT id, (SELECT COUNT(*) FROM t t2 WHERE t2.id <= t.id) AS rank FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Subquery_col_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; rank : 'a1; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let rank =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
          column = ("(SELECT COUNT(*) FROM t t2 WHERE t2.id <= t.id)");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method rank = rank
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, name TEXT)") T.no_params
  
    let subquery_col db ~col callback =
      let col = col Subquery_col_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let subquery_col db ~col callback acc =
        let col = col Subquery_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let subquery_col db ~col callback =
        let col = col Subquery_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: CASE WHEN as dynamic column:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, status INT);
  > -- [sqlgg] dynamic_select=true
  > -- @case_col
  > SELECT id, CASE WHEN status = 1 THEN 'active' ELSE 'inactive' END AS label FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
      module Enum_0 = T.Make_enum(struct
        type t = [`Active | `Inactive]
        let inj = function | "active" -> `Active | "inactive" -> `Inactive | s -> failwith (Printf.sprintf "Invalid enum value: %s" s)
        let proj = function  | `Active -> "active"| `Inactive -> "inactive"
      end)
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Case_col_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; label : 'a1; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let label =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (Enum_0.get_column row idx, idx + 1));
          column = ("CASE WHEN status = 1 THEN 'active' ELSE 'inactive' END");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method label = label
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, status INT)") T.no_params
  
    let case_col db ~col callback =
      let col = col Case_col_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let case_col db ~col callback acc =
        let col = col Case_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let case_col db ~col callback =
        let col = col Case_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: function call with multiple args as column:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, first_name TEXT, last_name TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @func_col
  > SELECT id, CONCAT(first_name, ' ', last_name) AS full_name FROM t;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Func_col_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; full_name : 'a1; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let full_name =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("CONCAT(first_name, ' ', last_name)");
          count = 0;
          phantom = None;
        }
      let all = object
        method id = id
        method full_name = full_name
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, first_name TEXT, last_name TEXT)") T.no_params
  
    let func_col db ~col callback =
      let col = col Func_col_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let func_col db ~col callback acc =
        let col = col Func_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let func_col db ~col callback =
        let col = col Func_col_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: arithmetic with param at expression start:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, price DECIMAL(10,2));
  > -- [sqlgg] dynamic_select=true
  > -- @param_start_expr
  > SELECT id, @multiplier * price AS scaled FROM t WHERE id = @id;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Param_start_expr_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < id : 'a0; scaled : 'a1; .. >
  
      let id =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("id");
          count = 0;
          phantom = None;
        }
      let scaled multiplier =
        let _set_scaled p =
          begin match multiplier with None -> T.set_param_null p | Some v -> T.set_param_Decimal p v end;
          ()
        in
        {
          set = _set_scaled;
          read = (fun row idx -> (T.get_column_Decimal_nullable row idx, idx + 1));
          column = ("" ^ "?" ^ " * price");
          count = 1;
          phantom = None;
        }
      let all = object
        method id = id
        method scaled = scaled
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (id INT, price DECIMAL(10,2))") T.no_params
  
    let param_start_expr db ~col ~id callback =
      let col = col Param_start_expr_col.all in
      let set_params stmt =
        let p = T.start_params stmt (1 + col.count) in
        col.set p;
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let param_start_expr db ~col ~id callback acc =
        let col = col Param_start_expr_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let param_start_expr db ~col ~id callback =
        let col = col Param_start_expr_col.all in
        let set_params stmt =
          let p = T.start_params stmt (1 + col.count) in
          col.set p;
          T.set_param_Int p id;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t WHERE id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Virtual select: explicit choices with alias alongside plain columns:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE t (id INT, name TEXT, category TEXT);
  > -- [sqlgg] dynamic_select=true
  > -- @mixed_explicit
  > SELECT id, @col { Name { name } | Cat { category } } AS detail FROM t WHERE id = @id;
  > EOF
  Failed mixed_explicit: SELECT id, @col { Name { name } | Cat { category } } AS detail FROM t WHERE id = @id
  At : @col { Name { name } | Cat { category } }
  Fatal error: exception Failure("sharing choices not implemented")
  [2]

Virtual select: tab-separated columns (non-space whitespace):
  $ printf 'CREATE TABLE t (a INT, b TEXT);\n-- [sqlgg] dynamic_select=true\n-- @tab_sep\nSELECT a,\tb FROM t;\n' | sqlgg -gen caml -no-header -dialect=mysql - 2>&1
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
    module Dynamic_select = Sqlgg_trait_types.Make_dynamic_select(struct
      type params = T.params
      type row = T.row
    end)
  
    module Tab_sep_col = struct
      include Dynamic_select
      type 'row all = 'row constraint 'row = < a : 'a0; b : 'a1; .. >
  
      let a =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Int_nullable row idx, idx + 1));
          column = ("a");
          count = 0;
          phantom = None;
        }
      let b =
        {
          set = (fun _p -> ());
          read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
          column = ("b");
          count = 0;
          phantom = None;
        }
      let all = object
        method a = a
        method b = b
      end
    end
  
  
    let create_t db  =
      T.execute db ("CREATE TABLE t (a INT, b TEXT)") T.no_params
  
    let tab_sep db ~col callback =
      let col = col Tab_sep_col.all in
      let set_params stmt =
        let p = T.start_params stmt (0 + col.count) in
        col.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.column ^ " FROM t")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col)
  
    module Fold = struct
      let tab_sep db ~col callback acc =
        let col = col Tab_sep_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let tab_sep db ~col callback =
        let col = col Tab_sep_col.all in
        let set_params stmt =
          let p = T.start_params stmt (0 + col.count) in
          col.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.column ^ " FROM t")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.read row 0 in callback
            ~col:__sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

