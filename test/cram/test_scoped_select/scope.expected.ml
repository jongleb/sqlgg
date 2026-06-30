module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Scope = Sqlgg_scope.Make(T)

  module Scope_q1_col = struct
    let id = { Scope.read = (fun row -> T.get_column_Int row 0) }
    let name = { Scope.read = (fun row -> T.get_column_Text_nullable row 1) }
    let price = { Scope.read = (fun row -> T.get_column_Decimal_nullable row 2) }
    let category = { Scope.read = (fun row -> T.get_column_Text_nullable row 3) }

    let select db (fieldset : _ Scope.t) ~id =
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select_one_maybe db ("SELECT id, name, price, category FROM products WHERE id = ?") set_params (fun row -> fieldset.Scope.read row)

  end

  module Scope_q2_col = struct
    let stock = { Scope.read = (fun row -> T.get_column_Int_nullable row 0) }
    let id = { Scope.read = (fun row -> T.get_column_Int row 1) }
    let name = { Scope.read = (fun row -> T.get_column_Text_nullable row 2) }

    let select db (fieldset : _ Scope.t) ~min_stock callback =
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p min_stock;
        T.finish_params p
      in
      T.select db ("SELECT stock, id, name FROM products WHERE stock > ?") set_params (fun row -> callback (fieldset.Scope.read row))

    module Fold = struct
      let select db (fieldset : _ Scope.t) ~min_stock callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p min_stock;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT stock, id, name FROM products WHERE stock > ?") set_params (fun row -> r_acc := callback (fieldset.Scope.read row) !r_acc))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (fieldset : _ Scope.t) ~min_stock callback =
        let set_params stmt =
          let p = T.start_params stmt (1) in
          T.set_param_Int p min_stock;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT stock, id, name FROM products WHERE stock > ?") set_params (fun row -> r_acc := callback (fieldset.Scope.read row) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_products db  =
    T.execute db ("CREATE TABLE products (\n\
    id INT PRIMARY KEY,\n\
    name TEXT,\n\
    price DECIMAL(10,2),\n\
    category TEXT,\n\
    stock INT\n\
)") T.no_params

  let scope_q1 db ~id =
    let get_row stmt =
      (T.get_column_Int stmt 0), (T.get_column_Text_nullable stmt 1), (T.get_column_Decimal_nullable stmt 2), (T.get_column_Text_nullable stmt 3)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p id;
      T.finish_params p
    in
    T.select_one_maybe db ("SELECT id, name, price, category FROM products WHERE id = ?") set_params get_row

  let scope_q2 db ~min_stock callback =
    let invoke_callback stmt =
      callback
        ~stock:(T.get_column_Int_nullable stmt 0)
        ~id:(T.get_column_Int stmt 1)
        ~name:(T.get_column_Text_nullable stmt 2)
    in
    let set_params stmt =
      let p = T.start_params stmt (1) in
      T.set_param_Int p min_stock;
      T.finish_params p
    in
    T.select db ("SELECT stock, id, name FROM products WHERE stock > ?") set_params invoke_callback

  module Single = struct
    let scope_q1 db ~id callback =
      let invoke_callback stmt =
        callback
          ~id:(T.get_column_Int stmt 0)
          ~name:(T.get_column_Text_nullable stmt 1)
          ~price:(T.get_column_Decimal_nullable stmt 2)
          ~category:(T.get_column_Text_nullable stmt 3)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p id;
        T.finish_params p
      in
      T.select_one_maybe db ("SELECT id, name, price, category FROM products WHERE id = ?") set_params invoke_callback

  end (* module Single *)
  
  module Fold = struct
    let scope_q2 db ~min_stock callback acc =
      let invoke_callback stmt =
        callback
          ~stock:(T.get_column_Int_nullable stmt 0)
          ~id:(T.get_column_Int stmt 1)
          ~name:(T.get_column_Text_nullable stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p min_stock;
        T.finish_params p
      in
      let r_acc = ref acc in
      IO.(>>=) (T.select db ("SELECT stock, id, name FROM products WHERE stock > ?") set_params (fun x -> r_acc := invoke_callback x !r_acc))
      (fun () -> IO.return !r_acc)

  end (* module Fold *)
  
  module List = struct
    let scope_q2 db ~min_stock callback =
      let invoke_callback stmt =
        callback
          ~stock:(T.get_column_Int_nullable stmt 0)
          ~id:(T.get_column_Int stmt 1)
          ~name:(T.get_column_Text_nullable stmt 2)
      in
      let set_params stmt =
        let p = T.start_params stmt (1) in
        T.set_param_Int p min_stock;
        T.finish_params p
      in
      let r_acc = ref [] in
      IO.(>>=) (T.select db ("SELECT stock, id, name FROM products WHERE stock > ?") set_params (fun x -> r_acc := invoke_callback x :: !r_acc))
      (fun () -> IO.return (List.rev !r_acc))

  end (* module List *)
end (* module Sqlgg *)
