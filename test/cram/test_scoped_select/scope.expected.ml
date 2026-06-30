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

end (* module Sqlgg *)
