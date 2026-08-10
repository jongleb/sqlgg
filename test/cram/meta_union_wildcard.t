Discriminated union: NULL placeholders and enum literals in a UNION branch carry no domain
of their own, so module= metadata of the sibling branches survives:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE left_rows (
  >   -- [sqlgg] module=Left_id
  >   id BIGINT NOT NULL
  > );
  > CREATE TABLE right_rows (
  >   -- [sqlgg] module=Right_id
  >   id BIGINT NOT NULL,
  >   -- [sqlgg] module=Row_status
  >   status ENUM('draft','published','failed') NOT NULL
  > );
  > -- @fetch_merged
  > SELECT l.id AS left_id, NULL AS right_id, 'published' AS row_status FROM left_rows l
  > UNION ALL
  > SELECT NULL AS left_id, r.id AS right_id, r.status AS row_status FROM right_rows r;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
    let create_left_rows db  =
      T.execute db ("CREATE TABLE left_rows (\n\
      id BIGINT NOT NULL\n\
  )") T.no_params
  
    let create_right_rows db  =
      T.execute db ("CREATE TABLE right_rows (\n\
      id BIGINT NOT NULL,\n\
      status ENUM('draft','published','failed') NOT NULL\n\
  )") T.no_params
  
    let fetch_merged db  callback =
      let invoke_callback stmt =
        callback
          ~left_id:(Left_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
          ~right_id:(Right_id.get_column_nullable (T.get_column_int64_nullable stmt 1))
          ~row_status:(Row_status.get_column (T.get_column_string stmt 2))
      in
      T.select db ("SELECT l.id AS left_id, NULL AS right_id, 'published' AS row_status FROM left_rows l\n\
  UNION ALL\n\
  SELECT NULL AS left_id, r.id AS right_id, r.status AS row_status FROM right_rows r") T.no_params invoke_callback
  
    module Fold = struct
      let fetch_merged db  callback acc =
        let invoke_callback stmt =
          callback
            ~left_id:(Left_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
            ~right_id:(Right_id.get_column_nullable (T.get_column_int64_nullable stmt 1))
            ~row_status:(Row_status.get_column (T.get_column_string stmt 2))
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT l.id AS left_id, NULL AS right_id, 'published' AS row_status FROM left_rows l\n\
  UNION ALL\n\
  SELECT NULL AS left_id, r.id AS right_id, r.status AS row_status FROM right_rows r") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let fetch_merged db  callback =
        let invoke_callback stmt =
          callback
            ~left_id:(Left_id.get_column_nullable (T.get_column_int64_nullable stmt 0))
            ~right_id:(Right_id.get_column_nullable (T.get_column_int64_nullable stmt 1))
            ~row_status:(Row_status.get_column (T.get_column_string stmt 2))
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT l.id AS left_id, NULL AS right_id, 'published' AS row_status FROM left_rows l\n\
  UNION ALL\n\
  SELECT NULL AS left_id, r.id AS right_id, r.status AS row_status FROM right_rows r") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)

Conflicting module= between branches still drops the metadata:
  $ sqlgg -gen caml -no-header -dialect=mysql - <<'EOF' 2>&1
  > CREATE TABLE rows_a (
  >   -- [sqlgg] module=Status_a
  >   status ENUM('draft','published') NOT NULL
  > );
  > CREATE TABLE rows_b (
  >   -- [sqlgg] module=Status_b
  >   status ENUM('draft','published') NOT NULL
  > );
  > -- @merged
  > SELECT status FROM rows_a UNION ALL SELECT status FROM rows_b;
  > EOF
  module Sqlgg (T : Sqlgg_traits.M) = struct
  
    module IO = Sqlgg_io.Blocking
  
      module Enum_0 = T.Make_enum(struct
        type t = [`Draft | `Published]
        let inj = function | "draft" -> `Draft | "published" -> `Published | s -> failwith (Printf.sprintf "Invalid enum value: %s" s)
        let proj = function  | `Draft -> "draft"| `Published -> "published"
      end)
  
    let create_rows_a db  =
      T.execute db ("CREATE TABLE rows_a (\n\
      status ENUM('draft','published') NOT NULL\n\
  )") T.no_params
  
    let create_rows_b db  =
      T.execute db ("CREATE TABLE rows_b (\n\
      status ENUM('draft','published') NOT NULL\n\
  )") T.no_params
  
    let merged db  callback =
      let invoke_callback stmt =
        callback
          ~status:(Enum_0.get_column stmt 0)
      in
      T.select db ("SELECT status FROM rows_a UNION ALL SELECT status FROM rows_b") T.no_params invoke_callback
  
    module Fold = struct
      let merged db  callback acc =
        let invoke_callback stmt =
          callback
            ~status:(Enum_0.get_column stmt 0)
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db ("SELECT status FROM rows_a UNION ALL SELECT status FROM rows_b") T.no_params (fun x -> r_acc := invoke_callback x !r_acc))
        (fun () -> IO.return !r_acc)
  
    end (* module Fold *)
    
    module List = struct
      let merged db  callback =
        let invoke_callback stmt =
          callback
            ~status:(Enum_0.get_column stmt 0)
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db ("SELECT status FROM rows_a UNION ALL SELECT status FROM rows_b") T.no_params (fun x -> r_acc := invoke_callback x :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))
  
    end (* module List *)
  end (* module Sqlgg *)
