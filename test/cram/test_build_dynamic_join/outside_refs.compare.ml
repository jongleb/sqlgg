module Sqlgg (T : Sqlgg_traits.M) = struct

  module IO = Sqlgg_io.Blocking
  module Ref_in_group_col = struct
    type source = |

    (* what travels into SELECT: the columns' SQL text, the row reader, the params *)
    type 'a projection = {
      set: T.params -> unit;
      read: T.row -> int -> 'a * int;
      column: string;
      count: int;
    }

    (* a selector: a projection plus the sources it pulls into FROM *)
    type 'a t = {
      projection: 'a projection;
      deps: source list;
    }

    let pure x = {
      projection = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      };
      deps = [];
    }

    let apply f a = {
      projection = {
        set = (fun p -> f.projection.set p; a.projection.set p);
        read = (fun row idx ->
          let (vf, i1) = f.projection.read row idx in
          let (va, i2) = a.projection.read row i1 in
          (vf va, i2));
        column = (match f.projection.column, a.projection.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.projection.count + a.projection.count;
      };
      deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
    }

    let map f a = apply (pure f) a

    let (let+) t f = map f t
    let (and+) a b = apply (map (fun a b -> (a, b)) a) b

    (* lift a pure projection into a selector, tagging the sources it depends on *)
    let lift deps projection = { projection; deps }
    let id =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
        column = ("u.id");
        count = 0;
      }
    let bio =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.bio");
        count = 0;
      }

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.projection.count) in
        col.projection.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY p.bio")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Ref_in_order_col = struct
    type source = |

    (* what travels into SELECT: the columns' SQL text, the row reader, the params *)
    type 'a projection = {
      set: T.params -> unit;
      read: T.row -> int -> 'a * int;
      column: string;
      count: int;
    }

    (* a selector: a projection plus the sources it pulls into FROM *)
    type 'a t = {
      projection: 'a projection;
      deps: source list;
    }

    let pure x = {
      projection = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      };
      deps = [];
    }

    let apply f a = {
      projection = {
        set = (fun p -> f.projection.set p; a.projection.set p);
        read = (fun row idx ->
          let (vf, i1) = f.projection.read row idx in
          let (va, i2) = a.projection.read row i1 in
          (vf va, i2));
        column = (match f.projection.column, a.projection.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.projection.count + a.projection.count;
      };
      deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
    }

    let map f a = apply (pure f) a

    let (let+) t f = map f t
    let (and+) a b = apply (map (fun a b -> (a, b)) a) b

    (* lift a pure projection into a selector, tagging the sources it depends on *)
    let lift deps projection = { projection; deps }
    let id =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
        column = ("u.id");
        count = 0;
      }
    let bio =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.bio");
        count = 0;
      }

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.projection.count) in
        col.projection.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id ORDER BY p.bio")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Ref_in_having_col = struct
    type source = |

    (* what travels into SELECT: the columns' SQL text, the row reader, the params *)
    type 'a projection = {
      set: T.params -> unit;
      read: T.row -> int -> 'a * int;
      column: string;
      count: int;
    }

    (* a selector: a projection plus the sources it pulls into FROM *)
    type 'a t = {
      projection: 'a projection;
      deps: source list;
    }

    let pure x = {
      projection = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      };
      deps = [];
    }

    let apply f a = {
      projection = {
        set = (fun p -> f.projection.set p; a.projection.set p);
        read = (fun row idx ->
          let (vf, i1) = f.projection.read row idx in
          let (va, i2) = a.projection.read row i1 in
          (vf va, i2));
        column = (match f.projection.column, a.projection.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.projection.count + a.projection.count;
      };
      deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
    }

    let map f a = apply (pure f) a

    let (let+) t f = map f t
    let (and+) a b = apply (map (fun a b -> (a, b)) a) b

    (* lift a pure projection into a selector, tagging the sources it depends on *)
    let lift deps projection = { projection; deps }
    let id =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
        column = ("u.id");
        count = 0;
      }
    let bio =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.bio");
        count = 0;
      }

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.projection.count) in
        col.projection.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id GROUP BY u.id HAVING MAX(p.user_id) > 0")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Complex_proj_col = struct
    type source = |

    (* what travels into SELECT: the columns' SQL text, the row reader, the params *)
    type 'a projection = {
      set: T.params -> unit;
      read: T.row -> int -> 'a * int;
      column: string;
      count: int;
    }

    (* a selector: a projection plus the sources it pulls into FROM *)
    type 'a t = {
      projection: 'a projection;
      deps: source list;
    }

    let pure x = {
      projection = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      };
      deps = [];
    }

    let apply f a = {
      projection = {
        set = (fun p -> f.projection.set p; a.projection.set p);
        read = (fun row idx ->
          let (vf, i1) = f.projection.read row idx in
          let (va, i2) = a.projection.read row i1 in
          (vf va, i2));
        column = (match f.projection.column, a.projection.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.projection.count + a.projection.count;
      };
      deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
    }

    let map f a = apply (pure f) a

    let (let+) t f = map f t
    let (and+) a b = apply (map (fun a b -> (a, b)) a) b

    (* lift a pure projection into a selector, tagging the sources it depends on *)
    let lift deps projection = { projection; deps }
    let id =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
        column = ("u.id");
        count = 0;
      }
    let shout =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("CONCAT(p.bio, '!')");
        count = 0;
      }

    let select db (col : _ t) ~uid callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.projection.count) in
        col.projection.set p;
        T.set_param_Int p uid;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~uid callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.projection.count) in
          col.projection.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~uid callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.projection.count) in
          col.projection.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Subq_in_where_col = struct
    type source = |

    (* what travels into SELECT: the columns' SQL text, the row reader, the params *)
    type 'a projection = {
      set: T.params -> unit;
      read: T.row -> int -> 'a * int;
      column: string;
      count: int;
    }

    (* a selector: a projection plus the sources it pulls into FROM *)
    type 'a t = {
      projection: 'a projection;
      deps: source list;
    }

    let pure x = {
      projection = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      };
      deps = [];
    }

    let apply f a = {
      projection = {
        set = (fun p -> f.projection.set p; a.projection.set p);
        read = (fun row idx ->
          let (vf, i1) = f.projection.read row idx in
          let (va, i2) = a.projection.read row i1 in
          (vf va, i2));
        column = (match f.projection.column, a.projection.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.projection.count + a.projection.count;
      };
      deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
    }

    let map f a = apply (pure f) a

    let (let+) t f = map f t
    let (and+) a b = apply (map (fun a b -> (a, b)) a) b

    (* lift a pure projection into a selector, tagging the sources it depends on *)
    let lift deps projection = { projection; deps }
    let id =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
        column = ("u.id");
        count = 0;
      }
    let bio =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.bio");
        count = 0;
      }

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.projection.count) in
        col.projection.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles)")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles)")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id IN (SELECT user_id FROM profiles)")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Unqualified_where_col = struct
    type source = |

    (* what travels into SELECT: the columns' SQL text, the row reader, the params *)
    type 'a projection = {
      set: T.params -> unit;
      read: T.row -> int -> 'a * int;
      column: string;
      count: int;
    }

    (* a selector: a projection plus the sources it pulls into FROM *)
    type 'a t = {
      projection: 'a projection;
      deps: source list;
    }

    let pure x = {
      projection = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      };
      deps = [];
    }

    let apply f a = {
      projection = {
        set = (fun p -> f.projection.set p; a.projection.set p);
        read = (fun row idx ->
          let (vf, i1) = f.projection.read row idx in
          let (va, i2) = a.projection.read row i1 in
          (vf va, i2));
        column = (match f.projection.column, a.projection.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.projection.count + a.projection.count;
      };
      deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
    }

    let map f a = apply (pure f) a

    let (let+) t f = map f t
    let (and+) a b = apply (map (fun a b -> (a, b)) a) b

    (* lift a pure projection into a selector, tagging the sources it depends on *)
    let lift deps projection = { projection; deps }
    let id =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
        column = ("u.id");
        count = 0;
      }
    let bio =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("p.bio");
        count = 0;
      }

    let select db (col : _ t) callback =
      let set_params stmt =
        let p = T.start_params stmt (0 + col.projection.count) in
        col.projection.set p;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x'")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) callback acc =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x'")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) callback =
        let set_params stmt =
          let p = T.start_params stmt (0 + col.projection.count) in
          col.projection.set p;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE bio = 'x'")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end

  module Join_unreferenced_col = struct
    type source = |

    (* what travels into SELECT: the columns' SQL text, the row reader, the params *)
    type 'a projection = {
      set: T.params -> unit;
      read: T.row -> int -> 'a * int;
      column: string;
      count: int;
    }

    (* a selector: a projection plus the sources it pulls into FROM *)
    type 'a t = {
      projection: 'a projection;
      deps: source list;
    }

    let pure x = {
      projection = {
        set = (fun _p -> ());
        read = (fun _row idx -> (x, idx));
        column = "";
        count = 0;
      };
      deps = [];
    }

    let apply f a = {
      projection = {
        set = (fun p -> f.projection.set p; a.projection.set p);
        read = (fun row idx ->
          let (vf, i1) = f.projection.read row idx in
          let (va, i2) = a.projection.read row i1 in
          (vf va, i2));
        column = (match f.projection.column, a.projection.column with
          | "", c | c, "" -> c
          | c1, c2 -> c1 ^ ", " ^ c2);
        count = f.projection.count + a.projection.count;
      };
      deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
    }

    let map f a = apply (pure f) a

    let (let+) t f = map f t
    let (and+) a b = apply (map (fun a b -> (a, b)) a) b

    (* lift a pure projection into a selector, tagging the sources it depends on *)
    let lift deps projection = { projection; deps }
    let id =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Int row idx, idx + 1));
        column = ("u.id");
        count = 0;
      }
    let name =
      lift [] {
        set = (fun _p -> ());
        read = (fun row idx -> (T.get_column_Text_nullable row idx, idx + 1));
        column = ("u.name");
        count = 0;
      }

    let select db (col : _ t) ~uid callback =
      let set_params stmt =
        let p = T.start_params stmt (1 + col.projection.count) in
        col.projection.set p;
        T.set_param_Int p uid;
        T.finish_params p
      in
      T.select db
      ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?")
      set_params (fun row -> let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col)

    module Fold = struct
      let select db (col : _ t) ~uid callback acc =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.projection.count) in
          col.projection.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref acc in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col !r_acc)))
        (fun () -> IO.return !r_acc)

    end (* module Fold *)

    module List = struct
      let select db (col : _ t) ~uid callback =
        let set_params stmt =
          let p = T.start_params stmt (1 + col.projection.count) in
          col.projection.set p;
          T.set_param_Int p uid;
          T.finish_params p
        in
        let r_acc = ref [] in
        IO.(>>=) (T.select db
        ("SELECT " ^ col.projection.column ^ " FROM users u LEFT JOIN profiles p ON p.user_id = u.id WHERE u.id = ?")
        set_params (fun row -> r_acc := (let (__sqlgg_r_col, __sqlgg_idx_after_col) = col.projection.read row 0 in callback
          __sqlgg_r_col) :: !r_acc))
        (fun () -> IO.return (List.rev !r_acc))

    end (* module List *)

  end


  let create_users db  =
    T.execute db ("CREATE TABLE users (id INT PRIMARY KEY, name TEXT)") T.no_params

  let create_profiles db  =
    T.execute db ("CREATE TABLE profiles (user_id INT PRIMARY KEY, bio TEXT)") T.no_params

  module Fold = struct
  end (* module Fold *)
  
  module List = struct
  end (* module List *)
end (* module Sqlgg *)
