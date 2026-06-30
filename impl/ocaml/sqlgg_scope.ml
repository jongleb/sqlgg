type ('a, 'q, 'row, 'params) col = {
  set : 'params -> unit;
  read : 'row -> int -> 'a * int;
  column : string;
  count : int;
  deps : 'q list;
}

let pure x = {
  set = (fun _p -> ());
  read = (fun _row idx -> (x, idx));
  column = "";
  count = 0;
  deps = [];
}

let apply f a = {
  set = (fun p -> f.set p; a.set p);
  read = (fun row idx ->
    let (vf, i1) = f.read row idx in
    let (va, i2) = a.read row i1 in
    (vf va, i2));
  column = begin match f.column, a.column with
    | "", c | c, "" -> c
    | c1, c2 -> c1 ^ ", " ^ c2
    end;
  count = f.count + a.count;
  deps = f.deps @ List.filter (fun d -> not (List.mem d f.deps)) a.deps;
}

module Dynamic (Row : sig type t end) (Params : sig type t end) = struct
  type ('a, 'q) t = ('a, 'q, Row.t, Params.t) col

  let pure = pure
  let apply = apply

  module Ops (Q : sig type t end) = struct
    let pure : 'a -> ('a, Q.t) t = pure
    let apply : ('a -> 'b, Q.t) t -> ('a, Q.t) t -> ('b, Q.t) t = apply

    let map f a = apply (pure f) a

    let ( let+ ) t f = map f t
    let ( and+ ) a b = apply (map (fun a b -> (a, b)) a) b
  end
end
