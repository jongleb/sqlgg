module Applicative_ops (A : sig
  type 'a t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end) = struct
  open A

  let map f a = apply (pure f) a

  let ( let+ ) t f = map f t
  let ( and+ ) a b = apply (map (fun a b -> (a, b)) a) b
end

module Make (T : sig type row end) = struct
  type 'a t = { read : T.row -> 'a }

  let pure x = { read = (fun _row -> x) }
  let apply f a = { read = (fun row -> (f.read row) (a.read row)) }

  include Applicative_ops (struct
    type nonrec 'a t = 'a t
    let pure = pure
    let apply = apply
  end)
end

module Dynamic (T : sig type row type params end) = struct
  type 'a t = {
    set : T.params -> unit;
    read : T.row -> int -> 'a * int;
    column : string;
    count : int;
  }

  let pure x = {
    set = (fun _p -> ());
    read = (fun _row idx -> (x, idx));
    column = "";
    count = 0;
  }

  let apply f a = {
    set = (fun p -> f.set p; a.set p);
    read = (fun row idx ->
      let (vf, i1) = f.read row idx in
      let (va, i2) = a.read row i1 in
      (vf va, i2));
    column = (match f.column, a.column with
      | "", c | c, "" -> c
      | c1, c2 -> c1 ^ ", " ^ c2);
    count = f.count + a.count;
  }

  include Applicative_ops (struct
    type nonrec 'a t = 'a t
    let pure = pure
    let apply = apply
  end)
end
