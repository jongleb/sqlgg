module Applicative_ops (A : sig
  type 'a t
  val pure : 'a -> 'a t
  val apply : ('a -> 'b) t -> 'a t -> 'b t
end) = struct
  let pure = A.pure
  let apply = A.apply

  let map f a = apply (pure f) a

  let ( let+ ) t f = map f t
  let ( and+ ) a b = apply (map (fun a b -> (a, b)) a) b
end

module Make (Row : sig type t end) = struct
  type ('a, 'q) t = { read : Row.t -> 'a }

  let pure : 'a -> ('a, 'q) t = fun x -> { read = (fun _row -> x) }
  let apply : ('a -> 'b, 'q) t -> ('a, 'q) t -> ('b, 'q) t =
    fun f a -> { read = (fun row -> (f.read row) (a.read row)) }

  module Ops (Q : sig type t end) = Applicative_ops (struct
    type nonrec 'a t = ('a, Q.t) t
    let pure = pure
    let apply = apply
  end)
end

module Dynamic (Row : sig type t end) (Params : sig type t end) = struct
  type ('a, 'q) t = {
    set : Params.t -> unit;
    read : Row.t -> int -> 'a * int;
    column : string;
    count : int;
  }

  let pure : 'a -> ('a, 'q) t = fun x -> {
    set = (fun _p -> ());
    read = (fun _row idx -> (x, idx));
    column = "";
    count = 0;
  }

  let apply : ('a -> 'b, 'q) t -> ('a, 'q) t -> ('b, 'q) t = fun f a -> {
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

  module Ops (Q : sig type t end) = Applicative_ops (struct
    type nonrec 'a t = ('a, Q.t) t
    let pure = pure
    let apply = apply
  end)
end
