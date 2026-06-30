module Make (T : sig type row end) = struct
  type 'a t = { read : T.row -> 'a }

  let pure x = { read = (fun _row -> x) }
  let apply f a = { read = (fun row -> (f.read row) (a.read row)) }
  let map f a = apply (pure f) a

  let ( let+ ) t f = map f t
  let ( and+ ) a b = apply (map (fun a b -> (a, b)) a) b
end
