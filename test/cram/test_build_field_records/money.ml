type t = { cents : int64 }

let of_cents (c : int64) : t = { cents = c }

let get_column (x : int64) : t = { cents = x }
let get_column_nullable (x : int64 option) : t option = Option.map (fun c -> { cents = c }) x
let set_param (x : t) : int64 = x.cents

let to_string (x : t) : string =
  let dollars = Int64.div x.cents 100L in
  let rem = Int64.abs (Int64.rem x.cents 100L) in
  Printf.sprintf "%Ld.%02Ld" dollars rem
