type t = { raw : int64 }

let get_column (x : int64) : t = { raw = x }
let get_column_nullable (x : int64 option) : t option = Option.map (fun x -> { raw = x }) x
let set_param (x : t) : int64 = x.raw

let to_string (x : t) : string = Int64.to_string x.raw
