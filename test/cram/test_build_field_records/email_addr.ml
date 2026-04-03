type t = { addr : string; domain : string }

let of_string (s : string) : t =
  match String.split_on_char '@' s with
  | [local; domain] -> { addr = local ^ "@" ^ domain; domain }
  | _ -> { addr = s; domain = "invalid" }

let get_column (x : string) : t = of_string x
let get_column_nullable (x : string option) : t option = Option.map of_string x
let set_param (x : t) : string = x.addr

let to_string (x : t) : string = x.addr
let domain (x : t) : string = x.domain
