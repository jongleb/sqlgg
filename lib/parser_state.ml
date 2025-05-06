
type mode = Normal | Ignore | Ident
let mode = ref Normal
let mode_normal () = mode := Normal
let mode_ignore () = mode := Ignore
let mode_ident () = mode := Ident

let is_statement = ref false
let current_props : (string * string) list ref = ref []

let read_props () = 
  let props = !current_props in
  current_props := [];
  props
