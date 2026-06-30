module Scope = struct
  type 'a t = { read : string array -> 'a }
  let pure x = { read = (fun _ -> x) }
  let apply f a = { read = (fun r -> (f.read r) (a.read r)) }
end

type who = { id : int64; name : string option }
[@@deriving sqlgg]

module Q_col = struct
  let id = { Scope.read = (fun r -> Int64.of_string r.(0)) }
  let name = { Scope.read = (fun r -> match r.(1) with "" -> None | s -> Some s) }
  let extra = { Scope.read = (fun r -> float_of_string r.(2)) }
end

type t = { id : int64; name : string option }
[@@deriving sqlgg]

let () =
  let frag = who_of_scope (module Q_col) in
  let value = frag.Scope.read [| "7"; "hi"; "9.0" |] in
  assert (value = { id = 7L; name = Some "hi" });
  let frag_t : t Scope.t = of_scope (module Q_col) in
  let value_t = frag_t.Scope.read [| "7"; "hi"; "9.0" |] in
  assert (value_t = { id = 7L; name = Some "hi" });
  let _check : (module Sqlgg_scope) = (module Q_col) in
  let _check_who : (module Sqlgg_who_scope) = (module Q_col) in
  print_endline "ppx sqlgg: who_of_scope OK"
