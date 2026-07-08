module Scope = struct
  type ('a, 'q) t = { read : string array -> 'a }
  let pure x = { read = (fun _ -> x) }
  let apply f a = { read = (fun r -> (f.read r) (a.read r)) }
end

type who = { id : int64; name : string option }
[@@deriving sqlgg]

module Q_col = struct
  type t
  let id : (_, t) Scope.t = { Scope.read = (fun r -> Int64.of_string r.(0)) }
  let name : (_, t) Scope.t = { Scope.read = (fun r -> match r.(1) with "" -> None | s -> Some s) }
  let extra : (_, t) Scope.t = { Scope.read = (fun r -> float_of_string r.(2)) }
end

module Q2_col = struct
  type t
  let id : (_, t) Scope.t = { Scope.read = (fun r -> Int64.of_string r.(1)) }
  let name : (_, t) Scope.t = { Scope.read = (fun r -> match r.(0) with "" -> None | s -> Some s) }
end

type t = { id : int64; name : string option }
[@@deriving sqlgg]

let () =
  let frag = who_of_scope (module Q_col) in
  let value = frag.Scope.read [| "7"; "hi"; "9.0" |] in
  assert (value = { id = 7L; name = Some "hi" });
  let frag2 = who_of_scope (module Q2_col) in
  let value2 = frag2.Scope.read [| "yo"; "3" |] in
  assert (value2 = { id = 3L; name = Some "yo" });
  let frag_t : (t, Q_col.t) Scope.t = of_scope (module Q_col) in
  let value_t = frag_t.Scope.read [| "7"; "hi"; "9.0" |] in
  assert (value_t = { id = 7L; name = Some "hi" });
  let _check : (module Sqlgg_scope) = (module Q_col) in
  let _check_who : (module Sqlgg_who_scope) = (module Q_col) in
  print_endline "ppx sqlgg: who_of_scope OK"
