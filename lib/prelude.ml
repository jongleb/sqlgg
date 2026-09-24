
exception At of ((int * int) * exn)
exception Sql_error of string

let sql_errors_depth = ref 0

let with_sql_errors f =
  incr sql_errors_depth;
  Fun.protect
    ~finally:(fun () -> decr sql_errors_depth)
    f

let ($) f g = function x -> f (g x)

external identity : 'a -> 'a = "%identity"

let const c _ = c
let flip f x y = f y x

let tuck l x = l := x :: !l
let option_list = function Some x -> [x] | None -> []

let hashtbl_restore h s = Hashtbl.clear h; Hashtbl.iter (Hashtbl.replace h) s

let unique_by ~key l =
  let module Seen = Set.Make (String) in
  let (_, kept) =
    List.fold_left (fun ((seen, kept) as acc) x ->
      let k = key x in
      if Seen.mem k seen then acc else (Seen.add k seen, x :: kept))
      (Seen.empty, []) l
  in
  List.rev kept

let sql_exception message =
  if !sql_errors_depth > 0 then Sql_error message else Failure message

let fail fmt = Printf.ksprintf (fun message -> raise (sql_exception message)) fmt
let failed ~at fmt =
  Printf.ksprintf
    (fun message -> raise (At (at, sql_exception message)))
    fmt
let printfn fmt = Printf.ksprintf print_endline fmt
let eprintfn fmt = Printf.ksprintf prerr_endline fmt
