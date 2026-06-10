let run label f =
  Printf.printf "=== %s ===\n%!" label;
  Print_impl.clear_mock_responses ();
  Print_impl.setup_select_response [];
  f ()

module On_shapes = On_shapes.Sqlgg(Print_impl)

let () =
  let open On_shapes.Param_in_on_col in
  run "on_shapes/param_in_on: pick id -> join kept (param in ON)" (fun () -> ignore (List.select () id ~b:"x" ~uid:1L (fun x -> x)))

let () =
  let open On_shapes.Extra_const_on_col in
  run "on_shapes/extra_const_on: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "on_shapes/extra_const_on: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L (fun x -> x)))

let () =
  let open On_shapes.On_inequality_col in
  run "on_shapes/inequality: pick id -> join kept" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)))

let () =
  let open On_shapes.No_alias_col in
  run "on_shapes/no_alias: pick id -> join dropped" (fun () -> ignore (List.select () id ~uid:1L (fun x -> x)));
  run "on_shapes/no_alias: pick bio -> join present" (fun () -> ignore (List.select () bio ~uid:1L (fun x -> x)))
