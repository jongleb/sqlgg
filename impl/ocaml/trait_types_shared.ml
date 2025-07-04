module Json_function_types = struct
  type json = [
    | `Null
    | `String of string
    | `Float of float
    | `Int of int
    | `Bool of bool
    | `List of json list
    | `Assoc of (string * json) list 
  ]

  type json_path = [ 
    | `Root
    | `Descend of json_path
    | `Key of json_path * string
    | `Index of json_path * int
    | `Key_wildcard of json_path
    | `Index_wildcard of json_path
  ] list

  type one_or_all = [ `One | `All ]
end

module Json_path = struct 
  let parse_json_path s : Json_function_types.json_path =
    let len = String.length s in

    let is_ident_char = function
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' -> true
      | _ -> false
    in

    let parse_ident i =
      let j =
        let rec loop j =
          if j < len && is_ident_char s.[j] then loop (j + 1) else j
        in loop i
      in
      if j = i then failwith ("Expected identifier at pos " ^ string_of_int i)
      else String.sub s i (j - i), j
    in

    let rec parse acc i =
      if i >= len then List.rev acc
      else match s.[i] with
      | '$' when i = 0 ->
          parse (`Root :: acc) (i + 1)

      | '.' ->
          if i + 2 <= len && s.[i + 1] = '*' && s.[i + 2] = '*' then
            let inner = parse [] 0 in
            parse (`Descend inner :: acc) (i + 3)
          else if i + 1 < len && s.[i + 1] = '*' then
            let inner = parse [] 0 in
            parse (`Key_wildcard inner :: acc) (i + 2)
          else
            let key, j = parse_ident (i + 1) in
            let inner = parse [] 0 in
            parse (`Key (inner, key) :: acc) j

      | '[' ->
          if i + 2 < len && s.[i + 1] = '*' && s.[i + 2] = ']' then
            let inner = parse [] 0 in
            parse (`Index_wildcard inner :: acc) (i + 3)
          else
            let j =
              let rec loop j =
                if j >= len then failwith ("Unclosed '[' at pos " ^ string_of_int i)
                else if s.[j] = ']' then j
                else loop (j + 1)
              in loop (i + 1)
            in
            let substr = String.sub s (i + 1) (j - i - 1) in
            let idx =
              try int_of_string substr
              with Failure _ ->
                failwith ("Invalid index at pos " ^ string_of_int i ^ ": " ^ substr)
            in
            let inner = parse [] 0 in
            parse (`Index (inner, idx) :: acc) (j + 1)

      | c ->
          failwith ("Unexpected char '" ^ String.make 1 c ^ "' at pos " ^ string_of_int i)
    in

    parse [] 0


  let json_path_to_string path =
    let buffer = Buffer.create 64 in
    let rec build_path = function
      | [] -> ()
      | `Root :: rest -> 
          Buffer.add_char buffer '$';
          build_path rest
      | `Key_access key :: rest ->
          Buffer.add_char buffer '.';
          Buffer.add_string buffer key;
          build_path rest
      | `Index_access index :: rest ->
          Buffer.add_char buffer '[';
          Buffer.add_string buffer (string_of_int index);
          Buffer.add_char buffer ']';
          build_path rest
      | `Key_wildcard :: rest ->
          Buffer.add_string buffer ".*";
          build_path rest
      | `Index_wildcard :: rest ->
          Buffer.add_string buffer "[*]";
          build_path rest
          in
          build_path path;
          Buffer.contents buffer
end
