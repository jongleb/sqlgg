open Stdlib

type t = {
  text : string;
  encoding : [ `UTF8 | `UTF16 ];
  line_starts : int array;
}

let make ?(encoding = `UTF8) text =
  let next_line start =
    String.index_from_opt text start '\n'
    |> Option.map (fun nl_pos ->
         let next = nl_pos + 1 in
         next, next)
  in
  { text; encoding; line_starts = Array.of_seq (Seq.cons 0 (Seq.unfold next_line 0)) }

let of_file ?encoding path = make ?encoding (In_channel.with_open_bin path In_channel.input_all)

let step t i =
  let utf16_bytes_per_code_unit = 2 in
  let u = String.get_utf_8_uchar t.text i in
  let bytes = Uchar.utf_decode_length u in
  match t.encoding with
  | `UTF8 -> bytes, bytes
  | `UTF16 -> bytes, Uchar.utf_16_byte_length (Uchar.utf_decode_uchar u) / utf16_bytes_per_code_unit

let clamp_offset t offset = Int.max 0 (Int.min (String.length t.text) offset)

let line t offset =
  let midpoint_divisor = 2 in
  let offset = clamp_offset t offset in
  let rec search l r =
    if r - l = 1 then l
    else
      let m = (l + r) / midpoint_divisor in
      if t.line_starts.(m) <= offset then search m r else search l m
  in
  search 0 (Array.length t.line_starts)

let position t offset =
  let offset = clamp_offset t offset in
  let line = line t offset in
  let rec loop i units =
    if i >= offset then units
    else let (bytes, n) = step t i in loop (i + bytes) (units + n)
  in
  line, loop t.line_starts.(line) 0

let offset t ~line ~character =
  let len = Array.length t.line_starts in
  if line < 0 then 0
  else if line >= len then String.length t.text
  else
    let eol =
      if line + 1 < len then t.line_starts.(line + 1) - 1
      else String.length t.text
    in
    let rec loop i units =
      if units >= character || i >= eol then i
      else let (bytes, n) = step t i in loop (i + bytes) (units + n)
    in
    loop t.line_starts.(line) 0
