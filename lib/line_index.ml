open Stdlib

type position_encoding = [ `UTF8 | `UTF16 ]

type position = { line : int; character : int }

type t = {
  text : string;
  position_encoding : position_encoding;
  line_starts : int array;
}

let make ?(position_encoding = `UTF8) text =
  let next_line start =
    String.index_from_opt text start '\n'
    |> Option.map (fun newline ->
         let next = newline + 1 in
         next, next)
  in
  let starts = Seq.cons 0 (Seq.unfold next_line 0) in
  { text; position_encoding; line_starts = Array.of_seq starts }

let of_file ?position_encoding path =
  make ?position_encoding (In_channel.with_open_bin path In_channel.input_all)

let clamp_offset text offset = Int.max 0 (Int.min (String.length text) offset)

let step t i =
  let utf16_bytes_per_code_unit = 2 in
  let decoded = String.get_utf_8_uchar t.text i in
  let byte_length = Uchar.utf_decode_length decoded in
  match t.position_encoding with
  | `UTF8 -> byte_length, byte_length
  | `UTF16 ->
    let uchar = Uchar.utf_decode_uchar decoded in
    byte_length, Uchar.utf_16_byte_length uchar / utf16_bytes_per_code_unit

let line t offset =
  let offset = clamp_offset t.text offset in
  let rec search l r =
    if r - l <= 1 then l
    else
      let m = (l + r) / 2 in
      if t.line_starts.(m) <= offset then search m r else search l m
  in
  search 0 (Array.length t.line_starts)

let line_end t line =
  let stop =
    if line + 1 < Array.length t.line_starts
    then t.line_starts.(line + 1) - 1
    else String.length t.text
  in
  if stop > t.line_starts.(line) && Char.equal t.text.[stop - 1] '\r' then stop - 1 else stop

let position t offset =
  let offset = clamp_offset t.text offset in
  let line = line t offset in
  let stop = Int.min offset (line_end t line) in
  let rec loop i units =
    if i >= stop then units
    else let (byte_length, width) = step t i in loop (i + byte_length) (units + width)
  in
  { line; character = loop t.line_starts.(line) 0 }

let offset t ~line ~character =
  let lines = Array.length t.line_starts in
  if line < 0 then 0
  else if line >= lines then String.length t.text
  else
    let stop = line_end t line in
    let rec loop i units =
      if units >= character || i >= stop then i
      else let (byte_length, width) = step t i in loop (i + byte_length) (units + width)
    in
    loop t.line_starts.(line) 0
