type kind =
  | RegularString of string
  | Prop of string * string

type t = kind Queue.t

let add_char c queue = Queue.add (RegularString (String.make 1 c)) queue

let add_string s queue = Queue.add (RegularString s) queue

let add_prop (name, value) queue = Queue.add (Prop (name, value)) queue

let queue_byte_feeder ~feeder_queue =
  let feeder_queue = Queue.copy feeder_queue in
  let rec fill_buffer buffer max_len filled_len segment_opt =
    if filled_len = max_len then
      filled_len
    else
      match segment_opt with
      | Some (s, pos) ->
          let s_len = String.length s in
          let remaining_in_segment = s_len - pos in
          let bytes_to_copy = min (max_len - filled_len) remaining_in_segment in
          if bytes_to_copy > 0 then (
            Bytes.blit_string s pos buffer filled_len bytes_to_copy;
            fill_buffer buffer max_len (filled_len + bytes_to_copy) (Some (s, pos + bytes_to_copy))
          ) else (
            fill_buffer buffer max_len filled_len None
          )
      | None ->
        begin try
          let next_element = Queue.pop feeder_queue in
          match next_element with
          | RegularString s ->
              fill_buffer buffer max_len filled_len (Some (s, 0))
          | Prop (k, v) ->
              Parser_state.current_props := (k, v) :: !Parser_state.current_props;
              fill_buffer buffer max_len filled_len None
        with
        | Queue.Empty ->
            filled_len
        end
  in
  fun buffer max_len -> fill_buffer buffer max_len 0 None


let elements_to_sql elements =
  let b = Buffer.create 1024 in
  let temp_queue = Queue.copy elements in
  while not (Queue.is_empty temp_queue) do
    match Queue.take temp_queue with
    | RegularString s -> Buffer.add_string b s
    | Prop _ -> ()
  done;
  Buffer.contents b

let create = Queue.create
