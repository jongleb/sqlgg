
open Hmx_lattice

type param_spec =
  | Same
  | As of Refined.t
  | Free

type params =
  | Args of param_spec list
  | Varargs of { head : param_spec list; tail : param_spec list }

type null_rule =
  | Join
  | Meet
  | Const of bool
  | Group_join

type t = {
  params : params;
  ret : Refined.t option;
  preds : Pred.t list;
  nulls : null_rule;
  agg : bool;
  proves_not_null : [ `None | `All | `First ];

  compares : bool;

  carries : bool;

}

let make ?ret ?(preds = []) ?(nulls = Join) ?(agg = false) ?(compares = false)
    ?(proves_not_null = `None) ?(carries = true) params =
  { params; ret; preds; nulls; agg; compares; proves_not_null; carries }

type scheme = {
  formals : param_spec list;
  result : Refined.t option;
  result_null : null_rule;
  same_at : bool list;
  preds : Pred.t list;
}

let expand_params params arity =
  match params with
  | Args l -> if List.length l = arity then Ok l else Error (List.length l)
  | Varargs { head; tail } ->
    let h = List.length head and t = List.length tail in
    let rest = arity - h in
    if rest < 0 then Error h
    else if rest = 0 then Ok head
    else if t = 0 then Error h
    else if rest mod t <> 0 then Error h
    else
      let rec repeat acc n = if n = 0 then acc else repeat (acc @ tail) (n - 1) in
      Ok (repeat head (rest / t))

let instantiate sg arity =
  match expand_params sg.params arity with
  | Error expected ->
    Error (Printf.sprintf "wrong number of arguments: got %d, expected %s%d" arity
             (match sg.params with Args _ -> "" | Varargs _ -> "at least ") expected)
  | Ok formals ->
    Ok { formals;
         result = sg.ret;
         result_null = sg.nulls;
         same_at = List.map (function Same -> true | As _ | Free -> false) formals;
         preds = sg.preds }

let b = Refined.of_base
let int = b Base.Int
let text = b Base.Text
let blob = b Base.Blob
let float = b Base.Float
let json = b Base.Json
let json_path = b Base.Json_path
let one_or_all = b Base.One_or_all
let datetime = b Base.Datetime
let boolean = b Base.Bool

let nullable = Const true
let strict = Const false

let mono ?nulls ?preds ret args = make ?nulls ?preds ~ret (Args (List.map (fun t -> As t) args))

let rep ?nulls ?preds ?ret ?(head = []) tail = make ?nulls ?preds ?ret (Varargs { head; tail })

let as_ t = As t
let anything = Free

let names l sg = List.map (fun n -> n, sg) l

let any = None
let at n = Some n

let table : (string * int option * t) list =
  let many names arity sg = List.map (fun n -> n, arity, sg) names in
  List.concat [

    many [ "count" ] (at 0) (make ~agg:true ~ret:int ~nulls:strict (Args []));
    many [ "count" ] (at 1) (make ~agg:true ~ret:int ~nulls:strict (Args [ Free ]));
    many [ "sum" ] (at 1) (make ~agg:true ~preds:[ Pred.Num ] ~nulls:Group_join (Args [ Same ]));
    many [ "max"; "min" ] (at 1) (make ~agg:true ~preds:[ Pred.Ord ] ~nulls:Group_join (Args [ Same ]));
    many [ "avg" ] (at 1) (make ~agg:true ~preds:[ Pred.Num ] ~ret:float ~nulls:nullable (Args [ Same ]));

    many [ "max"; "min"; "least"; "greatest" ] any (make ~preds:[ Pred.Ord ] (Varargs { head = []; tail = [ Same ] }));

    many [ "lower"; "upper"; "unhex"; "md5"; "sha"; "sha1"; "sha2"; "trim"; "to_base64" ] (at 1)
      (mono text [ text ]);
    many [ "hex" ] (at 1) (mono text [ int ]);
    many [ "length" ] (at 1) (mono int [ text ]);
    many [ "sha2" ] (at 2) (mono text [ text; int ]);
    many [ "substring" ] (at 2) (mono text [ text; int ]);
    many [ "substring" ] (at 3) (mono text [ text; int; int ]);
    many [ "substring_index" ] (at 3) (mono text [ text; text; int ]);
    many [ "replace" ] (at 3) (mono text [ text; text; text ]);
    many [ "concat"; "concat_ws"; "strftime" ] any (rep ~ret:text [ As text ]);
    many [ "uuid" ] (at 0) (mono text []);
    many [ "is_uuid" ] (at 1) (mono boolean [ text ]);
    many [ "similarity"; "word_similarity"; "strict_word_similarity" ] (at 2) (mono float [ text; text ]);

    many [ "random"; "rand"; "last_insert_id"; "uuid_short" ] (at 0) (mono int []);
    many [ "rand"; "last_insert_id" ] (at 1) (mono int [ int ]);
    many [ "floor" ] (at 1) (mono int [ float ]);
    many [ "pow"; "power" ] (at 2) (mono float [ float; int ]);
    many [ "julianday" ] any (rep ~ret:float [ As text ]);

    many [ "date"; "last_day" ] (at 1) (mono datetime [ datetime ]);
    many [ "time" ] (at 1) (mono text [ datetime ]);
    many [ "from_unixtime" ] (at 1) (mono datetime [ int ]);
    many [ "from_unixtime" ] (at 2) (mono text [ int; text ]);
    many [ "unix_timestamp" ] (at 0) (mono int []);
    many [ "unix_timestamp" ] (at 1) (mono int [ datetime ]);
    many [ "extract"; "dayofmonth"; "dayofweek"; "dayofyear"; "microsecond"; "second"; "minute";
           "hour"; "day"; "week"; "month"; "quarter"; "year" ] (at 1) (mono int [ datetime ]);
    many [ "current_date"; "current_timestamp"; "current_time"; "localtime"; "localtimestamp";
           "now"; "getdate" ] (at 0) (mono datetime []);
    many [ "timestampdiff"; "timestampadd" ] (at 3) (mono int [ datetime; datetime; datetime ]);
    many [ "date_add"; "date_sub" ] (at 2) (mono datetime [ datetime; datetime ]);
    many [ "date_format"; "time_format" ] (at 2) (mono text [ datetime; text ]);
    many [ "str_to_date" ] (at 2) (mono datetime [ text; text ]);
    many [ "makedate" ] (at 2) (mono datetime [ int; int ]);

    many [ "nullif" ] (at 2) (make ~nulls:nullable (Args [ Same; Same ]));
    many [ "ifnull" ] (at 2) (make ~nulls:Meet (Args [ Same; Same ]));
    many [ "coalesce" ] any (make ~nulls:Meet (Varargs { head = [ Same ]; tail = [ Same ] }));
    many [ "any_value" ] (at 1) (make (Args [ Same ]));

    many [ "json_array_append"; "json_set"; "json_array_insert"; "json_insert"; "json_replace" ] any
      (rep ~ret:json ~head:[ As json; As json_path; Free ] [ As json_path; Free ]);
    many [ "json_search" ] (at 3) (mono ~nulls:nullable json [ json; one_or_all; text ]);
    many [ "json_search" ] any
      (rep ~nulls:nullable ~ret:json ~head:[ As json; As one_or_all; As text; As text ] [ As json_path ]);
    many [ "json_remove" ] any (rep ~ret:json ~head:[ As json; As json_path ] [ As json_path ]);
    many [ "json_extract" ] any
      (rep ~nulls:nullable ~ret:json ~head:[ As json; As json_path ] [ As json_path ]);
    many [ "json_contains_path" ] any
      (rep ~nulls:nullable ~ret:boolean ~head:[ As json; As one_or_all; As json_path ] [ As json_path ]);

    many [ "json_array" ] any (rep ~nulls:strict ~ret:json [ Free ]);
    many [ "json_object" ] (at 0) (mono json []);
    many [ "json_object" ] any (rep ~nulls:strict ~ret:json ~head:[ As text; Free ] [ As text; Free ]);
    many [ "json_contains" ] (at 2) (mono ~nulls:nullable boolean [ json; json ]);
    many [ "json_contains" ] (at 3) (mono ~nulls:nullable boolean [ json; json; json_path ]);
    many [ "json_unquote"; "json_pretty"; "json_type" ] (at 1) (mono text [ json ]);
    many [ "json_quote" ] (at 1) (mono text [ text ]);
    many [ "json_depth"; "json_storage_size" ] (at 1) (mono int [ json ]);
    many [ "json_length" ] (at 1) (mono ~nulls:strict int [ json ]);
    many [ "json_length" ] (at 2) (mono ~nulls:strict int [ json; json_path ]);
    many [ "json_keys" ] (at 1) (mono json [ json ]);
    many [ "json_keys" ] (at 2) (mono json [ json; json_path ]);
    many [ "json_merge"; "json_merge_patch"; "json_merge_preserve" ] any (rep ~ret:json [ As json ]);
    many [ "json_valid" ] (at 1) (mono boolean [ text ]);

    many [ "is_null"; "is_not_null" ] (at 1) (make ~nulls:strict ~ret:boolean (Args [ Same ]));
    many [ "is_distinct" ] (at 2)
      (make ~compares:true ~nulls:strict ~ret:boolean (Args [ Same; Same ]));
    many [ "and"; "or"; "xor" ] (at 2) (make ~ret:boolean (Args [ As boolean; As boolean ]));
    many [ "eq"; "comparison" ] (at 2)
      (make ~compares:true ~proves_not_null:`All ~ret:boolean (Args [ Same; Same ]));
    many [ "any_cmp"; "all_cmp" ] (at 2) (make ~compares:true ~ret:boolean (Args [ Same; Same ]));
    many [ "not_distinct" ] (at 2)
      (make ~compares:true ~nulls:strict ~ret:boolean (Args [ Same; Same ]));
    many [ "not"; "excl" ] (at 1) (make ~proves_not_null:`All ~ret:boolean (Args [ As boolean ]));
    many [ "in"; "in_select"; "in_param" ] any
      (make ~compares:true ~proves_not_null:`First ~ret:boolean
         (Varargs { head = [ Same ]; tail = [ Same ] }));
    many [ "between" ] (at 3)
      (make ~compares:true ~proves_not_null:`First ~ret:boolean (Args [ Same; Same; Same ]));

    many [ "like" ] (at 2)
      (make ~compares:true ~proves_not_null:`All ~ret:boolean ~preds:[ Pred.Stringable ]
         (Args [ Same; Same ]));
    many [ "like_escape" ] (at 2)
      (make ~compares:true ~proves_not_null:`All ~ret:boolean (Args [ As boolean; As text ]));
    many [ "exists" ] (at 1) (make ~ret:boolean (Args [ Free ]));
    many [ "if" ] (at 3) (make (Args [ As boolean; Same; Same ]));
    many [ "interval" ] (at 1) (make ~ret:datetime (Args [ As int ]));
    many [ "default" ] (at 1) (make (Args [ Same ]));
    many [ "numeric_bin_op"; "mod" ] (at 2)
      (make ~proves_not_null:`All ~carries:false (Args [ Same; Same ]));
    many [ "num_div"; "text_dist" ] (at 2)
      (make ~proves_not_null:`All ~carries:false ~ret:float (Args [ Free; Free ]));
    many [ "div" ] (at 2) (make ~proves_not_null:`All ~carries:false ~ret:int (Args [ Free; Free ]));
    many [ "cast_unsigned" ] (at 1) (make ~ret:(b Base.UInt64) (Args [ Free ]));
    many [ "cast_signed" ] (at 1) (make ~ret:int (Args [ Free ]));
    many [ "group_concat" ] any
      (make ~agg:true ~ret:text ~nulls:Group_join (Varargs { head = []; tail = [ Free ] }));
    many [ "json_arrayagg" ] (at 1) (make ~agg:true ~ret:json ~nulls:Group_join (Args [ Free ]));

    many [ "first_value"; "last_value"; "nth_value"; "lag"; "lead" ] (at 1)
      (make ~agg:true ~nulls:Group_join (Args [ Same ]));
  ]

let excluded = [ "strftime", 1 ]

let user : (string * int, t) Hashtbl.t = Hashtbl.create 8

let declare name arity sg = Hashtbl.replace user (String.lowercase_ascii name, arity) sg

let find name arity =
  let name = String.lowercase_ascii name in
  if List.mem (name, arity) excluded then None
  else match Hashtbl.find_opt user (name, arity) with
  | Some sg -> Some sg
  | None ->
    let pick a = List.find_opt (fun (n, k, _) -> String.equal n name && k = a) table in
    match pick (Some arity) with
    | Some (_, _, sg) -> Some sg
    | None -> (match pick None with Some (_, _, sg) -> Some sg | None -> None)

let is_agg name arity = match find name arity with Some sg -> sg.agg | None -> false
