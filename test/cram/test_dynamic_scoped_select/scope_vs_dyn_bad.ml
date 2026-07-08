(* MUST NOT compile: a fixed-scoped fragment ([Scope.t]) cannot be passed to a
   dynamic select, which expects [Dynamic_select.t]. *)
module Check (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Any.t = string) = struct

  module Sql = Output2.Sqlgg(T)
  open Sql

  type who = { id : int64; name : string option } [@@deriving sqlgg { mode = both }]

  let _bad db =
    Dyn_q_col.(select db (who_of_scope (module Fixed_q_col.Cols)) ~id:1L)
end
