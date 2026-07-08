module Frag (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  type who = { id : int64; name : string option } [@@deriving sqlgg { mode = both }]

  let _fixed db = Fixed_q_col.(select db (who_of_scope (module Cols)) ~id:1L)
  let _dyn db = Dyn_q_col.(select db (who_of_dyn (module Cols)) ~id:1L)
end
