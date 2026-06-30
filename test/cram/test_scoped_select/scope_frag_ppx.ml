module Frag (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  type who = { id : int64; name : string option } [@@deriving sqlgg]

  let _q1 db = Scope_q1.(select db (who_of_cols (module Cols)) ~id:1L)
  let _q2 db =
    Scope_q2.(select db (who_of_cols (module Cols)) ~min_stock:10L (fun _ -> ()))

  type t = { id : int64; name : string option } [@@deriving sqlgg]

  let _q3 db = Scope_q1.(select db (of_cols (module Cols)) ~id:2L)
end
