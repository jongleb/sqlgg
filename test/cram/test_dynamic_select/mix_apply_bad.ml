module Check (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  let _mixed =
    Dynamic_select.apply
      (Dynamic_select.apply
         (Dynamic_select.pure (fun a b -> (a, b)))
         Q1_col.id)
      Q2_col.category
end
