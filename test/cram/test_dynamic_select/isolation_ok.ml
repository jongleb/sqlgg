(* Positive control: fragments composed inside their own query module compile. *)
module Check (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  let _q1 db =
    Q1_col.(select db (let+ id = id and+ name = name in (id, name)) ~id:1L (fun _ -> ()))

  let _q2 db =
    Q2_col.(select db category ~id:1L (fun _ -> ()))
end
