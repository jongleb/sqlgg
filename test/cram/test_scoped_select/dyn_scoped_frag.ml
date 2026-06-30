module Frag (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  module type WHO = sig
    val id   : int64 Dynamic_select.t
    val name : string option Dynamic_select.t
  end
  type who = { id : int64; name : string option }

  let who_fr (module M : WHO) : who Dynamic_select.t =
    let open Dynamic_select in
    let+ id = M.id and+ name = M.name in { id; name }

  let _q1 db = Dscope_q1_col.select db (who_fr (module Dscope_q1_col)) ~id:1L
  let _q2 db =
    Dscope_q2_col.select db (who_fr (module Dscope_q2_col)) ~min_stock:10L (fun _ -> ())
end
