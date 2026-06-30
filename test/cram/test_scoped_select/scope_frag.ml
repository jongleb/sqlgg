module Frag (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  module type WHO = sig
    type t
    val id   : (int64, t) Dynamic_select.t
    val name : (string option, t) Dynamic_select.t
  end
  type who = { id : int64; name : string option }

  let who_fr (type q) (module M : WHO with type t = q) : (who, q) Dynamic_select.t =
    let open Dynamic_select.Ops (M) in
    let+ id = M.id and+ name = M.name in { id; name }

  let _q1 db = Scope_q1.select db (who_fr (module Scope_q1.Cols)) ~id:1L
  let _q2 db =
    Scope_q2.select db (who_fr (module Scope_q2.Cols)) ~min_stock:10L (fun _ -> ())
end
