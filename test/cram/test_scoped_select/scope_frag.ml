module Frag (T : Sqlgg_traits.M with
  type Types.Int.t = int64 and
  type Types.Text.t = string and
  type Types.Decimal.t = float and
  type Types.Any.t = string) = struct

  module Sql = Output.Sqlgg(T)
  open Sql

  module type WHO = sig
    type t
    val id   : (int64, t) Scope.t
    val name : (string option, t) Scope.t
  end
  type who = { id : int64; name : string option }

  let who_fr (type q) (module M : WHO with type t = q) : (who, q) Scope.t =
    let open Scope.Ops (M) in
    let+ id = M.id and+ name = M.name in { id; name }

  let _q1 db = Scope_q1_col.select db (who_fr (module Scope_q1_col)) ~id:1L
  let _q2 db =
    Scope_q2_col.select db (who_fr (module Scope_q2_col)) ~min_stock:10L (fun _ -> ())
end
