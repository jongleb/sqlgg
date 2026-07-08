(* MUST NOT compile: mixing selectors of two queries through the
   library-level brand-polymorphic [Dynamic_select.apply]. Regression test:
   without explicit annotations on [pure]/[apply] the phantom brands were
   inferred independently and this used to typecheck. *)
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
