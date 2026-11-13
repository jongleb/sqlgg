(** Applies and reverts compiled migrations, recording progress in the
    [sqlgg_migrations] state table.

    DDL auto-commits on the MySQL family, so every migration is bracketed by
    state changes: the row is written before the migration runs and updated
    after it. A migration whose outcome is unknown stays in a non-final state
    and blocks further runs until an operator resolves it by hand. *)

(** The database interaction that failed. *)
type operation =
  | Create_state_table
  | Read_state
  | Begin_apply of Migration_id.t
  | Apply of Migration_id.t
  | Finish_apply of Migration_id.t
  | Begin_revert of Migration_id.t
  | Revert of Migration_id.t
  | Finish_revert of Migration_id.t

(** A migration failed and the failure could not be written to the state
    table either, so the stored row still says [applying] or [reverting]. *)
type failure_recording = private {
  migration_id : Migration_id.t;
  operation : [ `Apply | `Revert ];
  failure : exn;
  recording_failure : [ `Raised of exn | `Unexpected_state ];
}

type error = [
  | `Planner of Planner.error
  | `State_row of Migration_state.error
  | `Database of operation * exn
  | `State_conflict of operation
    (** The state row was not in the state this run left it in: something
        else is changing [sqlgg_migrations] concurrently. *)
  | `Failure_recording of failure_recording
]

(** One change of a migration's stored state. *)
type state_change =
  | Insert_applying of { batch : int }  (** No row -> [applying]. *)
  | Mark_applied  (** [applying] -> [applied]. *)
  | Mark_reverting  (** [applied] -> [reverting]. *)
  | Mark_apply_failed  (** [applying] -> [apply_failed]. *)
  | Mark_revert_failed  (** [reverting] -> [revert_failed]. *)
  | Delete_reverting  (** [reverting] -> no row. *)

(** The compiled migrations, as emitted by
    [sqlgg migrations generate --lang ocaml]. *)
module type MIGRATIONS = sig
  type -'a connection
  type execute_response
  type 'a future

  module Migration : sig
    type t = private
      | Reversible of {
          id : Migration_id.t;
          apply : [ `WR ] connection -> execute_response future;
          revert : [ `WR ] connection -> execute_response future;
        }
      | Irreversible of {
          id : Migration_id.t;
          apply : [ `WR ] connection -> execute_response future;
        }
  end

  val migration_plan : Migration.t list
end

(** Storage of migration state. *)
module type STATE_STORE = sig
  type -'a connection
  type 'a future

  val create_state_table : [ `RO | `WR ] connection -> unit future

  val list_rows :
    [ `RO | `WR ] connection ->
    (Migration_state.row list, Migration_state.error) result future

  (** Changes the row of one migration only if it is in the state the
      change leaves, and reports whether exactly that row was changed. *)
  val change :
    [ `RO | `WR ] connection ->
    Migration_id.t ->
    state_change ->
    [ `Changed | `Unexpected_state ] future
end

module type S = sig
  type -'a connection
  type 'a future

  (** Ids of compiled migrations that are not applied yet, in order. *)
  val pending :
    [ `RO | `WR ] connection -> (string list, error) result future

  (** Applies every pending migration as one new batch and returns the ids
      it applied. Stops at the first failure; earlier migrations of the batch
      stay applied. *)
  val migrate :
    [ `RO | `WR ] connection -> (string list, error) result future

  (** Reverts the latest batch, newest migration first, and returns the ids
      it reverted. Nothing is reverted when the batch contains an
      irreversible migration. *)
  val rollback :
    [ `RO | `WR ] connection -> (string list, error) result future
end

(** Runner over a custom state store: the extension point for databases
    other than the MySQL family, and for tests. *)
module Make_with_store
    (T : sig
       type -'a connection
       type execute_response

       module IO : sig
         type 'a future

         val return : 'a -> 'a future
         val ( >>= ) : 'a future -> ('a -> 'b future) -> 'b future

         val try_bind :
           (unit -> 'a future) ->
           ('a -> 'b future) ->
           (exn -> 'b future) ->
           'b future
       end
     end)
    (_ : STATE_STORE
           with type 'a connection := 'a T.connection
            and type 'a future := 'a T.IO.future)
    (_ : MIGRATIONS
           with type 'a connection := 'a T.connection
            and type execute_response := T.execute_response
            and type 'a future := 'a T.IO.future) :
  S
    with type 'a connection := 'a T.connection
     and type 'a future := 'a T.IO.future

(** Runner for MySQL, MariaDB and TiDB over any sqlgg traits module. *)
module Make
    (T : Sqlgg_traits.M_control_io)
    (_ : MIGRATIONS
           with type 'a connection := 'a T.connection
            and type execute_response := T.execute_response
            and type 'a future := 'a T.IO.future) :
  S
    with type 'a connection := 'a T.connection
     and type 'a future := 'a T.IO.future

val error_to_string : error -> string
