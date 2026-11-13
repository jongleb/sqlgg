(** Pure migration planning: compares the compiled migrations with the rows
    of the state table and decides what may run. ['run] is whatever the
    interpreter needs to execute one direction of one migration. *)

type 'run migration

(** One unit of work. Only the planner can build a step, so an interpreter
    can only run what was validated against the stored state. For a step
    returned by {!migrate}, [run] applies the migration and [batch] is the
    new batch; for {!rollback}, [run] reverts it and [batch] is the batch
    being reverted. *)
type 'run step = private {
  migration_id : Migration_id.t;
  batch : int;
  run : 'run;
}

type error = [
  | `Duplicate_id of Migration_id.t
  | `Out_of_order of Migration_id.t * Migration_id.t
  | `Unknown_stored_id of Migration_id.t
  | `Missing_stored_id of Migration_id.t
  | `Non_final_state of Migration_id.t * Migration_state.t
  | `Non_monotonic_batch of int * int
  | `Missing_batch of int
  | `Reappeared_batch of int
  | `Irreversible of Migration_id.t
  | `Batch_overflow
]

val reversible :
  Migration_id.t ->
  apply:'run ->
  revert:'run ->
  'run migration

val irreversible :
  Migration_id.t ->
  apply:'run ->
  'run migration

(** Compiled migrations not yet recorded in the state table. *)
val pending :
  'run migration list ->
  Migration_state.row list ->
  (Migration_id.t list, error) result

(** Steps applying every pending migration as one new batch, in order. *)
val migrate :
  'run migration list ->
  Migration_state.row list ->
  ('run step list, error) result

(** Steps reverting the latest batch, newest migration first. Fails before
    producing any step when the batch contains an irreversible migration. *)
val rollback :
  'run migration list ->
  Migration_state.row list ->
  ('run step list, error) result

val error_to_string : error -> string
