(** The [migrate], [rollback] and [pending] commands of an application, as
    one Cmdliner group to mount under the application's own command line:

    {[
      module Runner = Sqlgg_migrations.Runner.Make (Db) (Migrations)
      module Migrate = Sqlgg_migrations_cmdliner.Command.Make (Db) (Runner) (App)

      let () = exit (Cmdliner.Cmd.eval' Migrate.command)
    ]}

    The application decides how to reach its database; sqlgg never opens a
    connection itself. *)

module Make
    (T : sig
       type -'a connection

       module IO : sig
         type 'a future
       end
     end)
    (_ : Sqlgg_migrations.Runner.S
           with type 'a connection := 'a T.connection
            and type 'a future := 'a T.IO.future)
    (_ : sig
       (** Whatever the application needs from its command line to connect,
           for example a database URL. *)
       type options

       val options : options Cmdliner.Term.t

       (** Runs the future against one writable connection, to completion. *)
       val with_connection :
         options ->
         ([ `RO | `WR ] T.connection -> 'a T.IO.future) ->
         'a
     end) : sig
  (** Exits with [0] on success, [1] when [pending] found migrations to apply
      and [2] when a migration or the state table failed. *)
  val command : Cmdliner.Cmd.Exit.code Cmdliner.Cmd.t
end
