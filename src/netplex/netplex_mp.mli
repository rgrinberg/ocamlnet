(* $Id$ *)

(** Multi-processing provider *)

class mp : ?keep_fd_open:bool -> unit -> Netplex_types.parallelizer
  (** Uses [Unix.fork] to create new threads.

      After forking the child process is initialized by 
      calling [Netsys_posix.run_post_fork_handlers].

      By default, all file descriptors are closed that are not explicitly
      shared with the parent process. This can be wrong for some kind
      of applications. By setting [keep_fd_open] another behavior can
      be demanded: The descriptors are kept open except those that need
      to be closed.
   *)

val mp : ?keep_fd_open:bool -> unit -> Netplex_types.parallelizer
  (** Uses [Unix.fork] to create new threads. See {!class: Netplex_mp.mp}
      for details.
   *)
