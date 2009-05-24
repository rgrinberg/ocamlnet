(* $Id$ *)

(** Container environment
  *
  * Some helper functions to explore the environment from a container.
  * The following functions {b must} be called from a container context,
  * i.e. from a process or thread that acts as container.
 *)

open Netplex_types

exception Not_in_container_thread
  (** Raised when the caller's thread is not a container thread *)

val self_cont : unit -> container
  (** Returns the container running the code of the caller *)

val log : level -> string -> unit
  (** Writes a log message *)

val logf : level -> ('a, unit, string, unit) format4 -> 'a
  (** Writes a log message like [printf] *)

type timer
  (** A timer *)

val create_timer : (timer -> bool) -> float -> timer
  (** [create_timer f tmo]: Creates a timer with timeout value [tmo]:
      In [tmo] seconds [f] is called, and if this function returns [true],
      the timer remains active, and another round of timing is arranged.
      If the functions returns [false] or raises an exception, the timer
      is stopped.

      Timers are also stopped on container shutdown.

      Timers are attached to the container event system, and run only
      if this event system runs.
   *)

val cancel_timer : timer -> unit
  (** Cancels the timer: The callback function is not called any longer *)

val cancel_all_timers : unit -> unit
  (** Cancels all active timers *)

val timer_id : timer -> int
  (** Returns an ID, e.g. useful for debugging *)


val admin_connector : unit -> Rpc_client.mode2
  (** Determines the admin socket of the controller, and returns an RPC
      client connector suitable for connecting with the admin interface
      of the controller. For instance to initiate a system shutdown from
      the context of a container:

      {[
         let conn = Netplex_cenv.admin_connector() in
         let client = Netplex_ctrl_clnt.Admin.V2.create_client2 conn in
         Netplex_ctrl_clnt.Admin.V2.system_shutdown client ();
         Rpc_client.shut_down client
       ]}

      Note that the admin interface is going to evolve, and it is advisable
      to avoid admin calls whenever possible.
   *)

val system_shutdown : unit -> unit
  (** Initiates a system shutdown (like the [shutdown] method of the
      controller)
     *)

val system_restart : unit -> unit
  (** Initiates a system restart (like the [restart] method of the
      controller)
     *)


(**/**)

val register_par : parallelizer -> unit
val register_cont : container -> par_thread -> unit
val unregister_cont : container -> par_thread -> unit
