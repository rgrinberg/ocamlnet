(* $Id$ *)

(** Container environment
  *
  * Some helper functions to explore the environment from a container
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

(**/**)

val register_par : parallelizer -> unit
val register_cont : container -> par_thread -> unit
val unregister_cont : container -> par_thread -> unit
