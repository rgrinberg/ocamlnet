(* $Id$ *)

(** Workload management
  *
  * Workload managers control when additional containers are started or
  * idle containers are stopped.
 *)

open Netplex_types

val create_constant_workload_manager : int -> workload_manager
  (** A constant number of threads is created (the int argument). If threads
    * crash, new threads are created until the specified number is again
    * reached.
   *)
