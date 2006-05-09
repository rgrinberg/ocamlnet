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

class type dynamic_workload_config =
object
  method max_jobs_per_thread : int
    (** How many jobs every thread can execute concurrently until it is
      * considered as fully loaded. For configurations where the threads
      * can only handle one connection at a time this number must be 1.
     *)

  method min_free_job_capacity : int
    (** The manager starts as many threads as required to ensure that this
      * number of jobs can be executed.
     *)

  method max_free_job_capacity : int
    (** If more job capacity is available than this number, threads are
      * terminated.
     *)

  method max_threads : int
    (** The manager does not start more threads than this number *)

end


val create_dynamic_workload_manager :
      dynamic_workload_config -> workload_manager
