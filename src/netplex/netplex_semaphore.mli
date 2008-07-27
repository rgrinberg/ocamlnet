(* $Id$ *)

(** Netplex-wide semaphores *)

open Netplex_types

(** Semaphores are counters with atomic increment and decrement operations.
    They are very useful for counting the number of uses of a shared
    resource, and allow the identification of the first use (so the resource
    must be made available at all), and the last use (the resource can be
    released).

    This implementation works in both multi-processing and
    multi-threading netplex environments. It is, however, not very
    fast, because the counters live in the controller, and the
    increment/decrement operations are realized by RPC's. It is good
    enough when these operations are only infrequently called, e.g. in
    the post-start and pre-finish processor callbacks.
  *)

val plugin : plugin
  (** To enable semaphores, call the controller's [add_plugin] method 
      with this object as argument. This can e.g. be done in the
      [post_add_hook] of the processor.
   *)


(** The folloing functions can {b only} be invoked in container
    contexts. Outside of such a context the exception
    {!Netplex_cenv.Not_in_container_thread} is raised. 
 *)

val increment : ?protected:bool -> string -> int64
  (** Increment the named semaphore by 1, and return the new value.
      If the semaphore does not exist yet, it is created with an initial
      value of 0, which is then incremented. If [protected], the semaphore
      is automatically decremented by some value when the container
      calling this function terminates. This value is [pi - d] where
      [pi] is the number of protected increments and [d] is the number
      of (successful) decrements requested by the container.

      Semaphore names are global to the whole netplex system. By convention,
      these names are formed like ["service_name.local_name"], i.e. they
      are prefixed by the socket service to which they refer.
   *)

val decrement : string -> int64
  (** Decrement the named semaphore by 1, and return the new value.
      Semaphore values cannot become negative. If the value is already 0,
      it is not decremented.
   *)

(** Example (code fragment):

    Override the processor callbacks as follows to count the number of
    containers for the service:

  {[ 
    method post_add_hook sockserv ctrl =
      ctrl # add_plugin Netplex_semaphore.plugin

    method post_start_hook container =
      let sem_name = container#socket_service#name ^ ".counter" in
      let n =
        Netplex_semaphore.increment ~protected:true sem_name in
      if n=1 then
        prerr_endline "First container"

    method pre_finish_hook container =
      let sem_name = container#socket_service#name ^ ".counter" in
      let n =
        Netplex_semaphore.decrement sem_name in
      if n=0 then
        prerr_endline "Last container"
   ]}
 *)

