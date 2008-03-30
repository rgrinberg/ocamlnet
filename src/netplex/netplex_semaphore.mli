(* $Id$ *)

(** Netplex-wide semaphores *)

open Netplex_types

val plugin : plugin
  (** To enable semaphores, call the controller's [add_plugin] method 
      with this object as argument.
   *)

val increment : ?protected:bool -> string -> int64
  (** Increment the named semaphore by 1, and return the new value.
      If the semaphore does not exist yet, it is created with an initial
      value of 0, which is then incremented. If [protected], the semaphore
      is automatically decremented by some value when the container
      calling this function terminates. This value is [pi - d] where
      [pi] is the number of protected increments and [d] is the number
      of (successful) decrements requested by the container.

      This function can only be invoked in container contexts. Outside
      of such a context the exception {!Netplex_cenv.Not_in_container_thread}
      is raised.

      Semaphore names are global to the whole netplex system. By convention,
      these names are formed like ["service_name.local_name"], i.e. they
      are prefixed by the socket service to which they refer.
   *)

val decrement : string -> int64
  (** Decrement the named semaphore by 1, and return the new value.
      Semaphore values cannot become negative. If the value is already 0,
      it is not decremented.
   *)
