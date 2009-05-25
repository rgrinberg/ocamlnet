(* $Id$ *)

(** Container environment
  *
  * Some helper functions to explore the environment from a container.
  * The following functions {b must} be called from a container context,
  * i.e. from a process or thread that acts as container, otherwise
  * the exception [Not_in_container_thread] is raised.
 *)

open Netplex_types

exception Not_in_container_thread
  (** Raised when the caller's thread is not a container thread *)

(** {2 Logging} *)

val log : level -> string -> unit
  (** Writes a log message *)

val logf : level -> ('a, unit, string, unit) format4 -> 'a
  (** Writes a log message like [printf] *)

(** {2 Timer} *)

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


(** {2 Container variables} *)

(** Container variables exist once per container. Primary access is
    done via the [var] and [set_var] methods of the container class.
    The following functions are often more convenient, however.
 *)

exception Container_variable_not_found of string
  (** The variable does not exist *)

exception Container_variable_type_mismatch of string
  (** The (dynamically typed) variable has the wrong type *)


val int_var : string -> int
val string_var : string -> string
val float_var : string -> float
val bool_var : string -> bool
  (** Access a variable with simple type. May raise 
      [Container_variable_not_found] or [Container_variable_type_mismatch]
   *)

val set_int_var : string -> int -> unit
val set_string_var : string -> string -> unit
val set_float_var : string -> float -> unit
val set_bool_var : string -> bool -> unit
  (** Set a variable with simple type *)

val make_var_type :
      ('a -> exn) -> (exn -> 'a) -> ((string -> 'a) * (string -> 'a -> unit))
  (** Create get and set functions for any (monomorphic) type. For example,
      to create such function for a type [foo], do

      {[ 
          exception Foo of foo
          let (get, set) = 
            make_var_type
               (fun x -> Foo x) 
               (fun (Foo x) -> x)
      ]}

      (The latter function may also be written as
       {[ (function (Foo x) -> x | _ -> raise Not_found) ]} to
      prevent compiler warnings.)

      Read on for using functors to create [get] and [set].
   *)

module type TYPE = sig type t end
  (** Just a (monomorphic) type [t] *)

module type VAR_TYPE = sig
  type t 
  val get : string -> t
  val set : string -> t -> unit
end
  (** A (monomorphic) type [t] with two functions [get] and [set]
      accessing the container variables
   *)

module Make_var_type(T:TYPE) : VAR_TYPE with type t = T.t
  (** Creates [get] and [set] like [make_var_type]. Call it like

      {[
         module Foo_var = 
           Make_var_type(struct t = foo end)
      ]}

      and use [Foo_var.get] and [Foo_var.set] to access the container
      variables of type [foo].
   *)




(** {2 System control} *)

val system_shutdown : unit -> unit
  (** Initiates a system shutdown (like the [shutdown] method of the
      controller)
     *)

val system_restart : unit -> unit
  (** Initiates a system restart (like the [restart] method of the
      controller)
     *)


(** {2 Direct container and admin interface access} *)

val self_cont : unit -> container
  (** Returns the container running the code of the caller *)

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

val run_in_controller_context : controller -> (unit -> unit) -> unit
  (** [run_in_controller_context ctrl f]: Arranges that [f()] is executed
      in the context of the controller. {b This is only possible for
      multi-threading but not for multi-processing style!}

      For example, this allows it to start helper threads via
      {!Netplex_kit.add_helper_service} at any time.
   *)


(**/**)

val register_par : parallelizer -> unit
val register_cont : container -> par_thread -> unit
val unregister_cont : container -> par_thread -> unit
