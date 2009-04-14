(* $Id$ *)

(** Controller *)

(** The controller is the main part of the Netplex system that starts and
  * stop the individual service containers.
 *)

open Netplex_types

val create_controller : parallelizer -> controller_config -> controller
  (** Create a controller with the default event system *)

val create_controller_for_esys : 
      Unixqueue.event_system -> parallelizer -> controller_config -> controller
  (** Create a controller for the passed event system *)


val extract_config : 
  logger_factory list -> config_file -> controller_config
