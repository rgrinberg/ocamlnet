(* $Id$ *)

(** Containers
  *
  * The container is the management object for the concurrently running
  * service processor.
 *)

open Netplex_types

val create_container : 
      socket_service -> container
  (** The container for normal services *)

val create_admin_container : 
      Unixqueue.unix_event_system -> socket_service -> container
  (** {b Internally used.} The container for the special admin service *)
