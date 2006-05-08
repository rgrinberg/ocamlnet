(* $Id$ *)

(** Containers
  *
  * The container is the management object for the concurrently running
  * service processor.
 *)

open Netplex_types

val create_container : socket_service -> container
