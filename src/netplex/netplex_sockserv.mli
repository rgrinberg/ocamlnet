(* $Id$ *)

(** Socket service creation
  *
  * A socket service object is an encapsulation of a user-defined processor
  * for a list of sockets.
 *)

open Netplex_types

val create_socket_service :
      processor ->
      socket_service_config ->
        socket_service
