(* $Id$ *)

(** Socket service creation
  *
  * A socket service object is an encapsulation of a user-defined processor
  * for a list of sockets.
 *)

open Netplex_types

val create_socket_service :
      ?pre_start_hook:(controller -> container_id -> unit) -> 
      ?post_start_hook:(container -> unit) ->
      ?pre_finish_hook:(container -> unit) ->
      ?post_finish_hook:(controller -> container_id -> unit) ->
      processor ->
      socket_service_config ->
        socket_service
