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

val any_file_client_connector : string -> Rpc_client.connector
  (** Interprets a file name as connector for a local RPC service. The
      file must either be a Unix Domain socket, or it must be a text
      file as written by Netplex with the details of the service
      endpoint.
   *)

val client_connector : extended_address -> Rpc_client.connector
  (** Returns the RPC client connector for this Netplex address *)
