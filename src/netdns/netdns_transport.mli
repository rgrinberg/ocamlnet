(* $Id$ *)

(** Transporting DNS messages *)

(** We can reuse code of the [rpc] library. In particular, we choose to
  * use the [rpc_multiplex_controller] for DNS, too. The datagram 
  * implementation for RPC can be directly reused for DNS. The stream
  * implementation is slightly different (other method for record
  * marking).
 *)

(* When including this library in Ocamlnet, stream_rpc_multiplex_controller
 * should be changed so it support both the SunRPC and the DNS record
 * marking methods.
 *)

class type dns_multiplex_controller =
object
  (* This is the same as in rpc_multiplex_controller: *)
  method alive : bool
  method event_system : Unixqueue.event_system
  method getsockname : Rpc_transport.sockaddr
  method getpeername : Rpc_transport.sockaddr
  method peer_user_name : string option
  method protocol : Rpc.protocol
  method reading : bool
  method read_eof : bool
  method cancel_rd_polling : unit -> unit
  method abort_rw : unit -> unit
  (* method skip_message : unit -> unit *)
  method writing : bool
  method start_shutting_down :
    when_done:(unit Rpc_transport.result -> unit) -> unit -> unit
  method cancel_shutting_down : unit -> unit
  method set_timeout : notify:(unit -> unit) -> float -> unit
  method inactivate : unit -> unit

  (* Only change: packed_value => string *)

  method start_reading :
    when_done:( (string * Rpc_transport.sockaddr) Rpc_transport.result_eof -> unit) -> unit -> unit

  method start_writing :
    when_done:(unit Rpc_transport.result -> unit) -> string -> Rpc_transport.sockaddr -> unit

end


val datagram_dns_multiplex_controller :
       ?close_inactive_descr:bool ->
       Unix.file_descr -> Unixqueue.event_system ->
         dns_multiplex_controller

val stream_dns_multiplex_controller :
       ?close_inactive_descr:bool ->
       Unix.file_descr -> Unixqueue.event_system ->
         dns_multiplex_controller
