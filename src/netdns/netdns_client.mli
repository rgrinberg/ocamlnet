(* $Id$ *)

(** DNS clients *)

(** Roughly patterned after Rpc_client *)

exception Message_lost
  (** got EOF when some pending procedure calls were not replied or even sent *)

exception Message_timeout
  (** After all retransmissions, there was still no reply *)

exception Communication_error of exn
  (** an I/O error happened *)

exception Client_is_down
  (** The DNS call cannot be performed because the client has been shut down
   * in the meantime. You can get this exception if you begin a new call,
   * but the connection is closed now.
   *)

exception Message_too_long
  (** This can be raised by [add_call] if the message would be too long
    * for the transport protocol.
   *)

exception Queue_overflow
  (** This can be raised by [add_call] if we run out of IDs *)

type t
  (** The type of DNS clients *)

type connector =
  [ `Internet of (Unix.inet_addr * int)
  | `Descriptor of Unix.file_descr
  ]

val shutdown_connector : t -> Netdns_transport.dns_multiplex_controller -> unit
  (** The default implementation to shut down the connector.
   * For [`Descriptor] this is a no-op, 
   * for the other connector types the socket is closed.
   *)

class type socket_config =
object
  method multiplexing :
    close_inactive_descr:bool ->
    Rpc.protocol -> Unix.file_descr -> Unixqueue.event_system ->
      Netdns_transport.dns_multiplex_controller Uq_engines.engine
end
  (** Configuration for [`Socket] (see below). *)

val default_socket_config : socket_config
  (** Default configuration *)

class default_socket_config : socket_config
  (** Default configuration as class *)

type mode =
    [ `Socket_endpoint of Rpc.protocol * Unix.file_descr 
    | `Multiplexer_endpoint of Netdns_transport.dns_multiplex_controller
    | `Socket of Rpc.protocol * connector * socket_config
    ]
  (** Determines the type of the client for [create]:
    *
    * - [`Socket_endpoint(proto,fd)]: Socket [fd] is a connected socket
    *   descriptor used for communication. [proto] determines the
    *   encapsulation; should be [Tcp] for stream sockets and [Udp] for
    *   datagram sockets. The descriptor will be closed when the client
    *   terminates.
    *
    * - [`Multiplexer_endpoint m]: [m] is an DNS multiplex controller.
    *
    * - [`Socket(proto, conn, config)]: Creates and connect a client
    *   socket according to [conn]. [proto] determines the
    *   encapsulation; should be [Tcp] for stream sockets and [Udp] for
    *   datagram sockets. [config] specifies configuration details.
   *)

val create :
      ?initial_xid:int ->
      ?shutdown:(t -> Netdns_transport.dns_multiplex_controller -> unit) ->
      mode ->
      Unixqueue.event_system ->
      t
  (** Creates a client. *)

val configure : t -> int -> float -> unit
  (** [configure client retransmissions timeout]:
   * sets the number of retransmissions and the timeout for the next calls.
   * (These values are defaults; the actual values are stored with each
   * call.)
   *)

val set_exception_handler : t -> (exn -> unit) -> unit
  (** sets an exception handler (the default is a 'do nothing' exception
   * handler). Only exceptions resulting from invocations of a
   * callback function are forwarded to this handler.
   * Exceptions occuring in the handler itself are silently dropped.
   *)

val add_call :
    t ->
    Netdns_message.msg ->
    ((unit -> Netdns_message.msg) -> unit) ->
       unit
  (** [add_call client msg f]: add the call with message [msg] to the
    * queue of unprocessed calls.
    *
    * The following components of the message header are automatically
    * set to reasonable values:
    * - [msg_id]: Set to the next session ID
    * - [msg_is_query]: Set to true
    * - [msg_is_trunc]: Set to false (too long messages are rejected)
    *
    * When the reply has arrived or an error situation is detected, the
    * function [f] is called back. The argument of [f] is another function
    * that will return the result or raise an exception:
    *
    * {[ let my_f get_result =
    *      try
    *        let result = get_result() in
    *        ...
    *      with
    *         exn -> ...
    *    in
    *    add_call client arg my_f
    * ]}
    *
    * If [f] does not catch the exception, the pluggable exception handler
    * of the client is called (see [set_exception_handler]). Exceptions are
    * either [Message_lost], [Message_timeout], or [Communication_error].
    *
    * Note: There is no guarantee that the response matches the query!
    *
    * [add_call] may raise exceptions, especially [Message_too_long],
    * [Queue_overflow], and [Netdns_message.Cannot_generate_message].
   *)

val event_system : t -> Unixqueue.event_system
  (** Returns the unixqueue to which the client is attached *)

val get_socket_name : t -> Unix.sockaddr
val get_peer_name : t -> Unix.sockaddr
  (** Return the addresses of the client socket and the server socket, resp.
    * Note that these are only available when the client is already connected.
    * The function calls fail in this case. It is also possible that the
    * underlying transport mechanism does not know these data.
   *)

val get_protocol : t -> Rpc.protocol
  (** Get the protocol flavour *)

val shut_down : t -> unit
  (** Shuts down the connection. Any unprocessed calls get the exception
   * [Message_lost].
   *)

val verbose : bool -> unit
  (** set whether you want debug messages or not *)
