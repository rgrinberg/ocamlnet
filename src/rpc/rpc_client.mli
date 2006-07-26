(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** RPC clients *)

(** This module implements an RPC client, i.e. provides means to connect
 * to an RPC service and call remote procedures.
 * In general, this module works in an asynchronous way and is implemented
 * event-driven. All events are handled by an event queue of type
 * Unixqueue.t that must already exist and to which this module adds its
 * own event handlers and event resources. This means that this module
 * can co-exist with other services and share the same event queue with
 * them.
 *
 * You can push several procedure calls on the event queue at once.
 * The queue serves then as a pipeline; the calls are sent to the
 * server as long as the server accepts new calls. Replies are received
 * in any order, and the return values of the remote procedures are
 * delivered using a callback function.
 *
 * You can set timeouts and force automatic retransmission if you want
 * this; these features are enabled by default if the underlying transport
 * mechanism is UDP. Timeouts and other exceptions are delivered to the
 * callback functions, too.
 *
 * The whole mechanism is designed to allow maximum parallelism without
 * needing to use the multi-threading features of O'Caml. Especially,
 * the following parallelisms can be done:
 * - Call several procedures of the same server in parallel. Note that
 *   this does not necessarily mean that the procedures are run in
 *   parallel since the server is free to decide whether to work
 *   in a synchronous or asynchronous way.
 * - Call several procedures of different servers in parallel. To do so,
 *   simply add several RPC clients to the same event queue.
 * - Call a procedure and do something completely different in the
 *   background; this works well as long as the other task can be
 *   programmed using file descriptor events, too.
 *
 * However, there are still some restrictions concerning asynchronous
 * calls. Some of them will be removed in the future, but others are
 * difficult to tackle:
 * - Authentication methods requiring RPC calls or other network services are
 *   performed in an synchronous way, too.
 * - Name service lookups are synchronous, too.
 *)

open Rpc
open Xdr
open Rtypes

(* The following exceptions are delivered to the callback function: *)

exception Message_lost
  (** got EOF when some pending procedure calls were not replied or even sent *)

exception Message_timeout
  (** After all retransmissions, there was still no reply *)

exception Communication_error of exn
  (** an I/O error happened *)

exception Client_is_down
  (** The RPC call cannot be performed because the client has been shut down
   * in the meantime. You can get this exception if you begin a new call,
   * but the connection is closed now.
   *)

exception Keep_call
  (** This exception can be raised by the callback function that is invoked
   * when the server response arrives. It causes that the RPC call record
   * is kept in the housekeeping structure of the client. If the server
   * sends another response, the callback function will be invoked again.
   * I.e. one call can be replied several times (batching).
   *)


type t
  (** The type of RPC clients *)


type connector =
    Inet of (string * int)                    
      (** Hostname or IP address, port *)
  | Internet of (Unix.inet_addr * int)
      (** The address plus port *)
  | Unix of string
      (** Path to unix dom sock *)
  | Descriptor of Unix.file_descr
      (** Pass an already open socket descriptor. The descriptor will not
        * be closed when the client is done!
        *)
  | Dynamic_descriptor of (unit -> Unix.file_descr)
      (** The function is called to get the socket descriptor. 
        * Unlike [Descriptor], the descriptor will be closed when the
        * client is done
        *)
  | Portmapped of string
      (** The portmapper on this host is queried to get address information *)

val shutdown_connector : t -> Rpc_transport.rpc_multiplex_controller -> unit
  (** The default implementation to shut down the connector.
   * For [Descriptor] this is a no-op, 
   * for the other connector types the socket is closed.
   *)

val create :
      ?program_number:uint4 ->
      ?version_number:uint4 ->
      ?initial_xid:int ->
      ?shutdown:(t -> Rpc_transport.rpc_multiplex_controller -> unit) ->
      Unixqueue.event_system ->
      connector ->
      protocol ->
      Rpc_program.t ->
      t
  (** Opens a connection to the server specified by the [connector].
   * The server is assumed to implement an RPC program as specified by
   * the [Rpc_program.t] argument. (You can override the program and version
   * numbers stored in this argument by the optional parameters
   * [program_number] and [version_number].)
   *
   * All communication to the server is handled using the given queue
   * [Unixqueue.event_system].
   *
   * If the protocol is Tcp, the communication will be handled stream-
   * oriented. In this case, no timeout is detected and no retransmissions
   * are done.
   *
   * If the protocol is Udp, a datagram-oriented communication style is
   * used. This works only for Internet UDP sockets because these are
   * bidirectional (Unix domain sockets are unidirectional and do not
   * work). For Udp, there is a timeout of 15 seconds and a maximum
   * of 3 retransmissions (i.e. a total of 4 transmission trials).
   *
   * Unlike [create2], servers made with [create] always use blocking
   * [connect] for backwards compatibility.
   *
   * @param program_number Overrides the program number in [Rpc_program.t]
   *
   * @param version_number Overrides the version number in [Rpc_program.t]
   *
   * @param initial_xid The initial value for the session identifier.
   *
   * @param shutdown This function is called when the client is shut down
   *   to close the client socket. By default, [shutdown_connector] is
   *   called.
   *
   *)

class type socket_config =
object
  method non_blocking_connect : bool
   (** [non_blocking_connect]: Whether the remote service is connected
    *   in the background. In this case, [create2] immediately returns,
    *   and it is already possible to add procedure calls. However, these
    *   calls are deferred until the connection is established.
    *)
  method multiplexing :
    close_inactive_descr:bool ->
    protocol -> Unix.file_descr -> Unixqueue.event_system ->
      Rpc_transport.rpc_multiplex_controller Uq_engines.engine
end
  (** Configuration for [`Socket] (see below). *)


val default_socket_config : socket_config
  (** Default configuration with [non_blocking_connect] = true *)

class default_socket_config : socket_config
  (** Default configuration as class *)

val blocking_socket_config : socket_config
  (** Configuration with [non_blocking_connect] = false *)

class blocking_socket_config : socket_config
  (** blocking [connect] configuration as class *)

type mode2 =
    [ `Socket_endpoint of protocol * Unix.file_descr 
    | `Multiplexer_endpoint of Rpc_transport.rpc_multiplex_controller
    | `Socket of protocol * connector * socket_config
    ]
  (** Determines the type of the client for [create2]:
    *
    * - [`Socket_endpoint(proto,fd)]: Socket [fd] is a connected socket
    *   descriptor used for communication. [proto] determines the
    *   encapsulation; should be [Tcp] for stream sockets and [Udp] for
    *   datagram sockets. The descriptor will be closed when the client
    *   terminates.
    *
    * - [`Multiplexer_endpoint m]: [m] is an RPC multiplex controller.
    *
    * - [`Socket(proto, conn, config)]: Creates and connect a client
    *   socket according to [conn]. [proto] determines the
    *   encapsulation; should be [Tcp] for stream sockets and [Udp] for
    *   datagram sockets. [config] specifies configuration details.
   *)

val create2 :
      ?program_number:uint4 ->
      ?version_number:uint4 ->
      ?initial_xid:int ->
      ?shutdown:(t -> Rpc_transport.rpc_multiplex_controller -> unit) ->
      mode2 ->
      Rpc_program.t ->
      Unixqueue.event_system ->
      t
  (** Creates a new style client. See [create] and [mode2] for explanations. *)

val configure : t -> int -> float -> unit
  (** [configure client retransmissions timeout]:
   * sets the number of retransmissions and the timeout for the next calls.
   * (These values are defaults; the actual values are stored with each
   * call.)
   *
   * Values of [retransmissions > 0] are semantically only valid if the
   * called procedures are idempotent, i.e. invoking them several times
   * with the same values has the same effect as only one invocation.
   * Positive values for [retransmissions] should only be used for Udp-style
   * communication.
   *
   * The timeout value determines how long the client waits until the
   * next retransmission is done, or, if no more retransmissions are
   * permitted, a [Message_timeout] exception is delivered to the receiving
   * callback function. A [timeout] value of 0.0 means immediate timeout
   * and is usually senseless. A negative [timeout] value means 'no timeout'.
   * Positive [timeout] values are possible for both Udp and Tcp connections.
   * Timeout values are measured in seconds.
   *
   * There is a special application for the timeout value 0.0: If you
   * don't expect an answer from the server at all ("batch mode"), this
   * timeout value will cause that the message handler will get
   * a [Message_timeout] exception immediately. You should ignore this
   * timeout for batch mode. The positive effect from the timeout is that
   * the internal management routines will remove the remote call from
   * the list of pending calls such that this list will not become too long.
   *)

val set_exception_handler : t -> (exn -> unit) -> unit
  (** sets an exception handler (the default is a 'do nothing' exception
   * handler). Only exceptions resulting from invocations of a
   * callback function are forwarded to this handler.
   * Exceptions occuring in the handler itself are silently dropped.
   *)

val add_call :
    ?when_sent:(unit -> bool) ->
    t ->
    string ->
    xdr_value ->
    ((unit -> xdr_value) -> unit) ->
       unit
  (** [add_call client proc_name arg f]: add the call to the procedure [name]
    * with argument [arg] to the queue of unprocessed calls.
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
    *    add_call client name arg my_f
    * ]}
    *
    * If [f] does not catch the exception, the pluggable exception handler
    * of the client is called (see [set_exception_handler]). Exceptions are
    * either [Message_lost], [Message_timeout], or [Communication_error].
    *
    * The function [f] can raise the exception [Keep_call] to indicate
    * the special handling that a further reply of the call is expected
    * (batching).
    *
    * [when_sent]: This function is called when the call has been fully sent
    * to the server, but before the reply arrives. The function returns whether
    * to continue the call. By returning [false] the call is removed from the
    * internal bookkeeping. The function [f] is not called in this case.
   *)


val event_system : t -> Unixqueue.event_system
  (** Returns the unixqueue to which the client is attached *)

val program : t -> Rpc_program.t
  (** Returns the program the client represents *)

val get_socket_name : t -> Unix.sockaddr
val get_peer_name : t -> Unix.sockaddr
  (** Return the addresses of the client socket and the server socket, resp.
    * Note that these are only available when the client is already connected.
    * The function calls fail in this case. It is also possible that the
    * underlying transport mechanism does not know these data.
   *)

val get_protocol : t -> Rpc.protocol
  (** Get the protocol flavour *)


val sync_call :
    t ->            (* which client *)
    string ->       (* which procedure (name) *)
    xdr_value ->    (* the parameter of the procedure *)
    xdr_value       (* the result of the procedure *)
  (** Calls the procedure synchronously.
   * Note that this implies that the underlying unixqueue is started and that
   * all events are processed regardless of whether they have something to do
   * with this call or not.
   *)



val shut_down : t -> unit
  (** Shuts down the connection. Any unprocessed calls get the exception
   * [Message_lost].
   *)

class type auth_session =
object
  method next_credentials : t ->
                            (string * string * string * string)
         (** Returns (cred_flavour, cred_data, verifier_flavor, verifier_date)
	  *)
  method server_rejects : server_error -> unit
         (** Called if the server rejects the credentials or the verifier
	  * (Auth_xxx). This method may
	  * raise an exception to indicate that authentication has finally
	  * failed.
	  *)
  method server_accepts : string -> string -> unit
         (** Called if the server accepts the credentials. The two strings
	  * are the returned verifier_flavor and verifier_data.
	  * This method may raise [Rpc_server Rpc_invalid_resp] to indicate
	  * that the returned verifier is wrong.
	  *)
end
  (** An [auth_session] object is normally created for every client instance.
   * It contains the current state of authentication. The methods are only
   * interesting for implementors of authentication methods.
   *
   * This class type might be revised in the future to allow asynchronous
   * authentication (authentication often uses some network service).
   *)


class type auth_method =
object
  method name : string
         (** The name of this method, used for errors etc. *)
  method new_session : unit -> auth_session
         (** Begin a new session *)
end
  (** An [auth_method] object represents a method of authentication. Such an
   * object can be shared by several clients.
   *)


val auth_none : auth_method
  (** The authentication method that does not perform authentication. *)


val set_auth_methods : t -> auth_method list -> unit
  (** Set the authentication methods for this client. The passed methods
   * are tried in turn until a method is accepted by the server.
   * The default is [ auth_none ]
   *)

val verbose : bool -> unit
  (** set whether you want debug messages or not *)
