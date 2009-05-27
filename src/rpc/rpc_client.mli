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
   * I.e. one call can be replied several times (server-driven batching).
   *)

exception Unbound_exception of exn
  (** This exception can be raised by the callback function that is invoked
   * when the server response arrives. It simply causes that the inner
   * exception bypasses the exception handler, and falls through to the 
   * caller of [Unixqueue.run]. This is useful to jump out of the running RPC
   * routines.
   *)


type t
  (** The type of RPC clients *)


type connector =
    Inet of (string * int)                    
      (** Hostname or IP address, port *)
  | Internet of (Unix.inet_addr * int)
      (** The address plus port *)
  | Unix of string
      (** Path to unix dom sock. On Win32, this is a normal file containing
          the local inet4 port number (as emulation).
       *)
  | Pipe of string
      (** Path to named pipe (only Win32) *)
  | Descriptor of Unix.file_descr
      (** Pass an already open socket descriptor. The descriptor will not
        * be closed when the client is done! On Win32, the proxy descriptors
        * as returned by {!Netsys_win32.pipe_descr} are also accepted.
        *)
  | Dynamic_descriptor of (unit -> Unix.file_descr)
      (** The function is called to get the socket descriptor. 
        * Unlike [Descriptor], the descriptor will be closed when the
        * client is done (unless it is a proxy descriptor)
        *)
  | Portmapped of string
      (** The portmapper on this host is queried to get address information *)

val shutdown_connector : 
  t -> Rpc_transport.rpc_multiplex_controller -> (unit -> unit) -> unit
  (** The default implementation to shut down the connector. Actions are
    * triggered that will take the connector down at some time in the future.
    * At this time, the callback function is invoked.
    *
    * For [Descriptor] connector the socket is shut down but not closed.
    * For the other connector types the socket is also closed. 
    * Win32 named pipes are shut down.
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
      ?shutdown:(t -> Rpc_transport.rpc_multiplex_controller -> 
		   (unit -> unit) -> unit) ->
      mode2 ->
      Rpc_program.t ->
      Unixqueue.event_system ->
      t
  (** New style clients:
   * Opens a connection to the server specified by [mode2].
   * The server is assumed to implement an RPC program as specified by
   * the [Rpc_program.t] argument. (You can override the program and version
   * numbers stored in this argument by the optional parameters
   * [program_number] and [version_number]. If you need to call several
   * programs/versions with the same client, use [unbound_create] instead.)
   *
   * All communication to the server is handled using the given queue
   * [Unixqueue.event_system].
   *
   * If the protocol (passed along with [mode2]) is Tcp, the communication 
   * will be handled stream-oriented. In this case, no timeout is detected
   * and no retransmissions are done.
   *
   * If the protocol is Udp, a datagram-oriented communication style is
   * used. This works only for Internet UDP sockets because these are
   * bidirectional (Unix domain sockets are unidirectional and do not
   * work). For Udp, there is a timeout of 15 seconds and a maximum
   * of 3 retransmissions (i.e. a total of 4 transmission trials).
   *
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

val unbound_create :
      ?initial_xid:int ->
      ?shutdown:(t -> Rpc_transport.rpc_multiplex_controller -> 
		   (unit -> unit) -> unit) ->
      mode2 ->
      Unixqueue.event_system ->
      t
  (** Creates an unbound client. This is like [create2], but the client is
      not restricted to a particular RPC program.

      One can convert an unbound client into a bound client by calling
      [bind], see below. It is possible to bind several times, so several
      programs can be called with the same client (provided the server is
      also capable of dealing with several programs).

      This function does not support [Portmapped] connectors.
   *)

val bind : t -> Rpc_program.t -> unit
  (** Binds this program additionally *)

val use : t -> Rpc_program.t -> unit
  (** If there are no bound programs, this is a no-op. Otherwise it is 
      checked whether the passed program is bound. If not, an exception
      is raised.

      Programs are compared by comparing {!Rpc_program.id}. The program
      must be the same value, but it is also allowed to 
      {!Rpc_program.update} it in the meantime, i.e. to change program
      and version numbers.
   *)

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
   * (see next paragraph). A negative [timeout] value means 'no timeout'.
   * Positive [timeout] values are possible for both Udp and Tcp connections.
   * Timeout values are measured in seconds.
   *
   * There is a special application for the timeout value 0.0: If you
   * don't expect an answer from the server at all ("batch mode"), this
   * timeout value will cause that the message handler will get
   * a [Message_timeout] exception immediately. You should ignore this
   * exception for batch mode. The positive effect from the timeout is that
   * the internal management routines will remove the remote call from
   * the list of pending calls such that this list will not become too long.
   * (You can get a similar effect by calling [set_batch_call], however.)
   *
   * Note that the meaning of timeouts for TCP connections is unclear.
   * The TCP stream may be in an undefined state. Because of this, the
   * client does not make any attempt to clean the state up for TCP.
   * The user is advised to shut down the client, and reconnect.
   *
   * There is another subtle difference between UDP and TCP. For UDP,
   * the timer is started when the packet is sent. For TCP, however,
   * the timer is already started when the RPC call is added to the
   * queue, i.e. much earlier. This means that the time for connecting
   * to the remote service is also bound by the timeout. The rationale
   * is that TCP timeouts are usually set to catch total service failures
   * rather than packet losses, and this behaviour is best for this purpose.
   *)

val configure_next_call : t -> int -> float -> unit
  (** Same as [configure], but it only affects the next call *)

val set_dgram_destination : t -> Unix.sockaddr option -> unit
  (** [set_dgram_destination client addr_opt]: This function is required
    * for using the client in conjunction with unconnected UDP sockets.
    * For connected sockets, the destination of datagrams is implicitly
    * given. For unconnected sockets, one has to set the destination
    * explicitly. Do so by calling [set_dgram_destination] with
    * [Some addr] as [addr_opt] argument before calling.
    * Passing [None] as [addr_opt] removes the explicit destination again.
    * Note that unconnected sockets differ from connected sockets also in
    * the relaxation that they can receive messages from any IP address,
    * and not only the one they are connected to.
    *
    * The current destination is used for all following calls. It is
    * not automatically reset to [None] after the next call.
   *)

val set_batch_call : t -> unit
  (** The next call will be a batch call. The client does not wait for the
      response of a batch call. Instead, the client immediately fakes the
      response of a "void" return value.

      It is required that the next call has a "void" return type. Otherwise,
      the client raises an exception, and ignores the call.

      This setting only affects the next call.
   *)

val set_exception_handler : t -> (exn -> unit) -> unit
  (** sets an exception handler (the default prints the exception to stderr).
   * Only exceptions resulting from invocations of a
   * callback function are forwarded to this handler (unless wrapped
   * by [Unbound_exception]).
   *
   * Exceptions occuring in the handler itself are not caught, and will
   * fall through.
   *)

val event_system : t -> Unixqueue.event_system
  (** Returns the unixqueue to which the client is attached *)

val programs : t -> Rpc_program.t list
  (** Returns the list of all bound programs *)

val get_socket_name : t -> Unix.sockaddr
val get_peer_name : t -> Unix.sockaddr
  (** Return the addresses of the client socket and the server socket, resp.
    * Note that these are only available when the client is already connected.
    * The function calls fail otherwise. It is also possible that the
    * underlying transport mechanism does not know these data.
   *)

val get_sender_of_last_response : t -> Unix.sockaddr
  (** Return the address of the sender of the last received response. *)

val get_protocol : t -> Rpc.protocol
  (** Get the protocol flavour *)

val unbound_sync_call : 
      t -> Rpc_program.t -> string -> xdr_value -> xdr_value
  (** [unbound_sync_call client pgm proc arg]: Invoke the remote procedure
      [proc] of the program [pgm] via [client]. The input arguments are
      [arg]. The result arguments are returned (or an error is raised)
   *)

val unbound_async_call :
      t -> Rpc_program.t -> string -> xdr_value -> 
      ((unit -> xdr_value) -> unit) -> unit
  (** [unbound_ssync_call client pgm proc arg emit]: Invoke the remote 
      procedure
      [proc] of the program [pgm] via [client]. The input arguments are
      [arg]. When the result [r] is available, the client will call
      [emit (fun () -> r)] back. When an exception [e] is available, the
      client will call [emit (fun () -> raise e)] back.
   *)

class unbound_async_call :
      t -> Rpc_program.t -> string -> xdr_value -> [xdr_value] Uq_engines.engine
  (** Same as [unbound_async_call], but with an engine API. The engine
      is initially in state [`Working 0]. When the call is finished, the
      engine transitions to [`Done r] where [r] is the response value.
      If an error happens, it transitions to [`Error e] where [e] is the
      exception.

      One can [abort] the engine, but one caveat: This does not stop
      the transmission of the current message (the underlying RPC client
      doing this is not aborted). Aborting can only prevent that a
      message is sent before it is sent, and it can remove the call from the
      housekeeping data structures before the response arrives. Of course,
      one can shut the client down to achieve immediate stop of data
      transmission.
   *)

val shut_down : t -> unit
  (** Shuts down the connection. Any unprocessed calls get the exception
   * [Message_lost]. It is no error to shut down a client that is already
   * down - nothing happens in this case.
   *
   * Shutdowns can be complex operations. For this reason, this function
   * implements some magic that is usually the right thing, but may also
   * be wrong:
   *  - If called outside the event loop, it is assumed that a synchronous
   *    shutdown is desired, and the event loop is started to complete the
   *    shutdown immediately. This is right
   *    when the only task connected with the event loop is the shutdown,
   *    which is then done, and this function returns finally to the caller. If
   *    there are other tasks on the event loop, these tasks are also run,
   *    however, which may lead to side effects and infinite delay. This can
  *     be wrong.
   *  - If called from within the event loop, the shutdown is only triggered
   *    but not immediately done. When the caller returns to the event loop
   *    the shutdown will be performed. This case is problematic when you
   *    pass the file descriptor explicitly with [Descriptor] to the client.
   *    You don't know when the client is finally down, and the descriptor
   *    can be closed.
   *
   * The following functions allow more fine grained control of the shutdown.
   *)

val sync_shutdown : t -> unit
  (** Enforces a synchronous shutdown of the connection. This is only
    * possible if called from outside the event loop. This function fails
    * if called from within the event loop.
    *
    * You can be sure that the shutdown is completely done when this
    * function returns normally.
   *)

val trigger_shutdown : t -> (unit -> unit) -> unit
  (** Triggers the shutdown, and calls the passed function back when it is
    * done.
    *
    * The function is not only called when the client has to be taken
    * down, but also if the client is already down.
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


(** This module type is what the generated "clnt" module assumes about the
    client interface
 *)
module type USE_CLIENT = sig
  type t
    (** The client type *)

  val use : t -> Rpc_program.t -> unit
    (** Announcement that this program will be used. The client may
        reject this by raising an exception.
     *)

  val unbound_sync_call : 
        t -> Rpc_program.t -> string -> xdr_value -> xdr_value
    (** [unbound_sync_call client pgm proc arg]: Invoke the remote procedure
        [proc] of the program [pgm] via [client]. The input arguments are
        [arg]. The result arguments are returned (or an error is raised)
     *)

  val unbound_async_call :
        t -> Rpc_program.t -> string -> xdr_value -> 
        ((unit -> xdr_value) -> unit) -> unit
    (** [unbound_ssync_call client pgm proc arg emit]: Invoke the remote 
        procedure
        [proc] of the program [pgm] via [client]. The input arguments are
        [arg]. When the result [r] is available, the client will call
        [emit (fun () -> r)] back. When an exception [e] is available, the
        client will call [emit (fun () -> raise e)] back.
     *)

end


(** {2 Deprecated Interfaces} *)


val create :
      ?program_number:uint4 ->
      ?version_number:uint4 ->
      ?initial_xid:int ->
      ?shutdown:(t -> Rpc_transport.rpc_multiplex_controller -> 
		   (unit->unit) -> unit) ->
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
   * @deprecated This function should not be used any more in new programs.
   *    Use [create2] or [unbound_create].
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

val program : t -> Rpc_program.t
  (** Returns the program the client represents.

      @deprecated This is the same as [List.hd (Rpc_client.programs client)]
   *)

val add_call :
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
    * @deprecated [add_call] is restricted to the case that there is only
    *   one bound program. It will fail in other cases. Use 
    *   [unbound_async_call] instead. Note also that there is no longer
    *   the optional [when_sent] argument. Use [set_batch_call] instead
   *)

val sync_call :
    t ->            (* which client *)
    string ->       (* which procedure (name) *)
    xdr_value ->    (* the parameter of the procedure *)
    xdr_value       (* the result of the procedure *)
  (** Calls the procedure synchronously.
   * Note that this implies that the underlying unixqueue is started and that
   * all events are processed regardless of whether they have something to do
   * with this call or not.
   *
   * @deprecated [sync_call] is restricted to the case that there is only
   *   one bound program. It will fail in other cases. Use 
   *   [unbound_sync_call] instead.
   *)

