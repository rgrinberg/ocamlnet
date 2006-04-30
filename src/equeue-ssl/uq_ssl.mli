(* $Id$ *)

(** Asynchronous SSL connections *)

exception Ssl_error of Ssl.ssl_error
  (** Used in [when_done] callbacks to indicate an SSL-specific error code *)


(** The [ssl_multiplex_controller] is an extended multiplex controller
  * which can also control SSL handshakes.
  *
  * Important note: SSL/TLS does not support half-open connections. When
  * one party closes the connection, the other party must immediately 
  * close the connection, too, throwing away any pending writes. See
  * RFC 2246.
 *)
class type ssl_multiplex_controller =
object
  inherit Uq_engines.multiplex_controller

  method ssl_socket : Ssl.socket

  method ssl_socket_state : [ `Unset | `Client | `Server | `Unclean | `Clean ]
    (** - [`Unset]: A fresh socket
      * - [`Client]: A socket playing the SSL client role
      * - [`Server]: A socket playing the SSL server role
      * - [`Unclean]: The socket state is unclean. The socket is no longer usable.
      * - [`Clean]: The SSL connection has been cleanly shut down.
      *)


  method ssl_connecting : bool
    (** Whether the initial SSL handshake is in progress that makes this
      * socket a client.
     *)

  method ssl_accepting : bool
    (** Whether the initial SSL handshake is in progress that makes this
      * socket a server.
     *)

  method start_ssl_connecting : 
    when_done:(exn option -> unit) -> unit -> unit
    (** Makes the socket an SSL client socket by initiating the handshake.
      * The [when_done] callback is invoked when the handshake is done.
      *
      * One can neither read nor write before that.
     *)

    (* CHECK: When SSL_connect returns 0, the connection is not established,
     * but the O'Caml function does not indicate this.
     *)

  method start_ssl_accepting :
    when_done:(exn option -> unit) -> unit -> unit
    (** Makes the socket an SSL server socket by initiating the handshake.
      * The [when_done] callback is invoked when the handshake is done.
      *
      * One can neither read nor write before that.
     *)

    (* CHECK: When SSL_accept returns 0, the connection is not established,
     * but the O'Caml function does not indicate this.
     *)

    (* Notes:
     * CHECK:
     * - It is not possible to shut down the SSL connection only for writing.
     * - 
     *)
end




val create_ssl_multiplex_controller : 
       ?close_inactive_descr:bool ->
       Unix.file_descr -> Ssl.context -> Unixqueue.event_system ->
         ssl_multiplex_controller
  (** Creates a multiplex controller for an SSL socket. The descriptor must
    * be a connected socket descriptor.
    *
    * [close_inactive_descr]: Whether to close the file descriptor by
    * [inactivate].
   *)

val ssl_connect_engine : 
       ssl_multiplex_controller -> unit Uq_engines.engine
  (** This engine performs the client handshake. *)

val ssl_accept_engine : 
       ssl_multiplex_controller -> unit Uq_engines.engine
  (** This engine performs the server handshake. *)
