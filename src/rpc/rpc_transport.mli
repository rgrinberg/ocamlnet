(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Low-level RPC transporters *)

open Rpc
open Rpc_packer


type 't result =
    [ `Ok of 't
    | `Error of exn
    ]

type 't result_eof =
    [ 't result
    | `End_of_file
    ]


type sockaddr =
    [ `Implied
    | `Sockaddr of Unix.sockaddr
    ]


exception Error of string
  (** Passed back as [`Error]. Such errors are fatal. *)

(** Note errors on stream connections: These are normally not recoverable.
  * One should close the connection and open a new one.
 *)


class type rpc_multiplex_controller =
object
  method alive : bool
    (** If the controller is alive, the socket is not yet completely down. *)

  method event_system : Unixqueue.event_system
    (** Returns the event system *)

  method getsockname : sockaddr
    (** The address of this socket *)

  method getpeername : sockaddr
    (** The address of the peer's socket. Only available if the socket
      * is connected. (Fails otherwise.)
     *)

  method protocol : protocol
    (** The protocol encapsulation *)

  method peer_user_name : string option
    (** If the transport mechanism provides a way to authenticate the
      * peer, it can return the name here.
     *)

  method reading : bool
    (** True iff there is a reader *)

  method read_eof : bool
    (** Whether the EOF marker has been read *)

  method start_reading : 
    ?peek:( unit -> unit) ->
    ?before_record:( int -> sockaddr -> unit ) ->
    when_done:( (packed_value * sockaddr) result_eof -> unit) -> unit -> unit
    (** Start reading from the connection. When a whole message has been
      * received, the [when_done] callback is invoked. 
      *
      * This starts one-time read job only, i.e. it is not restarted
      * after [when_done] has been invoked.
      *
      * It is an error to start reading several times.
      *
      * [peek]: This function is called immediately before a data chunk is
      * read from the underlying data connection.
      *
      * [before_record]: If passed, this function is called back whenever
      * a record of data is started. The integer is the estimated size of the
      * message in bytes. It is guaranteed that the function is
      * invoked at least once before [when_done].
     *)

  method skip_message : unit -> unit
    (** Skips the current/next received message, i.e. the [when_done] callback
      * will not be invoked for it.
      *)

  method writing : bool
   (** True iff there is a writer *)

  method start_writing :
    when_done:(unit result -> unit) -> packed_value -> sockaddr -> unit
    (** Starts writing the message. Invoked [when_done] when it is written,
      * or an error condition is reached.
      *
      * This starts one-time write job only, i.e. it is not restarted
      * after [when_done] has been invoked.
      *
      * It is an error to start writing several times.
     *)

  method cancel_rd_polling : unit -> unit
    (** Cancels polling for the next input message. This method must not be
      * called from the [before_record] callback function. Polling can be
      * resumed by calling [start_reading] again.
     *)

  method abort_rw : unit -> unit
    (** Aborts the current reader and/or writer forever. Note that there is no
      * clean way of resuming reading and/or writing. The readers/writers
      * are not notified about cancellation.
     *)

  method start_shutting_down :
    when_done:(unit result -> unit) -> unit -> unit
    (** Start shutting down the connection. After going through the shutdown
      * procedure, the [when_done] callback is invoked reporting the success
      * or failure.
      *
      * The underlying file descriptor (if any) is not closed. A shutdown
      * is only a protocol handshake. After a shutdown,[read_eof]
      * is true. Call [inactivate] to close the descriptor.
     *)
 
  method cancel_shutting_down : unit -> unit
    (** Cancels the shutdown procedure. After that, the state of the 
      * connection is undefined. The [when_done] callback is invoked with
      * the [`Cancelled].
      *
      * It is no error if no shutdown is in progress.
     *)

  method inactivate : unit -> unit
    (** Inactivates the connection immediately, and releases any resources
      * the controller is responsible for (e.g. closes file descriptors). 
      * Note that this is more than
      * cancelling all pending operations and shutting the connection down.
      * However, the details of this method are implementation-defined.
      * Callbacks are not invoked.
     *)

end


val stream_rpc_multiplex_controller :
       ?close_inactive_descr:bool ->
       Unix.file_descr -> Unixqueue.event_system ->
         rpc_multiplex_controller
  (** The multiplex controller for stream encapsulation *)


val datagram_rpc_multiplex_controller :
       ?close_inactive_descr:bool ->
       Unix.file_descr -> Unixqueue.event_system ->
         rpc_multiplex_controller
  (** The multiplex controller for datagrams *)


class stream_rpc_multiplex_controller : 
         sockaddr -> sockaddr -> string option ->
         Uq_engines.multiplex_controller -> 
         Unixqueue.event_system ->
            rpc_multiplex_controller
  (** The class is exported for the SSL transporter *)
