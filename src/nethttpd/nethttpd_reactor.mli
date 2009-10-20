(* $Id$
 *
 *)

(*
 * Copyright 2005 Baretta s.r.l. and Gerd Stolpmann
 *
 * This file is part of Nethttpd.
 *
 * Nethttpd is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Nethttpd is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with Nethttpd; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** {1 The reactive encapsulation of the HTTP daemon}
  *
  * This is a user-friendlier encapsulation of the HTTP daemon. It uses a
  * "pull model": One pulls HTTP requests from the "reactor" one after the
  * other. Request/response pairs have a common environment that represents
  * the input and output facilities. Input and output are realised by
  * [Netchannels], and the implementation details are completely hidden.
  *
  * This encapsulation can be easily used in a multi-threaded environment
  * when for every arriving HTTP connection a separate thread is used.
 *)

open Nethttp
open Nethttpd_types

class type http_processor_config =
object
  (* This config is also used for engines *)

  inherit Nethttpd_kernel.http_protocol_config

  method config_timeout_next_request : float
    (** Timeout in seconds to wait for the next request after the previous one
      * has been completely received. (-1) means no timeout.
     *)

  method config_timeout : float
    (** General timeout until new data arrives.  (-1) means no timeout. *)

  method config_cgi : Netcgi.config
    (** The CGI configuration to use in the Netcgi environment *)

  method config_error_response : int -> string
    (** Get HTML error text for the HTTP status code. Must return a generic
      * text for unknown codes.
     *)

  method config_log_error : 
    Unix.sockaddr option -> Unix.sockaddr option -> http_method option -> 
    http_header option -> string -> unit
    (** The function logs an error message. Arguments are:
      * - Our socket address
      * - Peer's socket address
      * - Request method
      * - Request header
      * - Message
     *)

  method config_log_access :
    Unix.sockaddr -> Unix.sockaddr -> http_method ->
    http_header -> int64 -> (string * string) list -> bool -> 
    int -> http_header -> int64 -> unit
    (** [config_log_access sockaddr peeraddr reqmeth reqhdr props reqrej
        status resphdr size]: This method is called after a request has been
        processed:
        - [sockaddr]: our socket address
        - [peeraddr]: peer's socket address
        - [reqmeth]: the request method (incl. URI)
        - [reqhdr]: the request header
        - [reqsize]: the size of the request body in bytes
        - [props]: the properties (see below)
        - [reqrej]: whether the request body has been rejected
        - [status]: the 3-digit numeric reply status
        - [resphdr]: the response header
        - [respsize]: the size of the response body in bytes

        The [props] are the [cgi_properties] from the environment. As there
        is no automatic way of recording the last, finally used version of
        this list, it is required that users call [log_props] of the extended
        environment whenever the properties are updated. This is done by
        all [Nethttpd] modules.
     *)
end


class type http_reactor_config =
object
  inherit http_processor_config
  method config_reactor_synch : [ `Connection | `Close | `Flush | `Write ]
    (** Specifies when to synchronize output, i.e. force that all channel data are
      * actually transmitted to the client:
      * - [`Connection] means only at the end of the connection. This means that the
      *   channels of all pending requests may be buffered - needs a huge amount of
      *   memory
      * - [`Close] means only when closing the output channel (after every response). 
      *   This means that the whole response may be buffered - needs a lot of memory.
      * - [`Flush] means only when the [flush] method is called. This is a good idea
      *   when one can control that.
      * - [`Write] means every time the internal output buffer overflows. This is the
      *   recommended setting in general.
     *)
end


class type internal_environment =
object
  inherit extended_environment

  method unlock : unit -> unit
  method req_method : http_method
  method response : Nethttpd_kernel.http_response
  method log_access : unit -> unit
end
  (** For private use only *)


class http_environment : #http_processor_config ->
                         string -> string -> protocol -> http_header ->
                         Unix.sockaddr -> Unix.sockaddr ->
                         Netchannels.in_obj_channel -> int64 ref ->
                         Netchannels.out_obj_channel -> output_state ref -> 
                         Nethttpd_kernel.http_response -> (unit -> unit) ->
                         bool ref -> int64 ->
                           internal_environment
  (** For private use only *)


class type http_reactive_request =
object
  method environment : extended_environment
    (** The Netcgi environment representing the request header, the response header, and
      * the channels to receive the request body and to send the response body.
      * The channels are locked until either [accept_body] or [reject_body] have been
      * called - using the channels before raises exceptions.
      *
      * This environment is not fully CGI-compatible. In particular, the following
      * differences exist:
      * - There is no [cgi_path_info] and no [cgi_path_translated].
      * - The user is always unauthenticated.
      * - The [Status] response header works as in CGI. The [Location] header, however,
      *   must be a full URL when set (only browser redirects)
      * - When the request body is transmitted by chunked encoding, the header
      *   [Content-Length] is not set. In CGI this is interpreted as missing body.
      *   It is unlikely that clients send requests with chunked encoding, as this
      *   may cause interoperability problems anyway.
      *   
     *)

  method accept_body : unit -> unit
    (** Call this method to unlock the body channels. In terms of HTTP, this sends the
      * "100 Continue" response when necessary. One can reply with a positive or
      * negative message.
     *)

  method reject_body : unit -> unit
    (** Call this method to unlock the body channels. In terms of HTTP, this prevents
      * sending the "100 Continue" response. Any arriving request body is silently
      * discarded. One should immediately reply with an error mesage.
     *)

  method finish_request : unit -> unit
    (** Reads the rest of the body (if any), and discards that data *)

  method finish : unit -> unit
    (** This method should be called after the request has been fully processed.
      * It takes care that the HTTP protocol is fulfilled, and the next request
      * can be properly detected and parsed. If the request body has not been
      * fully read, this is now done, and its data are dropped. If the response
      * is incomplete, it is completed. If the error is not recoverable, a "Server
      * Error" is generated.
     *)

end



(** The [http_reactor] allows one to pull the next request from a connected
  * client, and to deliver the response to the protocol engine.
 *)
class http_reactor : #http_reactor_config -> Unix.file_descr ->
object
  method next_request : unit -> http_reactive_request option
    (** Tries to read the next request. When the header of the request is successfully
      * read, the method returns the request object (see above). It is connected
      * with the socket and can read the request body.
      *
      * After receiving the request, one must either call [accept_body] when the
      * request is acceptable and one want to reply afer evaluating the body, or
      * invoke [reject_body] when the request can be denied without looking at the
      * body. One must also call [accept_body] when there is not any body (it
      * is a no-op then). The HTTP protocol explicitly forbids to perform the request
      * when [reject_body] has been invoked ("[The origin server] MUST NOT
      * perform the requested method if it returns a final status code").
      *
      * The response must be written to the Netcgi environment. Depending on 
      * [config_reactor_synch] the response is immediately transmitted to the
      * client or at some specified time in the future (untransmitted data is buffered
      * in this case).
      *
      * While transmitting output, the reactor is able to read the next request
      * in the background when the limits for the pipeline size allows that.
      *
      * While receiving input, the reactor is able to write untransmitted response
      * data in the background.
      *
      * It is {b an error} to call [next_request] again before the previous request
      * is completely processed (you can ensure this by calling [finish]).
      * In this case the HTTP connection is immediately shut down.
      *
      * The method [next_request] returns [None] when all requests of the
      * connection are processed.
     *)

  method close : unit -> unit
    (** Closes the file descriptor with a reliable method. This method must be
      * called after [next_request] returned [None]. It can also be called at any
      * time to shut down the connection prematurely (this means a lingering close,
      * and may cost some time).
     *)
end


val process_connection : 
      #http_reactor_config -> Unix.file_descr -> 'a http_service -> unit
  (** Processes all HTTP requests in turn arriving at the file descriptor, and
    * calls the service provider for every request. Finally, the descriptor is
    * closed.
    *
    * All stages of HTTP processing, as defined by the service provider, are
    * executed in the current thread.
    *
    * Any exceptions are caught and logged. The connection is immediately closed
    * in this case.
   *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module
     *)
end
