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
 * along with WDialog; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(** Type definitions for the HTTP daemon, and an introduction
  *
  * {b Contents}
  *
  * - {!Nethttpd_types.exceptions}
  * - {!Nethttpd_types.environment}
  * - {!Nethttpd_types.service}
  * - {!Nethttpd_types.helpers}
  * - {!Nethttpd_types.overview}
 *)

(** Many types can also be found in the [Nethttp] module (part of netstring).
  * Furthermore, [Netcgi_env] and [Netcgi_types] are of interest (part of cgi).
 *)

(** {1:exceptions Exceptions} *)

open Nethttp

exception Standard_response of http_status * http_header option * string option
  (** Some HTTP containers allow you to raise this exception. The standard
    * response corresponding to [http_status] is sent back to the client.
    * If the third argument exists, an entry into the error log
    * is written.
   *)

(**********************************************************************)

(** {1:environment Environment} *)

(** An extension of [cgi_environment] for use with the daemon. The methods
  * retrieve the socket addresses are virtual.
 *)
class type virtual v_extended_environment =
object
  inherit Netcgi_env.cgi_environment

  method virtual server_socket_addr : Unix.sockaddr
  method virtual remote_socket_addr : Unix.sockaddr
    (** These are always the physical IP addresses and ports of the two endpoints
      * of the current connection.
     *)

  method send_file : Unix.file_descr -> int64 -> unit
    (** Sends the output header with a file as body. The file must already be open,
     * and positioned where the transmission begins. The number is the length
     * of the transmission.
     *
     * This method may return immediately when it is possible to open the file, and
     * to set the kernel up for file transmission. Otherwise a [Unix_error] is
     * raised. It is also allowed that this method blocks until the file is actually
     * transmitted.
     *
     * It is not allowed to print to the output channel and to call [send_file].
     * Only one transmission method must be invoked.
     *)

end



(** Same as [v_extended_environment], but no virtual methods 
 *)
class type extended_environment =
object
  inherit v_extended_environment
  method server_socket_addr : Unix.sockaddr
  method remote_socket_addr : Unix.sockaddr
end

(** {2 Construction of environments} *)

class virtual empty_environment :
object
  inherit v_extended_environment
  val mutable config : Netcgi_env.cgi_config
  val mutable in_state : Netcgi_env.input_state
  val mutable out_state : Netcgi_env.output_state
  val mutable protocol : Netcgi_env.protocol
  val mutable in_header : http_header
  val mutable out_header : http_header
  val mutable properties : (string * string) list
  val mutable in_channel : Netchannels.in_obj_channel
  val mutable out_channel : Netchannels.out_obj_channel
end
  (** This class implements an environment with defined internal containers.
    * These containers are empty, but fully functional.
    * The following methods are empty and should be redefined:
    * - [send_output_header]
    * - [send_file]
    * - [log_error]
    *
    * The virtual methods, of course, must be defined!
   *)

class redirected_environment : 
        ?in_state : Netcgi_env.input_state ->
        ?in_header : http_header ->
        ?properties : (string * string) list ->
        ?in_channel : Netchannels.in_obj_channel ->
        extended_environment ->
          extended_environment
  (** This class overlays the input-side containers of an existing environment.
    * The output-side containers ([out_state], [out_header], and [out_channel])
    * are physically identical with the existing environment.
    *
    * If one of the argument is not passed on class instantiation, the corresponding
    * overlay container is initialized with the current value of the passed
    * environment. As exception of this rule, the input channel is initialized with
    * an empty input channel.
   *)

(** {2 Auxiliary Functions for Environments} *)

val output_static_response : #extended_environment ->
                             http_status -> http_header option -> string -> unit
  (** Outputs the string argument as response body, together with the given status and
    * the header (optional). Response header fields are set as follows:
    * - The [Content-Length] is set to the length of the string.
    * - The [Content-Type] is set to "text/html" unless given by the header.
    *
    * If the header is not passed, the header of the environment is taken. If the header
    * argument exists, however, it overrides the header of the environment.
   *)

val output_file_response : #extended_environment ->
                           http_status -> http_header option ->
                           string -> int64 -> int64 -> unit
  (** Outputs the contents of a file as response body, together with the given status and
    * the header (optional). The string is the file name. The first int64 number is
    * the position in the file where to start, and the second number is the length
    * of the body.  Response header fields are set as follows:
    * - The [Content-Length] is set to the length of the string.
    * - The [Content-Type] is set to "text/html" unless given by the header.
    *
    * Note that [Content-Range] is not set automatically, even if the file is only
    * partially transferred.
    *
    * If the header is not passed, the header of the environment is taken. If the header
    * argument exists, however, it overrides the header of the environment.
    *
    * The function raises [Sys_error] when the file cannot be read.
   *)

class type min_config =
object
  method config_error_response : int -> string
  method config_log_error : 
    Unix.sockaddr option -> Unix.sockaddr option -> http_method option -> http_header option -> string -> unit
end
  (** Minimal configuration needed for [output_std_response] *)

val output_std_response : #min_config -> #extended_environment ->
                          http_status -> http_header option -> string option -> unit
  (** Outputs a "standard response" for the [http_status]. The string argument
    * is an optional entry into the error log.
    *
    * If the header is not passed, an empty header is taken. If the header argument
    * exists, this header is taken. The header of the environment is never taken.
   *)


(**********************************************************************)

(** {1:service Service Providers}
  *
  * Service providers are defined using the three class types:
  * - [http_service]: The service provider as such. When a HTTP header has been
  *   received, and the service provider is invoked, it returns an object
  *   fitting to the next class type, [http_service_receiver]. This object
  *   is tagged with [`Accept_body]; at this point there are also alternate ways
  *   of processing, see below.
  * - [http_service_receiver]: The task of this object is to receive the request
  *   body. When the body has been completely received, the object is notified,
  *   and returns a third object of type [http_service_generator].
  * - [http_service_generator]: The task of this object is to generate the
  *   response.
  *
  * An implementor is free to define only one class that satisfies all three
  * class types at once. However, this is only an option.
  *
  * The three objects reflect three stages of HTTP processing. The stages have
  * made explicit to allow the implementor of services to intercept the points
  * in time when the processing of the next stage begins. Furthermore, in multi-threaded
  * environments it is allowed that the stages are performed in the contexts of
  * different threads.
  *
  * In addition to the three-stage model there also several faster paths of 
  * processing:
  * - [`Reject_body] can be used to refuse the acceptance of the request body when
  *   it is already clear that an error response is sent back. This path skips
  *   the stage [http_service_receiver].
  * - [`Static] can be used to send a constant string back (only to be used
  *   when the string needs not to be computed)
  * - [`File] can be used to send the contents of a file back (only to be used
  *   when the file already exists)
 *)

exception Redirect_request of string * http_header
  (** The "early" redirect is only allowed in stage 1 of HTTP processing.
    * The string argument is the new URI path of the request. The header can also
    * be exchanged except the fields that are needed to decode the request
    * body. It is not possible to change the method.
   *)

exception Redirect_response of string * http_header
  (** The "late" redirect is only allowed in stage 3 of HTTP processing.
    * The string argument is the new URI path of the request.  The header can also
    * be exchanged except the fields that are needed to decode the request
    * body. {b The method is always changed to [GET].}
   *)


class type http_service_generator =
object
  method generate_response : extended_environment -> unit
    (** Third stage of HTTP processing:
      * This method is called when the HTTP request has been completely received,
      * and the response is going to be generated. This method can again be called
      * from a different thread than the previous stages. It is allowed to spend
      * any necessary time to compute the response.
      *
      * When the method returns, the request processing is finished. No more data
      * is allowed to be written to the output channel past this moment.
      *
      * The method may raise [Standard_response] to generate one of the
      * standard messages.
     *)
end


class type http_service_receiver =
object
  method process_body : extended_environment -> http_service_generator
    (** Second stage of HTTP processing:
      * This method is called when the body is expected to be arriving. Note that
      * the main difference to [process_header] is that this method can be
      * called from a different thread. It is allowed (and expected) that this method
      * blocks while reading the input. Of course, the input and output
      * channels of the environment are unlocked, and can be used.
      * 
      * When the method returns, the request processing continues with stage 3.
      * Any body data not read is dropped.
      *
      * It is allowed that this method generates a response (or part of it),
      * although this should be better done in stage 3.
      *
      * The method may raise [Standard_response] to generate one of the
      * standard messages.
      *
      * One way of implementing this method is to instantiate [Netcgi.std_activation].
     *)

end


type http_service_reaction =
    [ `Accept_body of http_service_receiver
    | `Reject_body of http_service_generator
    | `Static of http_status * http_header option * string
    | `File of http_status * http_header option * string * int64 * int64
    | `Std_response of http_status * http_header option * string option
    ]
    (** Indicates the immediate reaction upon an arriving HTTP header:
      * - [`Accept_body] is the regular way of processing requests. If necessary,
      *   the client is told to continue sending the rest of the request.
      * - [`Reject_body] can be used when the body of the request is not needed,
      *   and the response will be negative.
      * - [`Static] means to send the header and a constant string back as response.
      *   The header is taken from the environment if not explicitly passed,
      *   Note: The [Content-Length] header is automatically added. The [Content-Type]
      *   defaults to "text/html".
      * - [`File] is similar to this, but the data come from a file. The file
      *   is specified by an absolute pathname. The range of the file is given
      *   by the start position and the length of the range.
      *   The header is taken from the environment if not explicitly passed,
      *   Note: The [Content-Length] header is automatically added. The [Content-Type]
      *   defaults to "text/html".
      * - [`Std_response] is similar to [`Static], however the body is the standard
      *   text for the status code. If the header is omitted, it is taken as empty.
      *   The third argument is an optional entry into the error log.
      *   Note: The [Content-Length] header is automatically added. The [Content-Type]
      *   defaults to "text/html".
     *)

class type ['a] http_service =
object
  method name : string
    (** The name of the type of the service provider *)

  method def_term :'a
    (** The definition term *)

  method print : Format.formatter -> unit
    (** Outputs the definition term to a formatter *)

  method process_header : extended_environment -> http_service_reaction
    (** First stage of HTTP processing:
      * This method is called when the HTTP header has been received. This method
      * must return quickly without blocking the thread how to go on. For example,
      * this could look as follows:
      * - Check whether the client is allowed to access this resource. If this
      *   can be done immediately, it should be done now. (If an external service
      *   must be queried, the check must not be done now, but deferred to the
      *   second or third stage.) If the access is denied, an error response can
      *   be sent back using [`Static], [`File], or, if computed, using [`Reject_body].
      * - Check whether the request is delegated to another service provider
      *   (e.g. lookup by hostname, by port number, or by URI). In this case,
      *   the result of this [process_header] call is simply the result of the
      *   [process_header] call of the other service provider.
      * - If this service provider generates the contents, there are mainly two
      *   types of reaction. If the contents are stored in a file, one can simply
      *   return [`File]. Otherwise, return [`Accept_body] to continue with the
      *   second stage. Note that even if no body is expected to arrive, one must
      *   go through the second stage, and drop any unexpected body.
      *
      * The argument of this function is the Netcgi environment. The header is
      * complete, including the request method. One cannot access the input and
      * output channels at this stage of processing.
     *)
end

(**********************************************************************)

(** {1:helpers Helpers} *)

val update_alist : ('a * 'b) list -> ('a * 'b) list -> ('a * 'b) list
  (** [update_alist updl l]: Returns the alist with all elements of [updl]
    * and all elements of [l] that are not member of [updl].
   *)

(**********************************************************************)

(** {1:overview Overview over the HTTP daemon}

This library implements an HTTP 1.1 server. Because it is a library and not
a stand-alone server like Apache, it can be used in very flexible ways.
The disadvantage is that the user of the library must do more to get
a running program than just configuring the daemon.

The daemon has five modules:

- {!Nethttpd_types} is just a module with common type definitions used
  by the other modules
- {!Nethttpd_kernel} is the implementation of the HTTP protocol. If we
  talk about the "kernel" we mean this module. The kernel has two 
  interface sides: There is the "socket side" and the "message side"
  that are connected by bidirectional data flow.
  The task of the kernel is to decode input received by the socket side
  and to deliver it to a consumer on the message side, and conversely
  to encode input coming in through the message side and to send it
  to the socket. The kernel is a quite low-level module; the socket
  is accessed in a multiplexing-compatible style, and the messages
  are sequences of tokens like "status line", "header", "body chunk"
  and so on. Normally a user of the daemon does not program the kernel
  directly. It is, however, possible to pass certain configuration
  options to the kernel even if an encapsulation is used.
- {!Nethttpd_reactor} is an encapsulation of the kernel with a nicer
  interface. An instance of the reactor processes, like the kernel,
  only a single HTTP connection. It is used as follows: The user
  of the reactor pulls the arriving HTTP requests from the reactor,
  processes them, and writes the responses back to the reactor. This
  means that the requests are processed in a strictly sequential
  way. The reactor hides the details of the HTTP protocol. The
  reactor is able to perform socket input and output at the same time,
  i.e. when the response is sent to the client the next request(s) can
  already be read (pipelining). The reactor can be configured such that
  buffering of requests and responses is avoided, even if large
  messages are transferred. As mentioned, the reactor can only
  deal with one connection at the same time. In order to serve
  several connections, one must use multi-threading.
- {!Nethttpd_engine} is another encapsulation of the kernel. It is
  event-based, and this makes it possible that several instances
  can work at the same time without using multi-threading. The user
  of the engine is called back when the beginning of the next HTTP
  request arrives and at certain other events. The user processes
  the event, and generates the response. 
  The engine is a
  highly efficient implementation of an HTTP server, but there are
  also drawbacks, so user may feel more comfortable with the reactor.
  Especially, the engine needs large buffers for input and output
  (there is an idea to use helper threads to avoid these buffers,
  but this has not been implemented yet). Of course, the engine
  also supports pipelining.
- {!Nethttpd_services} has functions to compose complex service
  functions from simpler ones. In particular, one can configure
  name- or IP-based virtual hosting, one can bind services to
  URLs, and one can define static file serving, directory listings,
  and dynamic services. It is quite easy to turn a Netcgi program
  into a dynamic service for Nethttpd.

It is also important to mention what Nethttpd does not include:

- There is no function to create the socket, and to accept connections.

- There is no function to manage threads or subprocesses

It is hoped to add this functionality later in a generic way (i.e.
not only for Nethttpd).

{2 Suggested strategy}

First, look at {!Nethttpd_services}. This module allows the user
to define the services of the web server. For example, the following
code defines a single host with an URL space:

{[
let fs_spec =
  { file_docroot = "/data/docroot";
    file_uri = "/";
    file_suffix_types = [ "txt", "text/plain";
                          "html", "text/html" ];
    file_default_type = "application/octet-stream";
    file_options = [ `Enable_gzip;
                     `Enable_listings simple_listing
                   ]
  }

let srv =
  host_distributor
    [ default_host ~pref_name:"localhost" ~pref_port:8765 (),
      uri_distributor
        [ "*", (options_service());
          "/files", (file_service fs_spec);
          "/service", (dynamic_service
                           { dyn_handler = process_request;
                             dyn_activation = std_activation `Std_activation_buffered;
                             dyn_uri = Some "/service";
                             dyn_translator = file_translator fs_spec;
                             dyn_accept_all_conditionals = false
                           })
        ]
    ]
]}

The [/files] path is bound to a static service, i.e. the files found in
the directory [/data/docroot] can be accessed over the web. The record
[fs_spec] configures the static service.

The [/service] path is bound to a dynamic service, i.e. the requests
are processed by the user-defined function [process_request]. This function
is very similar to the request processors used in Netcgi.

The symbolic [*] path is only bound for the [OPTIONS] method. This is
recommended, because clients can use this method to find out the
capabilities of the server.

Second, select an encapsulation. As mentioned, the reactor is much simpler
to use, but you must take a multi-threaded approach to serve multiple
connections simultaneously. The engine is more efficient, but may use
more memory (unless it is only used for static pages).

Third, write the code to create the socket and to accept connections.
For the reactor, you should do this in a multi-threaded way (but
multi-processing is also possible). For the engine, you should do this in
an event-based way.

Now, just call {!Nethttpd_reactor.process_connection} or 
{!Nethttpd_engine.process_connection}, and pass the socket descriptor
as argument. These functions do all the rest.

The Ocamlnet source tarball includes examples for several approaches.
Especially look at [file_reactor.ml], [file_mt_reactor.ml], and 
[file_engine.ml].

{2 Configuration}

One of the remaining questions is: How to set all these configuration 
options.

The user configures the daemon by passing a configuration object.
This object has a number of methods that usually return constants, but
there are also a few functions, e.g.

{[
  let config : http_reactor_config =
    object
      method config_timeout_next_request = 15.0
      method config_timeout = 300.0
      method config_reactor_synch = `Write
      method config_cgi = Netcgi_env.default_config
      method config_error_response n = "<html>Error " ^ string_of_int n ^ "</html>"
      method config_log_error _ _ _ _ msg =
        printf "Error log: %s\n" msg
      method config_max_reqline_length = 256
      method config_max_header_length = 32768
      method config_max_trailer_length = 32768
      method config_limit_pipeline_length = 5
      method config_limit_pipeline_size = 250000
    end 
]}

Some of the options are interpreted by the encapsulation, and some by the
kernel. The object approach has been taken, because it can be arranged that
the layers of the daemon correspond to a hierarchy of class types.

The options are documented in the modules where the class types are
defined. Some of them are difficult to understand. In doubt, it is
recommended to just copy the values found in the examples, because these
are quite reasonable for typical usage scenarios.
 *)
