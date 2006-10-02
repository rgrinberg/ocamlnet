(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** AJP Connection management *)

(* Support for the JSERV protocol (see http://java.apache.org/jserv/).
 * See also the explanations under examples/jserv.
 *)

open Netchannels

type t
  (** The type of an AJP server *)

type auth =
    { auth_challenge_length : int;  (** The length of the challenge *)
      auth_secret : string;         (** The shared secret (password) *)
    }
  (** Authentication record *)

type controlpipe = [ `None | `Descriptor of Unix.file_descr | `Allocate ]
  (** The type of the control pipe *)

exception Server_is_down
  (** The AJP server is not connected with the web server *)

exception Signal_shutdown
  (** The web server signals to shut down the AJP server *)

exception Signal_restart
  (** The web server signals to restart the AJP server *)

val server_init :
      ?backlog:int ->
      ?reuseaddr:bool ->
      Unix.inet_addr ->  (* IP address *)
      int ->             (* port *)
	Unix.file_descr
  (** Initializes the AJP server by listening on a TCP port. The [inet_addr]
   * is the address to listen on (e.g. [Unix.inet_addr_any]), and the 
   * integer is the port. The function returns the file descriptor of
   * the master socket.
   *
   * @param backlog Length of the backlog queue (connections not yet
   *   accepted by the AJP server)
   * @param reuseaddr Whether to reuse the port
   *)

val server_loop :
      ?controlpipe:[ `None | `Descriptor of Unix.file_descr | `Allocate ] ->
      ?onrestart:(t -> unit) ->
      ?onshutdown:(t -> unit) ->
      ?select_accept:
	(Unix.file_descr list -> Unix.file_descr ->
	   (Unix.file_descr list * Unix.file_descr option)) ->
      ?allow_hosts: Unix.inet_addr list ->
      (t -> auth option -> in_obj_channel -> out_obj_channel -> unit) -> 
	  (* connection handler *)
      auth option ->
      Unix.file_descr ->
	unit
  (** Accepts connection coming from the web server on the [file_descr],
   * and calls the connection handler.
   *
   * The connection handler is the function
   * {[ t -> auth option -> in_obj_channel -> out_obj_channel -> unit ]}
   * where
   * - [t] is the AJP server
   * - [auth option] is the optional authentication record
   * - [in_obj_channel] is the input channel over [file_descr]
   * - [out_obj_channel] is the output channel over [file_descr]
   *
   * The connection handler serves the complete connection, and must
   * close the two channels when it is done. It can return normally,
   * but it is also possible to raise the following exceptions:
   * 
   * - [Signal_shutdown]: The master socket is closed, and the whole
   *   server stops operation. Before this happens, however, the [onshutdown]
   *   function is called.
   * - [Signal_restart]: The [onrestart] function is called. It is
   *   up to this function to implement restarting.
   *
   * There is an alternate way to indicate these two signals for
   * multi-threaded servers, i.e. for servers that create a new thread
   * for every connection. Obviously, it is not possible to raise the
   * mentioned exceptions, as the thread context is different. The
   * work around is to call {!Netcgi_jserv.signal_restart} and
   * {!Netcgi_jserv.signal_shutdown}.
   *
   * @param controlpipe The control pipe is used to indicate "restart" or "shutdown"
   *   when the request handler is running in another thread or process than
   *   the server loop. The default is [`Allocate] and means that a new
   *   control pipe is allocated. The value [`None] means that no control pipe
   *   is created, and as a consequence, that the functions [signal_restart] and
   *   [signal_shutdown] will not work. The value [`Descriptor] means that
   *   the functions [signal_restart] and [signal_shutdown] write their
   *   messages to this descriptor, but the server loop ignores any messages.
   *
   * @param select_accept This function is called when the server waits until a 
   *   file descriptor becomes readable, and when it waits until a new
   *   connection begins. The function has two arguments: First the list of
   *   file descriptors to wait for, and second the master socket it listens
   *   to. The function must block until one of the file descriptors in the
   *   list becomes readable, or there is a new connection (or both).
   *   The result is a pair; the left component is the list of file descriptors
   *   that are readable, and the right component is the new slave socket
   *   for the connection (or None). The default is:
   *   {[
   *   (fun sel sock -> 
   *      let sel',_,_ = restart (Unix.select (sock::sel) [] []) (-1.0) in 
   *      let sel'' = List.filter (fun fd -> fd <> sock) sel' in
   *      let slave = 
   *        if List.mem sock sel' then 
   *          Some(fst(restart Unix.accept sock))
   *        else
   *          None in
   *      (sel'', slave)
   *   )
   *   ]}
   *   (where [restart] handles EINTR errors from [Unix.select]).
   *
   *   Note that even the [select_accept] function is allowed to raise the 
   *   exceptions [Signal_shutdown] and [Signal_restart].
   *
   * @param allow_hosts The list of IP addresses that are allowed to connect to this
   *   service. The empty list (the default) means that any address is
   *   allowed. (Net masks are not supported)
   *
   * @param onrestart this function is called when a "restart" is performed. 
   *   The default is [(fun _ -> ())]
   * @param onshutdown the function is called when a "shutdown" is performed.
   *   The default is [(fun _ -> ())]
   *)

val server :
      ?backlog:int ->
      ?reuseaddr:bool ->
      ?controlpipe:controlpipe ->
      ?onrestart:(t -> unit) ->
      ?onshutdown:(t -> unit) ->
      ?select_accept:
	(Unix.file_descr list -> Unix.file_descr ->
	   (Unix.file_descr list * Unix.file_descr option)) ->
      ?allow_hosts: Unix.inet_addr list ->
      (t -> auth option -> in_obj_channel -> out_obj_channel -> unit) -> 
	  (* connection handler *)
      auth option ->
      Unix.inet_addr ->  (* IP address *)
      int ->             (* port *)
        unit
  (** The function [server]
   * simply calls [server_init] first to get the file descriptor from the
   * port specification, and it then calls [server_loop] to process the
   * requests.
   *)

val signal_restart : t -> unit
  (** Signals the server that a "restart" must be done. This works only if
   * [signal_restart] is not called from the same thread as the server!
   * If it is called from the same thread, [signal_restart] will block infinitely.
   *)

val signal_shutdown : t -> unit
  (** Signals the server that a "shutdown" must be done. This works only if
   * [signal_restart] is not called from the same thread as the server!
   * If it is called from the same thread, [signal_restart] will block infinitely.
   *)

val read_control_pipe : Unix.file_descr -> 'a
  (** Reads the next message from the control pipe, and raises either
   * [Signal_restart] or [Signal_shutdown]. It is also possible that the
   * function raises [End_of_file].
   * It is not possible that the function returns normally.
   *)

val random_8bits : unit -> int
  (** Returns the next byte of the PRNG *)

val prng_init : 
      ?lock:(unit -> unit) ->
      ?unlock:(unit -> unit) ->
      string -> 
	unit
  (** Initializes the PRNG and seeds it with the passed key. 
   *
   * [lock], [unlock]: By default, the PRNG is not reentrant. This can be changed
   *   by passing a mutex as follows:
   *  {[
   *     let mutex = Mutex.create() in
   *     let lock() = Mutex.lock mutex in
   *     let unlock() = Mutex.unlock mutex in
   *     prng_init ~lock ~unlock seed; ]}
   *)

val prng_init_from_file : 
      ?lock:(unit -> unit) ->
      ?unlock:(unit -> unit) ->
      ?length:int -> 
      string -> 
	unit
  (** Initializes the PRNG and seeds it with the contents of the passed file.
   * Only the first ~length bytes of the file are used (or fewer if the
   * file is shorter).
   * [length] defaults to 256.
   * For some operating systems, there is a device [/dev/random] that can 
   * easily be used to seed the PRNG:
   * {[
   *   prng_init_from_file ~length:8 "/dev/random" ]}
   *
   * [lock], [unlock]: same meaning as in [prng_init].
   *)

val parse_properties : string -> (string * string) list
  (** Parses a property file and returns it as alist *)

val jvm_emu_main : 
  ((string * string) list -> auth option -> Unix.inet_addr -> int -> unit) ->
    unit
  (** This function fakes the "java" command (JVM startup). It interprets
   * [Sys.argv] as follows:
   * - The option [-classpath <path>] is ignored
   * - The first anonymous argument (Java class name) is ignored
   * - The second anonymous argument is the file name of the property file.
   *
   * The property file is parsed, and the following properties are used:
   * - "bindaddress": The address the server socket is bound to. Can be
   *   specified as IP address or hostname or "*" (default: "localhost")
   * - "port": The port number the server socket is bound to. Defaults to
   *   8007.
   * - "security.authentication": If "true", the server expects that the 
   *   web server authenticates itself. Defaults to "true".
   * - "security.challengeSize": The length of the challenge string.
   *   Defaults to 5.
   * - "security.secretKey": The file containing the secret key used for
   *   authentication.
   * - "security.allowHost": Only the web server with this IP address is allowed
   *   to connect (this option can occur several times). DNS names are resolved
   *   at startup time.
   * - "jakarta.servletSubString": The substring that is used as indicator for
   *   the servlet name (for mod_jk only). Defaults to "/servlets/".
   * - "ocamlnet.https": Whether HTTPS is assumed as basic protocol or not.
   *   Defaults to "false".
   *
   * Other properties are ignored.
   *
   * Finally, the passed function is called. The first argument is the 
   * property list, the second argument is the authentication record (or
   * None), the third argument is the inet_addr ("bindaddress"), and the
   * fourth argument is the port. The task of this function is to set
   * up the server socket and to accept incoming connections.
   *)


(** {1 Example}
 *
 * A simple single-threaded server looks as follows:
 * 
 * {[
 * let onconnect srv =
 *   Netcgi_jserv_ajp12.serve_connection
 *     (fun zone servlet env -> ... )
 * in
 * jvm_emu_main
 *   (fun props auth addr port ->
 *      server 
 *        ~onrestart 
 *        ~onshutdown
 *        onconnect 
 *        auth 
 *        addr
 *        port);;
 * ]}
 *)
