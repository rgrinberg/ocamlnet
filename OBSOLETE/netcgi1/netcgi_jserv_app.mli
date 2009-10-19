(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** AJP-based Application Servers *)


(* Support for the JSERV protocol (see http://java.apache.org/jserv/).
 * See also the explanations under examples/jserv.
 *)

(** This module contains a "ready to use" application server framework
 * for single-threaded and multi-process servers.
 *)

type request_handler =
    { req_activate       : Netcgi_types.cgi_activation -> unit;
      req_processing     : string -> Netmime.mime_header -> 
	                                             Netcgi.argument_processing;
      req_operating_type : Netcgi.operating_type;
    }
  (** The request handler consists of:
   * - [req_activate]: A function to process requests and to generate
   *   responses. The function gets a fully initialized [cgi_activation]
   *   object, and is expected to write the response.
   * - [req_processing]: Style of CGI argument processing. Same meaning as in
   *   {!Netcgi}.
   * - [req_operating_type]: Style of CGI response buffering. Same meaning as in
   *   {!Netcgi}.
   *)

type server_type =
    [ `Sequential   of        (string * request_handler) list
    | `Forking      of int * ((string * request_handler) list)
    | `Process_pool of int * ((string * request_handler) list)
    ]
  (** Server type:
   * - [`Sequential servlets]: The server processes the requests sequentially.
   * - [`Forking(n,servlets)]: The server forks for every request, and processes it in the
   *    child process. The integer [n] is the maximum number of children 
   *    processes; if it is exceeded, an error message is displayed immediately
   *    without forking.
   * - [`Process_pool(n,servlets)]: The server forks a fixed number of times (the integer [n]).
   *    The children will process the requests concurrently. If more requests
   *    arrive than children are available, the requests must wait until
   *    a child becomes free again.
   *
   * In [servlets] the list of available servlets is passed. The strings
   * are the names of the servlets (part of the URL), and the [request_handlers]s
   * are the corresponding handlers.
   *)

type protocol_type =
    [ `Ajp_1_2
    ]
  (** Selects the protocol. *)

type jserv_config =
    { js_backlog : int;
      js_reuseaddr : bool;
      js_cgiconfig : Netcgi_env.cgi_config;
      js_init_process : unit -> unit;
      js_fini_process : unit -> unit;    
      js_idle_worker : unit -> unit;
      js_idle_master : unit -> unit;
    }
    (** Server configuration:
     * - [js_backlog] is the maximum length of the backlog queue (containing
     *   client connections that are not yet accepted by the application
     *   server)
     * - [js_reuseaddr]: Whether to reuse the port immediately
     * - [js_cgiconfig]: The CGI-level configuration
     * - [js_init_process]: This hook is called when a new process is
     *   initialized. For [`Sequential] servers it is called once before
     *   the server begins to accept connections. For [`Forking] and
     *   [`Process_pool] servers it is called when new processes are
     *   forking.
     * - [js_fini_process]: The reverse hook of [js_init_process]: Called
     *   after a process receives the shutdown notification. 
     *   For [`Sequential] servers it is called once after
     *   the server stops to accept connections. For [`Forking] and
     *   [`Process_pool] servers it is called before sub processes exit.
     * - [js_idle_master]: This hook is called every second by the
     *   master process that accepts new connections. When it raises
     *   an exception, the master socket is closed, and the exception
     *   falls through to the caller.
     * - [js_idle_worker]: This hook is called every second by the
     *   worker process that processes connections. Exceptions are logged.
     *
     * Examples:
     * - [js_init_process]: Open a new database connection for every process
     * - [js_fini_process]: Close the database connection
     * - [js_idle_master]: Check whether the server should shut down, and
     *   if so, raise an exception to exit
     * - [js_idle_worker]: Close database connections after a period
     *   of inactivity
     *)

val std_config : jserv_config
  (** The standard configuration:
   *
   * - small backlog
   * - ports are reused
   * - default CGI configuration
   * - callback functions are all (fun () -> ()).
   *)

val logger : (string -> string -> unit) ref
  (** This variable contains the logger function. The function is called to
   * log error conditions. The first passed string is the servlet name,
   * or "?" if not available. The second passed string is the message.
   * By default, the messages are written to stderr.
   *)


val run :
      ?config:jserv_config ->
      server_type ->
      protocol_type ->
      (* props: *) (string * string) list ->
      Netcgi_jserv.auth option -> 
      (* addr: *) Unix.inet_addr -> 
      (* port: *) int ->
	unit
  (** Starts the server. The last four arguments are compatible with the 
   * function accepted by {!Netcgi_jserv.jvm_emu_main}, so [run] can be
   * used as follows:
   *
   * {[ Netcgi_jserv.jvm_emu_main (run ~config srvtype prttype) ]}
   *
   * The server runs until it gets a shutdown notification from Apache.
   * 
   * Restart notifications are currently ignored.
   *
   * Another remark about [`Process_pool]. The signal handler for SIGALRM
   * is temporarily redefined while a process waits for a lock. The old
   * handler is suspended until the lock can be acquired.
   *)
