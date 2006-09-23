(* netcgi_ajp.mli

   (C) 2005 Christophe Troestler

   This code may be used under either, the GNU GPL, or the same license
   as ocamlnet (see the file LICENSE).

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

(** Apache JServ Protocol (AJP) 1.3 connector.
 *
 * See the {!Netcgi_ajp.setup} section at the end of this file to know
 * how to configure your web server.
 *)

open Netcgi

val arg_parse :
  (Arg.key * Arg.spec * Arg.doc) list -> Arg.anon_fun -> Arg.usage_msg
  -> (string * string) list
  (** [arg_parse speclist anon_fun usage_msg] parses the command line
      and return an associative list describing the content of the
      property file (see {!Netcgi_ajp.props_of_file}).  This function
      allows to fakes the "java" command (JVM startup):

      - the option [-classpath <path>] is ignored;
      - the first anonymous argument (Java class name) is ignored;
      - the second anonymous argument is the name of the property file;
      - other options are interpreted according to the [speclist].

      @raise Failure and prints a usage message if the property file
      cannot be read.  *)

val props_of_file : string -> (string * string) list
  (** [props_of_file fname] parses the property file [fname] and
      returns it as an associative list.  The following properties are
      used:

      - "bindaddress": The address the server socket is bound to.  Can be
      specified as IP address or hostname or "*" (default: "localhost").
      - "port": The port number the server socket is bound to.  Defaults to
      8007.
      - "security.authentication": If "true", the server expects that the
      web server authenticates itself.  Defaults to "true".
      - "security.challengeSize": The length of the challenge string.
      Defaults to 5.
      - "security.secretKey": The file containing the secret key used for
      authentication.
      - "security.allowHost": Only the web server with this IP address is
      allowed to connect (this option can occur several times).
      DNS names are resolved at startup time.
      - "jakarta.servletSubString": The substring that is used as indicator
      for the servlet name (for mod_jk only).  Defaults to "/servlets/".
      - "ocamlnet.https": Whether HTTPS is assumed as basic protocol or not.
      Defaults to "false".

      Other properties are ignored.

      @raise Invalid_argument if the file does not exist or is not readable.
  *)


val run :
  ?props:(string * string) list ->
  ?config:config ->
  ?script_name:string ->
  ?allow:(Unix.sockaddr -> bool) ->
  ?output_type:output_type ->
  ?arg_store:arg_store ->
  ?exn_handler:exn_handler ->
  ?port:int ->
  (cgi -> unit) -> unit
  (** [run f] executes [f cgi] for each AJP request.

      @param config Default: {!Netcgi.default_config}
      @param allow Tells whether a connection from the socket is allowed.
                   Default: allow from all.
      @param output_type Default: [`Direct ""]
      @param arg_store Default: [`Automatic] for all arguments.
      @param port The port used by the web server to send the requests
                  (Default: 8009).
      @param exn_handler See {!Netcgi.exn_handler}.  Default: delegate
      all exceptions to the default handler.  *)

(*
  val socket : ?props:(string * string) list -> ?backlog:int ->
  ?reuseaddr:bool -> ?addr:Unix.inet_addr -> ?port:int  ->
  unit -> Unix.file_descr
(** [socket ?props ?backlog ?reuseaddr ?addr ?port] setup a AJP
  socket listening to incomming requests.

  @param backlog Length of the backlog queue (connections not yet
  accepted by the AJP server)
  @param reuseaddr Whether to reuse the port
  @param addr defaults to localhost.
  @param port if not present, assume the program is launched
  by the web server.
*)

val accept_fork : ?props:(string * string) list ->
  ?onrestart:(unit -> unit) ->
  ?onshutdown:(unit -> unit) ->
  ?allow_hosts:Unix.inet_addr list ->
  fork:((Unix.file_descr -> unit) -> int * Unix.file_descr) ->
  'message connection_handler -> Unix.inet_addr -> unit

val accept_threads : ?props:(string * string) list ->
  ?onrestart:(unit -> unit) ->
  ?onshutdown:(unit -> unit) ->
  ?allow_hosts:Unix.inet_addr list ->
  ?thread:((unit -> unit) -> unit) ->
  'message connection_handler -> Unix.inet_addr -> unit

val handle_connection : ?props:(string * string) list ->
  ?config:config ->
  ?auth:(int * string) ->
  (cgi -> unit) -> 'message connection_handler
*)


(* ---------------------------------------------------------------------- *)

(** {2:setup Setup}


    {3 Apache}

    You need to use mod_jk to have support for AJP/1.3.  To install
    it, please see
    {{:http://tomcat.apache.org/tomcat-3.3-doc/mod_jk-howto.html}Working
    with mod_jk}.

    In httpd.conf or in a file, say mod_jk.conf, in
    /etc/apache/conf.d/, add the following:
    {v
    # Shared memory file name (Unix only).  The parent dir must exist.
    JkShmFile  /var/tmp/jk-runtime-status
    LoadModule jk_module mod_jk.so
    # Declare the module for <IfModule> (remove this line on Apache 2.x)
    AddModule  mod_jk.c

    <IfModule mod_jk.c>
      # Configure mod_jk
      JkWorkersFile /etc/libapache-mod-jk/workers.properties
      JkLogFile     /var/log/apache/mod_jk.log
      JkLogLevel    info

      # JkMount [URL prefix] [Worker name]
      JkMount /*.jsp ajp13_worker
      JkMount /servlet/* ajp13_worker
    </IfModule>
    v}


    {3 Other web severs}

    Please go to this
    {{:http://tomcat.apache.org/connectors-doc/index.html}configuration
    page}.  Mail us specific instructions or tips for other web
    servers so we can include them here.


    {3 Workers.properties}

    Here is an example of workers.properties:
    {v
    # Comma separated list of worker names:
    worker.list=ajp13_worker
    # Set properties for ajp13_worker
    worker.ajp13_worker.type=ajp13
    worker.ajp13_worker.host=localhost
    worker.ajp13_worker.port=8009
    worker.ajp13_worker.cachesize=1
    v}
*)
