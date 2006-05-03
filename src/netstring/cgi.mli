(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(**********************************************************************)
(***                        DEPRECATED MODULE                       ***)
(**********************************************************************)

(* The module Cgi is now deprecated. Please use the Netcgi modules for
 * new applications.
 * Because of its significance, Cgi will be kept as part of ocamlnet
 * until the end of 2002. But then it will eventually be removed from
 * the distribution.
 *)

(**********************************************************************)

(* FOR SIMPLE CGI PROGRAMS:
 *
 * If you do not need all the features of the API below, the following may
 * be enough:
 *
 * - At the beginning of the main program, call 'parse_arguments' with
 *   either 'default_config' as argument or with a customized configuration.
 * - Use 'argument_value(name)' to get the string value of the CGI parameter
 *   'name'. If you like, you can also open the Cgi.Operators module and
 *   write '!$ name' instead. Here, !$ is a prefix operator equivalent to
 *   argument_value.
 * - Print the header using the header function, then print your own
 *   material.
 *
 * EXAMPLE:
 *
 * let main() =
 *   Cgi.parse_arguments Cgi.default_config;
 *   let x = Cgi.argument_value "x" in
 *   ...
 *   Cgi.header "";
 *   print_endline "<HTML>";
 *   ...
 *
 * OR:
 * 
 * open Cgi.Operators
 * let main() =
 *   Cgi.parse_arguments Cgi.default_config;
 *   let x = !$ "x" in
 *   ...
 *   Cgi.header "";
 *   print_endline "<HTML>";
 *   ...
 *
 * If you do not change the default configuration, you do not need to
 * worry about temporary files - there are not any.
 *)


(**********************************************************************)
(* Types                                                              *)
(**********************************************************************)

(* First, the general interface to the CGI argument parser. *)

exception Resources_exceeded

type argument

type argument_processing =
    Memory        (* Keep the value of the argument in memory *)
  | File          (* Store the value of the argument into a temporary file *)
  | Automatic     (* Store only large arguments into files. An argument
		   * value is large if it is longer than about one block (4K).
		   * This is not an exact definition.
		   *)

type workaround =
    Work_around_MSIE_Content_type_bug
      (* There is a bug in MSIE I observed together with SSL connections.
       * The CONTENT_TYPE passed to the server has sometimes the wrong
       * format. This option enables a workaround if the user agent string
       * contains the word "MSIE".
       *)
  | Work_around_backslash_bug
      (* There is a bug in many browsers: The backslash character is not
       * handled as an escaping character in MIME headers. Because DOS-
       * based systems use the backslash regularly in filenames, this bug
       * matters.
       * This option changes the interpretation of backslashes such that
       * these are handled as normal characters. I do not know any browser
       * that is not affected by this bug, so there is no check on
       * the user agent string.
       *)


type config =
    { maximum_content_length : int;
          (* The maximum CONTENT_LENGTH. Bigger requests trigger an
	   * Resources_exceeded exception. This feature can be used
	   * to detect primitive denial-of-service attacks.
	   *)
      how_to_process_arguments : argument -> argument_processing;
          (* After the beginning of an argument has been decoded, the
	   * type of processing is decided by invoking this function on
	   * the argument. Note that the passed argument is incomplete -
	   * it does not have a value. You can assume that name, filename,
	   * MIME type and the whole header are already known.
	   * - THIS CONFIGURATION PARAMETER ONLY AFFECTS ARGUMENTS
	   * "POST"ED FROM THE CLIENT IN FORM-ENCODED REPRESENTATION.
	   * All other transport methods can only handle the Memory
	   * processing type.
	   *)
      tmp_directory : string;
          (* The temporary directory to use for the temporary files. *)
      tmp_prefix : string;
	  (* A prefix for temporary files. It is recommended that the prefix
	   * contains a part that is random or that depends on rapidly changing
	   * environment properties. For example, the process ID is a good
	   * candidate, or the current system time. It is not required that
	   * the prefix is unique; there is a fail-safe algorithm that
	   * computes a unique file name from the prefix, even if several
	   * CGI programs run concurrently.
	   *)
      workarounds : workaround list;
          (* Specifies which workarounds should be enabled. *)
      enable_testing : bool;
          (* If true, and if the typical CGI environment variables are 
	   * missing, and if no ~state argument is used, the function
	   * parse_arguments assumes that the CGI program is tested from
	   * the command line. You can pass arguments by the following ways:
	   * (1) Command-line: Arguments of the form "name=value" are accepted.
	   *     Furthermore, there are several command-line options (you
	   *     can list them as usual by -help)
	   * (2) Interactive: CURRENTLY NOT IMPLEMENTED!
	   *)
    }


(**********************************************************************)
(* state                                                              *)
(**********************************************************************)

type state
  (* This is the type of the state of this module. Normally you need not
   * to use it because it is sufficient to keep the state implicit. However,
   * there are special applications of this module that need to pass the
   * ~state argument to the functions that depend on the state.
   * (See also the following comment on create_state.)
   *)

val create_state : unit -> state
  (* Create a new state variable. Such a variable can be passed as the 
   * ~state argument to the functions below.
   *
   * If you do not pass a ~state value the functions work with the
   * implicit state of the module. This is the normal way of using
   * Cgi. The implicit state is created at program startup, and it 
   * is the default value for all ~state parameters. The lifetime of
   * the implicit state is the lifetime of the program, i.e. a classical
   * CGI program is assumed where every request starts a new program.
   *
   * However, you can also create a state variable. This makes it possible
   * to have several instances of the Cgi module:
   *   let cgi1 = create_state() in
   *   let cgi2 = create_state() in
   *   parse_arguments ~state:cgi1 ~input:pipe1 conf;
   *   parse_arguments ~state:cgi2 ~input:pipe2 conf; ...
   * Here, the different state variables ensure that the two invocations
   * of parse_arguments do not interfer with each other. Furthermore, the
   * lifecycle of state variables is fully under control of the programmer.
   *)


(* LIFECYCLE OF A STATE VARIABLE:
 *
 * The indentended lifecycle is as follows:
 * 
 * Phase 0) 
 *     The state has just been created and is now uninitialized. In this
 *     phase, many functions will fail.
 * Phase 1)
 *     The environment has been initialized. For the implicit state, 
 *     this is done automatically, and the values come from the real
 *     process environment. For self-created state variables, the 
 *     environment is initially empty, and is modified by the Env.set*
 *     functions.
 * Phase 2)
 *     The CGI arguments have been initialized. This must be done by
 *     either calling parse_arguments or set_arguments.
 *     In this phase, the state is fully initialized.
 * Phase 3)
 *     The CGI arguments have been manually modified (update_argument,
 *     delete_argument); this may be the simplest way to create 
 *     self-referencing URLs using this_url.
 * Phase 4)
 *     The state has been cleaned. This means that all temporary files
 *     have been deleted (if still existing). The file-based arguments
 *     have no values.
 *
 * After phase 4, the state can be simply dropped, and it will be collected
 * by the GC.
 *
 * Notes:
 * - For the implicit state it is possible to set environment variables
 *   before parse_argument is called. For example:
 *     Env.set_variable ~http_user_agent:"Mozilla" ();
 *     parse_arguments()
 *   In this case, the manually set variables override the variables coming
 *   from the real process environment. (http_user_agent will be "Mozilla"
 *   even if there is a different value in the HTTP_USER_AGENT process 
 *   environment variable.)
 * - For the implicit state, phase 1 is only theory, because the initialization
 *   of the environment is done by lazy evaluation. So the following strange
 *   code works:
 *     parse_arguments();
 *     Unix.putenv "SERVER_SOFTWARE" "super duper server";
 *     let server = Env.server_software()
 *   It works, because parse_arguments() does not read this variable, and
 *   the initialization is deferred until the first real access. In contrast
 *   to this, the same technique does not work for HTTP_USER_AGENT, as
 *   the parser needs this variable, and it is initialized at the moment
 *   when parse_arguments is called.
 *   This means that the programmer should not assume that the environment
 *   variables are initialized at a certain point. It will happen when it 
 *   is necessary, and this may be hard to predict. In order to override
 *   values from the process environment, call Env.set_variable which is
 *   always possible.
 *)

(**********************************************************************)
(* configuration                                                      *)
(**********************************************************************)

val default_config : config
    (* maximum_content_length = maxint
     * how_to_process_arguments = "use always Memory"
     * tmp_directory = "/var/tmp"
     * tmp_prefix = "cgi"
     * workarounds = [ Work_around_MSIE_content_type_bug;
     *                 Work_around_backslash_bug;
     *               ]
     * enable_testing = true
     *
     * Note 1: On some Unixes, a special file system is used for /tmp that
     * stores the files into the virtual memory (main memory or swap area).
     * Because of this, /var/tmp is preferred as default.
     *
     * Note 2: Filename.temp_file is not used because it depends on
     * environment variables which are usually not set in a CGI environment.
     *)

(**********************************************************************)
(* initialization                                                     *)
(**********************************************************************)

val parse_arguments : ?state:state -> 
                      ?input:in_channel -> 
                      ?content_length:int ->
                      ?content_type:string ->
                      config -> 
                        unit
val arguments : ?state:state -> unit -> (string * argument) list
    (* - let () = parse_arguments config:
     * Decodes the CGI arguments. 'config' specifies limits and processing
     * hints; you can simply pass default_config (see below).
     *
     * - let arglist = get_arguments():
     * The function returns a list with (name, arg) pairs. The name is
     * passed back as string while the value is returned as opaque type
     * 'argument'. Below accessor functions are defined. These functions
     * require that parse_arguments was invoked before.
     *
     * Note 1: You can invoke 'parse_arguments' several times, but only
     * the first time the arguments are read in. If you call the function
     * again, it does nothing (even if the config changes). This is also
     * true if 'parse_arguments' has been invoked after 'set_arguments'.
     *
     * Note 2: It is not guaranteed that stdin has been read until EOF.
     * Only CONTENT_LENGTH bytes are read from stdin (following the CGI spec).
     *
     * Note 3: If arguments are processed in File or Automatic mode, the
     * caller of 'parse_arguments' is responsible for deleting the files
     * after use. You may consider to apply the at_exit function of the
     * core library for this purpose. See also 'cleanup' below.
     *
     * ~state: see create_state
     * ~input: the input channel to read from (by default stdin)
     * ~content_length: the number of bytes to read from the input channel.
     *    This value is only used if the REQUEST_METHOD is "POST". The
     *    default value is the CGI environment variable CONTENT_LENGTH.
     *    If CONTENT_LENGTH is not set, the function will fail.
     * ~content_type: the MIME type of the data read from the input channel.
     *    This value is only used if the REQUEST_METHOD is "POST". The
     *    default value is the CGI environment variable CONTENT_TYPE.
     *    If CONTENT_TYPE is not set, the function will fail.
     *
     * Note that the CGI variables such as CONTENT_TYPE are taken from
     * the Env module (below) which is normally initialized from the real
     * environment, but the values can also be manually set.
     *)

val set_arguments : ?state:state -> 
                    ?set_query_string:bool ->    (* default: see below *)
                    argument list -> 
                      unit
    (* Alternatively, you can set the arguments to use. This overrides any
     * previously parsed set of arguments, and also any following parsing
     * (i.e., if you call parse_arguments after set_arguments there is no
     * effect).
     * - Intended for debugging, and to make it possible to replace the
     * CGI parser by a different one while retaining this API.
     *
     * ~state: see create_state
     * ~set_query_string: if true, the QUERY_STRING variable is set
     *   (to the usual name=val&name=val... encoding). Only memory arguments
     *   are used for QUERY_STRING.
     *   The default of this option is: If the REQUEST_METHOD is "GET",
     *   the variable will be set, and otherwise not.
     *)

(**********************************************************************)
(* CGI arguments                                                      *)
(**********************************************************************)

val update_argument : ?state:state -> argument -> unit
    (* Updates an argument (which is added to the arglist
     * if it is new).
     *)

val update_argument_value : ?state:state -> name:string -> string -> unit
    (* Updates the value of an argument. If the argument already exists in 
     * the argument list, only the value is changed (the other properties
     * such as MIME type are not modified). If the argument is new, the
     * argument is created by mk_simple_arg (see below).
     *
     * The function fails if the argument is a file value.
     *)

val delete_argument : ?state:state -> string -> unit
    (* Deletes an argument from the list. It is not an error if the argument
     * does not exist.
     * Note: if the argument is a file, the file will not be deleted. It is
     * only removed from the argument list. A later call of 'cleanup' will
     * not delete the file, too, because 'cleanup' simply scans the current
     * argument list. So the caller is fully responsible for the file.
     *)


val arg_name     : argument -> string
val arg_value    : argument -> string
val arg_file     : argument -> string option
val arg_mimetype : argument -> string
val arg_filename : argument -> string option
val arg_header   : argument -> (string * string) list
    (* The accessor functions that return several aspects of arguments.
     * arg_name: returns the name of the argument
     * arg_value: returns the value of the argument. If the value is stored
     *     in a temporary file, the contents of this file are returned, i.e.
     *     the file is loaded. This may have some consequences:
     *     (1) The function may fail because of I/O errors.
     *     (2) The function may be very slow, especially if the file is
     *         non-local.
     *     (3) If the value is bigger than Sys.max_string_length, the function
     *         raises the exception Resources_exceeded. On 32 bit architectures,
     *         strings are limited to 16 MB.
     *     Note that loaded values are put into weak arrays. This makes it
     *     possible that subsequent calls of 'arg_value' on the same argument
     *     can avoid loading the value again, and that unused values will
     *     nevertheless be collected by the GC.
     * arg_file: returns 'Some filename' if the value resides in a temporary
     *     file, and 'filename' is the absolute path of this file. If the
     *     value is only available in memory, None is returned.
     * arg_mimetype: returns the MIME type of the argument. Note that the
     *     default MIME type is "text/plain", and that the default is returned
     *     if the MIME type is not available.
     * arg_filename: returns 'Some filename' if the argument is associated
     *     with a certain filename (e.g. from a file upload); otherwise None
     * arg_header: returns pairs (name,value) containing the complete header
     *     of the argument. If the transmission protocol does not specify
     *     a header, the empty list is passed back.
     *)

val mk_simple_arg : name:string -> string -> argument
    (* mk_simple_arg name value:
     * Creates a simple argument with only name, and a value passed by string.
     * The MIME type is "text/plain".
     *)

val mk_memory_arg
    : name:string -> ?mime:string -> ?filename:string -> 
      ?header:((string * string) list) -> string -> argument
    (* mk_memory_arg name mimetype filename header value:
     * Creates an argument whose value is kept in memory.
     *
     * Note: The signature of this function changed in release 0.8.
     *)

val mk_file_arg
    : name:string -> ?mime:string -> ?filename:string -> 
      ?header:((string * string) list) -> string -> argument
    (* mk_file_arg name mimetype filename header value_filename:
     * Creates an argument whose value is stored in the file
     * 'value_filename'. If this file name is not absolute, it is interpreted
     * relative to the directory returned by Sys.getcwd() - this might not
     * be what you want with respect to mount points and symlinks (and it
     * depends on the operating system as getcwd is only POSIX.1). The
     * file name is turned into an absolute name immediately, and the
     * function arg_file returns the rewritten name.
     *
     * Note: The signature of this function changed in release 0.8.
     *)

(* Convenience functions: *)

val argument : ?state:state -> string -> argument
    (* let argument name = List.assoc name (arguments()) -- i.e. returns
     * the (first) argument with the passed name. Of course, this function
     * expects that 'parse_arguments' was called before.
     *
     * ~state: see create_state
     *)

val argument_value : ?state:state -> string -> string
    (* let argument_value name = arg_value(argument name) -- i.e. returns
     * the value of the argument.
     * See also Operators.( !$ ) below.
     *
     * ~state: see create_state
     *)

(* For toploop printers: *)

val print_argument : argument -> unit


(**********************************************************************)
(* finalization                                                       *)
(**********************************************************************)

val cleanup : ?state:state -> unit -> unit
    (* Removes all temporary files that occur in the current set of arguments
     * (as returned by 'arguments()'). It is recommended to call this function
     * once before exiting the CGI process. This can be enforced by 
     * registering the function with at_exit:
     *
     *   at_exit Cgi.cleanup
     *
     * Note that the cleanup function is not called when the process terminates
     * because of uncaught exceptions.
     *
     * ~state: see create_state
     *)


(**********************************************************************)
(* CGI output                                                         *)
(**********************************************************************)

type status =   (* Status codes from RFC 2616 *)
  (* 2xx: (successful) *)
  [ `Ok
  | `Created
  | `Accepted
  | `Non_authoritative
  | `No_content
  | `Reset_content
  | `Partial_content
  (* 3xx: (redirection) *)
  | `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Temporary_redirect
  (* 4xx: (client error) *)
  | `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_auth_required
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Request_entity_too_large
  | `Request_uri_too_long
  | `Unsupported_media_type
  | `Requested_range_not_satisfiable
  | `Expectation_failed
  (* 5xx: (server error) *)
  | `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout
  | `Http_version_not_supported ]


type cache_control =
    [ `No_cache
    | `Max_age of int
    | `Unspecified
    ]
  (* This is only a small subset of the HTTP 1.1 cache control features.
   * The directives mean:
   * - `No_cache:
   *   Caches are disabled. The following headers are sent:
   *   Cache-control: no-cache, Pragma: no-cache, Expires: (now - 1 second)
   * - `Max_age n:
   *   Caches are allowed to store a copy of the response for n seconds.
   *   After that, the response must be revalidated.
   *   Cache-control: max-age n, Cache-control: must-revalidate,
   *   Expires: (now + n seconds)
   * - `Unspecified:
   *   No cache control header is added to the response.
   *
   * Notes:
   * - Cache control directives only apply to GET requests; POST requests
   *   are never cached
   * - Not only proxies are considered as cache, but also the local disk
   *   cache of the browser
   * - HTTP/1.0 did not specify cache behaviour as strictly as HTTP/1.1
   *   does. Because of this the "Pragma" and "Expires" headers are sent, too.
   *)


type cookie =
    { cookie_name : string;
      cookie_value : string;           (* may contain every character *)
      cookie_expires : float option;
        (* None: the cookie expires when the browser session ends.
	 * Some t: the cookie expires at the time t (seconds since the epoch)
	 *)
      cookie_domain : string option;
        (* None: the domain is the hostname of the server
	 * Some domain: the domain is this string
	 *)
      cookie_path : string option;
        (* None: the path is script name + path_info
	 * Some p: the path is p
	 *)
      cookie_secure : bool;
        (* Whether SSL is necessary to set the cookie *)
    }


val header : ?output:out_channel -> 
             ?status:status -> 
             ?cache:cache_control ->
             ?filename:string ->
             ?language:string ->
             ?script_type:string ->
             ?style_type:string ->
             ?set_cookie:cookie list ->
             ?fields:(string * string) list ->
             string -> 
                unit
    (* Prints the content-type header.
     * the argument is the MIME type (default value is "text/html" if the
     * argument is the empty string). Note that you can specify the
     * character encoding here:
     *   header "text/html; charset=utf-8"
     *
     * By default, this function prints to stdout. This can be changed by
     * passing the optional ~output argument.
     *
     * The default ~status is not to include a "Status" line (which normally
     * means `Ok).
     *
     * The ~cache option summarizes several common cache-control directives.
     * See the type cache_control. The default is `Unspecified.
     *
     * The ~filename option adds a content-disposition header to the
     * response. This is one way to tell the browser the suggested filename
     * for downloaded files; however, not all browsers understand it.
     * (The other way is to add a path to the URI, i.e. use something
     * like http://host/dir/download.cgi/suggested_filename. It is recommended
     * to apply both techniques.)
     *
     * The ~language option specifies the language of the content
     * (Content-language).
     *
     * The ~script_type option specifies the MIME type of scripts. This
     * option should be specified if HTML code contains "onevent" attributes
     * before any <SCRIPT> tag. Example: ~script_type:"text/javascript".
     *
     * The ~style_type option specifies the MIME type of style sheets.
     * Example: ~style_type:"text/css"
     *
     * The ~set_cookie option specifies the cookies to set.
     *
     * The ~fields argument may contain (name,value) pairs describing 
     * further header fields.
     *)

val redirect : ?output:out_channel -> string -> unit
    (* Performs a redirect to the passed location. If the location 
     * is a path (e.g. "/other/resource"), the server will carry out
     * the redirection internally ("server redirection"). If the location
     * is a complete URL, the server will produce a 302 redirection
     * status.
     *
     * By default, this function prints to stdout. This can be changed by
     * passing the optional ~output argument.
     *)

val this_url : ?state:state -> 
               ?with_host_and_port:bool ->  (* default: true *) 
               ?with_script_name:bool ->    (* default: true *)
               ?with_path_info:bool ->      (* default: true *)
               ?with_query_string:bool ->   (* default: false *)
               ?update_query_string:bool -> (* default: false *)
               unit -> 
                 string
    (* Returns the address of the CGI. The ~with_xxx options specify which
     * parts of the URI are returned. The values of the URI parts come from
     * the Env module below.
     *
     * ~update_query_string: If true, the current CGI parameters are taken
     * for the query string, and not the old query string from the environment. 
     *
     * Notes:
     * - It is recognized whether the connection is secure, and the
     *   protocol prefix is "https:" in this case.
     * - If the port is the default port for the protocol, the port number
     *   is omitted. The default port for insecure connections is 80,
     *   and for secure connections it is 443.
     *)


(**********************************************************************)
(* The CGI environment                                                *)
(**********************************************************************)

module Env : sig
  (* The following functions support only the official CGI environment
   * variables (and most HTTP_* variables have been omitted). Many servers
   * support additional variables. For example, Apache normally also
   * passes:
   *   REQUEST_URI, SCRIPT_FILENAME, SERVER_ADMIN, SERVER_SIGNATURE, and
   *   DOCUMENT_ROOT.
   * But these are non-standard, and you may run into problems if you want
   * to run your program together with other servers. You can still access
   * them using [get] below.
   *)

  val gateway_interface  : ?state:state -> unit -> string
  val server_software    : ?state:state -> unit -> string
  val server_name        : ?state:state -> unit -> string
  val server_protocol    : ?state:state -> unit -> string
  val server_port        : ?state:state -> unit -> int option
  val request_method     : ?state:state -> unit -> string
  val path_info          : ?state:state -> unit -> string
  val path_translated    : ?state:state -> unit -> string
  val script_name        : ?state:state -> unit -> string
  val query_string       : ?state:state -> unit -> string
  val remote_host        : ?state:state -> unit -> string
  val remote_addr        : ?state:state -> unit -> string
  val auth_type          : ?state:state -> unit -> string
  val remote_user        : ?state:state -> unit -> string
  val remote_ident       : ?state:state -> unit -> string
  val http_user_agent    : ?state:state -> unit -> string
    (* Some of these variables may not be available. In this case,
     * an EMPTY STRING or None are returned (for simplicity).
     *
     * For the implicit state (i.e. you don't pass ~state), these 
     * variables are extracted from the process environment. For 
     * self-created state variables, the variables are initially empty.
     *)

  val protocol_is_http  : ?state:state -> unit -> bool
  val protocol_is_https : ?state:state -> unit -> bool
    (* Convenience functions evaluating SERVER_PROTOCOL and HTTPS *)

  val set_variable : 
            ?state:state ->               (* default: implicit state *)
            ?gateway_interface:string ->
            ?server_software:string ->
            ?server_name:string ->
	    ?server_protocol:string ->
	    ?server_port:int option ->
	    ?request_method:string ->
	    ?path_info:string ->
	    ?path_translated:string ->
	    ?script_name:string ->
	    ?query_string:string ->
	    ?remote_host:string ->
	    ?remote_addr:string ->
	    ?auth_type:string ->
	    ?remote_user:string ->
	    ?remote_ident:string ->
	    ?http_user_agent:string ->
	    unit ->
	      unit
    (* Set the CGI variables. Only the variables corresponding to the 
     * passed arguments are modified, the other variables remain unchanged.
     *)

  (* Support for cookies: *)

  val cookies : ?state:state -> unit -> (string * string) list
    (* Returns the decoded cookies from HTTP_COOKIE *)


  (* The following functions are on a lower level: *)

  val get : ?state:state -> string -> string
    (* Returns the value of the named CGI variable, or "" *)

  val set : ?state:state -> string -> string -> unit
    (* Sets the CGI variable to the passed value *)

  val set_list : ?state:state -> (string * string) list -> unit
    (* Sets all CGI variables by (name,value) pairs *)

end


(**********************************************************************)
(* compatibility                                                      *)
(**********************************************************************)

(* Now, the compatibility functions. Don't use them in new programs. *)

val parse_args : unit -> (string * string) list
    (* Decodes the arguments of the CGI and returns them as an association list
     * Works whatever the method is (GET or POST)
     *
     * NOTE: This function is DEPRECATED and exists only for backward
     * compatibility. Use 
     *   parse_arguments default_config;
     *   let args = arguments()
     * instead
     *)

val parse_args_with_mimetypes : unit -> (string * string * string) list
    (* Like parse_args, but returns also the MIME type.
     * The triples contain (name, mime_type, value).
     * If an encoding was chosen that does not transfer the MIME type,
     * "text/plain" is returned.
     *
     * THIS FUNCTION SHOULD BE CONSIDERED AS DEPRECATED.
     * It was included in netstring-0.4, but most people want not only
     * the MIME type. parse_arguments should be used instead.
     *)

(**********************************************************************)
(* The Operators module                                               *)
(**********************************************************************)

(* If you open the Operators module, you can write
 *     !% "name"      instead of     argument "name", and
 *     !$ "name"      instead of     argument_value "name"
 * The Operators module uses always the implicit state of Cgi.
 *)

module Operators : sig
  val ( !% ) : string -> argument
      (* same as 'argument' above *)
  val ( !$ ) : string -> string
      (* same as 'argument_value' above *)
end

(**********************************************************************)
(* Low-level functions                                                *)
(**********************************************************************)

(* Encoding/Decoding within URLs:
 *
 * The following two functions perform the '%'-substitution for
 * characters that may otherwise be interpreted as metacharacters.
 *
 * See also the Netencoding module. This interface contains these functions
 * to keep the compatibility with the old Cgi module.
 *)

val decode : string -> string
val encode : string -> string

(* URL-encoded parameters:
 *
 * The following two functions create and analyze URL-encoded parameters.
 * Format: name1=val1&name2=val2&...
 *
 * See also the Netencoding module. This interface contains these functions
 * to keep the compatibility with older versions of the Cgi module.
 *)

val mk_url_encoded_parameters : (string * string) list -> string
val dest_url_encoded_parameters : string -> (string * string) list

(* Form-encoded parameters:
 *
 * According to: RFCs 2388, 2183, 2045, 2046
 *
 * General note: This is a simple API to encode/decode form-encoded parameters.
 * Especially, it is not possible to pass the header of the parts through
 * this API.
 *)

val mk_form_encoded_parameters : argument list ->
                                     (string * string)
    (* This function takes CGI arguments and encodes them.
     * The result is (parstr, boundary), where 'parstr' is the
     * single form-encoded parameter string, and 'boundary' is the
     * boundary to separate the message parts.
     *)

val  dest_form_encoded_parameters : string -> boundary:string -> config ->
                                       argument list
    (* The first argument is the form-encoded parameter string.
     * The second argument is the boundary (extracted from the mime type).
     * Third argument: Only the workarounds component is used.
     * The result is
     * the corresponding list of arguments (all in memory).
     * If there is a format error, the function fails.
     * Note: embedded multipart/mixed types are returned as they are,
     *   and are not recursively decoded.
     * Note: The content-transfer-encodings "7bit", "8bit", "binary",
     *   "base64", and "quoted-printable" are supported.
     * Note: Parameter names which include spaces or non-alphanumeric
     *   characters may be problematic (the rules of RFC 2047 are NOT applied).
     * Note: The returned MIME type is not normalized.
     *)

val dest_form_encoded_parameters_from_netstream
    : Netstream.in_obj_stream -> boundary:string -> config -> argument list
    (* let arglist = dest_form_encoded_parameters_from_netstream s b c:
     * Reads the form-encoded parameters from netstream s. The boundary
     * is passed in b, and the configuration in c.
     * A list of arguments is returned.
     *
     * See also dest_form_encoded_parameters.
     *
     * Restriction: In contrast to dest_form_encoded_parameters, this
     * function is not able to handle the content-transfer-encodings
     * "base64" and "quoted-printable". (This is not really a restriction
     * because no browser uses these encodings in conjunction with HTTP.
     * This is different if mail transport is chosen. - The reason for
     * this restriction is that there are currently no stream functions
     * for decoding.)
     *)

(* Private functions: *)

val init_mt : (unit -> unit) -> (unit -> unit) -> unit

val status_line : status -> string

(**********************************************************************)
(* How to catch errors                                                *)
(**********************************************************************)

(* There is no magic trick such that runtime errors are caught and
 * reported appropriately. You must do it yourself, but it is relatively
 * simple.
 *
 * In the following dicussion we assume that the contents of the page
 * are generated by a function "generate", and that the whole request
 * is handled by a function "process_request".
 *
 * VARIANT 1:
 *
 * - It is directly written to stdout:
 *   let generate() =
 *     print_string "<HTML>...";
 * - Exceptions are caught and turned into error messages:
 *   let process_request() =
 *     header "";
 *     try
 *       generate();
 *       flush stdout;
 *     with
 *       error ->
 *         error_message (Printexc.to_string error)
 * - The disadvantage of this variant is that the error message can
 *   appear somewhere in the middle of the regular page, or (worse) that
 *   the error message is invisible.
 * - The advantage is that output is displayed immediately and that
 *   no additional memory is required.
 *
 * VARIANT 2:
 *
 * - The generated contents are collected in a buffer:
 *   let generate buf =
 *     Buffer.add_string buf "<HTML>...";
 * - Exceptions are caught and turned into error messages:
 *   let process_request() =
 *     header "";
 *     try
 *       let buf = Buffer.create 20000 in
 *       generate buf;
 *       Buffer.output_buffer stdout buf
 *       flush stdout;
 *     with
 *       error ->
 *         error_message (Printexc.to_string error)
 * - The advantage is that error messages are not mixed up with
 *   regular output.
 * - The disadvantage is that output is deferred until the page
 *   is complete. Furthermore, more main memory is required.
 *
 * VARIANT 3:
 *
 * - The generated contents are collected in a file:
 *   let generate file =
 *     output_string file "<HTML>...";
 * - Exceptions are caught and turned into error messages:
 *   let process_request() =
 *     header "";
 *     try
 *       let filename = Filename.temp_file "cgioutput" ".data" in
 *       let wfile = open_out_bin filename in
 *       let rfile = open_in_bin filename in
 *       Sys.remove filename;
 *       generate wfile;
 *       close_out wfile;
 *       <Read rfile and write contents to stdout>;
 *       close_in rfile;
 *       flush stdout;
 *     with
 *       error ->
 *         error_message (Printexc.to_string error)
 * - The advantage is that error messages are not mixed up with
 *   regular output. Additional main memory is not required.
 * - The disadvantage is that output is deferred until the page
 *   is complete. This variant is the slowest.
 *)


(**********************************************************************)
(* Compatibility with CGI library by J.-C. Filliatre                  *)
(**********************************************************************)

(* The following functions are compatible with J.-C. Filliatre's CGI
 * library:
 *
 * parse_args, header, this_url, decode, encode.
 *
 * Note that the new implementation of parse_args can be safely invoked
 * several times.
 *
 * Since release 0.8, Netstring's CGI implementation is again thread-safe.
 *)


(* ======================================================================
 * History:
 *
 * $Log$
 * Revision 2.5  2002/01/14 01:05:41  stolpmann
 * 	Added the comment that this module is DEPRECATED!
 *
 * Revision 2.4  2001/12/28 21:17:32  stolpmann
 * 	Using the new OO interface of Netstream.
 *
 * Revision 2.3  2001/09/27 22:04:54  stolpmann
 * 	Exporting the function status_line now. It's useful in
 * Netcgi.
 *
 * Revision 2.2  2001/09/24 21:37:05  stolpmann
 * 	Removed the comment that mk_form_encoded_parameters is
 * unimplemented.
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.10  2001/09/01 15:34:24  gerd
 * 	Updated docs.
 *
 * Revision 1.9  2001/08/30 19:45:19  gerd
 * Added a lot of new features:
 *
 * - Testing: If the CGI program is started from the command-line it recognizes
 *   several command-line options. Furthermore, there is an interactive
 *   mode.
 * - It is now possible to have multiple instances of the CGI module.
 * - The CGI environment and the arguments can be read from arbitrary
 *   sources. (Not only stdin, and the process environment.)
 * - CGI arguments can be updated.
 * - Enhanced "header" function; it includes cache-control directives,
 *   and cookies
 * - There is now a "redirect" function for server redirects.
 * - "this_url" has been enhanced (supports non-standard ports, https,
 *   and the inclusion of the CGI parameters)
 * - There is now a layer representing the CGI environment variables
 *   (module Env).
 *
 * Revision 1.8  2000/06/25 22:34:43  gerd
 * 	Added labels to arguments.
 *
 * Revision 1.7  2000/06/25 21:40:36  gerd
 * 	Added printer.
 *
 * Revision 1.6  2000/06/25 21:15:48  gerd
 * 	Checked thread-safety.
 *
 * Revision 1.5  2000/05/16 22:28:13  gerd
 * 	New "workarounds" config component.
 *
 * Revision 1.4  2000/04/15 16:47:27  gerd
 * 	Last minor changes before releasing 0.6.
 *
 * Revision 1.3  2000/04/15 13:09:01  gerd
 * 	Implemented uploads to temporary files.
 *
 * Revision 1.2  2000/03/02 01:15:30  gerd
 * 	Updated.
 *
 * Revision 1.1  2000/02/25 15:21:12  gerd
 * 	Initial revision.
 *
 *
 *)
