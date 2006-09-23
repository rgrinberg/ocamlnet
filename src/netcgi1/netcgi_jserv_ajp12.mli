(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Implementation of the AJP/1.2 protocol *)

(** This module implements the core of the AJP/1.2 protocol. This protocol is supported
 * by both the old mod_jserv and by the newer mod_jk (Jakarta/Tomcat).
 * Note however, that mod_jserv passes more variables, and that it is fully CGI
 * compatible. In contrast to this, mod_jk omits some variables (e.g.
 * PATH_INFO, and SCRIPT_NAME). These variables must be fixed by special
 * rules, see below. Furthermore, mod_jk has a different format
 * of the property file, but it is not (yet) accepted by this library.
 * mod_jk does not support authentication for AJP/1.2. So try to get 
 * mod_jserv if possible.
 *)

open Netcgi_jserv
open Netchannels

val serve_connection : 
      ?config:(Netcgi_env.cgi_config) ->
      ?https:bool ->                           (* default: false *)
      ?jk_servletSubString:string ->           (* default: "/servlets/" *)
      (string option -> string option -> Netcgi_env.cgi_environment -> unit) ->
        (* request handler *)
      auth option ->
      in_obj_channel ->
      out_obj_channel ->
        unit
  (** Serves the connection designated by the [in_obj_channel] and the
   * [out_obj_channel]. The function ensures that both channels are closed
   * before it returns to the caller (and before it raises an exception).
   * 
   * If an authentication record [auth] is passed, the connection is authenticated
   * first. If this fails, the channels will be closed immediately, and the
   * function will return normally.
   *
   * The request is read from the [in_obj_channel], and a [cgi_environment]
   * is created from it. The request handler is called with this
   * environment and two extra string option arguments. The first is the
   * zone, and the second is the servlet identifier. The request handler
   * may look like a small CGI program:
   *
   * {[
   * (fun zone servlet env ->
   *   let cgi = new std_activation ~env () in
   *   cgi # set_header();
   *   cgi # output # output_string  "Hello world!";
   *   cgi # output # commit_work();
   * )
   * ]}
   *
   * If the request arriving at the [in_obj_channel] is a signal, the function
   * will close both channels, and will raise either [Signal_restart] or 
   * [Signal_shutdown].
   *
   * @param config This is the configuration of the [cgi_environment]. It defaults
   *   to {!Netcgi_env.default_config}
   * @param https Because AJP/1.2 does not pass the [HTTPS] variable, it is necessary
   *   to set this argument to [true] if the server is secure.
   * @param jk_servletSubString The fields [servlet], [path_info], and [script_name]
   *   are always empty if mod_jk is used. There is a fixup routine that
   *   is controlled by this optional argument, and that will be invoked if
   *   servlet is empty. The fixup is
   *   that the string [jk_servletSubString] is searched in the request URI,
   *   and if it is found, the following modifications will be applied:
   *   The [servlet] is set to the path component following [jk_servletSubString].
   *   The [path_info] is set to the rest of the URI. The [script_name] is set
   *   to the beginning of the URI until the servlet name (inclusive).
   *   Other fields are not modified. If the string [jk_servletSubString] is
   *   not found, or if it is the empty string, the [servlet] name and the
   *   [script_name] will be set to the request URI, and the [path_info] will be set
   *   to the empty string.
   *)

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.5  2004/07/24 18:30:49  stolpmann
 * 	ocamldoc
 *
 * Revision 1.4  2002/04/12 21:48:56  stolpmann
 * 	Better support for mod_jk.
 *
 * Revision 1.3  2002/02/04 15:21:31  stolpmann
 * 	Updated comments.
 *
 * Revision 1.2  2001/12/09 02:43:44  stolpmann
 * 	Fixed minor error.
 *
 * Revision 1.1  2001/12/09 01:29:01  stolpmann
 * 	Initial revision.
 *
 * 
 *)
