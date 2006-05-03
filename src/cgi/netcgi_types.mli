(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Basic types for CGI and related protocols *)


exception Resources_exceeded
  (** Raised when the CGI input is longer than the configured maximum *)


(* {b Representation of CGI arguments} 
 *
 * There are two representations of CGI arguments, {!Netcgi_types.simple_message}
 * and {!Netmime.mime_message}. The first consists only of a name and a value
 * while the second representation has additionally a MIME header.
 *
 * The [simple_message] arguments are used when the transport mechanism
 * does not allow the inclusion of a header. This is the case for all
 * [GET] parameters, and for [POST] parameters in URL-encoded format.
 * The [mime_message] representation is used if the posted message is
 * form-encoded.
 *
 * Independently of the representation, the arguments are stored somewhere.
 * Supported stores are currently [`Memory] and [`File]. 
 * (Restriction: A [simple_message] can only be stored in [`Memory] for now.)
 *)

class type simple_message = Netmime.mime_body
 (** A [simple_message] stores the value of the CGI argument as an
  * unstructured string value. It happens that {!Netmime.mime_body}
  * is already a class type for such unstructured values, so we are
  * reusing this type here.
  *)

(* The types mime_header and mime_message are now defined in Netmime. *)

type store =
  [ `Memory
  | `File of string
  ]
  (** Determines where the data of the CGI argument are actually stored.
   * - [`Memory]: In an O'Caml string
   * - [`File name]: In the file [name]. The file contains the value of
   *   the argument after all transfer-related encodings have been
   *   removed (i.e. URL-encoding, and MIME transfer encodings).
   *)

type representation =
  [ `Simple of simple_message
  | `MIME of Netmime.mime_message
  ]
(** Representations of CGI arguments:
 * - [`Simple msg]: The argument is unstructured
 * - [`MIME msg]: The argument has a MIME header in addition to the value
 *)

(* In general, the [store] is just a container, and the interpretation
 * of the [store] depends on the [representation] of the argument. For the
 * two defined representations, the container holds the decoded value of
 * the argument. For future extensions of [representation], the [store] might
 * be interpreted differently, though.
 *)

(* The exception Value_is_immutable has been renamed to just "Immutable",
 * and its definition has been moved to the module Netmime.
 *)


(** The interface of CGI argument objects *)
class type cgi_argument =
object
  method name : string
    (** The name of the CGI argument *)
  method value : string
    (** The value of the CGI argument, after all transfer encodings have
     * been removed. If the
     * value is stored in a file, the file will be loaded.
     *)
  method open_value_rd : unit -> Netchannels.in_obj_channel
    (** Opens the contents of the value as an input channel. This works
     * for all kinds of arguments, independent of [store] and [representation].
     *)
  method ro : bool
    (** Whether this argument is read-only or not *)
  method store : store
    (** Returns where the argument value is stored *)
  method content_type : string
    (** Returns the content type of the header, or ["text/plain"] when the
     * header is missing. Parameters of the content type have been stripped
     * off (e.g. [charset]).
     *)
  method content_type_params : (string * Mimestring.s_param) list
    (** The parameters of the content type of the header, or [[]] when
     * the header is missing. Below you find access method for frequently
     * used parameters.
     *)
  method charset : string
    (** The [charset] parameter of the content type of the header, or [""]
     * when there is no such parameter, or no header.
     *)
  method filename : string option
    (** The [filename] parameter found in the header of file uploads.
     * When present, [Some name] is returned, and [None] otherwise.
     *)
  method representation : representation
    (** The representation of the CGI argument *)
  method finalize : unit -> unit
    (** Arguments stored in temp files must be deleted when the argument is no 
     * longer used. You can call [finalize] to delete such files. The
     * method does not have any effect when [store = `Memory].
     * The method does never raise any exceptions. If the file does no longer
     * exist (e.g. because it is moved away), or if there are any problems
     * deleting the file, the error will be suppressed.
     * The [finalize] method is not registered in the garbage collector.
     * You can do that, but it is usually better to call this method manually.
     * [cgi_activation] supports this.
     *)

  method set_value : string -> unit
    (** If the [representation] supports mutable values, the value is set
     * to the passed string. The other properties of the argument are not
     * modified.
     *
     * If the [representation] does not support this feature, the exception
     * {!Netmime.Immutable} will be raised.
     *)
  method open_value_wr : unit -> Netchannels.out_obj_channel
    (** Opens the value for writing. The current value is overwritten. 
     * If the value is immutable, the exception {!Netmime.Immutable} will 
     * be raised.
     *)
end


type cgi_cookie = Nethttp.cookie =
    { cookie_name : string;
        (** The name of the cookie *)
      cookie_value : string;
        (** The value of the cookie. There are no restrictions on the
	 * value of the cookie
	 *)
      cookie_expires : float option;
        (** Expiration:
	 *  - [None]: the cookie expires when the browser session ends.
         *  - [Some t]: the cookie expires at the time [t] (seconds since
	 *    the epoch)
         *) 
      cookie_domain : string option;
        (** Cookies are bound to a certain domain, i.e. the browser sends
	 * them only when web pages of the domain are requested:
	 *
	 *  - [None]: the domain is the hostname of the server
         *  - [Some domain]: the domain is [domain]
         *)
      cookie_path : string option;
        (** Cookies are also bound to certain path prefixes, i.e. the browser
	 * sends them only when web pages at the path or below are requested.
	 *
	 * - [None]: the path is script name + path_info
         * - [Some p]: the path is [p]. With [Some "/"] you can disable the
	 *   path restriction completely.
         *)
      cookie_secure : bool;
        (** Cookies are also bound to the type of the web server: 
	 * [false] means servers without SSL, [true] means servers with
	 * activated SSL ("https").
	 *)
    }


type status = Nethttp.http_status

type request_method =
  [ `GET
  | `HEAD
  | `POST
  | `DELETE
  | `PUT of cgi_argument
  ]
  (** The supported request methods:
   * - [`GET]: Side effect-free request of a web resource
   * - [`POST]: Request with side effects
   * - [`HEAD]: Only the header of the corresponding [`GET] are requested
   * - [`DELETE]: Request to delete the web resource
   * - [`PUT arg]: Request to upload the web resource
   *)

  (* Note that there is also a configuration option in the environment
   * that specifies which methods are allowed at all.
   *)    

type cache_control =
    [ `No_cache
    | `Max_age of int
    | `Unspecified
    ]
  (** This is only a small subset of the HTTP 1.1 cache control features,
   * but they are usually sufficient, and they work for HTTP/1.0 as well.
   * The directives mean:
   * - [`No_cache]:
   *   Caches are disabled. The following headers are sent:
   *   [Cache-control: no-cache], [Pragma: no-cache], [Expires:] (now - 1 second)
   * - [`Max_age n]:
   *   Caches are allowed to store a copy of the response for [n] seconds.
   *   After that, the response must be revalidated.
   *   [Cache-control: max-age n], [Cache-control: must-revalidate],
   *   [Expires:] (now + [n] seconds)
   * - [`Unspecified]:
   *   No cache control header is added to the response.
   *
   * Notes:
   * - Cache control directives only apply to GET requests; POST requests
   *   are never cached
   * - Not only proxies are considered as cache, but also the local disk
   *   cache of the browser
   * - HTTP/1.0 did not specify cache behaviour as strictly as HTTP/1.1
   *   does. Because of this the [Pragma] and [Expires] headers are sent, too.
   *   These fields are not interpreted by HTTP/1.1 clients because 
   *   [Cache-control] has higher precedence.
   *)

type query_string_spec =
    [ `Initial | `Current | `Args of cgi_argument list | `None ]
 (** Determines how the query part of URLs is generated:
  *
  * - [`Initial]: The query string is created from the initial
  *    CGI arguments
  * - [`Current]: The query string is created from the current
  *    CGI arguments
  * - [`Args l]: The query string is created from the specified argument list
  * - [`None]: The query string is omitted
  *)

type other_url_spec =
    [ `Env | `This of string | `None ]
  (** Determines how an URL part is generated:
   *
   * - [`Env]: Take the value from the environment
   * - [`This v]: Use this value [v]. It must already be URL-encoded.
   * - [`None]: Do not include this part into the URL
   *)


(** The common interface of CGI activation objects *)
class type cgi_activation =
object
  method environment : Netcgi_env.cgi_environment
    (** The CGI environment object. This object is the "outer layer" of the
     * activation object that connects it with real I/O channels.
     *)

  method request_method : request_method
    (** The HTTP method *)

  (** Initial arguments
   *
   * Initial arguments are the CGI arguments at the time the arguments
   * were parsed from the environment.
   *)

  method initial_arguments : (string * cgi_argument) list
    (** The complete list of initial arguments *)

  method initial_argument : string -> cgi_argument
    (** Returns a certain initial argument, or raises [Not_found] *)

  method initial_argument_value : ?default:string -> string -> string
    (** Returns the value of the initial argument as string. If the
     * argument does not exist, the [default] is returned. The
     * [default] defaults to [""].
     *)

  method initial_multiple_argument : string -> cgi_argument list
    (** Returns a certain initial argument that occurs several times in
     * the set of arguments
     *)

  (** Current arguments
   *
   * The current arguments can be modified, but they are initialized
   * to the initial arguments at object creation time:
   *)

  method arguments : (string * cgi_argument) list
    (** The complete list of current arguments *)

  method argument : string -> cgi_argument
    (** Returns a certain current argument, or raises [Not_found] *)

  method argument_value : ?default:string -> string -> string
    (** Returns the value of the current argument as string. If the
     * argument does not exist, the [default] is returned. The
     * [default] defaults to [""].
     *)

  method multiple_argument : string -> cgi_argument list
    (** Returns a certain current argument that occurs several times in
     * the set of arguments
     *)

  (** Modify the set of current arguments *)

  method set_arguments : ?fin:bool -> cgi_argument list -> unit
    (** Replaces the set of current arguments with a new set.
     * @param fin If [true], the default, the arguments of the old set
     *   that are not member of the new set are finalized
     *)

  method update_argument : ?fin:bool -> cgi_argument -> unit
    (** The passed argument replaces the current argument
     *    (or multiple argument) with the same name; if there is no such
     *    argument, the passed argument is added to the list of current args
     * @param fin If [true], the default, the replaced arguments are
     *    finalized (unless they happen to be the same as the new argument)
     *)

  method update_multiple_argument : ?fin:bool -> cgi_argument list -> unit
    (** The passed arguments must all have the same name. They replace
     * the current argument (or multiple argument) with the same name;
     * if there is no such argument, the passed arguments are added to
     * the set of current arguments.
     * @param fin If [true], the default, the replaced arguments are
     *    finalized (unless they happen to be the same as the new arguments)
     *)

  method delete_argument : ?fin:bool -> string -> unit
    (** Deletes all arguments with the passed name.
     * @param fin If [true], the default, the deleted arguments are
     *  finalized
     *)

    (* DISCUSS: We are not sure whether it is good or bad to make the
     * arguments mutable here.
     * If mutability is only for the "url" method, this can be solved
     * in a purely functional way (pass the changed arguments to "url"
     * directly).
     *)

  (** Self-referencing URL *)

  method url : ?protocol:Netcgi_env.protocol ->   
                                            (* default: from environment *)
               ?with_authority:other_url_spec ->        (* default: `Env *) 
               ?with_script_name:other_url_spec ->      (* default: `Env *)
               ?with_path_info:other_url_spec ->        (* default: `Env *)
               ?with_query_string:query_string_spec ->  (* default: `None *)
               unit ->
		 string
    (** Returns the URL of the current CGI activation.
     *
     * - [protocol]: The URL scheme. By default, the URL scheme is used
     *   that is described in the environment
     * - [with_authority]: Whether to include authority part of the URL, and
     *   if yes, from which source. Default: [`Env]
     * - [with_script_name]: Whether to include the part of the URL path
     *   identifying the CGI script, and if yes, from which source.
     *   Default: [`Env]
     * - [with_path_info]: Whether to include the rest of the URL path
     *   exceeding the script name, and if yes, from which source.
     *   Default: [`Env]
     * - [with_query_string]: Whether to include the query string,
     *   and if yes, which one. Default: [`None], i.e. no query string
     *)

  (** Generating Output *)

  method output : Netchannels.trans_out_obj_channel
    (** The output channel to which the generated content is intended to
     * be written.
     *
     * The output channel may have transactional semantics, and because of
     * this, it is an [trans_out_obj_channel]. Implementations are free
     * to support transactions or not.
     *
     * After all data have been written, the method [commit_work] must be
     * called, even if there is no support for transactions.
     *
     * Simple Example:
     *
     * {[
     * cgi # output # output_string "Hello world!\n";
     * cgi # output # commit_work()
     * ]}
     *
     * Example for an error handler and a transaction buffer:
     * If an error happens, it is possible to roll the channel back, and
     * to write the error message.
     * {[
     * try
     *   cgi # set_header ... ();
     *   cgi # output # output_string "Hello World!"; ...
     *   cgi # output # commit_work();
     * with
     *   err ->
     *     cgi # output # rollback_work();
     *     cgi # set_header ... ();
     *     cgi # output # output_string "Software error!"; ...
     *     cgi # output # commit_work();
     * ]}
     *)

  method set_header :
           ?status:status -> 
	   ?content_type:string ->
           ?cache:cache_control ->
           ?filename:string ->
           ?language:string ->
           ?script_type:string ->
           ?style_type:string ->
           ?set_cookie:cgi_cookie list ->
           ?fields:(string * string list) list ->
	   unit ->
	     unit
    (** Sets the header.
     *
     * When the output channel supports transactions, it is possible to
     * set the header until the channel is commited for the first time.
     * When there is no support for transactions, the header must be
     * set before the first byte of output is written.
     *
     * If [set_header] is called several times, {i all} of the header fields
     * are overwritten.
     *
     * - [status]: Sets the HTTP status of the reply. Defaults to "no status",
     *   but the server normally complements an [`Ok] status in this case
     * - [content_type]: Sets the content type. Defaults to "text/html" if the
     *   content type is not yet set.
     * - [cache]: Sets the cache behavior for replies to GET requests. The
     *   default is [`Unspecified]. {b It is strongly recommended to specify
     *   the caching behaviour!!!} You are on the safe side with [`No_cache],
     *   forcing every page to be regenerated. If your data do not change
     *   frequently, [`Max_age n] tells the caches to store the data at most
     *   [n] seconds.
     * - [filename]: Sets the filename associated with the page. This filename
     *   is taken for the "save as..." dialog. Default: no filename.
     *   Note: It is bad practice if the filename contains problematic characters
     *   (backslash, double quote, space), or the names of directories
     * - [script_type]: Sets the language of the script tag (for HTML replies).
     *   It is recommended to use this field if there are [ONXXX] attributes
     *   containing scripts before the first [<SCRIPT>] element, because you
     *   cannot specify the script language for the [ONXXX] attributes otherwise.
     *   [script_type] must be a media type, e.g. "text/javascript".
     *   Default: no language is specified.
     * - [style_type]: Sets the language of the style tag (for HTML replies).
     *   It is recommended to use this field if there are [STYLE] attributes
     *   containing scripts before the first [<STYLE>] element, because you
     *   cannot specify the style language for the [STYLE] attributes otherwise.
     *   [style_type] must be a media type, e.g. "text/css". 
     *   Default: no language is specified.
     * - [set_cookie]: Sets a number of cookies.
     *   Default: [[]]
     *   You can query the cookies using [environment#cookies].
     * - [fields]: Sets other fields of the header.
     *)

  method set_redirection_header :
           string ->
	     unit
    (** Sets the header such that a redirection to the specified URL
     * is performed. If the URL begins with "http:" the redirection directive is
     * passed back to the client, and the client will repeat the request for
     * the new location. If the URL begins with "/", the server performs the
     * redirection, and it is invisible for the client.
     *)

    (* {b Notes about the header}
     * 
     * The header is automatically prepended to the selected output channel.
     * You must call [set_header] or [set_redirection_header] (at least) once.
     * If the output channel supports transactions, it is sufficient to call one of
     * the methods until [commit_work] is invoked, and it is possible to call
     * them several times.
     * If the class does not support transactions, only one of the methods
     * must be called, and it must be called before any other output is
     * generated.
     *
     * The header is treated in a special way. It is passed to the environment
     * object, and the [send_output_header] method of this object is called at
     * the right moment, usually when the output data are committed. This
     * means that the environment object is responsible for sending the
     * header to the client, and not the implementation of [cgi_activation].
     *)

  (*
   * To avoid confusion: There are two output channels, and they are
   * layered. In cgi_environment, the output channel is usually a 
   * wrapped stdout, so writing to this channel writes directly to
   * stdout. In cgi_activation, the output channel is usually a
   * transactional channel (a channel that buffers output until it is
   * committed). In the current design, the header is _not_ written
   * to the transactional channel, but directly into variables of
   * cgi_environment, and cgi_environment is told when the header is
   * valid and should be sent to stdout (usually when the data of the
   * transactional channel is committed). 
   *
   * This looks complicated, but I think there are several advantages:
   * - It is possible to change header fields at every moment before
   *   the commitment happens. For example, it is possible to set
   *   the content-length field which is normally only known just
   *   at the time of the commit operation.
   * - The cgi_environment object can process the header; for example
   *   it can fix header fields.
   * - It is simpler to connect to environments which transport the
   *   header in non-standard ways. Example: Assume that the environment
   *   is the web server process (e.g. we are an Apache module). Typically
   *   the header must be stored in different structures than the 
   *   body of the message.
   *)

  (* later: 
   * method send_mime_message : mime_message -> unit
   * method begin_multipart_message : XXX -> unit
   * method end_multipart_message : XXX -> unit
   *)

  (** Cleaning Up *)

  method finalize : unit -> unit
    (** This method calls [finalize] for every CGI argument to ensure that
     * all files are deleted.
     * It does not close the in/out channels, however.
     * This method is not registered in the garbage collector, and it is
     * a bad idea to do so.
     *)

end



(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.12  2005/07/25 22:36:40  stolpmann
 * Merge of nethttpd.
 * Adding the "overview" section to Nethttpd_types.
 * Cosmetic fixes.
 * Release updates.
 *
 * Revision 1.11.2.1  2005/04/30 16:31:17  stolpmann
 * Integration of the new Nethttpd component into Ocamlnet.
 *
 * Revision 1.11  2004/07/23 21:19:32  stolpmann
 * 	Added ocamldoc
 *
 * Revision 1.10  2004/07/01 12:56:20  stolpmann
 * 	Removed dependency on Cgi.
 *
 * Revision 1.9  2002/10/24 23:47:48  stolpmann
 * 	Support for the HEAD method.
 * 	Workaround for a bug in MSIE: Empty cookies are represented
 * in the wrong way
 *
 * Revision 1.8  2002/01/14 01:10:50  stolpmann
 * 	cgi_argument bases now on Netmime.mime_message.
 *
 * Revision 1.7  2001/11/17 23:27:07  stolpmann
 * 	Changed options for [url] method: The with_XXX options
 * have the values [ `Env | `This of string | `None ] instead of
 * just bool.
 *
 * Revision 1.6  2001/10/14 15:45:52  stolpmann
 * 	Added a comment.
 *
 * Revision 1.5  2001/10/07 19:46:17  stolpmann
 * 	New comments, esp. set_header.
 *
 * Revision 1.4  2001/10/04 01:07:15  stolpmann
 * 	Moved from directory /src/netstring to /src/cgi
 *
 * Revision 1.3  2001/09/30 00:02:52  stolpmann
 * 	Documentation only.
 *
 * Revision 1.2  2001/09/28 21:21:53  stolpmann
 * 	Enhanced MIME objects.
 * 	Improved handling of finalization.
 *
 * Revision 1.1  2001/09/27 21:59:00  stolpmann
 * 	Initial revision
 *
 * 
 *)
