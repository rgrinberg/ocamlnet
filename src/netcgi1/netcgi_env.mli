(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Access to the environment for CGI and related protocols *)

type input_mode = 
    [ `Standard (* | `Direct *) ]
    (** Determines how to read the request:
     * - [`Standard]: Only the request body is read from the input
     *    channel (CGI standard)
     * - Not yet implemented: [`Direct]: Both header and body of the
     *   request are read from the input channel
     *)

type input_state =
    [ `Start | 
      `Receiving_header | `Received_header |
      `Receiving_body | `Received_body
    ]
    (** The input processing state:
     * - [`Start]: Input data have not yet been received
     * - [`Receiving_header]: The request header is currently being 
     *   received
     * - [`Received_header]: The request header has been completely
     *   received, and nothing of the request body has yet been
     *   received
     * - [`Receiving_body]: The request body is currently being 
     *   received
     * - [`Received_body]: The request body has been completely
     *   received
     *
     * Transition diagram:
     * {[ 
     * `Start -> 
     * `Receiving_header -> 
     * `Received_header ->
     * `Receiving_body -> 
     * `Received_body ]}
     *)

type output_mode =
    [ `Standard (* | `Direct *) ]
    (** Determines how to deliver the response:
     * - [`Standard]: The format of the response header has CGI format,
     *   followed by the response body
     * - Not yet implemented: [`Direct]: The format of the response
     *   header has HTTP format, followed by the response body. This
     *   is also known as "non-parsed header" format.
     *)

type output_state =
    [ `Start | 
      `Sending_header      | `Sent_header | 
      `Sending_body        | `Sent_body |
      `Sending_part_header | `Sent_part_header |
      `Sending_part_body   | `Sent_part_body |
      `End
    ]
    (** The output processing state:
     * - [`Start]: Output has not yet been sent
     * - [`Sending_header]: The response header is currently being sent
     * - [`Sent_header]: The response header has been completely sent,
     *   and nothing of the body has yet been sent
     * - [`Sending_body]: The response body is currently being sent
     * - [`Sent_body]: The response body has been sent up to a
     *   check point
     * - [`End]: The response has been completely sent
     *
     * Transition diagram:
     * {[
     *              `Start ->
     *              `Sending_header ->
     *              `Sent_header ->
     *          +-> `Sending_body 
     *          |      |
     *          |      V
     *          +-- `Sent_body 
     *                 |
     *                 V
     *              `End ]}
     *
     * The state [`Sent_body] is reached when the output data are 
     * committed. It is possible to continue with another transaction,
     * which would mean to go back to [`Sending_body], or to finish
     * the body completely, by going to [`End].
     *
     * Extension for multi-part response messages (e.g. needed for
     * server push, not yet implemented):
     * - [`Sending_part_header]: The header of a message part is being
     *   sent
     * - [`Sent_part_header]: The header of a message part has been
     *   completely sent
     * - [`Sending_part_body]: The body of a message part is being
     *   sent
     * - [`Sent_part_body]: The body of a message part has been sent
     *   up to a check point
     *)

type protocol_version = Nethttp.protocol_version
    (** Now defined in [Nethttp] *)

type protocol_attribute = Nethttp.protocol_attribute
    (** Now defined in [Nethttp] *)

type protocol = Nethttp.protocol
    (** Now defined in [Nethttp] *)

type workaround =
  [ `Work_around_MSIE_Content_type_bug
  | `Work_around_backslash_bug
  ]
  (** Indicates special behaviour:
   * - [`Work_around_MSIE_Content_type_bug]: Versions of the Internet
   *   Explorer send illegal content types. This workaround extracts
   *   the right data from the malformed data field
   * - [`Work_around_backslash_bug]: Almost all browsers send illegal
   *   backslash sequences when backslashes occur in filenames.
   *   This workaround accepts such sequences.
   *)


type cgi_config =
    { (* System: *)
      tmp_directory : string;  
        (** The directory where to create temporary files. This should be
	 * an absolute path name
	 *)
      tmp_prefix : string;
        (** The name prefix for temporary files. This must be a non-empty
	 * string. It must not contain '/'.
	 *)
      (* Limits: *)
      permitted_http_methods : string list;
        (** The list of accepted HTTP methods (uppercase letters) *)
      permitted_input_content_types : string list;
        (** The list of accepted content types in requests.
         * Content type parameters (like "charset") are ignored.
	 * If the list is empty, all content types are allowed.
	 *)
      input_content_length_limit : int;
        (** The maximum size of the request *)
      workarounds : workaround list;
        (** The list of enabled workarounds *)
    }

val default_config : cgi_config
(** The default configuration is:
 * - [tmp_directory]: one of /var/tmp, /tmp, C:\temp, .
 * - [tmp_prefix]: "netstring"
 * - [permitted_http_methods]: ["GET"], ["POST"]
 * - [permitted_input_content_types]: ["multipart/form-data"], 
 *     ["application/x-www-form-urlencoded"]
 * - [input_content_length_limit]: [maxint]
 * - [workarounds]: all
 *
 * To create a custom configuration, it is suggested to use this
 * syntax:
 * {[ let custom_config =
 *    { default_config with tmp_prefix = "my_prefix" }
 * ]}
 *)

(* DISCUSS: Is the configuration part of cgi_environment or cgi_activation?
 *)

(** The class type cgi_environment contains the resources by which
 * the CGI activation is connected to the "outer world". In particular,
 * the following applies:
 *
 * - CGI properties: These are the global properties of the CGI request
 *   such as the HTTP method, which HTTP server serves the request, and
 *   which client sent the request.
 *   For a classic CGI environment, the properties are the environment
 *   variables not beginning with ["HTTP_"], and neither ["CONTENT_LENGTH"]
 *   nor ["CONTENT_TYPE"].
 * - Input header: The header of the HTTP request. 
 *   For a classic CGI environment, the input header can be extracted
 *   from the process environment. It consists of all variables beginning
 *   with ["HTTP_"] and the variables ["CONTENT_LENGTH"] and ["CONTENT_TYPE"].
 * - Input channel: Over the input channel the HTTP request can be read in.
 *   The input state tracks which parts of the request have already be
 *   read.
 *   For a classic CGI environment, the input channel contains only the
 *   body of the request, and the (required) header field content-length
 *   specifies the length of the body in bytes.
 * - Output header: The header of the HTTP response.
 * - Output channel: Over the output channel the HTTP response is sent.
 *   The output state tracks which parts of the response have already been
 *   sent.
 *
 * The CGI environment cannot only be used for classic CGI but also for
 * non-standard ways of communication with the HTTP server. By design,
 * the header and the body of both the request and the response are
 * separated, and because of this every of these message parts can be
 * processed independently of the other parts.
 *
 * There is a certain protocol between the environment and the 
 * [cgi_activation] objects. 
 * - The [cgi_activation] object expects that the input state of the
 *   environment is at least [`Received_header] when it starts to 
 *   process the request. This means that it is the task of the
 *   environment to read the request header in.
 * - The [cgi_activation] object reads the request body from
 *   [input_ch], and modifies the input state as it reads the body
 * - The [cgi_activation] object sets the response header in the
 *   environment, and calls [send_output_header] when the right
 *   moment for sending the output header is reached. It does not
 *   write the response header to [output_ch] itself. This is the
 *   sole task of the [send_output_header] method of the environment.
 * - After the output header is sent, the [cgi_activation] object
 *   writes the response body to [output_ch]. The output state is
 *   modified by this object.
 *)
class type cgi_environment =
object
  (* Configuration: *)
  method config : cgi_config
    (** The CGI configuration *)

  (** Standard properties
   *
   * The following properties are standardised by CGI. The methods
   * return [""] or [None] (in the case of the port number) when 
   * the property is not available.
   *)

  method cgi_gateway_interface  : string
  method cgi_server_software    : string
  method cgi_server_name        : string
  method cgi_server_protocol    : string
  method cgi_server_port        : int option
  method cgi_request_method     : string
  method cgi_path_info          : string
  method cgi_path_translated    : string
  method cgi_script_name        : string
  method cgi_query_string       : string
  method cgi_remote_host        : string
  method cgi_remote_addr        : string
  method cgi_auth_type          : string
  method cgi_remote_user        : string
  method cgi_remote_ident       : string

  (** Non-standard properties *)

  method cgi_property          : ?default:string -> string -> string
    (** Returns a (possibly non-standard) environment property. If the property
     * is not set, [Not_found] will be raised unless the [default] argument is 
     * passed. The [default] argument determines the result of the function in
     * this case.
     *
     * The method takes the case-sensitive name and returns the value
     * of the property. Usually, these properties have uppercase names.
     *
     * For example, [cgi_gateway_interface] returns the same as
     * {[ cgi_property ~default:"" "GATEWAY_INTERFACE" ]}
     *
     * You can normally not access those fields coming from the HTTP
     * header. Use the method [input_header_field] instead.
     *)

  method cgi_properties : (string * string) list
    (** All properties *)

  method cgi_https              : bool
    (** A well-known extension is the [HTTPS] property. It indicates whether
     * a secure connection is used (SSL/TLS). This method interprets this
     * property and returns true if the connection is secure.
     * This method fails if there is a [HTTPS] property with an unknown value.
     *)

  method cgi_request_uri : string
    (** This is the URI path as passed in the HTTP request, without preprocessing *)

  (* Convenience: *)
  method protocol : protocol

  (** Request header *)

  method input_header : Netmime.mime_header
    (** The whole header *)

  method input_header_field : ?default:string -> string -> string
    (** Returns the value of a field of the request header. If the field
     * does not exist, [Not_found] will be raised unless the [default] argument
     * is passed. The [default] argument determines the result of the function in
     * this case.
     *
     * If there are several fields with the same name only the first field
     * will be returned.
     *
     * The anonymous string is the name of the field. The name is case-insensitive,
     * and it does not matter whether it consists of lowercase or uppercase
     * letters. If the name is a compound name, the parts are separated by "-",
     * e.g. ["content-length"].
     *)

  method multiple_input_header_field : string -> string list
    (** Returns the values of all fields with the passed name of the request
     * header.
     *)

  method input_header_fields : (string * string) list
    (** Returns the input header as (name,value) pairs. The names may consist
     * of lowercase or uppercase letters.
     *)

  (* Convenience: *)
  method user_agent : string
    (** Returns the ["User-agent"] field of the request header *)

  method cookies : (string * string) list
    (** Returns the list of cookies found in the request header *)

  method input_content_length : int
    (** Returns the ["Content-length"] request header field, or raises 
     * [Not_found] if it is not set
     *)

  method input_content_type_string : string 
    (** Returns the ["Content-type"] request header field or [""] if it is
     * not set 
     *)

  method input_content_type : (string * (string * Mimestring.s_param) list)
    (** Returns the parsed ["Content-type"] request header field, or raises
     * [Not_found] if it is not set.
     * See also {!Mimestring.scan_mime_type_ep}.
     *)

  (** The input channel transferring the request body *)

  method input_ch : Netchannels.in_obj_channel
    (** The input channel as such *)

  method input_state : input_state
    (** Returns the current input state *)

  method set_input_state : input_state -> unit
    (** Sets the input state. This method should only be called by
     * [cgi_activation] implementations.
     *)

  (** Response header *)

  method output_header : Netmime.mime_header
    (** The whole response header *)

  method output_header_field : ?default:string -> string -> string
    (** Returns the value of a field of the response header. If the field
     * does not exist, [Not_found] will be raised unless the [default] argument
     * is passed. The [default] argument determines the result of the function in
     * this case.
     *
     * If there are several fields with the same name only the first field
     * will be returned.
     *
     * The anonymous string is the name of the field. The name is case-insensitive,
     * and it does not matter whether it consists of lowercase or uppercase
     * letters. If the name is a compound name, the parts are separated by "-",
     * e.g. ["content-length"].
     *)

  method multiple_output_header_field : string -> string list
    (** Returns the values of all fields with the passed name of the repsonse
     * header.
     *)

  method output_header_fields : (string * string) list
    (** Returns the output header as (name,value) pairs. The names may consist
     * of lowercase or uppercase letters.
     *)

  method set_output_header_field : string -> string -> unit
    (** Sets the value of a field of the response header. The previous value, if 
     * any, is overwritten. If there have been multiple values, all values
     * will be removed and replaced by the single new value.
     *)

  method set_multiple_output_header_field : string -> string list -> unit
    (** Sets multiple values of a field of the response header. Any previous
     * values are removed and replaced by the new values.
     *)

  method set_output_header_fields : (string * string) list -> unit
    (** Sets the complete repsonse header at once. *)

  method set_status : Nethttp.http_status -> unit
    (** Sets the response status. This is by definition the same as setting the
      * [Status] output header field.
     *)

  method send_output_header : unit -> unit
    (** If the output state is [`Start], this method will encode and send
     * the output header to the output channel, and the state will be
     * changed to [`Sent_header].
     *
     * The method will fail if the output state is not [`Start].
     *
     * Note that this method is usually only called by the
     * [cgi_activation] object.
     *)

  (** The output channel transferring the response *)

  method output_ch : Netchannels.out_obj_channel
    (** The output channel as such *)

  method output_state : output_state
    (** Returns the output state *)

  method set_output_state : output_state -> unit
    (** Sets the output state. This method should only be called by
     * [cgi_activation] implementations.
     *)

  method log_error : string -> unit
    (** Outputs a log message to the error log. *)

    
(* Note: Setting the output state to `End causes that the output
 * channel is closed. This is the preferred way of closing the
 * channel.
 *)

end


exception Std_environment_not_found
  (** Indicates that the process environment does not contain the
   * variables that are typical of CGI
   *)

class std_environment : ?config:cgi_config -> unit -> cgi_environment
  (** An implementation of [cgi_environment], intended to be used
   * for classical CGI.
   *
   * The input channel is [stdin], the input environment comes from the process
   * environment, the output channel is [stdout].
   *
   * The [new] operator will raise [Std_environment_not_found] if the typical
   * CGI environment variables are not set.
   *
   * @param config The configuration to use. Default: [default_config].
   *)

class test_environment : ?config:cgi_config -> unit -> cgi_environment
  (** An implementation of [cgi_environment], intended to test CGI
   * programs from the command-line.
   *
   * Interprets command-line arguments from Sys.argv as test arguments.
   * If there are not any arguments, the user is asked interactively
   * for arguments.
   *)


(** This class can be used to set up non-standard environments. After
 * creation, one sets the properties and the request header, and calls
 * [setup_finished].
 *
 * Please read the comments about the protocol between environments and
 * [cgi_activation] in the description of {!Netcgi_env.cgi_environment}
 * before using this class. In particular, it is the task of the
 * environment to receive the request header.
 *)
class custom_environment : ?config:cgi_config -> unit ->
object 
  inherit cgi_environment
    (** Implements this interface *)

  method set_cgi :
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
	   ?https:bool ->
           ?property:(string * string) ->
	   unit ->
	     unit
    (** Sets CGI properties *)

  method set_input_header_field : string -> string -> unit
    (** Sets a request header field *)

  method set_multiple_input_header_field : string -> string list -> unit
    (** Sets a request header field to a multiple value *)

  method set_input_header_fields : (string * string) list -> unit
    (** Sets all request header fields at once *)

  method set_input_ch : Netchannels.in_obj_channel -> unit
    (** Sets the input channel to use *)

  method set_input_content_length : int -> unit
    (** Sets the input content length. This is the same as setting
     * the [Content-length] field of the request header
     *)

  method set_input_content_type : string -> unit
    (** Sets the input content type. This is the same as setting
     * the [Content-type] field of the request header
     *)

  method set_output_ch : Netchannels.out_obj_channel -> unit
    (** Sets the output channel to use *)

  method set_error_log : (string -> unit) -> unit
    (** Sets the error log function ([prerr_endline] by default) *)

  method setup_finished : unit -> unit
    (** Finishes the setup. After that, it is no longer possible to change
     * it.
     *)
end

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.9  2005/07/25 22:36:40  stolpmann
 * Merge of nethttpd.
 * Adding the "overview" section to Nethttpd_types.
 * Cosmetic fixes.
 * Release updates.
 *
 * Revision 1.8.2.1  2005/04/30 16:31:17  stolpmann
 * Integration of the new Nethttpd component into Ocamlnet.
 *
 * Revision 1.8  2004/07/24 18:30:49  stolpmann
 * 	ocamldoc
 *
 * Revision 1.7  2002/10/24 23:47:48  stolpmann
 * 	Support for the HEAD method.
 * 	Workaround for a bug in MSIE: Empty cookies are represented
 * in the wrong way
 *
 * Revision 1.6  2002/01/14 01:12:10  stolpmann
 * 	Representing input and output headers as Netmime.mime_header.
 * 	Removed the method _cgi, and many references to the Cgi module.
 *
 * Revision 1.5  2001/10/04 01:04:58  stolpmann
 * 	Moved from directory /src/netstring to /src/cgi.
 *
 * Revision 1.4  2001/10/04 00:56:12  stolpmann
 * 	Implemented class [custom_environment].
 * 	Fixed method [user_agent].
 *
 * Revision 1.3  2001/09/30 00:03:28  stolpmann
 * 	Documentation only
 *
 * Revision 1.2  2001/09/27 22:00:43  stolpmann
 * 	Changed type protocol_attribute.
 *
 * Revision 1.1  2001/09/24 21:26:54  stolpmann
 * 	Initial revision (compiles, but untested)
 *
 * 
 *)
