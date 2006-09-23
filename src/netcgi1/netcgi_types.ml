(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Nethttp

exception Resources_exceeded

class type simple_message = Netmime.mime_body

type store =
  [ `Memory 
  | `File of string
  ]


type representation =
  [ `Simple of simple_message
  | `MIME of Netmime.mime_message
  ]


class type cgi_argument =
object
  method ro : bool
  method name : string
  method value : string
  method open_value_rd : unit -> Netchannels.in_obj_channel
  method store : store
  method content_type : string
  method content_type_params : (string * Mimestring.s_param) list
  method charset : string
  method filename : string option
  method representation : representation
  method set_value : string -> unit
  method open_value_wr : unit -> Netchannels.out_obj_channel
  method finalize : unit -> unit
end


type cgi_cookie = Nethttp.cookie =
    { cookie_name : string;
      cookie_value : string;
      cookie_expires : float option;
      cookie_domain : string option;
      cookie_path : string option;
      cookie_secure : bool;
    }


type status = http_status

type request_method =
  [ `GET
  | `HEAD
  | `POST
  | `DELETE
  | `PUT of cgi_argument
  ]

type cache_control =
    [ `No_cache
    | `Max_age of int
    | `Unspecified
    ]

type query_string_spec =
    [ `Initial | `Current | `Args of cgi_argument list | `None ]

type other_url_spec =
    [ `Env | `This of string | `None ]

class type cgi_activation =
object
  method environment : Netcgi_env.cgi_environment

  method request_method : request_method

  method initial_arguments : (string * cgi_argument) list
  method initial_argument : string -> cgi_argument
  method initial_argument_value : ?default:string -> string -> string
  method initial_multiple_argument : string -> cgi_argument list

  method arguments : (string * cgi_argument) list
  method argument : string -> cgi_argument
  method argument_value : ?default:string -> string -> string
  method multiple_argument : string -> cgi_argument list

  method set_arguments : ?fin:bool -> cgi_argument list -> unit
  method update_argument : ?fin:bool -> cgi_argument -> unit
  method update_multiple_argument : ?fin:bool -> cgi_argument list -> unit
  method delete_argument : ?fin:bool -> string -> unit

  method url : ?protocol:Netcgi_env.protocol ->   
               ?with_authority:other_url_spec ->
               ?with_script_name:other_url_spec ->
               ?with_path_info:other_url_spec ->
               ?with_query_string:query_string_spec ->
               unit ->
		 string

  method output : Netchannels.trans_out_obj_channel

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
  method set_redirection_header :
           string ->
	     unit

  method finalize : unit -> unit
end



(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.8  2005/07/25 22:36:40  stolpmann
 * Merge of nethttpd.
 * Adding the "overview" section to Nethttpd_types.
 * Cosmetic fixes.
 * Release updates.
 *
 * Revision 1.7.2.1  2005/04/30 16:31:17  stolpmann
 * Integration of the new Nethttpd component into Ocamlnet.
 *
 * Revision 1.7  2004/07/01 12:56:20  stolpmann
 * 	Removed dependency on Cgi.
 *
 * Revision 1.6  2002/10/24 23:47:48  stolpmann
 * 	Support for the HEAD method.
 * 	Workaround for a bug in MSIE: Empty cookies are represented
 * in the wrong way
 *
 * Revision 1.5  2002/01/14 01:10:50  stolpmann
 * 	cgi_argument bases now on Netmime.mime_message.
 *
 * Revision 1.4  2001/11/17 23:27:07  stolpmann
 * 	Changed options for [url] method: The with_XXX options
 * have the values [ `Env | `This of string | `None ] instead of
 * just bool.
 *
 * Revision 1.3  2001/10/04 01:07:40  stolpmann
 * 	Moved from directory /src/netstring to /src/cgi.
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
