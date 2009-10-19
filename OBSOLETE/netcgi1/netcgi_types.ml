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
