(* $Id$ *)

(** Compatibility with [netcgi2] based on the [netcgi1] API *)

(** This module has almost the same signature as the version of [Netcgi1_compat]
  * shipped with [netcgi2], the revised version of this library. This makes
  * it possible to build programs with either [netcgi1] or [netcgi2] by 
  * adding 
  *
  * {[ open Netcgi1_compat ]}
  *
  * at the beginning of every source file, or by referring to the [netcgi1]
  * modules as
  *
  * - [Netcgi1_compat.Netcgi]
  * - [Netcgi1_compat.Netcgi_types]
  * - [Netcgi1_compat.Netcgi_env]
  *
  * Note that these modules are only subsets of the full modules.
 *)

module Netcgi_env :
sig
  type input_mode = [ `Standard (* | `Direct *) ]
  type input_state =
      [ `Start
      | `Receiving_header | `Received_header
      | `Receiving_body | `Received_body      ]
  type output_mode = [ `Standard (* | `Direct *) ]
  type output_state =
      [ `Start
      | `Sending_header      | `Sent_header
      | `Sending_body        | `Sent_body
      | `Sending_part_header | `Sent_part_header
      | `Sending_part_body   | `Sent_part_body
      | `End
      ]
  type protocol_version = Nethttp.protocol_version
  type protocol_attribute = Nethttp.protocol_attribute
  type protocol = Nethttp.protocol
  type workaround =
      [ `Work_around_MSIE_Content_type_bug | `Work_around_backslash_bug  ]
  type cgi_config = Netcgi_env.cgi_config = {
    tmp_directory : string;
    tmp_prefix : string;
    permitted_http_methods : string list;
    permitted_input_content_types : string list;
    input_content_length_limit : int;
    workarounds : workaround list;
  }

  val default_config : cgi_config

(*
  val to_config : cgi_config -> Netcgi.config
    (** {b Portage:} [to_config c] transform the old configuration [c]
	into one suitable for the new interface. *)
 *)

  class type cgi_environment =
  object
    method config : cgi_config

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
    method cgi_property          : ?default:string -> string -> string
    method cgi_properties : (string * string) list
    method cgi_https              : bool

    method cgi_request_uri : string
    method protocol : protocol

    method input_header : Netmime.mime_header

    method input_header_field : ?default:string -> string -> string
    method multiple_input_header_field : string -> string list
    method input_header_fields : (string * string) list
    method user_agent : string
    method cookies : (string * string) list
    method input_content_length : int
    method input_content_type_string : string
    method input_content_type : (string * (string * Mimestring.s_param) list)

    method input_ch : Netchannels.in_obj_channel
    method input_state : input_state
    method set_input_state : input_state -> unit

    method output_header : Netmime.mime_header
    method output_header_field : ?default:string -> string -> string
    method multiple_output_header_field : string -> string list
    method output_header_fields : (string * string) list
    method set_output_header_field : string -> string -> unit
    method set_multiple_output_header_field : string -> string list -> unit
    method set_output_header_fields : (string * string) list -> unit
    method set_status : Nethttp.http_status -> unit
    method send_output_header : unit -> unit

    method output_ch : Netchannels.out_obj_channel
    method output_state : output_state
    method set_output_state : output_state -> unit
    method log_error : string -> unit
  end

(*
  val to_environment : cgi_environment -> Netcgi.cgi_environment
    (** {b Portage:} [to_environment e] converts the old environment
	[e] to the new interface. *)
 *)
end


module Netcgi_types :
sig
  class type simple_message = Netmime.mime_body

  type store = [ `Memory | `File of string ]
      (** Embedded in the single place of use. *)
  type representation =
      [ `Simple of simple_message | `MIME of Netmime.mime_message ]
	(** Embedded in the single place of use. *)

  class type cgi_argument =
  object
    method name : string
    method value : string
    method open_value_rd : unit -> Netchannels.in_obj_channel
    method ro : bool
    method store : store
    method content_type : string
    method content_type_params : (string * Mimestring.s_param) list
    method charset : string
    method filename : string option
    method representation : representation
    method finalize : unit -> unit
    method set_value : string -> unit
    method open_value_wr : unit -> Netchannels.out_obj_channel
  end

(*
  val to_argument : cgi_argument -> Netcgi.cgi_argument
    (** {b Portage:} [to_argument a] converts an old style argument
	[a] to a new style one.  Finalizing [to_argument a] will also
	finalize [a].  *)
 *)

  type cgi_cookie = Nethttp.cookie = {
    cookie_name : string;
    cookie_value : string;
    cookie_expires : float option;
    cookie_domain : string option;
    cookie_path : string option;
    cookie_secure : bool;
  }

  type status = Nethttp.http_status

  type request_method =
      [ `GET | `HEAD | `POST | `DELETE | `PUT of cgi_argument ]

  type cache_control = [ `No_cache | `Max_age of int | `Unspecified ]

  type query_string_spec =
      [ `Initial | `Current | `Args of cgi_argument list | `None ]

  type other_url_spec = [ `Env | `This of string | `None ]

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

    method url :
      ?protocol:Netcgi_env.protocol ->
      ?with_authority:other_url_spec ->        (* default: `Env *)
      ?with_script_name:other_url_spec ->      (* default: `Env *)
      ?with_path_info:other_url_spec ->        (* default: `Env *)
      ?with_query_string:query_string_spec ->  (* default: `None *)
      unit -> string

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
      unit -> unit

    method set_redirection_header : string -> unit

    method finalize : unit -> unit
  end

(*
  val to_cgi : cgi_activation -> Netcgi.cgi
    (** {b Portage:} [to_cgi] converts an old style cgi_activation to
	a new CGI-like object. *)
 *)
end

module Netcgi :
sig
  type argument_processing =
      [ `Memory
      | `File
      | `Automatic ]
	
  type operating_type =
      [ `Direct of string (* separator *)
      | `Transactional of
          (Netcgi_env.cgi_config -> Netchannels.out_obj_channel ->
             Netchannels.trans_out_obj_channel)
      ]

  class simple_argument :
    ?ro:bool -> string -> string -> Netcgi_types.cgi_argument

  class mime_argument :
    ?work_around_backslash_bug:bool ->
  string -> Netmime.mime_message -> Netcgi_types.cgi_argument

  class std_activation :
    ?env:Netcgi_env.cgi_environment ->
    ?processing:(string -> Netmime.mime_header -> argument_processing) ->
    ?operating_type:operating_type ->
    unit ->
      Netcgi_types.cgi_activation

  val buffered_transactional_optype : operating_type

  val tempfile_transactional_optype : operating_type

end
