(* netcgi_compat.ml

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)
(* $Id: netcgi_compat.ml,v 1.12 2005/11/04 00:52:05 chris_77 Exp $ *)

module Netcgi_env =
struct
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

  type cgi_config = {
    tmp_directory : string;
    tmp_prefix : string;
    permitted_http_methods : string list;
    permitted_input_content_types : string list;
    input_content_length_limit : int;
    workarounds : workaround list;
  }

  let default_config =
    let default_tmp_directory =
      let candidates =
	match Sys.os_type with
	| "Unix" | "Cygwin" -> [ "/var/tmp"; "/tmp"; "." ]
	| "Win32" -> [ "C:\\TEMP"; "." ]
	| "MacOS" -> [ Filename.current_dir_name ]
	| _ -> assert false in
      List.find Sys.file_exists candidates in
    {
      tmp_directory = default_tmp_directory;
      tmp_prefix = "netcgi";
      permitted_http_methods = ["GET"; "HEAD"; "POST"];
      permitted_input_content_types = [ "multipart/form-data";
					"application/x-www-form-urlencoded" ];
      input_content_length_limit = max_int;
      workarounds = [ `Work_around_MSIE_Content_type_bug;
		      `Work_around_backslash_bug ]
    }

  let meth_of_string err = function
    | "GET" -> `GET
    | "HEAD" -> `HEAD
    | "POST" -> `POST
    | "DELETE" -> `DELETE
    | "PUT" -> `PUT
    | _ -> failwith err


  let to_config c = {
    Netcgi.tmp_directory = c.tmp_directory;
    Netcgi.tmp_prefix = c.tmp_prefix;
    Netcgi.permitted_http_methods =
      List.map (meth_of_string "Netcgi_compat.config")
	c.permitted_http_methods;

    Netcgi.permitted_input_content_types = c.permitted_input_content_types;

    Netcgi.input_content_length_limit = c.input_content_length_limit;

    Netcgi.workarounds =
      List.map (function
		| `Work_around_MSIE_Content_type_bug -> `MSIE_Content_type_bug
		| `Work_around_backslash_bug -> `Backslash_bug
	       ) c.workarounds
  }



  class cgi_environment (env:Netcgi.cgi_environment) =
  object
    val env = env
    val mutable config : cgi_config Lazy.t = lazy(assert false)

    initializer
      (* Generate an old config *)
      config <- lazy(
	let c = env#config in
	{ tmp_directory = c.Netcgi.tmp_directory;
	  tmp_prefix = c.Netcgi.tmp_prefix;
	  permitted_http_methods =
	    List.map (function
		      | `GET	-> "GET"
		      | `HEAD	-> "HEAD"
		      | `POST	-> "POST"
		      | `DELETE -> "DELETE"
		      | `PUT 	-> "PUT")
	      c.Netcgi.permitted_http_methods;

	  permitted_input_content_types =
	    c.Netcgi.permitted_input_content_types;
	  input_content_length_limit =
	    c.Netcgi.input_content_length_limit;
	  workarounds =
	    List.map (function
		      | `MSIE_Content_type_bug ->
			  `Work_around_MSIE_Content_type_bug
		      | `Backslash_bug -> `Work_around_backslash_bug
		     ) c.Netcgi.workarounds
	}
      )

    method config = Lazy.force config

    method cgi_gateway_interface  = env#cgi_gateway_interface
    method cgi_server_software    = env#cgi_server_software
    method cgi_server_name        = env#cgi_server_name
    method cgi_server_protocol    = env#cgi_server_protocol
    method cgi_server_port        = env#cgi_server_port
    method cgi_request_method     = env#cgi_request_method
    method cgi_path_info          = env#cgi_path_info
    method cgi_path_translated    = env#cgi_path_translated
    method cgi_script_name        = env#cgi_script_name
    method cgi_query_string       = env#cgi_query_string
    method cgi_remote_host        = env#cgi_remote_host
    method cgi_remote_addr        = env#cgi_remote_addr
    method cgi_auth_type          = env#cgi_auth_type
    method cgi_remote_user        = env#cgi_remote_user
    method cgi_remote_ident       = env#cgi_remote_ident
    method cgi_property           = env#cgi_property
    method cgi_properties 	  = env#cgi_properties
    method cgi_https              = env#cgi_https
    method cgi_request_uri        = env#cgi_property ~default:"" "REQUEST_URI"
    method protocol = env#protocol

    method input_header = env#input_header

    method input_header_field = env#input_header_field
    method multiple_input_header_field = env#multiple_input_header_field
    method input_header_fields = env#input_header_fields
    method user_agent = env#user_agent
    method cookies =
      List.map (fun c -> (Netcgi.Cookie.name c, Netcgi.Cookie.value c))
	env#cookies
    method input_content_length = env#input_content_length
    method input_content_type_string = env#input_content_type_string
    method input_content_type = env#input_content_type()

    method input_ch =
      (failwith "input_ch: none of your business": Netchannels.in_obj_channel)
    method input_state =
      (failwith "input_state: none of your business": input_state)
    method set_input_state =
      (failwith "set_input_state : you were told not to play with it!":
	 input_state -> unit)

    method output_header = env#output_header
    method output_header_field = env#output_header_field
    method multiple_output_header_field = env#multiple_output_header_field
    method output_header_fields = env#output_header_fields
    method set_output_header_field = env#set_output_header_field
    method set_multiple_output_header_field =
      env#set_multiple_output_header_field
    method set_output_header_fields = env#set_output_header_fields
    method set_status = env#set_status
    method send_output_header = env#send_output_header

    method output_ch = env#out_channel
    method output_state =
      (failwith "output_state: none of your business": output_state)
    method set_output_state =
      (failwith "set_output_state: none of your business":
	 output_state -> unit)
    method log_error = env#log_error
  end


  let to_environment (env:cgi_environment) : Netcgi.cgi_environment =
  object
    inherit Netcgi_common.cgi_environment
      ~config:(to_config env#config)
      ~properties:env#cgi_properties
      ~input_header:[]
      env#output_ch

    method input_header = env#input_header
    method output_header = env#output_header
  end
end



module Netcgi_types =
struct
  class type simple_message = Netmime.mime_body

  type store = [ `Memory | `File of string ]
  type representation =
      [ `Simple of simple_message | `MIME of Netmime.mime_message ]
  class cgi_argument (arg:Netcgi.cgi_argument) =
  object
    val arg = arg

    method name = arg#name
    method value = arg#value
    method open_value_rd = arg#open_value_rd
    method ro = true
    method store = arg#store
    method content_type = fst(arg#content_type())
    method content_type_params = snd(arg#content_type())
    method charset = arg#charset
    method filename = arg#filename
    method representation = arg#representation
    method finalize = arg#finalize
    method set_value =
      (raise(Netmime.Immutable "Netcgi_types.cgi_argument"): string -> unit)
    method open_value_wr =
      (raise(Netmime.Immutable "Netcgi_types.cgi_argument"):
	 unit -> Netchannels.out_obj_channel)
  end

  let to_argument (arg:cgi_argument) : Netcgi.cgi_argument =
  object
    val arg = arg
    method name = arg#name
    method value = arg#value
    method open_value_rd = arg#open_value_rd
    method store = arg#store
    method content_type () = (arg#content_type, arg#content_type_params)
    method charset = arg#charset
    method filename = arg#filename
    method representation = arg#representation
    method finalize = arg#finalize
  end

  type cgi_cookie = Nethttp.cookie = {
    cookie_name : string;
    cookie_value : string;
    cookie_expires : float option;
    cookie_domain : string option;
    cookie_path : string option;
    cookie_secure : bool;
  }

  type status = Nethttp.http_status

  type request_method = [ `GET | `HEAD | `POST | `DELETE
  | `PUT of cgi_argument ]

  type cache_control = [ `No_cache | `Max_age of int | `Unspecified ]

  type query_string_spec =
      [ `Initial | `Current | `Args of cgi_argument list | `None ]

  type other_url_spec = [ `Env | `This of string | `None ]


  class cgi_activation (cgi:Netcgi.cgi) =
  object
    val env = new Netcgi_env.cgi_environment cgi#environment
    val args = List.map (fun a -> new cgi_argument a) cgi#arguments
      (* FIXME: the curr_args should duplicate the body and header of
	 initial ones -- need to duplicate the files... :( *)
    val mutable curr_args =
      List.map (fun a -> new cgi_argument a) cgi#arguments
    val cgi = cgi

    method environment = env
    method request_method : request_method =
      match cgi#request_method with
      | `GET | `HEAD | `POST | `DELETE as m -> m
      | `PUT cgi -> `PUT(new cgi_argument cgi)

    method initial_arguments = List.map (fun a -> (a#name, a)) args
    method initial_argument name =
      List.find (fun a -> a#name = name) args
    method initial_argument_value ?default name =
      try (List.find (fun a -> a#name = name) args)#value
      with Not_found -> (match default with
			 | None -> raise Not_found
			 | Some d -> d)
    method initial_multiple_argument name =
      List.filter (fun a -> a#name = name) args


    method arguments = List.map (fun a -> (a#name, a)) curr_args
    method argument name =
      List.find (fun a -> a#name = name) curr_args
    method argument_value ?default name =
      try (List.find (fun a -> a#name = name) curr_args)#value
      with Not_found -> (match default with
			 | None -> raise Not_found
			 | Some d -> d)
    method multiple_argument name =
      List.filter (fun a -> a#name = name) curr_args

    method set_arguments ?(fin=true) new_args =
      if fin then
	List.iter (fun a -> if not(List.mem a new_args) then a#finalize())
	  curr_args;
      curr_args <- new_args

    method update_argument ?(fin=true) new_arg =
      let name = new_arg#name in
      let keep a =
	if fin && a#name = name && a <> new_arg then (a#finalize(); false)
	else a#name <> name in
      curr_args <- new_arg :: (List.filter keep curr_args)

    (* All arguments in [arglist] must have the same name. *)
    method update_multiple_argument ?(fin=true) arglist =
      match arglist with
      | [] -> ()
      | a0 :: tl ->
	  let name = a0#name in
	  if List.exists (fun a -> a#name <> name) tl then
	    invalid_arg "update_multiple_argument";
	  let keep a =
	    if fin && a#name = name && not(List.mem a arglist) then
	      (a#finalize(); false)
	    else a#name <> name in
	  curr_args <- arglist @ List.filter keep curr_args

    method delete_argument ?(fin=true) name =
      let keep a =
	if fin && a#name = name then (a#finalize(); false)
	else a#name <> name in
      curr_args <- List.filter keep curr_args


    method url ?protocol ?with_authority ?with_script_name ?with_path_info
      ?(with_query_string=(`None: query_string_spec)) () =
      cgi#url ?protocol ?with_authority ?with_script_name
	?with_path_info
	~with_query_string:(
	  match with_query_string with
	  | `Initial -> `Env
	  | `Current ->   `This(List.map (fun a -> to_argument a) curr_args)
	  | `Args args -> `This(List.map (fun a -> to_argument a) args)
	  | `None -> `None)
	()


    method output = cgi#out_channel

    method set_header ?status ?content_type ?cache ?filename
      ?language ?script_type ?style_type ?(set_cookie=[]) ?fields () =
      let now = Unix.time() in
      let make_cookie c =
	Netcgi.Cookie.make
	  ?domain:c.cookie_domain
	  ?max_age:(match c.cookie_expires with
		    | None -> None
		    | Some t -> Some(truncate(t -. now)))
	  ?path:c.cookie_path
	  ~secure:c.cookie_secure
	  c.cookie_name c.cookie_value in
      cgi#set_header ?status ?content_type
	~set_cookies:(List.map make_cookie set_cookie)
	?cache ?filename ?language ?script_type ?style_type ?fields ()

    method set_redirection_header loc =
      cgi#set_redirection_header loc


    method finalize () =
      List.iter (fun a -> a#finalize()) args;
      List.iter (fun a -> a#finalize()) curr_args;
      cgi#finalize()
  end


  let to_cgi (cgi_act:cgi_activation) =
    let env = Netcgi_env.to_environment(cgi_act#environment) in
  object(self)
    (* We have no idea of the output_type of [cgi_act] so we
       initialize with [`Direct] and override [out_channel]. *)
    inherit Netcgi_common.cgi env (`Direct "")
      (match cgi_act#request_method with
       | `GET | `HEAD | `POST | `DELETE as m -> m
       | `PUT a -> `PUT(to_argument a))
      (List.map (fun (_, a) -> to_argument a) cgi_act#initial_arguments)

    (* Override methods that depend on the unkown output_type *)

    method out_channel = cgi_act#output

    method set_header ?status ?content_type ?content_length
      ?(set_cookie=[]) ?(set_cookies=[])
      ?cache ?filename ?language ?script_type ?style_type ?fields
      () =
      let now = Unix.time() in
      let old_cookie c =
	{ cookie_name = Netcgi_common.Cookie.name c;
	  cookie_value = Netcgi_common.Cookie.value c;
	  cookie_expires = (match Netcgi_common.Cookie.max_age c with
			    | None -> None
			    | Some t -> Some(float t +. now));
	  cookie_domain = Netcgi_common.Cookie.domain c;
	  cookie_path = Netcgi_common.Cookie.path c;
	  cookie_secure = Netcgi_common.Cookie.secure c;
	} in
      let fields = match content_length with
	| None -> []
	| Some size -> [ "content-length", [string_of_int size] ] in
      (* The old [set_header] knows whether the output is
	 transactional or not. *)
      cgi_act#set_header ?status ?content_type ?cache ?filename
	?language ?script_type ?style_type ~fields
	~set_cookie:(List.map old_cookie (set_cookie @ set_cookies))
	()

    method set_redirection_header ?set_cookies ?(fields=[]) loc =
      (* There is no way of getting the old set_redirection_header to
	 accept to set other fields.  Thus, use [set_header].  *)
      self#set_header ?set_cookies
	~fields:(fields @ [("Location", [loc])] )
	()
  end
end


module Netcgi =
struct
  class simple_argument ?(ro=true) name value =
  object
    val super = new Netcgi_common.simple_arg name value

    method name = super#name
    method value = super#value
    method open_value_rd = super#open_value_rd
    method ro = super#ro
    method store = super#store
    method content_type = fst(super#content_type())
    method content_type_params = snd(super#content_type())
    method charset = super#charset
    method filename = super#filename
    method representation = super#representation
    method finalize = super#finalize
    method set_value = super#set_value
    method open_value_wr = super#open_value_wr
  end

  class mime_argument ?work_around_backslash_bug name mime =
  object
    val super =
      new Netcgi_common.mime_arg ?work_around_backslash_bug ~name mime

    method name = super#name
    method value = super#value
    method open_value_rd = super#open_value_rd
    method ro = super#ro
    method store = super#store
    method content_type = fst(super#content_type())
    method content_type_params = snd(super#content_type())
    method charset = super#charset
    method filename = super#filename
    method representation = super#representation
    method finalize = super#finalize
    method set_value = super#set_value
    method open_value_wr = super#open_value_wr
  end
end
