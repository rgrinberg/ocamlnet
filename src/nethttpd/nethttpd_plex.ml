(* $Id$ *)

open Nethttpd_services
open Netplex_types

type config_log_error =
    Unix.sockaddr option -> Unix.sockaddr option -> 
      Nethttp.http_method option -> Nethttp.http_header option -> string -> 
	unit

type config_log_access =
    Unix.sockaddr -> Unix.sockaddr -> Nethttp.http_method ->
    Nethttp.http_header -> int64 -> (string * string) list -> bool -> 
    int -> Nethttp.http_header -> int64 -> unit

type ('a,'b) service_factory =
    (string * 'a Nethttpd_services.dynamic_service) list ->
    Netplex_types.config_file ->
    Netplex_types.address -> 
    string ->
      'b Nethttpd_types.http_service
    constraint 'b = [ `Dynamic_service of 'a Nethttpd_services.dynamic_service
                    | `File_service of Nethttpd_services.file_service 
		    ]


let std_log_error container _ peeraddr_opt meth_opt _ msg =
  let s =
    Printf.sprintf "[%s] [%s] %s"
      ( match peeraddr_opt with
	  | Some(Unix.ADDR_INET(addr,port)) ->
	      Unix.string_of_inet_addr addr
	  | Some(Unix.ADDR_UNIX path) ->
	      path
	  | None ->
	      "-"
      )
      ( match meth_opt with
	  | (Some(name,uri)) ->
	      name ^ " " ^ uri
	  | None ->
	      "-"
      )
      msg
  in
  container # log_subch "" `Err s


let std_log_access ?(debug=false) container sockaddr peeraddr req reqhdr
                    reqsize props rej code resphdr respsize =
  let peerstr =
    match peeraddr with
      | Unix.ADDR_INET(addr,port) ->
	  Unix.string_of_inet_addr addr
      | Unix.ADDR_UNIX path ->
	  path in

  let (meth,uri) = req in

  let s =
    Printf.sprintf "%s %s %s \"%s\" %d %Ld \"%s\" \"%s\""
      peerstr
      ( try List.assoc "REMOTE_USER" props with Not_found -> "-" )
      meth
      (String.escaped uri)
      code
      respsize
      ( String.escaped
	  (try reqhdr # field "Referer" with Not_found -> "-" ) )
      ( String.escaped
	  (try reqhdr # field "User-agent" with Not_found -> "-") ) in

  container # log_subch "access" `Info s;

  if debug then (
    let b = Buffer.create 500 in
    let b_ch = new Netchannels.output_buffer b in
    Printf.bprintf b "%s\n" s;
    Printf.bprintf b "Request header:\n";
    Mimestring.write_header ~soft_eol:"\n" ~eol:"\n" b_ch reqhdr#fields;
    Printf.bprintf b "Request body size: %Ld\n" reqsize;
    Printf.bprintf b "Request body rejected: %b\n\n" rej;
    Printf.bprintf b "CGI properties:\n";
    Mimestring.write_header ~soft_eol:"\n" ~eol:"\n" b_ch props;
    Printf.bprintf b "Response header (code %d):\n" code;
    Mimestring.write_header ~soft_eol:"\n" ~eol:"\n" b_ch resphdr#fields;
    Printf.bprintf b "Response body size: %Ld\n" respsize;
    container # log_subch "access" `Debug (Buffer.contents b)
  )

    

class nethttpd_processor mk_config srv : Netplex_types.processor =
object(self)
  method process ~when_done (container : Netplex_types.container) fd proto =
    let config = mk_config container in
    ( try
	Nethttpd_reactor.process_connection config fd srv
      with
	| err ->
	    container # log `Err ("Uncaught exception: " ^ 
				    Netexn.to_string err)
    );
    when_done()

  method receive_message _ _ _ = ()
  method receive_admin_message _ _ _ = ()
  method shutdown() = ()
  method post_add_hook _ _ = ()
  method post_rm_hook _ _ = ()
  method pre_start_hook _ _ _ = ()
  method post_start_hook _ = ()
  method pre_finish_hook _ = ()
  method post_finish_hook _ _ _ = ()
  method system_shutdown () = ()
  method supported_ptypes = [ `Multi_processing ; `Multi_threading ]
  method global_exception_handler _ = false
end

let nethttpd_processor mk_config srv =
  new nethttpd_processor mk_config srv


let is_options_request env =
  env # cgi_request_method = "OPTIONS" && env # cgi_request_uri = "*"

let is_any_request env =
  true

let ws_re = Pcre.regexp "[ \r\t\n]+"

let split_ws s =
  Netstring_pcre.split ws_re s

let name_port_re = Pcre.regexp "^([^:]+):([0-9]+)$"

let split_name_port s =
  match Netstring_pcre.string_match name_port_re s 0 with
    | Some m ->
	let name = Netstring_pcre.matched_group m 1 s in
	let port = Netstring_pcre.matched_group m 2 s in
	(name, int_of_string port)
    | None ->
	failwith "Bad name:port specifier"


let cfg_req_str_param cfg addr name =
  try
    cfg#string_param (cfg # resolve_parameter addr name)
  with
    | Not_found ->
	failwith ("Missing parameter: " ^ cfg#print addr ^ "." ^ name)

let cfg_opt_str_param cfg addr name =
  try
    Some(cfg#string_param (cfg # resolve_parameter addr name))
  with
    | Not_found -> None

let cfg_float_param cfg default addr name =
  try
    cfg#float_param (cfg # resolve_parameter addr name)
  with
    | Not_found -> default

let cfg_bool_param cfg addr name =
  try
    cfg#bool_param (cfg # resolve_parameter addr name)
  with
    | Not_found -> false

let restrict_file_service_config cfg addr =
  cfg # restrict_subsections addr [ "media_type" ];
  cfg # restrict_parameters addr [ "type"; "media_types_file";
				   "docroot"; "default_media_type";
				   "enable_gzip"; "index_files";
				   "enable_listings";
				   "hide_from_listings" ]

let read_file_service_config cfg addr uri_path =
  let req_str_param = cfg_req_str_param cfg in
  let opt_str_param = cfg_opt_str_param cfg in
  let bool_param = cfg_bool_param cfg in
  let suffix_types =
    ( List.map
	(fun addr ->
	   cfg # restrict_subsections addr [];
	   cfg # restrict_parameters addr [ "suffix"; "type" ];
	   (req_str_param addr "suffix", req_str_param addr "type")
	)
	(cfg # resolve_section addr "media_type")
    ) @
      ( match opt_str_param addr "media_types_file" with
	  | None -> []
	  | Some f ->
	      read_media_types_file f
      ) in
  let spec =
    { file_docroot = req_str_param addr "docroot";
      file_uri = ( match opt_str_param addr "uri" with
		     | None -> uri_path
		     | Some uri -> uri );
      file_suffix_types = suffix_types;
      file_default_type = 
	( match opt_str_param addr "default_media_type" with
	    | None -> "text/plain"
	    | Some t -> t);
      file_options = 
	( if bool_param addr "enable_gzip" then
	    [ `Enable_gzip ] 
	  else 
	    []
	) @
	  ( match opt_str_param addr "index_files" with
	      | None -> []
	      | Some s -> [ `Enable_index_file (split_ws s) ]
	  ) @
	  ( if bool_param addr "enable_listings" then
	      let hide = 
		match opt_str_param addr "hide_from_listings" with
		  | None -> []
		  | Some s -> split_ws s in
	      let l = simple_listing ~hide in
	      [ `Enable_listings l ]
	    else
	      []
	  )
    } in
  spec


let (default_file_service : ('a,'b) service_factory) handlers cfg addr uri_path =
  restrict_file_service_config cfg addr;
  let spec = read_file_service_config cfg addr uri_path in
  Nethttpd_services.file_service spec


let restrict_dynamic_service_config cfg addr =
  cfg # restrict_subsections addr [];
  cfg # restrict_parameters addr [ "type"; "handler" ]


let read_dynamic_service_config xhandlers cfg addr uri_path =
  let handler_name = cfg_req_str_param cfg addr "handler" in
  let xhandler =
    try
      List.assoc handler_name xhandlers
    with
      | Not_found ->
	  failwith ("Unknown handler `" ^ handler_name ^ "' in param " ^ 
		      cfg#print addr ^ ".handler") in
  let srv = xhandler cfg addr uri_path in
  srv

let default_dynamic_service handlers cfg addr uri_path =
  restrict_dynamic_service_config cfg addr;
  let xhandlers =
    List.map
      (fun (name,h) ->
	 (name, (fun _ _ _ -> h))
      )
      handlers in
  let spec = read_dynamic_service_config xhandlers cfg addr uri_path in
  Nethttpd_services.dynamic_service spec


let default_services =
  [ "file", default_file_service;
    "dynamic", default_dynamic_service
  ]

let create_processor config_cgi handlers services log_error log_access 
                     ctrl_cfg cfg addr =

  let req_str_param = cfg_req_str_param cfg in
  let opt_str_param = cfg_opt_str_param cfg in
  let float_param = cfg_float_param cfg in

  let rec sub_service outermost_flag uri_path addr =
    let host_sects = cfg # resolve_section addr "host" in
    let uri_sects = cfg # resolve_section addr "uri" in
    let method_sects = cfg # resolve_section addr "method" in
    let service_sects = cfg # resolve_section addr "service" in
    match (host_sects, uri_sects, method_sects, service_sects) with
      | [], [], [], [] ->
	  linear_distributor []   (* Forces a 404 response *)
      | _, [], [], [] ->
	  let hosts =
	    List.map (host_sub_service uri_path) host_sects in
	  host_distributor hosts
      | [], _, [], [] ->
	  if outermost_flag then
	    failwith ("Outermost subsection must be 'host': " ^ cfg#print addr);
	  let uris =
	    List.map (uri_sub_service uri_path) uri_sects in
	  uri_distributor uris
      | [], [], _, [] ->
	  if outermost_flag then
	    failwith ("Outermost subsection must be 'host': " ^ cfg#print addr);
	  let methods =
	    List.map (method_sub_service uri_path) method_sects in
	  method_distributor methods
      | [], [], [], _ ->
	  if outermost_flag then
	    failwith ("Outermost subsection must be 'host': " ^ cfg#print addr);
	  ( match service_sects with
	      | [] -> assert false
	      | [service_sect] ->
		  service uri_path service_sect
	      | _ ->
		  failwith ("Only one 'service' subsection is permitted: " ^ cfg#print addr);
	  )
      | _ ->
	  failwith("Only one type of subsections host/uri/method/service is allowed: " ^ cfg#print addr)

  and sub_service_ac uri_path addr =
    (* With access control *)
    let srv = sub_service false uri_path addr in
    let access_sects = cfg # resolve_section addr "access" in
    List.fold_left
      (fun srv access_sect ->
	 access_control access_sect srv)
      srv
      access_sects

  and host_sub_service uri_path addr =
    cfg # restrict_subsections addr [ "host"; "uri"; "method"; "service";
				      "access" ];
    cfg # restrict_parameters addr [ "names"; "pref_name"; "pref_port" ];

    let names_str = req_str_param addr "names" in
    let names = 
      List.map
	(fun s ->
	   try split_name_port s
	   with
	     | Failure m ->
		 failwith (m ^ ": " ^ cfg#print addr ^ ".names")
	)
	(split_ws names_str) in
    let host_def =
      { server_pref_name = opt_str_param addr "pref_name";
	server_pref_port =
	  ( try Some(cfg # int_param
		       (cfg # resolve_parameter addr "pref_port"))
	    with Not_found -> None
	  );
	server_names = names;
	server_addresses =
	  ( List.map
	      (fun (_, port) -> (Unix.inet_addr_any, port))
	      (List.filter
		 (fun (name, _) -> name = "*")
		 names)
	  )
      } in
    let srv = sub_service_ac uri_path addr in
    (host_def, srv)

  and uri_sub_service _ addr =
    cfg # restrict_subsections addr [ "host"; "uri"; "method"; "service";
				      "access" ];
    cfg # restrict_parameters addr [ "path" ];
    let path = req_str_param addr "path" in
    let srv = sub_service_ac path (* sic! *) addr in
    (path, srv)

  and method_sub_service uri_path addr =
    cfg # restrict_subsections addr [ "host"; "uri"; "method"; "service";
				      "access" ];
    cfg # restrict_parameters addr [ "allow"; "deny" ];
    let allow_opt = opt_str_param addr "allow" in
    let deny_opt = opt_str_param addr "deny" in
    let filter =
      match (allow_opt, deny_opt) with
	| (Some host_list), None ->
	    `Limit (split_ws host_list)
	| None, (Some host_list) ->
	    `Limit_except (split_ws host_list)
	| None, None ->
	    failwith ("Missing parameter 'allow' or 'deny': " ^ cfg#print addr)
	| _, _ ->
	    failwith ("It is forbidden to specify both 'allow' and 'deny': " ^
			cfg#print addr)
    in
    let srv = sub_service_ac uri_path addr in
    (filter, srv)

  and access_control addr srv =
    let typ = req_str_param addr "type" in
    match typ with
      | "host" -> host_access_control addr srv
      | _ ->
	  failwith ("Unknown access control type: " ^ cfg#print addr ^ ".type")

  and host_access_control addr srv =
    cfg # restrict_subsections addr [ ];
    cfg # restrict_parameters addr [ "type"; "allow"; "deny" ];
    let allow_opt = opt_str_param addr "allow" in
    let deny_opt = opt_str_param addr "deny" in
    let filter =
      match (allow_opt, deny_opt) with
	| (Some host_list), None ->
	    `Allow (split_ws host_list)
	| None, (Some host_list) ->
	    `Deny (split_ws host_list)
	| None, None ->
	    failwith ("Missing parameter 'allow' or 'deny': " ^ cfg#print addr)
	| _, _ ->
	    failwith ("It is forbidden to specify both 'allow' and 'deny': " ^
			cfg#print addr) in
    ac_by_host (filter, srv)

  and service uri_path addr =
    let typ = req_str_param addr "type" in
    let get_serv = 
      try List.assoc typ services
      with
	| Not_found ->
	    failwith ("Unknown service type: " ^ cfg#print addr ^ ".type") in
    let serv = get_serv handlers cfg addr (uri_path:string) in
    (serv
      :  ( [ `Dynamic_service of 'a Nethttpd_services.dynamic_service
           | `File_service of Nethttpd_services.file_service 
           ] Nethttpd_types.http_service ) 
      :> ( [> `Dynamic_service of 'a Nethttpd_services.dynamic_service
           | `File_service of Nethttpd_services.file_service
           ] Nethttpd_types.http_service )
    )
  in

  cfg # restrict_subsections addr
    [ "host"; "uri"; "method"; "service" ];
  cfg # restrict_parameters addr
    [ "type"; "timeout"; "timeout_next_request"; "access_log" ];

  let srv =
    linear_distributor
      [ is_options_request, options_service();
	is_any_request, sub_service true "/" addr
      ] in

  let timeout = float_param 300.0 addr "timeout" in
  let timeout_next_request = float_param 15.0 addr "timeout_next_request" in
  let access_enabled, access_debug = 
    match opt_str_param addr "access_log" with
      | None -> (false,false) 
      | Some "off" -> (false,false)
      | Some "enabled"  -> (true,false)
      | Some "debug" -> (true,true)
      | _ -> failwith "Bad parameter 'access_log'" in

  let mk_config container =
    let cle = log_error container in
    let cla = 
      if access_enabled then
	log_access ?debug:(Some access_debug) container
      else
	(fun _ _ _ _ _ _ _ _ _ _ -> ()) in
    (object
       method config_reactor_synch = `Write
       method config_timeout_next_request = timeout_next_request
       method config_timeout = timeout
       method config_cgi = config_cgi
       method config_error_response n = 
	 (* TODO *)
	 "<html><body>Error " ^ string_of_int n ^ "</body></html>"
       method config_log_error = cle
       method config_log_access = cla
       method config_max_reqline_length = 32768
       method config_max_header_length = 65536
       method config_max_trailer_length = 65536
       method config_limit_pipeline_length = 1
       method config_limit_pipeline_size = 65536
       method config_announce_server = `Ocamlnet
	 (* TODO *)
     end
    ) in

  nethttpd_processor
    mk_config
    srv
;;

class nethttpd_factory ?(name = "nethttpd") 
                       ?(config_cgi = Netcgi1_compat.Netcgi_env.default_config) 
                       ?(handlers=[]) 
		       ?(services=default_services)
		       ?(log_error = std_log_error)
		       ?(log_access = std_log_access)
		       () : processor_factory =
object
  method name = name
  method create_processor ctrl_cfg cfg addr =
    create_processor 
      config_cgi handlers services log_error log_access ctrl_cfg cfg addr
end

let nethttpd_factory =
  new nethttpd_factory 
