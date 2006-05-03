(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Netchannels
open Nethttp

type input_mode = 
    [ `Standard (* | `Direct *) ] ;;
type input_state =
    [ `Start | 
      `Receiving_header | `Received_header |
      `Receiving_body | `Received_body
    ] ;;
type output_mode =
    [ `Standard (* | `Direct *) ] ;; 
type output_state =
    [ `Start | 
      `Sending_header      | `Sent_header | 
      `Sending_body        | `Sent_body |
      `Sending_part_header | `Sent_part_header |
      `Sending_part_body   | `Sent_part_body |
      `End
    ] ;;
type protocol_version = Nethttp.protocol_version
type protocol_attribute = Nethttp.protocol_attribute
type protocol = Nethttp.protocol

type workaround =
  [ `Work_around_MSIE_Content_type_bug
  | `Work_around_backslash_bug
  ] ;;

type cgi_config =
    { tmp_directory : string;
      tmp_prefix : string;
      permitted_http_methods : string list;
      permitted_input_content_types : string list;
      input_content_length_limit : int;
      workarounds : workaround list;
    } ;;

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

  method cgi_property           : ?default:string -> string -> string
  method cgi_properties         : (string * string) list
  method cgi_https              : bool
  method cgi_request_uri        : string

  method protocol : protocol

  method input_header : Netmime.mime_header
  method input_header_field : ?default:string -> string -> string
  method multiple_input_header_field : string -> string list
  method input_header_fields : (string * string) list

  method user_agent : string
  method cookies : (string * string) list

  method input_ch : in_obj_channel

  method input_content_length : int

  method input_content_type_string : string 
  method input_content_type : (string * (string * Mimestring.s_param) list)

  method input_state : input_state
  method set_input_state : input_state -> unit

  method output_ch : out_obj_channel

  method output_header : Netmime.mime_header
  method output_header_field : ?default:string -> string -> string
  method multiple_output_header_field : string -> string list
  method output_header_fields : (string * string) list
  method set_status : http_status -> unit

  method set_output_header_field : string -> string -> unit
  method set_multiple_output_header_field : string -> string list -> unit
  method set_output_header_fields : (string * string) list -> unit
  method send_output_header : unit -> unit

  method output_state : output_state
  method set_output_state : output_state -> unit

  method log_error : string -> unit

end ;;

exception Std_environment_not_found ;;

let default_tmp_directory() =
  let candidates =
    match Sys.os_type with
	"Unix" | "Cygwin" -> [ "/var/tmp"; "/tmp"; "." ]
      | "Win32" -> [ "C:\\TEMP"; "." ]
      | "MacOS" -> [ Filename.current_dir_name ]
      | _ -> assert false
  in
  List.find Sys.file_exists candidates
;;


let default_config =
  { tmp_directory = default_tmp_directory();
    tmp_prefix = "netcgi";
    permitted_http_methods = [ "POST"; "GET"; "HEAD" ];
    permitted_input_content_types = [ "multipart/form-data";
				      "application/x-www-form-urlencoded" ];
    input_content_length_limit = max_int;
    workarounds = [ `Work_around_MSIE_Content_type_bug;
		    `Work_around_backslash_bug
		  ]
  } ;;


let minus_re = Pcre.regexp "-";;

let server_name_with_port_re = Pcre.regexp "^([^:]+):([0-9]+)$";;


class base_environment ?(config = default_config) () =
object (self)
  val cfg = config
  val mutable in_ch = new input_channel stdin
  val mutable out_ch = new output_null ()
  val mutable in_state = (`Start : input_state)
  val mutable out_state = (`Start : output_state)
  val mutable property = ([] : (string * string) list)
  val mutable in_header = new Netmime.basic_mime_header []
  val mutable out_header = new Netmime.basic_mime_header []

  initializer
    out_ch <- new output_channel 
                (* ~onclose:(fun() -> out_state <- `End) *) 
                stdout;

    (* Note: The onclose handler is commented out because it is difficult
     * to add the same functionality for the jserv stuff.
     *)


  method private fixup_server_name() =
    (* Fixup SERVER_NAME/SERVER_PORT: *)
    ( let server_name = List.assoc "SERVER_NAME" property in
      match Netstring_pcre.string_match server_name_with_port_re server_name 0
      with
	| Some m ->
	    let new_server_name = 
	      Netstring_pcre.matched_group m 1 server_name in
	    let new_server_port = 
	      Netstring_pcre.matched_group m 2 server_name in

	    property <- 
	      ("SERVER_NAME", new_server_name) ::
	      (List.filter (fun (n,_) -> n <> "SERVER_NAME") property);
	    
	    let cur_server_port =
	      try List.assoc "SERVER_PORT" property with Not_found -> "" in

	    if cur_server_port = "" then (
	      property <- 
		("SERVER_PORT", new_server_port) ::
		(List.filter (fun (n,_) -> n <> "SERVER_PORT") property);
	    )

	| None -> ()
    )


  (* This class works already, but lacks a complete initializer! *)

  method config = cfg

  method cgi_gateway_interface  = self # cgi_property ~default:"" 
				    "GATEWAY_INTERFACE"
  method cgi_server_software    = self # cgi_property ~default:"" 
				    "SERVER_SOFTWARE"
  method cgi_server_name        = self # cgi_property ~default:"" 
				    "SERVER_NAME"
  method cgi_server_protocol    = self # cgi_property ~default:"" 
				    "SERVER_PROTOCOL"
  method cgi_request_method     = self # cgi_property ~default:"" 
				    "REQUEST_METHOD"
  method cgi_path_info          = self # cgi_property ~default:"" 
				    "PATH_INFO"
  method cgi_path_translated    = self # cgi_property ~default:"" 
				    "PATH_TRANSLATED"
  method cgi_script_name        = self # cgi_property ~default:"" 
				    "SCRIPT_NAME"
  method cgi_query_string       = self # cgi_property ~default:"" 
				    "QUERY_STRING"
  method cgi_remote_host        = self # cgi_property ~default:"" 
				    "REMOTE_HOST"
  method cgi_remote_addr        = self # cgi_property ~default:"" 
				    "REMOTE_ADDR"
  method cgi_auth_type          = self # cgi_property ~default:"" 
				    "AUTH_TYPE"
  method cgi_remote_user        = self # cgi_property ~default:"" 
				    "REMOTE_USER"
  method cgi_remote_ident       = self # cgi_property ~default:"" 
				    "REMOTE_IDENT"
  method cgi_server_port        = 
    try Some(int_of_string(self # cgi_property "SERVER_PORT"))
    with Not_found -> None

  method cgi_property ?default name =
    match default with
	None   -> List.assoc name property
      | Some d -> try List.assoc name property with Not_found -> d

  method cgi_properties = property

  method cgi_https = 
    match String.lowercase(self # cgi_property ~default:"" "HTTPS") with
	"on"  -> true
      | "off" -> false
      | ""    -> false
      | _     -> failwith "Cannot interpret HTTPS property"

  method cgi_request_uri =
    self # cgi_property ~default:"" "REQUEST_URI"
      (* Apache has this usually *)

  method protocol : protocol =
    try
      let groups = Pcre.extract ~rex:(Pcre.regexp "^([^/]+)/(\\d+)\\.(\\d+)$")
		                (self # cgi_server_protocol) in
      if groups.(1) = "HTTP" then
	let atts =
	  if self # cgi_https then [ `Secure_https ] else [] in
	`Http ((int_of_string groups.(2), int_of_string groups.(3)), atts)
      else
	`Other
    with
	Not_found -> `Other

  method input_header = 
    in_header

  method input_header_field ?default name = 
    try in_header # field name 
    with Not_found as nf -> 
      match default with
	  None -> raise nf
	| Some d -> d

  method multiple_input_header_field name =
    in_header # multiple_field name

  method input_header_fields =
    in_header # fields

  method user_agent = 
    self # input_header_field ~default:"" "USER-AGENT"

  method cookies =
    Nethttp.Header.get_cookie self#input_header
      
  method input_ch = in_ch

  method input_content_length =
    int_of_string (self # input_header_field "CONTENT-LENGTH")

  method input_content_type_string =
    self # input_header_field ~default:"" "CONTENT-TYPE"

  method input_content_type =
    Mimestring.scan_mime_type_ep (self # input_header_field "CONTENT-TYPE") []

  method input_state = in_state
  method set_input_state s = in_state <- s

  method output_header =
    out_header

  method output_header_field ?default name =
    try out_header # field name 
    with Not_found as nf -> 
      match default with
	  None -> raise nf
	| Some d -> d

  method multiple_output_header_field name =
    out_header # multiple_field name

  method output_header_fields =
    out_header # fields

  method output_ch =
    out_ch 

  method set_output_header_field name value =
    out_header # update_field name value

  method set_multiple_output_header_field name values =
    out_header # update_multiple_field name values

  method set_output_header_fields h =
    out_header # set_fields h

  method set_status st =
    out_header # update_field "Status" (string_of_int (int_of_http_status st))

  method send_output_header () =
    if out_state <> `Start then
      failwith "send_output_header";
    out_state <- `Sending_header;
    (* Note: ~soft_eol:"" because linear whitespace is illegal in CGI
     * responses
     *)
    Mimestring.write_header ~soft_eol:"" ~eol:"\r\n" out_ch out_header#fields;
    out_state <- `Sent_header;

  method output_state = out_state
  method set_output_state s = 
    out_state <- s;
    if s = `End then
      ( try out_ch # close_out() with Closed_channel -> () )

  method log_error s =
    prerr_endline s
      (* This usually works for command-line and CGI *)

end ;;


exception Std_environment_not_found ;;

let equation_re = Pcre.regexp "^([^=]*)=(.*)$" ;;
let http_re     = Pcre.regexp "^HTTP_(.*)" ;;
let uscore_re   = Pcre.regexp "_" ;;

class std_environment ?config () =
object (self)
  inherit base_environment ?config ()

  initializer
    (* Check whether the environment is CGI: *)
    ( try
	ignore(Sys.getenv "SERVER_SOFTWARE");
        ignore(Sys.getenv "SERVER_NAME");
        ignore(Sys.getenv "GATEWAY_INTERFACE");
        ()
      with
	  Not_found -> raise Std_environment_not_found
    );
    (* Do we need to normalize CONTENT-TYPE? *)
    let user_agent = try Sys.getenv "HTTP_USER_AGENT" with Not_found -> "" in
    let norm_content_type s =
      if Pcre.pmatch ~pat:"MSIE" user_agent &&
         List.mem `Work_around_MSIE_Content_type_bug cfg.workarounds
      then begin
	(* Microsoft Internet Explorer: When used with SSL connections,
	 * this browser sometimes produces CONTENT_TYPEs like
	 * "multipart/form-data; boundary=..., multipart/form-data; boundary=..."
	 * Workaround: Throw away everything after ", ".
	 *)
	try
	  let groups =
	    Pcre.extract ~rex:(Pcre.regexp "([^,]*boundary[^,]*), .*boundary")
	                 s
	  in
	  groups.(1)
	with
	    Not_found -> s
      end
      else s
    in
    (* Read the environment and initialize [property] and [in_header]: *)
    let ih = ref [] in
    Array.iter
      (fun s ->
	 try
	   let [|_; name; value|] = Pcre.extract ~rex:equation_re s in
	   (* or raise Not_found *)
	   match name with
	       "CONTENT_TYPE" ->
		 (* Add to in_header, not property: *)
		 ih := ("CONTENT-TYPE", norm_content_type value) :: !ih
	     | "CONTENT_LENGTH" ->
		 (* Add to in_header, not property: *)
		 ih := ("CONTENT-LENGTH", value) :: !ih
	     | _ ->
		 try
		   let [|_; hname|] = Pcre.extract ~rex:http_re name in
		   (* or raise Not_found *)
		   (* No Not_found: The variable begins with HTTP_ and
		    * is a header field
		    *)
		   let hname' = Pcre.qreplace ~rex:uscore_re ~templ:"-" hname in
		   let hname'' = String.uppercase hname' in
		   ih := (hname'',value) :: !ih
		 with
		     Not_found ->
		       (* The variable is a property *)
		       property <- (name, value) :: property;
	 with
	     Not_found ->
	       (* The variable is malformed. Ignore it. *)
	       ()
      )
      (Unix.environment());
    (* Maybe the order of fields counts, so repair the order: *)
    in_header <- new Netmime.basic_mime_header ~ro:true (List.rev !ih);
    property <- List.rev property;
    self # fixup_server_name();
    (* Update the input state: *)
    in_state <- `Received_header;
end ;;


let isatty() =
  (* Is stdin connected to a tty? *)
  match Sys.os_type with
    | "Unix" 
    | "Cygwin" ->
	( try ignore(Unix.tcgetattr Unix.stdin); true
	  with Unix.Unix_error(Unix.ENOTTY,_,_) -> false
	)
    | "Win32" 
    | "MacOS" ->
	(* Don't know how to do this. So assume it's a tty anyway. *)
	true
    | _ ->
	assert false
;;

exception No_equation of string

let split_name_is_value s =
  (* Recognizes a string "name=value" and returns the pair (name,value).
   * If the string has the wrong format, the function will raise
   * No_equation, and the argument of the exception is the unparseable
   * string.
   *)
  try
    let p = String.index s '=' in
    (String.sub s 0 p, String.sub s (p+1) (String.length s - p - 1))
  with
	Not_found ->
          raise(No_equation s)
	  
class test_environment ?config () =
object (self)
  inherit base_environment ?config ()

  method private init_from_cmd_line () =
    let usage = ref (fun () -> ()) in
    let mimetype = ref "text/plain" in
    let filename = ref None in
    let args = ref [] in
    let fileargs = ref [] in
    let putarg = ref "" in
    let cgi_method = ref "GET" in
    let user = ref "" in
    let props = ref [] in
    let header = ref [] in
    let keywords =
      [ 
	"-get",
	Arg.Unit (fun _ -> cgi_method := "GET"),
	     "                   Set the method to GET (the default)";
	"-head",
	Arg.Unit (fun _ -> cgi_method := "HEAD"),
	     "                   Set the method to HEAD";
	"-post",
	Arg.Unit (fun _ -> cgi_method := "POST"),
	      "                  Set the method to POST enctype multipart/form-data";
	"-put",
	Arg.String (fun s -> cgi_method := "PUT"; putarg := s),
	          "file          Set the method to PUT and read this file";
	"-delete",
	Arg.Unit (fun _ -> cgi_method := "DELETE"),
	        "                Set the method to DELETE";
	"-mimetype", 
	Arg.String (fun s -> mimetype := s),
	          "type          Set the MIME type for the next file argument(s) (default: text/plain)";
	"-filename", 
	Arg.String (fun s -> if s = "" then filename := None 
                                       else filename := Some s),
	          "path          Set the filename property for the next file argument(s)";
	"-filearg",
	Arg.String
	  (fun s ->
	     let (name,file) = split_name_is_value s in
	     fileargs := !fileargs @ [name, (!mimetype,!filename,file)];
	  ),
	         "name=file      Specify a file argument whose contents are in the file";
	"-user",
	Arg.String (fun s -> user := s),
	      "name              Set REMOTE_USER to this name";
	"-prop",
	Arg.String (fun s -> props := !props @ [split_name_is_value s]),
	      "name=value        Set the environment property";
	"-header",
	Arg.String (fun s -> header := !header @ [split_name_is_value s]),
	        "name=value      Set the request header field";
	"-help",
	Arg.Unit (fun () -> !usage()),
	      "                  Output this help";
      ]
    in
    let usage_string = "This program expects a CGI environment. You can simulate such an environment\n\
                        by name=value command-line arguments. Furthermore, the following options\n\
                        are recognized:" in
    usage := (fun () -> Arg.usage keywords usage_string; flush stderr; exit 0);

    ( try 
	Arg.parse 
	    keywords
	      (fun s -> 
		 let (name,value) = split_name_is_value s in
		 args := !args @ [name, value];
	      )
	      usage_string;
      with
	  No_equation s ->
	    failwith ("CGI command-line parameter: Cannot parse: " ^ s)
    );
    
    let qs_methods = ["GET";"HEAD";"PUT";"DELETE"] in  
    (* methods requiring QUERY_STRING *)

    let mk_query_string() =
      if !fileargs <> [] then
	prerr_endline "Warning: Ignoring -filearg arguments (would need -post)";
      Netencoding.Url.mk_url_encoded_parameters !args
    in

    let ch, ch_len, ch_type =
      match !cgi_method with
	| "GET" 
	| "HEAD"
	| "DELETE" ->
	    (* Input is empty *)
	    (new input_string "", 0, "")
	| "PUT" ->
	    (* Input is the specified file *)
	    let f = open_in_bin !putarg in
	    (new input_channel f, in_channel_length f, "application/octet-stream")
	| "POST" ->
	    (* For simplicity, use a pipe to keep the generated POST data: *)
	    let post_ch = new pipe() in
	    (* Use Netmime to create the POSTed MIME message: *)
	    let simple_msgs =
	      List.map
		(fun (n,v) ->
		   let cdisp = Buffer.create 80 in
		   let cdisp_ch = new output_buffer cdisp in
		   cdisp_ch # output_string "form-data";
		   Mimestring.write_value cdisp_ch
		     (Mimestring.param_tokens 
			[ "name", Mimestring.mk_param n ]);
		   let hdr = new Netmime.basic_mime_header
			       [ "content-disposition", Buffer.contents cdisp ] in
		   let body = new Netmime.memory_mime_body v in
		   (hdr, `Body body)
		)
		!args in
	    let file_msgs =
	      List.map
		(fun (n,(mt,fn,fl)) ->
		   let cdisp = Buffer.create 80 in
		   let cdisp_ch = new output_buffer cdisp in
		   cdisp_ch # output_string "form-data";
		   Mimestring.write_value cdisp_ch
		     (Mimestring.param_tokens 
			( ("name", Mimestring.mk_param n) ::
			    ( match fn with
				  None -> []
				| Some x -> ["filename", Mimestring.mk_param x];
			    )));
		   let hdr = new Netmime.basic_mime_header
			       [ "content-disposition", Buffer.contents cdisp;
				 "content-type", mt;
			       ] in
		   let body = new Netmime.file_mime_body fl in
		   (hdr, `Body body)
		)
		!fileargs in
	    let whole_msg =
	      ( new Netmime.basic_mime_header 
		  [ "Content-type", "multipart/form-data"],
		`Parts (simple_msgs @ file_msgs)
	      )
	    in
	    (* Write the MIME message to the pipe: *)
	    let post_boundary = ref "" in
	    Netmime.write_mime_message 
	      ~wr_header:false ~ret_boundary:post_boundary
	      (post_ch :> out_obj_channel) 
	      whole_msg;
	    (* Close the pipe, so we can read from it and detect EOF: *)
	    let post_length = post_ch # pos_out in
	    post_ch # close_out();
	    let post_type =
	      "multipart/form-data;boundary=\"" ^ !post_boundary ^ "\"" in
	    (* Pass everything back: *)
	    ((post_ch :> in_obj_channel), post_length, post_type)
			    
	| _ -> assert false
    in

    in_ch <- ch;

    property <- [ "GATEWAY_INTERFACE", "CGI/1.1";
		  "SERVER_SOFTWARE",   "Netcgi_env/test_environment";
		  "SERVER_NAME",       "localhost";
		  "SERVER_PROTOCOL",   "HTTP/1.0";
		  "REQUEST_METHOD",    !cgi_method;
		  "SCRIPT_NAME",       "/[SCRIPTNAME]";
		  "QUERY_STRING",      if List.mem !cgi_method qs_methods
		                       then mk_query_string()
				       else "";
		  "REMOTE_HOST",       "localhost";
		  "REMOTE_ADDR",       "127.0.0.1";
                ] @
                (if !user <> "" then
		   [ "REMOTE_USER", !user;
		     "AUTH_TYPE",   "basic";
		   ]
		 else
		   []
		);

    let ih = ref [ "CONTENT-LENGTH", string_of_int ch_len;
		   "CONTENT-TYPE",   ch_type;
		   "USER-AGENT",     "Netcgi_env/test_environment";
		 ] in
    
    List.iter
      (fun (n,v) ->
	 property <- (n, v) :: List.remove_assoc n property)
      !props;

    List.iter
      (fun (n,v) ->
	 ih := (n, v) :: List.remove_assoc n !ih)
      !header;

    in_header <- new Netmime.basic_mime_header ~ro:true !ih;

    in_state <- `Received_header


  method private init_interactively() =
    prerr_endline "This is a CGI program. You can now input arguments, every argument on a new";
    prerr_endline "line in the format name=value. The request method is fixed to GET, and cannot";
    prerr_endline "be changed in this mode. Consider using the command-line for more options.";
    let continue = ref true in
    let args = ref [] in
    (try
       while !continue do
	 prerr_string "> ";
	 flush stderr;
	 let line = read_line () in
	 if line <> "." then
	   try
	     let n,v = split_name_is_value line in
	     args := !args @ [n, v];
	   with
	       No_equation _ ->
		 prerr_string "Error. Do you want to enter more arguments? (y/n) ";
		 flush stderr;
		 let answer = read_line () in
		 continue := (answer = "y") || (answer = "Y") ||
		             (answer = "yes") || (answer = "YES")
	 else
	   continue := false
       done
     with
	 End_of_file ->
	   prerr_endline "(Got EOF)";
    );
    prerr_endline "(Continuing the program)";
    flush stderr;

    in_ch <- new input_string "";

    property <- [ "GATEWAY_INTERFACE", "CGI/1.1";
		  "SERVER_SOFTWARE",   "Netcgi_env/test_environment";
		  "SERVER_NAME",       "localhost";
		  "SERVER_PROTOCOL",   "HTTP/1.0";
		  "REQUEST_METHOD",    "GET";
		  "SCRIPT_NAME",       "/[SCRIPTNAME]";
		  "QUERY_STRING",      Netencoding.Url.mk_url_encoded_parameters !args;
		  "REMOTE_HOST",       "localhost";
		  "REMOTE_ADDR",       "127.0.0.1";
                ];

    in_header <- new Netmime.basic_mime_header ~ro:true
                   [ "CONTENT-LENGTH", "0";
		     "USER-AGENT",     "Netcgi_env/test_environment";
		   ];

    in_state <- `Received_header;

  initializer
    let have_command_line_options = !Arg.current+1 < Array.length Sys.argv in
    if have_command_line_options then
      self # init_from_cmd_line()
    else
      if isatty() then
	self # init_interactively()
      else
	failwith "test_environment: Neither command line options nor tty to ask user"
end ;;


class custom_environment ?config () =
object (self)
  inherit base_environment ?config ()

  val mutable setup_phase = true
  val mutable error_log = prerr_endline


  method set_input_ch ch = 
    if not setup_phase then failwith "custom_environment: setup already over";
    in_ch <- ch

  method set_output_ch ch = 
    if not setup_phase then failwith "custom_environment: setup already over";
    out_ch <- ch

  method set_input_content_length n = 
    self # set_input_header_field "CONTENT-LENGTH" (string_of_int n)

  method set_input_content_type t =
    self # set_input_header_field "CONTENT-TYPE" t

  method set_input_header_field name value =
    in_header # update_field name value

  method set_multiple_input_header_field name values =
    in_header # update_multiple_field name values

  method set_input_header_fields h =
    in_header # set_fields h

  method set_error_log f =
    error_log <- f

  method set_cgi
           ?gateway_interface
           ?server_software
           ?server_name
           ?server_protocol
           ?server_port
           ?request_method
           ?path_info
           ?path_translated
           ?script_name
           ?query_string
           ?remote_host
           ?remote_addr
           ?auth_type
           ?remote_user
           ?remote_ident
	   ?https
           ?property:prop
           () =
    let set name value_opt =
      match value_opt with
	  None -> ()
	| Some v ->
	    property <- (name,v) :: (List.remove_assoc name property)
    in
    
    if not setup_phase then failwith "custom_environment: setup already over";

    set "GATEWAY_INTERFACE" gateway_interface;
    set "SERVER_SOFTWARE" server_software;
    set "SERVER_NAME" server_name;
    set "SERVER_PROTOCOL" server_protocol;
    set "SERVER_PORT" 
      (match server_port with 
	   None -> None 
	 | Some None -> Some ""
	 | Some (Some n) -> Some (string_of_int n));
    set "REQUEST_METHOD" request_method;
    set "PATH_INFO" path_info;
    set "PATH_TRANSLATED" path_translated;
    set "SCRIPT_NAME" script_name;
    set "QUERY_STRING" query_string;
    set "REMOTE_HOST" remote_host;
    set "REMOTE_ADDR" remote_addr;
    set "AUTH_TYPE" auth_type;
    set "REMOTE_USER" remote_user;
    set "REMOTE_IDENT" remote_ident;
    set "HTTPS" 
      (match https with 
	   None -> None 
	 | Some false -> Some "off" 
	 | Some true -> Some "on");

    (match prop with
	 None -> ()
       | Some (n,v) -> set n (Some v)
    );

    self # fixup_server_name();

  method setup_finished() = 
    setup_phase <- false;
    in_header <- new Netmime.basic_mime_header ~ro:true in_header#fields

  method log_error s =
    error_log s
end ;;

    

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.15  2006/03/06 16:51:14  stolpmann
 * Fix: Web servers sending SERVER_NAME strings including
 * port numbers are now tolerated.
 *
 * Revision 1.14  2005/07/25 22:36:40  stolpmann
 * Merge of nethttpd.
 * Adding the "overview" section to Nethttpd_types.
 * Cosmetic fixes.
 * Release updates.
 *
 * Revision 1.13.2.1  2005/04/30 16:31:16  stolpmann
 * Integration of the new Nethttpd component into Ocamlnet.
 *
 * Revision 1.13  2004/11/24 18:01:13  gremlin43820
 * commiting fastcgi compatibility patch from Kristof Pap which allows us to talk to a wider variety of fastcgi enabled web servers, and more closely follows the standard.
 *
 * Revision 1.12  2002/10/24 23:47:48  stolpmann
 * 	Support for the HEAD method.
 * 	Workaround for a bug in MSIE: Empty cookies are represented
 * in the wrong way
 *
 * Revision 1.11  2002/02/01 23:28:37  stolpmann
 * 	Fix: If the header of a CGI response contains embedded linefeeds,
 * these are now deleted. (Otherwise the server would complain.)
 *
 * Revision 1.10  2002/01/23 22:24:57  stolpmann
 * 	Removed the last references to the old Cgi module. Generating
 * POSTed bodies is now done using Netmime.
 *
 * Revision 1.9  2002/01/21 00:48:03  stolpmann
 * 	Using Netmime.write_header to output the header.
 *
 * Revision 1.8  2002/01/14 01:12:10  stolpmann
 * 	Representing input and output headers as Netmime.mime_header.
 * 	Removed the method _cgi, and many references to the Cgi module.
 *
 * Revision 1.7  2001/10/17 04:32:24  pdoane
 * Updated for 3.03
 *
 * Revision 1.6  2001/10/07 19:45:24  stolpmann
 * 	Method [cookies]: Now implemented directly here (the
 * implemention in Cgi is not used any longer). Works now even if there
 * are several cookie fields.
 *
 * Revision 1.5  2001/10/04 01:04:58  stolpmann
 * 	Moved from directory /src/netstring to /src/cgi.
 *
 * Revision 1.4  2001/10/04 00:56:12  stolpmann
 * 	Implemented class [custom_environment].
 * 	Fixed method [user_agent].
 *
 * Revision 1.3  2001/09/30 00:04:33  stolpmann
 * 	Implemented the [protocol] method.
 * 	Implemented the interactive test bench.
 * 	Minor fixes in the test_environment
 *
 * Revision 1.2  2001/09/27 22:01:30  stolpmann
 * 	Fixed the worst bugs.
 * 	Changed the type protocol_attribute.
 *
 * Revision 1.1  2001/09/24 21:26:54  stolpmann
 * 	Initial revision (compiles, but untested)
 *
 * 
 *)
