(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


module Str = Netstring_pcre

(**********************************************************************)
(* Types and exceptions                                               *)
(**********************************************************************)

exception Resources_exceeded

type argument_processing = Memory | File | Automatic;;

type argument =
    { mutable arg_name : string;
      mutable arg_processing : argument_processing;
      mutable arg_buf_value : Buffer.t;
      mutable arg_mem_value : string option;
              (* Here, the value is stored if it must be kept in memory *)
      mutable arg_disk_value : string Weak.t;
              (* This component is used iff arg_mem_value = None. The
	       * weak array has a length of 1, and the single element stores
	       * the value (if any).
	       *)
      mutable arg_file : string option;
              (* The filename of the temporary file storing the value *)
      mutable arg_fd : out_channel option;
              (* The file descriptor of the temp file (if open) *)
      mutable arg_mimetype : string;
      mutable arg_filename : string option;
      mutable arg_header : (string * string) list;
              (* For the last three components, see the description of the
	       * corresponding functions in the mli file.
	       *)
    }
;;

type workaround =
    Work_around_MSIE_Content_type_bug
  | Work_around_backslash_bug
;;

type config =
    { maximum_content_length : int;
      how_to_process_arguments : argument -> argument_processing;
      tmp_directory : string;
      tmp_prefix : string;
      workarounds : workaround list;
      enable_testing : bool;
    }
;;

type state =
    { mutable args : (string * argument) list;
      mutable args_set : bool;
              (* Whether args has been set (parse_arguments or set_arguments) *)
      mutable using_temp_files : bool;
              (* Whether files are temporary and cleanup may delete them. 
	       * This feature is disabled if files are specified on the 
	       * command line.
	       *)
      mutable env  : (string * string Lazy.t) list;  
              (* contains only the known environment variables *)
      mutable get_env : string -> string;
              (* how to get unknown environment variables *)
      mutable input_ch : in_channel;
      mutable input_ch_parsed : bool;
      mutable content_type : string;
      mutable content_length : int option;
      mutable config : config;
    }
;;

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
;;

type cache_control =
    [ `No_cache
    | `Max_age of int
    | `Unspecified
    ]
;;


type cookie =
    { cookie_name : string;
      cookie_value : string;
      cookie_expires : float option;
      cookie_domain : string option;
      cookie_path : string option;
      cookie_secure : bool;
    }
;;

(**********************************************************************)
(* aux functions for files                                            *)
(**********************************************************************)

let make_temporary_file config =
  (* Returns (filename, out_channel). *)
  let rec try_creation n =
    try
      let fn =
	Filename.concat
	  config.tmp_directory
	  (config.tmp_prefix ^ "-" ^ (string_of_int n))
      in
      let fd =
	open_out_gen
	  [ Open_wronly; Open_creat; Open_excl; Open_binary ]
	  0o666
	  fn
      in
      fn, fd
    with
	Sys_error m ->
	  (* This does not look very intelligent, but it is the only chance
	   * to limit the number of trials.
	   *)
	  if n > 1000 then
	    failwith ("Cgi: Cannot create temporary file: " ^ m);
	  try_creation (n+1)
  in
  try_creation 0
;;


let read_file name =
  (* Reads contents of file and returns them as string *)
  let b = Buffer.create 1024 in
  let s = String.create 1024 in
  let f = open_in_bin name in
  try
    while true do
      let n = input f s 0 (String.length s) in
      if n = 0 then raise End_of_file;
      Buffer.add_substring b s 0 n;
    done;
    assert false
  with
      End_of_file ->
	close_in f;
	Buffer.contents b
    | err ->
	close_in f;
	raise err
;;


let isatty() =
  (* Is stdin connected to a tty? *)
  match Sys.os_type with
    | "Unix" 
    | "Cygwin" ->
	(* Ask the "tty" program. If we had the Unix module, we could check
	 * this directly.
	 *)
	Sys.command "tty -s" = 0
    | "Win32" 
    | "MacOS" ->
	(* Don't know how to do this. So assume it's a tty anyway. *)
	true
    | _ ->
	assert false
;;


(**********************************************************************)
(* argument primitives                                                *)
(**********************************************************************)

let mk_simple_arg ~name v =
  { arg_name = name;
    arg_processing = Memory;
    arg_buf_value = Buffer.create 1;
    arg_mem_value = Some v;
    arg_disk_value = Weak.create 0;
    arg_file = None;
    arg_fd = None;
    arg_mimetype = "text/plain";
    arg_filename = None;
    arg_header = [];
  }
;;

let mk_memory_arg ~name ?(mime = "text/plain") ?filename ?(header = []) v =
  { arg_name = name;
    arg_processing = Memory;
    arg_buf_value = Buffer.create 1;
    arg_mem_value = Some v;
    arg_disk_value = Weak.create 0;
    arg_file = None;
    arg_fd = None;
    arg_mimetype = mime;
    arg_filename = filename;
    arg_header = header;
  }
;;

let mk_file_arg 
  ~name ?(mime = "text/plain") ?filename ?(header = []) v_filename =
  let v_abs_filename =
    if Filename.is_relative v_filename then
      Filename.concat (Sys.getcwd()) v_filename
    else
      v_filename
  in
  { arg_name = name;
    arg_processing = File;
    arg_buf_value = Buffer.create 1;
    arg_mem_value = None;
    arg_disk_value = Weak.create 1;
    arg_file = Some v_abs_filename;
    arg_fd = None;
    arg_mimetype = mime;
    arg_filename = filename;
    arg_header = header;
  }
;;


let print_argument arg =
  Format.printf
    "<CGIARG name=%s filename=%s mimetype=%s store=%s>"
    arg.arg_name
    (match arg.arg_filename with None -> "*" | Some n -> n)
    arg.arg_mimetype
    (match arg.arg_file with None -> "Memory" | Some n -> n)
;;


(**********************************************************************)
(* default configuration                                              *)
(**********************************************************************)

let default_config =
  { maximum_content_length = max_int;
    how_to_process_arguments = (fun _ -> Memory);
    tmp_directory = "/var/tmp";
    tmp_prefix = "cgi-";
    workarounds = [ Work_around_MSIE_Content_type_bug;
		    Work_around_backslash_bug;
		  ];
    enable_testing = true;
  }
;;

(**********************************************************************)
(* URL-encoding                                                       *)
(**********************************************************************)

let encode s = Netencoding.Url.encode s;;
let decode s = Netencoding.Url.decode s;;

let mk_url_encoded_parameters = Netencoding.Url.mk_url_encoded_parameters ;;
let dest_url_encoded_parameters = Netencoding.Url.dest_url_encoded_parameters ;;


let mk_query_string args =
  let mem_args =
    List.filter (fun (_,arg) -> arg.arg_mem_value <> None) args in
  let nv_pairs =
    List.map
      (fun (n,arg) ->
	 match arg.arg_mem_value with
	     Some v -> (n,v) 
	   | None   -> assert false
      )
      mem_args
  in
  mk_url_encoded_parameters nv_pairs
;;

(**********************************************************************)
(* state basics                                                       *)
(**********************************************************************)

let create_state() =
  { args = [];
    args_set = false;
    using_temp_files = true;
    env = [];
    get_env = (fun name -> raise Not_found);
    input_ch = stdin;
    input_ch_parsed = false;
    content_type = "";
    content_length = None;
    config = default_config;
  }
;;


let create_internal_state() =
  let state = create_state() in
  { state with
    get_env = (fun name -> failwith "Cgi: Cannot access environment before parse_arguments has been called");
  }
;;


let internal_state = create_internal_state();;

let have_env_var name =
  try ignore(Sys.getenv name); true with Not_found -> false
;;

let getenv state name =
  try
    Lazy.force(List.assoc name state.env)       (* already known? *)
  with
      Not_found ->
	let v =
	  try state.get_env name
	  with Not_found -> ""                  (* not set: same as "" *)
	in
	state.env <- (name,lazy v) :: state.env;
	v
;;


let method_supports_query_string meth =
  List.mem meth [ "GET"; "HEAD"; "PUT"; "DELETE" ]
;;


let state_supports_query_string state =
  method_supports_query_string (getenv state "REQUEST_METHOD")
;;



let get_content_type state config =
  (* Get the environment variable CONTENT_TYPE; if necessary apply
   * workarounds for browser bugs.
   *)
  let content_type = getenv state "CONTENT_TYPE" in
  let user_agent = getenv state "HTTP_USER_AGENT" in
  let eff_content_type =
    if Str.string_match 
         (Str.regexp ".*MSIE") user_agent 0 <> None &&
       List.mem Work_around_MSIE_Content_type_bug config.workarounds
    then begin
      (* Microsoft Internet Explorer: When used with SSL connections,
       * this browser sometimes produces CONTENT_TYPEs like
       * "multipart/form-data; boundary=..., multipart/form-data; boundary=..."
       * Workaround: Throw away everything after ", ".
       *)
      match Str.string_match 
	      (Str.regexp "([^,]*boundary[^,]*), .*boundary")
	      content_type 0
      with
	  Some result ->
	    Str.matched_group result 1 content_type
	| None ->
	    content_type
    end
    else
      content_type
  in
  eff_content_type
;;


let lazy_query_string state =
  lazy
    (if state_supports_query_string state then
       mk_query_string state.args
     else
       ""
    )
;;


let split_name_is_value s =
  (* Recognizes a string "name=value" and returns the pair (name,value).
   * If the string has the wrong format, the function will fail.
   *)
  try
    let p = String.index s '=' in
    (String.sub s 0 p, String.sub s (p+1) (String.length s - p - 1))
  with
      Not_found ->
	failwith ("Cgi: Cannot parse: " ^ s)
;;


let init_state ?(input=stdin) ?content_length ?content_type state config =
  assert(not state.args_set);
  assert(not state.input_ch_parsed);
  state.config <- config;
  state.input_ch <- input;
  state.content_length <- content_length;
  ( match content_type with
	Some t -> 
	  state.content_type <- t
      | None   -> 
	  state.content_type <- ""
  )
;;


let have_cgi_environment () =
  have_env_var "SERVER_SOFTWARE" && have_env_var "SERVER_NAME" &&
  have_env_var "GATEWAY_INTERFACE" 
;;


let have_command_line_options config =
  config.enable_testing  && !Arg.current+1 < Array.length Sys.argv
;;


let have_terminal config =
  config.enable_testing  &&  isatty()
;;


let init_internal_state ?(input=stdin) ?content_length ?content_type config =
  assert(not internal_state.args_set);
  assert(not internal_state.input_ch_parsed);
  internal_state.config <- config;
  (* First check whether we are in a real CGI environment: *)
  if have_cgi_environment() then begin
    internal_state.get_env <- Sys.getenv;
    internal_state.input_ch <- input;
    ( match content_length with
	  Some l -> internal_state.content_length <- Some l
	| None   ->
	    let l = getenv internal_state "CONTENT_LENGTH" in
	    if l = "" then
	      internal_state.content_length <- None
	    else
	      internal_state.content_length <- Some(int_of_string l)
    );
    ( match content_type with
	  Some t -> 
	    internal_state.content_type <- t
	| None   -> 
	    internal_state.content_type <- get_content_type internal_state config
    );
  end
  else
    (* Are there command-line options? *)
    if have_command_line_options config then begin
      let usage = ref (fun () -> ()) in
      let mimetype = ref "text/plain" in
      let filename = ref None in
      let args = ref [] in
      let cgi_method = ref "GET" in
      let user = ref "" in
      let other = ref [] in
      let keywords =
	[ "-mimetype", 
	  Arg.String (fun s -> mimetype := s),
	            "type          Set the MIME type for the next argument(s) (default: text/plain)";
	  "-filename", 
	  Arg.String (fun s -> if s = "" then filename := None 
                                         else filename := Some s),
	            "path          Set the filename property for the next argument(s)";
	  "-memarg",
	  Arg.String
	    (fun s ->
	       let (name,file) = split_name_is_value s in
	       args := !args @ [name, 
				mk_memory_arg
				~name 
				~mime:!mimetype 
				?filename:!filename
				  (read_file file)]
	    ),
	          "name=file       Read the contents of the memory argument from a file";
	  "-filearg",
	  Arg.String
	    (fun s ->
	       let (name,file) = split_name_is_value s in
	       args := !args @ [name, 
				mk_file_arg
				~name 
				~mime:!mimetype 
				?filename:!filename
				  file];
	    ),
	           "name=file      Specify a file argument whose contents are in the file";
	  "-method",
	  Arg.String (fun s -> cgi_method := s),
	          "name            Simulate this method (default is GET)";
	  "-user",
	  Arg.String (fun s -> user := s),
	        "name              Set REMOTE_USER to this name";
	  "-set",
	  Arg.String (fun s -> other := !other @ [split_name_is_value s]),
	       "name=value         Set the environment variable name to value";
	  "-help",
	  Arg.Unit !usage,
	        "                  Output this help";
	]
      in
      let usage_string = "This program expects a CGI environment. You can simulate such an environment\n\
                          by name=value command-line arguments. Furthermore, the following options\n\
                          are recognized:" in
      usage := (fun () -> Arg.usage keywords usage_string);
      Arg.parse 
	  keywords
	  (fun s -> 
	     let (name,value) = split_name_is_value s in
	     args := !args @ [name, 
			      mk_memory_arg
			        ~name 
			        ~mime:!mimetype 
			        ?filename:!filename
				value];
	  )
	  usage_string;
      internal_state.get_env <- (fun _ -> raise Not_found);
      internal_state.args <- !args;
      internal_state.args_set <- true;
      internal_state.using_temp_files <- false;
      internal_state.env <-
        [ "GATEWAY_INTERFACE", lazy "CGI/1.1";
	  "SERVER_SOFTWARE",   lazy "NetstringCGI/0";
	  "SERVER_NAME",       lazy "localhost";
	  "SERVER_PROTOCOL",   lazy "HTTP/1.0";
	  "REQUEST_METHOD",    lazy !cgi_method;
	  "SCRIPT_NAME",       lazy Sys.argv.(0);
	  "QUERY_STRING",      lazy (if method_supports_query_string !cgi_method
				     then mk_query_string !args
				     else "");
	  "REMOTE_HOST",       lazy "localhost";
	  "REMOTE_ADDR",       lazy "127.0.0.1";
	  "HTTP_USER_AGENT",   lazy "NetstringCGI/0";
	] @
        (if !user <> "" then
	   [ "REMOTE_USER", lazy !user;
	     "AUTH_TYPE",   lazy "basic";
	   ]
	 else
	   []
	);
      List.iter
	(fun (n,v) ->
	   internal_state.env <-
	     (n, lazy v) :: List.remove_assoc n internal_state.env
	)
	!other;
    end
    else
      if have_terminal config then begin
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
		 args := !args @ [n, mk_memory_arg ~name:n v];
	       with
		   Failure _ ->
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
	internal_state.get_env <- (fun _ -> raise Not_found);
	internal_state.args <- !args;
	internal_state.args_set <- true;
	internal_state.using_temp_files <- false;
	internal_state.env <-
          [ "GATEWAY_INTERFACE", lazy "CGI/1.1";
	    "SERVER_SOFTWARE",   lazy "NetstringCGI/0";
	    "SERVER_NAME",       lazy "localhost";
	    "SERVER_PROTOCOL",   lazy "HTTP/1.0";
	    "REQUEST_METHOD",    lazy "GET";
	    "SCRIPT_NAME",       lazy Sys.argv.(0);
	    "QUERY_STRING",      lazy (mk_query_string !args);
	    "REMOTE_HOST",       lazy "localhost";
	    "REMOTE_ADDR",       lazy "127.0.0.1";
	    "HTTP_USER_AGENT",   lazy "NetstringCGI/0";
	  ];
      end
      else begin
	(* Finally output an error message. *)
	prerr_endline "Cgi: No CGI environment found. For testing, consider using command-line options";
	failwith "Cgi: No CGI environment found.";
      end
;;


(**********************************************************************)
(* Form encoded parameters                                            *)
(**********************************************************************)

let dest_parameter_header header options =
  let get_name s =
    (* s is: form-data; ... name="fieldname" ...
     * Extract "fieldname"
     *)
    try
      let tok, params = Mimestring.scan_value_with_parameters s options in
      List.assoc "name" params
    with
	Not_found ->
	  failwith "Cgi.dest_form_encoded_parameters"
      | Failure "Mimestring.scan_value_with_parameters" ->
	  failwith "Cgi.dest_form_encoded_parameters"
  in

  let get_filename s =
    (* s is: form-data; ... filename="fieldname" ...
     * Extract "fieldname"
     *)
    try
      let tok, params = Mimestring.scan_value_with_parameters s options in
      Some(List.assoc "filename" params)
    with
	Not_found ->
	  None
      | Failure "Mimestring.scan_value_with_parameters" ->
	  failwith "Cgi.dest_form_encoded_parameters"
  in

  let mime_type =
    try List.assoc "content-type" header
    with Not_found -> "text/plain" in     (* the default *)

  let content_disposition =
    try List.assoc "content-disposition" header
    with
	Not_found ->
	  failwith "Cgi.dest_form_encoded_parameters: no content-disposition"
  in

  let name = get_name content_disposition in
  let filename = get_filename content_disposition in

  name, mime_type, filename
;;


let dest_form_encoded_parameters parstr ~boundary config =
  let options =
    if List.mem Work_around_backslash_bug config.workarounds then
      [ Mimestring.No_backslash_escaping ]
    else
      []
  in
  let parts =
    Mimestring.scan_multipart_body_and_decode
      parstr 0 (String.length parstr) boundary in
  List.map
    (fun (params, value) ->

      let name, mime_type, filename = dest_parameter_header params options in
      { arg_name = name;
	arg_processing = Memory;
	arg_buf_value = Buffer.create 1;
	arg_mem_value = Some value;
	arg_disk_value = Weak.create 1;
	arg_file = None;
	arg_fd = None;
	arg_mimetype = mime_type;
	arg_filename = filename;
	arg_header = params;
      }

    )
    parts
;;


let dest_form_encoded_parameters_from_netstream s ~boundary config =
  let parts = ref [] in
  let options =
    if List.mem Work_around_backslash_bug config.workarounds then
      [ Mimestring.No_backslash_escaping ]
    else
      []
  in

  let create header =
    (* CALLBACK for scan_multipart_body_from_netstream *)
    let name, mime_type, filename = dest_parameter_header header options in
    let p0 =
      { arg_name = name;
	arg_processing = Memory;
	arg_buf_value = Buffer.create 80;
	arg_mem_value = None;
	arg_disk_value = Weak.create 1;
	arg_file = None;
	arg_fd = None;
	arg_mimetype = mime_type;
	arg_filename = filename;
	arg_header = header;
      }
    in
    let pr = config.how_to_process_arguments p0 in
    let p = { p0 with arg_processing = pr } in
    if pr = File then begin
      let fn, fd = make_temporary_file config in
      p.arg_file <- Some fn;
      p.arg_fd   <- Some fd;
      p.arg_mem_value <- None;
    end;
    p
  in

  let add p s k n =
    (* CALLBACK for scan_multipart_body_from_netstream *)
    if (p.arg_processing = Automatic) &&
       (Buffer.length (p.arg_buf_value) >= s # block_size) then begin
	 (* This is a LARGE argument *)
	 p.arg_processing <- File;
	 let fn, fd = make_temporary_file config in
	 p.arg_file <- Some fn;
	 p.arg_fd   <- Some fd;
	 p.arg_mem_value <- None;
	 output_string fd (Buffer.contents p.arg_buf_value);
	 p.arg_buf_value <- Buffer.create 1;
       end;

    match p.arg_processing with
	(Memory|Automatic) ->
	  Buffer.add_substring
	    p.arg_buf_value
	    (Netbuffer.unsafe_buffer s#window)
	    k
	    n
      | File ->
	  let fd = match p.arg_fd with Some fd -> fd | None -> assert false in
	  output
	    fd
	    (Netbuffer.unsafe_buffer s#window)
	    k
	    n;
  in

  let stop p =
    (* CALLBACK for scan_multipart_body_from_netstream *)
    begin match p.arg_processing with
	(Memory|Automatic) ->
	  p.arg_mem_value <- Some (Buffer.contents p.arg_buf_value);
	  p.arg_buf_value <- Buffer.create 1;
      | File ->
	  let fd = match p.arg_fd with Some fd -> fd | None -> assert false in
	  close_out fd;
	  p.arg_mem_value <- None
    end;
    parts := p :: !parts
  in

  Mimestring.scan_multipart_body_from_netstream
    s
    boundary
    create
    add
    stop;

  List.rev !parts
;;

(**********************************************************************)
(* Argument parsing                                                   *)
(**********************************************************************)

let really_parse_args state config =
  let make_simple_arg (n,v) = mk_simple_arg n v in

  match getenv state "REQUEST_METHOD" with
      ("GET"|"HEAD") ->
	List.map
	  make_simple_arg
	  (dest_url_encoded_parameters(getenv state "QUERY_STRING"))

    | "POST" ->
	let n = 
	  match state.content_length with
	      None -> failwith "Cgi.parse_arguments: No content length"
	    | Some x -> x
	in
	if n > config.maximum_content_length then
	  raise Resources_exceeded;
	begin
	  let mime_type, params =
	    Mimestring.scan_mime_type state.content_type [] in
	  match mime_type with
	      "application/x-www-form-urlencoded" ->
		let buf = String.create n in
		assert (not state.input_ch_parsed);
		really_input state.input_ch buf 0 n;
		state.input_ch_parsed <- true;
		List.map
		  make_simple_arg
		  (dest_url_encoded_parameters buf)
	    | "multipart/form-data" ->
		let boundary =
		  try
		    List.assoc "boundary" params
		  with
		      Not_found ->
			failwith "Cgi.parse_arguments"
		in
		(* -------------------------------------------------- DEBUG
		   let f = open_out "/tmp/cgiout" in
		   output_string f buf;
		   close_out f;
		 * --------------------------------------------------
		 *)
		assert (not state.input_ch_parsed);
		let r =
		  dest_form_encoded_parameters_from_netstream
		    (new Netstream.input_stream ~len:n 
		       (new Netchannels.input_channel state.input_ch))
		    boundary
		    config in
		state.input_ch_parsed <- true;
		r
	    | _ ->
		failwith ("Cgi.parse_arguments: unknown content-type " ^ mime_type)
	end
    | "PUT" ->
	failwith "Cgi.parse_arguments: PUT method not supported"
    | "DELETE" ->
	failwith "Cgi.parse_arguments: DELETE method not supported"
    | "" ->
	failwith "Cgi.parse_arguments: REQUEST_METHOD is unset or empty"
    | _ ->
	failwith "Cgi.parse_arguments: unknown method"
;;


(**********************************************************************)
(* Interface                                                          *)
(**********************************************************************)


(* In multi-threaded programs, several threads may concurrently access 
 * CGI data. Such accesses are serialized by a mutex that is created 
 * only in MT contexts.
 *
 * Every function below that accesses state must be protected.
 *)

let lock   = ref (fun () -> ());;
let unlock = ref (fun () -> ());;

let init_mt new_lock new_unlock =
  lock   := new_lock;
  unlock := new_unlock
;;

let protect f =
  !lock();
  try
    let r = f() in
    !unlock();
    r
  with
      x ->
        !unlock();
        raise x
;;

let parse_arguments ?(state = internal_state) 
                    ?input ?content_length ?content_type 
                    config =
  protect
    (fun () ->
       (* Reset for toploop? *)
       if state.args_set
	  && state == internal_state 
	  && not(have_cgi_environment()) 
	  && not(have_command_line_options config)
	  && !Sys.interactive
	  && (have_terminal config) 
       then begin
	 prerr_string "Do you want to enter new CGI arguments? (y/n) ";
	 flush stderr;
	 let answer = read_line() in
	 if answer = "y" || answer = "Y" || answer = "yes" || answer = "YES"
	 then
	   state.args_set <- false
	 else
	   ( prerr_endline "(Keeping old arguments)"; flush stderr );
       end;
       if not state.args_set then begin
	 (* Initialize state. *)
	 if state == internal_state then
	   init_internal_state ?input ?content_length ?content_type config
	 else
	   init_state ?input ?content_length ?content_type state config;
	 (* It is possible that the arguments are set now. *)
	 if not state.args_set then begin
	   let args = really_parse_args state config in
	   state.args <- List.map (fun arg -> arg.arg_name, arg) args;
	   state.args_set <- true;
	 end;
       end
    )
;;


let arguments ?(state = internal_state) () =
  protect
    (fun () ->
       if state.args_set then 
	 state.args
       else
	 failwith "Cgi.arguments";
    )
;;

let set_arguments ?(state = internal_state) ?set_query_string arglist =
  protect 
    (fun () ->
       state.args <- List.map (fun arg -> arg.arg_name,arg) arglist;
       state.args_set <- true;
       ( match set_query_string with
	     Some b ->
	       if b then
		 state.env <- ("QUERY_STRING", 
			       lazy (mk_query_string state.args)) ::
		              List.remove_assoc "QUERY_STRING" state.env;
	   | None ->
	       if state_supports_query_string state then
		 state.env <- ("QUERY_STRING", 
			       lazy (mk_query_string state.args)) ::
   		              List.remove_assoc "QUERY_STRING" state.env; 
       )
    )
;;

let update_argument ?(state = internal_state) arg =
  protect
    (fun () ->
       if not state.args_set then failwith "Cgi.update_argument";
       let name = arg.arg_name in
       state.args <- (name, arg) :: List.remove_assoc name state.args
    )
;;


let update_argument_value ?state ~name value =
  try
    let arg = List.assoc name (arguments ?state ()) in  (* or Not_found *)
    if arg.arg_file <> None then failwith "Cgi.update_argument_value";
    arg.arg_mem_value <- Some value;
  with
      Not_found ->
	update_argument ?state (mk_simple_arg name value)
;;


let delete_argument ?(state = internal_state) name =
    protect
    (fun () ->
       if not state.args_set then failwith "Cgi.delete_argument";
       state.args <- List.remove_assoc name state.args
    )
;;


let arg_value arg =
  match arg.arg_mem_value with
      None ->
	begin
	  match Weak.get arg.arg_disk_value 0 with
	      None ->
		begin
		  match arg.arg_file with
		      None ->
			failwith "Cgi.arg_value: no value present"
		    | Some filename ->
			let s = read_file filename in
			Weak.set arg.arg_disk_value 0 (Some s);
			s
		end
	    | Some v -> v
	end
    | Some s ->
	s
;;

let arg_name     arg = arg.arg_name;;
let arg_file     arg = arg.arg_file;;
let arg_mimetype arg = arg.arg_mimetype;;
let arg_filename arg = arg.arg_filename;;
let arg_header   arg = arg.arg_header;;

let cleanup ?(state = internal_state) () =
  protect
    (fun () ->
       if state.args_set && state.using_temp_files then begin
	 List.iter
	   (fun (name, arg) ->
	      match arg.arg_file with
		  None -> ()
		| Some filename ->
		    (* We do not complain if the file does not exist anymore. *)
		    if Sys.file_exists filename then
		      Sys.remove filename;
		    arg.arg_file <- None
	   )
	   state.args
       end
    )
;;

let argument ?state name = 
  List.assoc name (arguments ?state ());;
let argument_value ?state name = 
  arg_value (argument ?state name);;

module Operators = struct
  let ( !% ) = argument ?state:None
  let ( !$ ) = argument_value ?state:None
end;;


let parse_args() =
  (* Note: works always with internal_state *)
  parse_arguments default_config;
  List.map
    (fun (name, arg) -> name, arg_value arg)
    (arguments())
;;

let parse_args_with_mimetypes() =
  (* Note: works always with internal_state *)
  parse_arguments default_config;
  List.map
    (fun (name, arg) -> name, arg_mimetype arg, arg_value arg)
    (arguments())
;;

module Env = struct
  let get ?(state = internal_state) name =
    protect (fun() -> getenv state name)

  let set ?(state = internal_state) name value =
    protect 
      (fun () ->
	 state.env <- (name,lazy value) :: List.remove_assoc name state.env)

  let set_list ?(state = internal_state) nv_pairs =
    protect
      (fun () ->
	 state.env <- List.map (fun (n,v) -> (n, lazy v)) nv_pairs)

  let gateway_interface ?state () = get ?state "GATEWAY_INTERFACE"
  let server_software   ?state () = get ?state "SERVER_SOFTWARE"
  let server_name       ?state () = get ?state "SERVER_NAME"
  let server_protocol   ?state () = get ?state "SERVER_PROTOCOL"
  let request_method    ?state () = get ?state "REQUEST_METHOD"
  let path_info         ?state () = get ?state "PATH_INFO"
  let path_translated   ?state () = get ?state "PATH_TRANSLATED"
  let script_name       ?state () = get ?state "SCRIPT_NAME"
  let query_string      ?state () = get ?state "QUERY_STRING"
  let remote_host       ?state () = get ?state "REMOTE_HOST"
  let remote_addr       ?state () = get ?state "REMOTE_ADDR"
  let auth_type         ?state () = get ?state "AUTH_TYPE"
  let remote_user       ?state () = get ?state "REMOTE_USER"
  let remote_ident      ?state () = get ?state "REMOTE_IDENT"
  let http_user_agent   ?state () = get ?state "HTTP_USER_AGENT"
  let server_port       ?state () = 
    let s = get ?state "SERVER_PORT" in
    if s = "" then
      None
    else
      Some(int_of_string s)

  let major_protocol ?state () =
    let p = server_protocol ?state () in
    try
      let k = String.index p '/' in
      String.sub p 0 k
    with
	Not_found -> p

  let https ?state () =
    String.lowercase (get ?state "HTTPS") = "on"

  let protocol_is_http ?state () = 
    major_protocol ?state () = "HTTP" && not (https ?state ())

  let protocol_is_https ?state () = 
    major_protocol ?state () = "HTTP" && (https ?state ())


  let cookies ?state () =
    let cstring = get ?state "HTTP_COOKIE" in
    let parts = Str.split 
		  (Str.regexp "[ \t\r\n]*;[ \t\r\n]*")
		  cstring in
    List.map
      (fun part ->
	 let n,v = split_name_is_value part in
	 let n_dec = Netencoding.Url.decode n in
	 let v_dec = Netencoding.Url.decode v in
	 (n_dec, v_dec)
      )
      parts


  let set_variable ?(state  = internal_state)
                   ?gateway_interface ?server_software ?server_name
                   ?server_protocol ?server_port ?request_method ?path_info
                   ?path_translated ?script_name ?query_string ?remote_host
                   ?remote_addr ?auth_type ?remote_user ?remote_ident 
                   ?http_user_agent () =
    let list = ref [] in
    let names = ref [] in
    let add n cond =
      match cond with
	  None -> ()
	| Some v -> list := (n, lazy v) :: !list;   names := n :: !names
    in
    let add_port n cond =
      match cond with
	  None -> ()
	| Some None -> add n (Some "")
	| Some (Some port) -> add n (Some (string_of_int port))
    in
    add "GATEWAY_INTERFACE" gateway_interface;
    add "SERVER_SOFTWARE" server_software;
    add "SERVER_NAME" server_name;
    add "SERVER_PROTOCOL" server_protocol;
    add_port "SERVER_PORT" server_port;
    add "REQUEST_METHOD" request_method;
    add "PATH_INFO" path_info;
    add "PATH_TRANSLATED" path_translated;
    add "SCRIPT_NAME" script_name;
    add "QUERY_STRING" query_string;
    add "REMOTE_HOST" remote_host;
    add "REMOTE_ADDR" remote_addr;
    add "AUTH_TYPE" auth_type;
    add "REMOTE_USER" remote_user;
    add "REMOTE_IDENT" remote_ident;
    add "HTTP_USER_AGENT" http_user_agent;
    protect
      (fun () ->
	 state.env <- !list @ 
	              (List.filter 
			 (fun (n,v) -> not (List.mem n !names))
			 state.env)
      )

end
;;


let mk_form_encoded_parameters args =
  (* The only difficulty is to find a good boundary string. *)
  let mk_string boundary =
    (* raise Exit if the boundary does not work *)
    let b = Buffer.create 500 in
    List.iter
      (fun arg ->
	 (* Add boundary: *)
	 Buffer.add_string b "--";
	 Buffer.add_string b boundary;
	 Buffer.add_string b "\r\n";
	 (* Add header: *)
	 Printf.bprintf b "Content-disposition: form-data; name=\"%s\""
	   arg.arg_name;      (* FIXME: quote double quotes and backslashes *)
	 ( match arg.arg_filename with
	       None   -> Buffer.add_string b "\r\n";
	     | Some n -> Printf.bprintf b "; filename=\"%s\"\r\n" n
		           (* FIXME: quoting... *)
	 );
	 Printf.bprintf b "Content-type: %s\r\n" arg.arg_mimetype;
	 Printf.bprintf b "Content-transfer-encoding: binary\r\n";
	 Buffer.add_string b "\r\n";
	 (* Add body. The boundary string must not occur inside the value. *)
	 let v = arg_value arg in
	 if Pcre.pmatch ~pat:boundary v then raise Exit;
	 Buffer.add_string b v;
	 Buffer.add_string b "\r\n";
      )
      args;
    (* The terminating boundary: *)
    Buffer.add_string b "--";
    Buffer.add_string b boundary;
    Buffer.add_string b "--\r\n";
    Buffer.contents b
  in
  let rec find_boundary_and_mk_string n =
    let s = string_of_float (Unix.gettimeofday()) ^ "/" ^ string_of_int n in
    let s' = Digest.string s in
    let b = Buffer.create 80 in
    Printf.bprintf b "netstring_";
    for k = 0 to 15 do
      Printf.bprintf b "%02x" (Char.code s'.[k])
    done;
    Printf.bprintf b "-------------------------------";
    let bs = Buffer.contents b in
    try
      (mk_string bs, bs)
    with
	Exit -> find_boundary_and_mk_string (n+1)
  in
  find_boundary_and_mk_string 0
;;


(**********************************************************************)
(* Output                                                             *)
(**********************************************************************)

let status_line (s:status) =
  match s with
      `Ok -> "200 OK"
    | `Created -> "201 Created"
    | `Accepted -> "202 Accepted"
    | `Non_authoritative -> "203 Non-Authoritative Information"
    | `No_content -> "204 No content"
    | `Reset_content -> "205 Reset Content"
    | `Partial_content -> "206 Partial Content"
    | `Multiple_choices -> "300 Multiple Choices"
    | `Moved_permanently -> "301 Moved Permanently"
    | `Found -> "302 Found"
    | `See_other -> "303 See Other"
    | `Not_modified -> "304 Not Modified"
    | `Use_proxy -> "305 Use Proxy"
    | `Temporary_redirect -> "307 Temporary Redirect"
    | `Bad_request -> "400 Bad Request"
    | `Unauthorized -> "401 Unauthorized"
    | `Payment_required -> "402 Payment Required"
    | `Forbidden -> "403 Forbidden"
    | `Not_found -> "404 Not Found"
    | `Method_not_allowed -> "405 Method Not Allowed"
    | `Not_acceptable -> "406 Not Acceptable"
    | `Proxy_auth_required -> "407 Proxy Authentication Required"
    | `Request_timeout -> "408 Request Timeout"
    | `Conflict -> "409 Conflict"
    | `Gone -> "410 Gone"
    | `Length_required -> "411 Length Required"
    | `Precondition_failed -> "412 Precondition Failed"
    | `Request_entity_too_large -> "413 Request Entity Too Large"
    | `Request_uri_too_long -> "414 Request-URI Too Long"
    | `Unsupported_media_type -> "415 Unsupported Media Type"
    | `Requested_range_not_satisfiable -> "416 Requested Range Not Satisfiable"
    | `Expectation_failed -> "417 Expectation Failed"
    | `Internal_server_error -> "500 Internal Server Error"
    | `Not_implemented -> "501 Not Implemented"
    | `Bad_gateway -> "502 Bad Gateway"
    | `Service_unavailable -> "503 Service Unavailable"
    | `Gateway_timeout -> "504 Gateway Timeout"
    | `Http_version_not_supported -> "505 HTTP Version Not Supported"
;;


let header ?(output = stdout) 
           ?status 
           ?(cache = `Unspecified)
           ?(filename = "")
           ?(language = "")
           ?(script_type = "")
           ?(style_type = "")
           ?(set_cookie = [])
           ?(fields = []) 
           s =
  let t =
    match s with
	"" -> "text/html"
      | _  -> s
  in
  output_string output ("Content-type: " ^ t ^ "\n");
  (match status with
       None -> ()
     | Some s -> output_string output ("Status: " ^ status_line s ^ "\n"));
  (match cache with
       `Unspecified -> ()
     | `No_cache ->
	 let over = Unix.time() -. 1.0 in
	 output_string output "Cache-control: no-cache\n";
	 output_string output "Pragma: no-cache\n";
	 output_string output ("Expires: " ^ Netdate.mk_mail_date over ^ "\n")
     | `Max_age n ->
	 let exp = Unix.time() +. float_of_int n in
	 output_string output ("Cache-control: max-age=" ^ string_of_int n ^
			       "\n");
	 output_string output "Cache-control: must-revalidate\n";
	 output_string output ("Expires: " ^ Netdate.mk_mail_date exp ^ "\n");
  );
  if filename <> "" then begin
    (* We need to quote quotation marks and backslashes: *)
    let qfn = Str.global_substitute
                (Str.regexp "[\"\\]")
                (fun r s -> "\\" ^ Str.matched_string r s)
                filename 
    in
    output_string output 
      ("Content-disposition: attachment; filename=\"" ^ qfn ^ "\"\n");
  end;
  if language <> "" then
    output_string output ("Content-language: " ^ language ^ "\n");
  if script_type <> "" then
    output_string output ("Content-script-type: " ^ script_type ^ "\n");
  if style_type <> "" then
    output_string output ("Content-style-type: "  ^ style_type ^ "\n");
  List.iter
    (fun c ->
       output_string output "Set-cookie: ";
       let enc_name  = Netencoding.Url.encode ~plus:false c.cookie_name in
       let enc_value = Netencoding.Url.encode ~plus:false c.cookie_value in
       output_string output (enc_name ^ "=" ^ enc_value);
       ( match c.cookie_expires with
	     None -> ()
	   | Some t -> 
	       output_string output (";EXPIRES=" ^ Netdate.mk_usenet_date t);
       );
       (match c.cookie_domain with
	    None -> ()
	  | Some d ->
	      output_string output (";DOMAIN=" ^ d);
       );
       (match c.cookie_path with
	    None -> ()
	  | Some p ->
	      output_string output (";PATH=" ^ p);
       );
       if c.cookie_secure then output_string output (";SECURE");
       output_string output "\n";
    )
    set_cookie;
  List.iter
    (fun (n,v) ->
       output_string output (n ^ ": " ^ v ^ "\n")
    )
    fields;
  output_string output "\n";
  flush output
;;


let redirect ?(output = stdout) s =
  output_string output ("Location: " ^ s ^ "\n\n");
  flush output
;;


let this_url ?(state = internal_state) 
             ?(with_host_and_port = true)
	     ?(with_script_name = true)
	     ?(with_path_info = true)
             ?(with_query_string = false)
	     ?(update_query_string = false)
	     () =
  ( if with_host_and_port then
      let proto, proto_port =
	( if Env.protocol_is_http ~state () then
	    "http://", 80
	  else
	    if Env.protocol_is_https ~state () then
	      "https://", 443
	    else
	      failwith "Cgi.this_url: unknown protocol"
	) in
      proto ^ 
      Env.server_name ~state () ^ 
      match Env.server_port ~state() with
	  None -> ""
	| Some port -> 
	    if port = proto_port then "" else ":" ^ string_of_int port
    else
      ""
  ) ^ 
  ( if with_script_name then
      Env.script_name ~state ()
    else
      ""
  ) ^ 
  ( if with_path_info then
      Env.path_info ~state ()
    else
      ""
  ) ^ 
  ( if with_query_string then
      let qs = 
	if update_query_string then
	  mk_query_string state.args 
	else
	  Env.query_string ~state()
      in
      if qs = "" then
	""
      else
	"?" ^ qs
    else
      ""
  )
;;


(* ======================================================================
 * History:
 *
 * $Log$
 * Revision 2.7  2002/01/14 01:06:28  stolpmann
 * 	Moved the implementation of mk/dest_url_encoded_parameters to
 * Netencoding.Cgi.
 *
 * Revision 2.6  2001/12/28 21:15:19  stolpmann
 * 	Using the new OO interface of Netstream.
 *
 * Revision 2.5  2001/10/07 19:42:17  stolpmann
 * 	Change: The function [status_line] only returns the rhs of the
 * status field.
 *
 * Revision 2.4  2001/09/30 00:11:07  stolpmann
 * 	Bugfix in mk_form_encoded_parameters.
 *
 * Revision 2.3  2001/09/24 21:36:27  stolpmann
 * 	Added an implementation for mk_form_encoded_parameters.
 *
 * Revision 2.2  2001/09/23 20:39:42  pdoane
 * Migration to PCRE
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
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
 * Revision 1.5  2000/05/16 22:29:36  gerd
 * 	Added support for two common file upload bugs.
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
