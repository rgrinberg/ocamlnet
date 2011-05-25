
open Http_client;;

let print_hex s =
  let hex = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
	       '8'; '9'; 'A'; 'B'; 'C'; 'D'; 'E'; 'F' |] in
  for i = 0 to String.length s - 1 do
    let x = Char.code (s.[i]) in
    print_char (hex.(x lsr 4));
    print_char (hex.(x land 15));
  done
;;

let rec string_of_exn x =
  match x with
      Failure f ->
	"Failure: " ^ f
    | Http_error (n,s) ->
	"Http_error(" ^ string_of_int n ^ ","  ^ s ^ ")"
    | Http_protocol x' ->
	"Http_protocol: " ^ string_of_exn x'
    | Bad_message s ->
	"Bad_message: " ^ s
    | e ->
	Printexc.to_string e
;;



let main() = 
  let server = ref "localhost" in
  let port = ref 80 in
  let realm = ref "" in
  let user = ref "" in
  let password = ref "" in
  let proxy = ref false in
  let proxy_user = ref "" in
  let proxy_password = ref "" in
  let verbose = ref false in
  let catch_unix_errors = ref false in
  let pipeline = ref (new pipeline) in
  let messages = ref [] in
  let handshake = ref false in
  let chreq = ref false in
  let ssl = ref false in

  Ssl.init();

  let setup () =
    if !verbose then begin
      let opt = !pipeline # get_options in
      !pipeline # set_options
	{ opt with verbose_status = true;
	           verbose_request_header = true;
		   verbose_response_header = true;
		   verbose_request_contents = true;
		   verbose_response_contents = true;
		   verbose_connection = true ;
		   verbose_events = true;
		   number_of_parallel_connections = 1;
	};
      let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
      let tct = Https_client.https_transport_channel_type ctx in
      !pipeline # configure_transport Http_client.https_cb_id tct
    end;
(*
    if !handshake then begin
      let opt = !pipeline # get_options in
      !pipeline # set_options
	{ opt with synchronization = Sync_with_handshake_before_request_body 1.0
	};
    end;
 *)
    if !proxy then begin
      !pipeline # set_proxy !server !port;
      if !proxy_user <> "" then 
	!pipeline # set_proxy_auth !proxy_user !proxy_password;
    end;
  in

  let scheme() =
    if !ssl then "https" else "http" in

  let demand_handshake m =
    (m # request_header `Base) # update_field "Expect" "100-continue" in

  let add_get_message path =
    setup();
    let m = new get (scheme() ^ "://" ^ !server ^ ":" ^ string_of_int !port ^ path) in
    messages := !messages @ [m];
    !pipeline # add m
  in

  let add_head_message path =
    setup();
    let m = new head (scheme() ^ "://" ^ !server ^ ":" ^ string_of_int !port ^ path) in
    if !handshake then demand_handshake m;
    messages := !messages @ [m];
    !pipeline # add m
  in

  let add_put_message size path =
    setup();
    let m = new put 
	      (scheme() ^ "://" ^ !server ^ ":" ^ string_of_int !port ^ path) 
	      ((String.make (size-1) 'x') ^ "\n")
    in
    if !handshake then demand_handshake m;
    if !chreq then m#set_chunked_request();
    messages := !messages @ [m];
    !pipeline # add m
  in

  let add_unframed_put_message size path =
    setup();
    let m = new put_call in
    m # set_request_uri 
      (scheme() ^ "://" ^ !server ^ ":" ^ string_of_int !port ^ path);
    m # request_body # set_value ((String.make (size-1) 'x') ^ "\n");
    if !handshake then demand_handshake m;
    if !chreq then m#set_chunked_request();
    messages := !messages @ [m];
    !pipeline # add m
  in

  let add_line_put_message size path =
    setup();
    let line = "abcdefghijklmnopqrstuvwxyz\n" in
    let b = ref "" in
    for i = 1 to size do
      b := !b ^ line 
    done;
    let m = new put 
	      (scheme() ^ "://" ^ !server ^ ":" ^ string_of_int !port ^ path) 
	      !b
    in
    if !handshake then demand_handshake m;
    if !chreq then m#set_chunked_request();
    messages := !messages @ [m];
    !pipeline # add m
  in

  let add_basic_auth() =
    if !user = "" then failwith "No user specified for authentication module";
    if !realm = "" then failwith "No realm specified for authentication module";
    if !password = "" then failwith "No password specified for authentication module";
    let m = new basic_auth_method in
    m # set_realm !realm !user !password;
    !pipeline # add_authentication_method m
  in

  let add_digest_auth() =
    if !user = "" then failwith "No user specified for authentication module";
    if !realm = "" then failwith "No realm specified for authentication module";
    if !password = "" then failwith "No password specified for authentication module";
    let m = new digest_auth_method in
    m # set_realm !realm !user !password;
    !pipeline # add_authentication_method m
  in

  let rec run_and_catch() =
    try
      !pipeline # run();
    with
	Unix.Unix_error(e,_,_) ->
	  if !verbose then
	    prerr_endline ("Unix error: " ^ Unix.error_message e);
	  run_and_catch()
  in

  let run_pipeline() =
    if !catch_unix_errors then
      run_and_catch()
    else
      !pipeline # run();
    List.iter
      (fun m ->
	 try
	   let (version, code, text) = m # dest_status() in
	   let body = m # get_resp_body()  in
	   let s = 
	     version ^ ":" ^ string_of_int code ^ ":" ^ text ^ ":" ^ 
	     String.concat 
	       "\n" 
	       (List.map
		  (fun (k,v) -> k ^ ": " ^ v)
		  (m # get_resp_header())) ^
	     body in
	   let d = Digest.string s in
	   print_hex d;
	   print_newline()
	 with
	     any ->
	       print_string (string_of_exn any);
	       print_newline();
	       if !verbose then
		 prerr_endline ("Message with exception: " ^ string_of_exn any);
      )
      !messages;
    (* pipeline := new pipeline; *)
    messages := []
  in

  Arg.parse
      [ "-port", Arg.Int (fun i -> port := i),
	      " <n>            specifies the port number of the server (default 80)";
	"-server", Arg.String (fun s -> server := s),
	        " <name>       specifies the server name (default localhost)";
	"-realm", Arg.String (fun s -> realm := s),
	       " <name>        sets the realm for next authentication module";
	"-user", Arg.String (fun s -> user := s),
	      " <name>         sets the user for next authentication module";
	"-password", Arg.String (fun s -> password := s),
	          " <name>     sets the password for next authentication module";
	"-basic-auth", Arg.Unit add_basic_auth,
	            "          adds basic authentication module to the pipeline";
	"-digest-auth", Arg.Unit add_digest_auth,
	             "         adds digest authentication module to the pipeline";
	"-proxy", Arg.Unit (fun () -> proxy := true),
	       "               sets that the proxy protocol variant is used";
	"-proxy-user", Arg.String (fun s -> proxy_user := s),
	            " <name>   sets the proxy user (for proxy authentication)";
	"-proxy-password", Arg.String (fun s -> proxy_password := s),
	                " <pw> sets the proxy password (for proxy authentication)";
	"-handshake", Arg.Set handshake,
	           "           enable 100 CONTINUE handshake for POST/PUT";
	"-chreq", Arg.Set chreq,
	       "               send request body with chunked encoding";
	"-ssl", Arg.Set ssl,
	     "                 use SSL to connect to server";
	"-get", Arg.String add_get_message,
	     " <path>          adds a GET request to the current pipeline";
	"-head", Arg.String add_head_message,
	      " <path>         adds a HEAD request to the current pipeline";
        "-put-small", Arg.String (add_put_message 64),
                   " <path>    adds a small PUT request (64 chars)";
        "-put-big", Arg.String (add_put_message 262144),
                 " <path>      adds a big PUT request (256K chars)";
	"-put-lines", Arg.String (add_line_put_message 2000),
	           " <path>    adds a PUT request with 2000 lines times 27 chars";
	"-unframed-put", Arg.String (add_unframed_put_message 32768),
                      " <path> adds an unframed PUT request (32k chars)";
	"-run", Arg.Unit run_pipeline,
             "                 runs through the current pipeline";
	"-catch", Arg.Set catch_unix_errors,
	       "               catch Unix errors while running the pipeline";
	"-verbose", Arg.Set verbose,
                 "             Outputs many messages";
	"-opt-inh-persistency", Arg.Unit (fun () ->
					    !pipeline # set_options
					      { !pipeline # get_options with
						  inhibit_persistency = true }),
	                     " Inhibits persistent connections";
	"-opt-timeout", Arg.Int (fun k ->
				   !pipeline # set_options
				     { !pipeline # get_options with
					 connection_timeout = float_of_int k }),
	             " <n>     Sets the connection timeout to n seconds";
				   
	"-debug", Arg.String (fun s -> Netlog.Debug.enable_module s),
	"<module>  Enable debug messages for <module>";
	
	"-debug-all", Arg.Unit (fun () -> Netlog.Debug.enable_all()),
	"  Enable all debug messages";
	
	"-debug-list", Arg.Unit (fun () -> 
                                   List.iter print_endline (Netlog.Debug.names());
                                   exit 0),
	"  Show possible modules for -debug, then exit";
	
	"-track-fd", Arg.Set Netlog.Debug.enable_fd_tracking,
	"  Enables messages for tracking file descriptors";
      ]
    (fun s -> if s <> "" then failwith ("Bad argument: " ^ s))
      "usage: test_client [options]

Executes the sequence of client operations which are specified by the
arguments. 
";

  ()
;;


try
  Sys.signal Sys.sigpipe Sys.Signal_ignore;
  main()
with
    any ->
      print_endline("Exception: " ^ string_of_exn any);
      prerr_endline("Exception: " ^ string_of_exn any);
      flush stdout;
      flush stderr;
      raise any         (* force backtrace *)
;;


flush stdout;
flush stderr;;

