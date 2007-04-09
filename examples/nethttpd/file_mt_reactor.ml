(* Serves $HOME as docroot. This is a simple-minded multithreaded variant *)

open Nethttpd_types
open Nethttpd_services
open Nethttpd_reactor
open Printf

let counter_lock = Mutex.create()
let counter = ref 0

let hit_counter env cgi =
  Mutex.lock counter_lock;
  let cur_counter = !counter in
  incr counter;
  Mutex.unlock counter_lock;
  cgi # output # output_string "<html><body>\n";
  cgi # output # output_string (sprintf "Counter = %d<br>\n" cur_counter);
  List.iter
    (fun (name, arg) ->
       cgi # output # output_string (sprintf "Arg %s = %s<br>\n"
				   (Netencoding.Html.encode_from_latin1 name)
				   (Netencoding.Html.encode_from_latin1 arg#value))
    )
    cgi # arguments;
  cgi # output # output_string (sprintf "SCRIPT_NAME = %s<br>\n" 
				  (Netencoding.Html.encode_from_latin1 
				     env#cgi_script_name));
  cgi # output # output_string (sprintf "PATH_INFO = %s<br>\n" 
				  (Netencoding.Html.encode_from_latin1 
				     env#cgi_path_info));
  cgi # output # output_string (sprintf "PATH_TRANSLATED = %s<br>\n" 
				  (Netencoding.Html.encode_from_latin1 
				     env#cgi_path_translated));
  cgi # output # output_string (sprintf "<a href='%s/tmp'>Append 'tmp'</a><br>\n"
				  env#cgi_script_name);
  cgi # output # output_string "<a href='?p=foo'>Append argument</a><br>\n";
  cgi # output # output_string "</body></html>\n";
  cgi # output # commit_work();
;;


let fs_spec =
  { file_docroot = Sys.getenv "HOME";
    file_uri = "/";
    file_suffix_types = [ "txt", "text/plain";
			  "html", "text/html" ];
    file_default_type = "application/octet-stream";
    file_options = [ `Enable_gzip;
		     `Enable_listings (simple_listing ?hide:None)
		   ]
  }

let srv =
  host_distributor
    [ default_host ~pref_name:"localhost" ~pref_port:8765 (),
      uri_distributor
	[ "*", (options_service());
	  "/", (file_service fs_spec);
	  "/easteregg", (dynamic_service
			   { dyn_handler = hit_counter;
			     dyn_activation = std_activation `Std_activation_buffered;
			     dyn_uri = Some "/easteregg";
			     dyn_translator = file_translator fs_spec;
			     dyn_accept_all_conditionals = false
			   })
	]
    ]
;;


let start () =
  let config : http_reactor_config =
    object
      method config_timeout_next_request = 15.0
      method config_timeout = 300.0
      method config_reactor_synch = `Write
      method config_cgi = Netcgi_env.default_config
      method config_error_response n = "<html>Error " ^ string_of_int n ^ "</html>"
      method config_log_error _ _ _ _ msg =
        printf "Error log: %s\n" msg
      method config_max_reqline_length = 256
      method config_max_header_length = 32768
      method config_max_trailer_length = 32768
      method config_limit_pipeline_length = 5
      method config_limit_pipeline_size = 250000
      method config_announce_server = `Ocamlnet
    end in

  let master_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt master_sock Unix.SO_REUSEADDR true;
  Unix.bind master_sock (Unix.ADDR_INET(Unix.inet_addr_any, 8765));
  Unix.listen master_sock 100;
  printf "Listening on port 8765\n";
  flush stdout;

  while true do
    try
      let conn_sock, _ = Unix.accept master_sock in
      Unix.set_nonblock conn_sock;
      let _ =
	Thread.create
	  (process_connection config conn_sock)
	  srv
      in
      ()
    with
        Unix.Unix_error(Unix.EINTR,_,_) -> ()  (* ignore *)
  done
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start();;
