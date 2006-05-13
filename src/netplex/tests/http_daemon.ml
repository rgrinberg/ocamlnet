(* Serves $HOME as docroot *)

open Nethttpd_types
open Nethttpd_services
open Nethttpd_reactor
open Netplex_types
open Printf

let counter = ref 0

let hit_counter env cgi =
  let cur_counter = !counter in
  incr counter;
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
    [ default_host ~pref_name:"localhost" ~pref_port:8181 (),
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


let processor () : processor =
  let config error_logger : http_reactor_config =
    object
      method config_timeout_next_request = 60.0
      method config_timeout = 300.0
      method config_reactor_synch = `Write
      method config_cgi = Netcgi_env.default_config
      method config_error_response n = "<html>Error " ^ string_of_int n ^ "</html>"
      method config_log_error _ peeraddr_opt meth_opt _ msg =
	let s =
	  sprintf "[%s] [%s] %s"
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
	error_logger s
      method config_max_reqline_length = 256
      method config_max_header_length = 32768
      method config_max_trailer_length = 32768
      method config_limit_pipeline_length = 5
      method config_limit_pipeline_size = 250000
    end in
object
  method process ~when_done (container : container) fd proto =
    let error_logger s = container # log `Err s in
    process_connection (config error_logger) fd srv;
    when_done()

  method receive_message _ _ _ = ()
  method receive_admin_message _ _ _ = ()
  method shutdown() = ()
  method post_start_hook c = c # log `Info "post_start_hook"
  method pre_finish_hook c = c # log `Info "pre_finish_hook"
end


let start () =

  let controller_config : controller_config =
    ( object
	method socket_directory = "/tmp/.netplex"
	method create_logger _ = Netplex_log.channel_logger stderr
      end
    ) in

  let socket_service_config : socket_service_config =
    ( object
	method name = "sample"
	method supported_ptypes = [ `Multi_processing ]
	method protocols =
	  [ object
	      method name = "http"
	      method addresses = [| Unix.ADDR_INET(Unix.inet_addr_any, 8181) |]
	      method lstn_backlog = 5
	      method lstn_reuseaddr = true
	      method configure_slave_socket _ = ()
	    end
	  ]
      end
    ) in

  let proc = processor () in
  let sockserv = 
    Netplex_sockserv.create_socket_service proc socket_service_config in

  let wrkmng_config =
    ( object
	method max_jobs_per_thread = 1
	method min_free_job_capacity = 2
	method max_free_job_capacity = 4
	method max_threads = 50
      end
    ) in

  let wrkmng =
    Netplex_workload.create_dynamic_workload_manager wrkmng_config in
(* Netplex_workload.create_constant_workload_manager 5 *)

  let par =
    new Netplex_mp.mp () in

  let ctrl =
    Netplex_controller.create_controller par controller_config in

  ctrl # add_service sockserv wrkmng;

  Unixqueue.run ctrl#event_system
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start();;
