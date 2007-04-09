(* Serves $HOME as docroot *)

open Nethttpd_types
open Nethttpd_services
open Nethttpd_engine
open Printf

let counter = ref 0

let hit_counter env (cgi : Netcgi_types.cgi_activation) =
  let cur_counter = !counter in
  incr counter;

  if cgi # argument_value "next" <> "" then
    raise (Redirect_response(cgi # argument_value "next", env # input_header));
  (* Quite funny:
   * http://localhost:8765/easteregg?next=/easteregg%3Fnext%3D/
   *)


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
  cgi # output # output_string "<form action='' method=post><input type=submit name=submit value='POST argument'></form>\n";
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
		     `Enable_listings (simple_listing ?hide:None);
		     `Enable_index_file ["index.html"]
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


let serve_connection ues fd =
  let config : http_engine_config =
    object
      method config_timeout_next_request = 15.0
      method config_timeout = 300.0
      method config_cgi = Netcgi_env.default_config
      method config_error_response n = "<html>Error " ^ string_of_int n ^ "</html>"
      method config_log_error _ _ _ _ msg =
        printf "Error log: %s\n" msg; flush stdout
      method config_max_reqline_length = 256
      method config_max_header_length = 32768
      method config_max_trailer_length = 32768
      method config_limit_pipeline_length = 5
      method config_limit_pipeline_size = 250000

      method config_input_flow_control = true
      method config_output_flow_control = true
      method config_announce_server = `Ocamlnet
    end in
  
  let pconfig = new Nethttpd_engine.buffering_engine_processing_config in

  Unix.set_nonblock fd;

  ignore(Nethttpd_engine.process_connection config pconfig fd ues srv)
;;

let rec accept ues srv_sock_acc =
  (* This function accepts the next connection using the [acc_engine]. After the   
   * connection has been accepted, it is served by [serve_connection], and the
   * next connection will be waited for (recursive call of [accept]). Because
   * [server_connection] returns immediately (it only sets the callbacks needed
   * for serving), the recursive call is also done immediately.
   *)
  let acc_engine = srv_sock_acc # accept() in
  Uq_engines.when_state ~is_done:(fun (fd,fd_spec) ->
                                if srv_sock_acc # multiple_connections then (
                                  serve_connection ues fd;
                                  accept ues srv_sock_acc
                                   ) else
                                  srv_sock_acc # shut_down())
                        ~is_error:(fun _ -> srv_sock_acc # shut_down())
                        acc_engine;
;;


let start () =

  let ues = Unixqueue.create_unix_event_system () in
  let opts = { Uq_engines.default_listen_options with
                 Uq_engines.lstn_backlog = 20;
                 Uq_engines.lstn_reuseaddr = true } in
  let lstn_engine =
    Uq_engines.listener
      (`Socket(`Sock_inet(Unix.SOCK_STREAM, Unix.inet_addr_any, 8765) ,opts)) ues in
  Uq_engines.when_state ~is_done:(accept ues) lstn_engine;

  printf "Listening on port 8765\n";
  flush stdout;
  (* Unixqueue.set_debug_mode true; *)
  Unixqueue.run ues
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start();;
