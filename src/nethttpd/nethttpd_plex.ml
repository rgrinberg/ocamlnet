(* $Id$ *)

type config_log_error =
    Unix.sockaddr option -> Unix.sockaddr option -> Nethttp.http_method option -> Nethttp.http_header option -> string -> unit

class netplex_processor mk_config srv : Netplex_types.processor =
object(self)
  method process ~when_done (container : Netplex_types.container) fd proto =
    let error_logger _ peeraddr_opt meth_opt _ msg =
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
       container # log `Err s
    in
    let config = mk_config error_logger in
    Nethttpd_reactor.process_connection config fd srv;
    when_done()

  method receive_message _ _ _ = ()
  method receive_admin_message _ _ _ = ()
  method shutdown() = ()
  method post_start_hook _ = ()
  method pre_finish_hook _ = ()
end

let netplex_processor mk_config srv =
  new netplex_processor mk_config srv
