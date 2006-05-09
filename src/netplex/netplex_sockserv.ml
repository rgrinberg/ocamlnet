(* $Id$ *)

open Netplex_types

let open_sockets prots =
  let fdlist = ref [] in
  let sockets = ref [] in
  try
    List.iter
      (fun proto ->
	 let fda =
	   Array.map
	     (fun addr ->
		let s =
		  Unix.socket
		    (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
		fdlist := s :: !fdlist;
		Unix.setsockopt s Unix.SO_REUSEADDR proto#lstn_reuseaddr;
		( match addr with
		    | Unix.ADDR_UNIX path ->
			( try Unix.unlink path with _ -> () )
		    | _ -> ()
		);
		Unix.bind s addr;
		Unix.set_nonblock s;
		Unix.set_close_on_exec s;
		Unix.listen s proto#lstn_backlog;
		s
	     )
	     proto#addresses in
	 sockets := (proto#name, fda) :: !sockets
      )
      prots;
    List.rev !sockets
  with
    | error ->
	List.iter (fun fd -> try Unix.close fd with _ -> ()) !fdlist;
	raise error
;;


class std_socket_service 
        ?(pre_start_hook = fun _ _ -> ())
        ?(post_start_hook = fun _ -> ())
        ?(pre_finish_hook = fun _ -> ())
        ?(post_finish_hook = fun _ _ -> ())
	proc
        config : socket_service =
  let sockets = open_sockets config#protocols in
object(self)
  method name = config#name
  method sockets = sockets
  method socket_service_config = config
  method pre_start_hook = pre_start_hook
  method post_start_hook = post_start_hook
  method pre_finish_hook = pre_finish_hook
  method post_finish_hook = post_finish_hook
  method processor = proc
  method create_container sockserv =
    Netplex_container.create_container sockserv

end


let create_socket_service
      ?pre_start_hook 
      ?post_start_hook
      ?pre_finish_hook
      ?post_finish_hook
      proc
      config =
  new std_socket_service 
    ?pre_start_hook ?post_start_hook ?pre_finish_hook ?post_finish_hook
    proc config
;;
