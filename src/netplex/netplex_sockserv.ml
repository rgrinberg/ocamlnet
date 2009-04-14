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
		let real_addr =
		  (* Win32: Emulate Unix domain *)
		  match Sys.os_type, Unix.domain_of_sockaddr addr with
		    | "Win32", Unix.PF_UNIX ->
			Unix.ADDR_INET(Unix.inet_addr_loopback, 0)
		    | _ -> 
			addr
		in

		let s =
		  Unix.socket
		    (Unix.domain_of_sockaddr real_addr) Unix.SOCK_STREAM 0 in
		fdlist := s :: !fdlist;
		Unix.setsockopt s Unix.SO_REUSEADDR proto#lstn_reuseaddr;
		Unix.setsockopt s Unix.SO_KEEPALIVE proto#so_keepalive;
		( match addr with
		    | Unix.ADDR_UNIX path ->
			( try Unix.unlink path with _ -> () )
		    | _ -> ()
		);
		Unix.bind s real_addr;
		Unix.set_nonblock s;
		Unix.set_close_on_exec s;
		Unix.listen s proto#lstn_backlog;
		( match Sys.os_type, addr with
		    | "Win32", Unix.ADDR_UNIX path ->
			(* Write the port number: *)
			( match Unix.getsockname s with
			    | Unix.ADDR_INET(_, port) ->
				let f = open_out path in
				output_string f (string_of_int port ^ "\n");
				close_out f
			    | _ -> ()
			)
		    | _, _ ->
			()
		);
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
	proc
        config : socket_service =
  let sockets = open_sockets config#protocols in
object(self)
  method name = config#name
  method sockets = sockets
  method socket_service_config = config
  method processor = proc
  method create_container sockserv =
    Netplex_container.create_container sockserv

end


let create_socket_service
      proc
      config =
  new std_socket_service 
    proc config
;;
