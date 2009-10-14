(* $Id$ *)

open Netplex_types
open Printf

let open_sockets srvname prots =
  let fdlist = ref [] in
  let sockets = ref [] in

  let open_socket proto addr =
    ( match addr with
	| Unix.ADDR_UNIX path ->
	    ( try Unix.unlink path with _ -> () )
	| _ -> ()
    );
    let s =
      Unix.socket
	(Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
    fdlist := s :: !fdlist;
    Unix.setsockopt s Unix.SO_REUSEADDR proto#lstn_reuseaddr;
    Unix.setsockopt s Unix.SO_KEEPALIVE proto#so_keepalive;
    Unix.bind s addr;
    Unix.set_nonblock s;
    Netsys.set_close_on_exec s;
    Unix.listen s proto#lstn_backlog;
    s
  in

  let open_socket_file proto name =
    let addr = Unix.ADDR_INET(Unix.inet_addr_loopback, 0) in
    let s =
      Unix.socket
	(Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
    fdlist := s :: !fdlist;
    Unix.setsockopt s Unix.SO_REUSEADDR proto#lstn_reuseaddr;
    Unix.setsockopt s Unix.SO_KEEPALIVE proto#so_keepalive;
    Unix.bind s addr;
    Unix.set_nonblock s;
    Netsys.set_close_on_exec s;
    Unix.listen s proto#lstn_backlog;
    ( match Unix.getsockname s with
	| Unix.ADDR_INET(_, port) ->
	    let f = open_out name in
	    Unix.chmod name 0o600;
	    output_string f "socket_file\n";
	    output_string f (string_of_int port ^ "\n");
	    close_out f
	| _ -> ()
    );
    s
  in

  let open_w32_pipe proto name =
    let psrv = 
      Netsys_win32.create_local_pipe_server
	name Netsys_win32.Pipe_duplex max_int in
    let s =
      Netsys_win32.pipe_server_descr psrv in
    fdlist := s :: !fdlist;
    Netsys_win32.pipe_listen psrv proto#lstn_backlog;
    s
  in

  let open_w32_pipe_file proto file_name =
    let name = 
      Netsys_win32.unpredictable_pipe_name() in
    let psrv = 
      Netsys_win32.create_local_pipe_server
	name Netsys_win32.Pipe_duplex max_int in
    let s =
      Netsys_win32.pipe_server_descr psrv in
    fdlist := s :: !fdlist;
    Netsys_win32.pipe_listen psrv proto#lstn_backlog;
    let f = open_out file_name in
    Unix.chmod file_name 0o600;
    output_string f "w32_pipe_file\n";
    output_string f (name ^ "\n");
    close_out f;
    s
  in

  try
    List.iter
      (fun proto ->
	 let fda =
	   Array.map
	     (fun addr ->
		let fd = 
		  match addr with
		    | `Socket s -> 
			open_socket proto s
		    | `Socket_file f -> 
			open_socket_file proto f
		    | `W32_pipe p -> 
			open_w32_pipe proto p
		    | `W32_pipe_file f -> 
			open_w32_pipe_file proto f in
		Netlog.Debug.track_fd
		  ~owner:"Netplex_sockserv"
		  ~descr:(sprintf 
			    "Master socket service=%s proto=%s %s"
			    srvname proto#name 
			    (Netsys.string_of_fd fd))
		  fd;
		fd
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


let close_sockets sockets =
  List.iter
    (fun (_, fda) ->
       Array.iter
	 (fun fd ->
	    let fd_style = Netsys.get_fd_style fd in
	    match fd_style with
	      | `W32_pipe_server ->
		  (* As a special case, we also have to close the connect
                     event descriptor
                     FIXME: How to avoid that we have to special-case this?
		   *)
		  let psrv = Netsys_win32.lookup_pipe_server fd in
		  let cn_ev = Netsys_win32.pipe_connect_event psrv in
		  let cn_fd = Netsys_win32.event_descr cn_ev in
		  Netsys.gclose `W32_event cn_fd;
		  Netlog.Debug.release_fd fd;
		  Netsys.gclose fd_style fd
	      | _ ->
		  Netlog.Debug.release_fd fd;
		  Netsys.gclose fd_style fd
	 )
	 fda
    )
    sockets
;;


class std_socket_service 
	proc
        config : socket_service =
  let sockets = open_sockets config#name config#protocols in
object(self)
  method name = config#name
  method sockets = sockets
  method socket_service_config = config
  method processor = proc
  method create_container sockserv =
    Netplex_container.create_container sockserv
  method shutdown () =
    close_sockets sockets
end


let create_socket_service
      proc
      config =
  new std_socket_service 
    proc config
;;


let any_file_client_connector =
  Netplex_util.any_file_client_connector

let client_connector = 
  Netplex_util.client_connector
