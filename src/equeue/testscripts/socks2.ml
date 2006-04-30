(* Test snippet:
 * Opens a server socket on the proxy host. Every line sent to the server
 * is immediately replied. The "echo" function is implemented by
 * a subprocess. A tridirectional copier is used to connect stdin/stdout
 * of the subprocess with the server socket.
 *)

open Uq_engines;;
open Uq_socks5;;

Unixqueue.set_debug_mode true;;

let ues = Unixqueue.create_unix_event_system();;
let p = new proxy_client (`Socket(`Sock_inet_byname(Unix.SOCK_STREAM,
                                                    "gate", 1080),
				  default_connect_options
				 ));;
let lstn = listener ~proxy:p (`Socket (`Sock_inet(Unix.SOCK_STREAM,
						  Unix.inet_addr_any, 6601
						 ),
				       default_listen_options
				      )
			     ) ues;;


let process_conn fd =
  (* Create two pipes: Input from fd goes to s_in;
   * Output from s_out goes to fd.
   *)
		
  let (s_in_sub, s_in) = Unix.pipe() in
  let (s_out, s_out_sub) = Unix.pipe() in
  let e = new copier (`Tridirectional(fd, s_in, s_out)) ues in

  Unix.set_close_on_exec s_in;
  Unix.set_close_on_exec s_out;

  (* Unlike "cat", this sh script copies line-by-line (for better
   * effect)
   *)
  let pid =
    Unix.create_process 
      "sh" 
      [| "sh"; "-c"; "while read line; do echo $line; done" |] 
      s_in_sub s_out_sub Unix.stderr in
  
  Unix.close s_in_sub;
  Unix.close s_out_sub;
	
  when_state
    ~is_done:   (fun _ -> ignore(Unix.waitpid [] pid))
    ~is_aborted:(fun _ -> ignore(Unix.waitpid [] pid))
    ~is_error:  (fun _ -> ignore(Unix.waitpid [] pid))
    e
  (* or: new poll_process_engine ~pid ues *)
;;


let rec accept_conn acceptor =
  let acc_eng = acceptor # accept() in

(*
  (* Abort after 30 seconds: *)
  let wd = new watchdog 30.0 acc_eng in
  when_state
    ~is_error:(fun _ -> 
		 prerr_endline "WATCHDOG TIMEOUT!";
		 acc_eng # abort();
	      )
    wd;
*)

  (* Accept connection, and process it: *)
  when_state
    ~is_done:(fun (fd, addr) ->
		prerr_endline "Connected!";
		process_conn fd;
		
		(* If the acceptor supports multiple connections, accept 
		 * the next connection now: 
		 *)
		if acceptor # multiple_connections then
		  accept_conn acceptor
		else
		  acceptor # shut_down()
	     )
    ~is_error:(fun _ -> acceptor # shut_down())
    ~is_aborted:(fun _ -> acceptor # shut_down())
    acc_eng
;;


when_state
  ~is_done:accept_conn
  lstn
;;


Unixqueue.run ues;;
