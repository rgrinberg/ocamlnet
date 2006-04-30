(* This example uses a SOCKS proxy to connect to the web server on
 * www.npc.de, and to perform one HTTP request.
 *)


#require "netstring";;

open Uq_engines;;
open Uq_socks5;;

let ues = Unixqueue.create_unix_event_system();;

let p = new proxy_client (`Socket(`Sock_inet_byname(Unix.SOCK_STREAM,
						    "gate", 1080),
				  default_connect_options
				 ));;

let c = connector ~proxy:p (`Socket(`Sock_inet_byname(Unix.SOCK_STREAM,
						      "www.npc.de", 80),
				    default_connect_options
				   )) ues;;

class my_buf b = 
object (self)
  inherit Netchannels.output_buffer b
  method can_output = true
  method request_notification (f : unit->bool) = ()
    (* Because [can_output] is constant, we can ignore all notification
     * requests.
     *)						   
end;;


let b = Buffer.create 10000;;

when_state
  ~is_done:(fun connstat ->
	      match connstat with
		  `Socket(fd, _) ->
		    prerr_endline "CONNECTED";
		    Unixqueue.set_debug_mode true;
		    let printer = new output_async_descr ~dst:fd ues in
		    let buffer = new my_buf b in
		    let receiver = new receiver ~src:fd ~dst:buffer ues in
		    let s = "GET / HTTP/1.0\n\n" in
		    ignore(printer # output s 0 (String.length s));
		| _ -> assert false
	   )
  c
;;

Unixqueue.run ues;;

(* Now in b is the reply of the web server *)
