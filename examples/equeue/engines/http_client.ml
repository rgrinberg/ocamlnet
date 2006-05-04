(* This is the HTTP client example from the User's Manual *)

open Uq_engines;;

class async_buffer b =
object (self)
  inherit Netchannels.output_buffer b
  method can_output = true
  method request_notification (f : unit->bool) = ()
end ;;


let main() =
  let ues = Unixqueue.create_unix_event_system() in
  let c = connector (`Socket(`Sock_inet_byname(Unix.SOCK_STREAM,
					       "www.npc.de", 80),
			     default_connect_options
			    )) ues in
  let b = Buffer.create 10000 in

  when_state
    ~is_done:(fun connstat ->
		match connstat with
		    `Socket(fd, _) ->
		      prerr_endline "CONNECTED";
		      let printer = new output_async_descr ~dst:fd ues in
		      let buffer = new async_buffer b in
		      let receiver = new receiver ~src:fd ~dst:buffer ues in
		      let s = "GET / HTTP/1.0\n\n" in
		      ignore(printer # output s 0 (String.length s));
	              when_state
                        ~is_done:(fun _ ->
                                    prerr_endline "HTTP RESPONSE RECEIVED!")
	                ~is_error:(fun _ ->
                                    prerr_endline "ERROR!")
                        receiver
		  | _ -> assert false
	     )
    c;

  Unixqueue.run ues;

  b
;;
