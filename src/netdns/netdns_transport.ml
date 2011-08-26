(* $Id$ *)

open Rpc_transport

class type dns_multiplex_controller =
object
  method alive : bool
  method event_system : Unixqueue.event_system
  method getsockname : sockaddr
  method getpeername : sockaddr
  method peer_user_name : string option
  method protocol : Rpc.protocol
  method reading : bool
  method read_eof : bool
  method cancel_rd_polling : unit -> unit
  method abort_rw : unit -> unit
  (* method skip_message : unit -> unit *)
  method writing : bool
  method start_shutting_down :
    when_done:(unit result -> unit) -> unit -> unit
  method cancel_shutting_down : unit -> unit
  method set_timeout : notify:(unit -> unit) -> float -> unit
  method inactivate : unit -> unit

  method start_reading :
    when_done:( (string * sockaddr) result_eof -> unit) -> unit -> unit

  method start_writing :
    when_done:(unit result -> unit) -> string -> sockaddr -> unit

end


let datagram_dns_multiplex_controller ?close_inactive_descr fd esys =
  let mc =
    Rpc_transport.datagram_rpc_multiplex_controller
      ?close_inactive_descr fd esys in
  ( object(self)
      method alive = mc#alive
      method event_system = esys
      method getsockname = mc#getsockname
      method getpeername = mc#getpeername
      method protocol = mc#protocol
      method peer_user_name = None
      method reading = mc#reading
      method read_eof = mc#read_eof
      (* method skip_message = mc#skip_message *)
      method writing = mc#writing
      method cancel_rd_polling = mc#cancel_rd_polling
      method abort_rw = mc#abort_rw
      method start_shutting_down = mc#start_shutting_down
      method cancel_shutting_down = mc#cancel_shutting_down
      method set_timeout = mc#set_timeout
      method inactivate = mc#inactivate

      method start_reading ~when_done () =
	mc # start_reading
	  ~when_done:(fun res ->
			match res with
			  | `End_of_file as r -> when_done r
			  | `Error e as r -> when_done r
			  | `Ok(pv,addr) ->
			      let s = 
				IFDEF OCAMLNET3 THEN
				  match pv with
				    | `Accept pv1 -> 
					Rpc_packer.string_of_packed_value pv1
				    | _ ->
					assert false
				ELSE
				  Rpc_packer.string_of_packed_value pv 
				ENDIF in
			      when_done (`Ok(s,addr))
		     )
	  ()

      method start_writing ~when_done s addr =
	if String.length s > 512 then
	  failwith "Netdns_transport: UDP message too long (>512 bytes)";
	mc # start_writing
	  ~when_done
	  (Rpc_packer.packed_value_of_string s)
	  addr

    end
  )


class stream_dns_multiplex_controller sockname peername
        (mplex : Uq_engines.multiplex_controller) esys 
      : dns_multiplex_controller =
object(self)
  val mutable wr_buffer = None
  val mutable wr_buffer_pos = 0

  val mutable rd_buffer = String.create 16384

  val mutable rm_buffer = String.create 2
  val mutable rm_buffer_len = 0
  val mutable rm = 0

  val mutable rd_mode = `RM
  val mutable rd_fragment = ""
  val mutable rd_fragment_len = 0
  val mutable rd_queue = Queue.create()
  val mutable rd_queue_len = 0

  val mutable rd_continuation = None

  method alive = mplex # alive
  method event_system = esys
  method getsockname = sockname
  method getpeername = peername
  method protocol = Rpc.Tcp
  method peer_user_name = None
  method reading = mplex # reading
  method read_eof = mplex # read_eof
  method writing = mplex # writing

  val mutable aborted = false
  (* val mutable skip_message = false *)

  method start_reading ~when_done () =
    let rec est_reading() =
      rd_continuation <- None;
      mplex # start_reading
	~when_done:(fun exn_opt n ->
		      self # timer_event `Stop `R;
		      match exn_opt with
			| None ->
			    process 0 n
			| Some End_of_file ->
			    if rd_mode = `RM && rd_fragment_len = 0 then
			      return_eof()   (* EOF between messages *)
			    else
			      return_error (Error "EOF within message")
			| Some Uq_engines.Cancelled ->
			    ()   (* Ignore *)
			| Some error ->
			    return_error error
		   )
	rd_buffer
	0
	(String.length rd_buffer);
      self # timer_event `Start `R

    and process pos len =
      if len > 0 then (
	match rd_mode with
	  | `RM ->
	      (* Read the record marker *)
	      let m = min (2 - rm_buffer_len) len in
	      String.blit rd_buffer pos rm_buffer rm_buffer_len m;
	      rm_buffer_len <- rm_buffer_len + m;
	      if rm_buffer_len = 2 then (
		rm <-
		  (Char.code rm_buffer.[0]) * 256 +
		  (Char.code rm_buffer.[1]);
		rd_mode <- `Payload;
		rd_fragment <- String.create rm;
		rd_fragment_len <- 0;
		(* before_record (rd_queue_len + rm) `Implied; *)
		process (pos+m) (len-m)
	      )
	      else
		est_reading()
		
	  | `Payload ->
	      (* Read payload data *)
	      let m = min (rm - rd_fragment_len) len in
	      String.blit rd_buffer pos rd_fragment rd_fragment_len m;
	      rd_fragment_len <- rd_fragment_len + m;
	      if rd_fragment_len = rm then (
		if true (* not skip_message *) then
		Queue.push rd_fragment rd_queue;
		rd_queue_len <- rd_queue_len + rd_fragment_len;
		rd_fragment <- "";
		rd_fragment_len <- 0;
		rm_buffer_len <- 0;
		rd_mode <- `RM;
		if false (* skip_message *) then (
		  ()
(*
		  skip_message <- false;
		  process (pos+m) (len-m)
 *)
		)
		else (
		  let msg = String.create rd_queue_len in
		  let p = ref 0 in
		  Queue.iter
		    (fun s ->
		       String.blit s 0 msg !p (String.length s);
		       p := !p + String.length s)
		    rd_queue;
		  Queue.clear rd_queue;
		  rd_queue_len <- 0;
		  rd_continuation <- 
		    Some (fun () -> process (pos+m) (len-m));
		  return_msg msg
		)
	      )
	      else
		est_reading()
      )
      else
	est_reading()

    and return_msg msg =
      if not aborted then (
	when_done (`Ok(msg, peername))
      )

    and return_error e =
      rd_continuation <- None;
      if not aborted then
	when_done (`Error e)

    and return_eof () =
      rd_continuation <- None;
      if not aborted then
	when_done `End_of_file 

    in
    match rd_continuation with
      | None ->
	  est_reading()
      | Some f ->
	  f()


  method start_writing ~when_done msg addr =
    let rec est_writing s pos =
      let len = String.length s - pos in
      mplex # start_writing
	~when_done:(fun exn_opt n ->
		      self # timer_event `Stop `W;
		      match exn_opt with
			| None ->
			    assert(n <= len);
			    if n = len then (
			      if not aborted then
				when_done (`Ok ())
			    )
			    else (
			      if not aborted then
				est_writing s (pos+n)
			    )
			| Some Uq_engines.Cancelled ->
			    ()  (* ignore *)
			| Some error ->
			    if not aborted then
			      when_done (`Error error)
		   )
	s
	pos
	(String.length s - pos);
      self # timer_event `Start `W
    in

    ( match addr with
	| `Implied -> ()
	| `Sockaddr a ->
	    if addr <> peername then
	      failwith "Rpc_transport.stream_rpc_multiplex_controller: cannot send to this address"
    );
    let msg_len = String.length msg in
    if msg_len > 65535 then
      failwith "Netdns_transport: TCP message too long (>65535 bytes)";
    let s = String.create (msg_len + 2) in
    s.[0] <- Char.chr (msg_len lsr 8);
    s.[1] <- Char.chr (msg_len land 0xff);
    String.blit msg 0 s 2 msg_len;
    est_writing s 0


  method cancel_rd_polling () =
    if mplex#reading then
      mplex # cancel_reading()

(*
  method skip_message () =
    Queue.clear rd_queue;
    rd_queue_len <- 0;
    skip_message <- true
 *)

  method abort_rw () =
    aborted <- true;
    mplex # cancel_reading();
    mplex # cancel_writing()
    
  method start_shutting_down ~when_done () =
    mplex # start_shutting_down
      ~when_done:(fun exn_opt ->
		    self # timer_event `Stop `D;
		    match exn_opt with
		      | None -> when_done (`Ok ())
		      | Some error -> when_done (`Error error)
		 )
      ();
    self # timer_event `Start `D

  method cancel_shutting_down () =
    self # timer_event `Stop `D;
    mplex # cancel_shutting_down()

  method inactivate () =
    self # stop_timer();
    mplex # inactivate()

  val mutable timer = None
  val mutable timer_r = `Stop
  val mutable timer_w = `Stop
  val mutable timer_d = `Stop
  val mutable timer_group = None

  method set_timeout ~notify tmo =
    timer <- Some(notify, tmo)

  method private timer_event start_stop which =
    ( match timer with
	| None -> ()
	| Some(notify, tmo) ->
	    ( match which with
		| `R -> timer_r <- start_stop
		| `W -> timer_w <- start_stop
		| `D -> timer_d <- start_stop
	    );
	    self # stop_timer();
	    if timer_r = `Start || timer_w = `Start || timer_d = `Start then (
	      let g = Unixqueue.new_group esys in
	      timer_group <- Some g;
	      Unixqueue.once esys g tmo
		(fun () -> 
		   timer_group <- None;
		   notify()
		)
	    );
    )


  method private stop_timer() =
    ( match timer_group with
	| None -> ()
	| Some g -> Unixqueue.clear esys g
    );
    timer_group <- None;
    timer_r <- `Stop;
    timer_w <- `Stop;
    timer_d <- `Stop

end



let stream_dns_multiplex_controller ?(close_inactive_descr=true) fd esys =
  let sockname = `Sockaddr(Unix.getsockname fd) in
  let peername = `Sockaddr(Unix.getpeername fd) in
  let mplex = 
    Uq_engines.create_multiplex_controller_for_connected_socket
      ~close_inactive_descr
      fd esys in
  new stream_dns_multiplex_controller sockname peername mplex esys
;;
