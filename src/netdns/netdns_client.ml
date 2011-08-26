(* $Id$ *)

open Rpc
open Unixqueue
open Netdns_message

exception Message_not_processable
exception Message_too_long
exception Queue_overflow

(* The following exceptions are delivered to the callback function: *)

exception Message_lost
exception Message_timeout
exception Communication_error of exn
exception Client_is_down


let rec exn_to_string err =
  match err with
    | Communication_error err' ->
	"Communication_error(" ^ exn_to_string err' ^ ")"
    | _ ->
	Printexc.to_string err


module SessionInt = struct
  type t = int
  let compare = (Pervasives.compare : int -> int -> int)
end

module SessionMap =
  Map.Make(SessionInt)


type call_state =
    Waiting    (* call has not yet been sent *)
  | Pending    (* call has been sent, still no answer *)
  | Done       (* got answer for the call *)

(* Normally, the state of the call is changed from Waiting to Pending to
 * Done.
 * In the case of a retransmission, the call is added to the waiting calls
 * again but its state remains 'Pending' (because the call is still member
 * of the set of pending calls).
 *)



type call =
      { mutable arg_value : msg;
	mutable value : string;
	mutable get_result : (unit -> msg) -> unit;

	mutable state : call_state;
	mutable retrans_count : int;        (* retransmission counter *)
	mutable xid : int;

	mutable call_timeout : float;
	mutable timeout_group : group option;
	  (* If a timeout handler has been set, this is the corresponding group *)

      }

and t =
      { mutable ready : bool;

        mutable trans : Netdns_transport.dns_multiplex_controller option;
	mutable prot :  protocol;
        mutable esys :  event_system;

	mutable shutdown_connector : t -> Netdns_transport.dns_multiplex_controller -> unit;

	mutable waiting_calls : call Queue.t;
	mutable pending_calls : call SessionMap.t;

	mutable next_xid : int;

	(* configs: *)
	mutable timeout : float;
        mutable max_retransmissions : int;

	mutable exception_handler : exn -> unit;
      }

type connector =
  [ `Internet of (Unix.inet_addr * int)
  | `Descriptor of Unix.file_descr
  ]


let debug = ref false


  (*****)

let stop_retransmission_timer cl call =
  match call.timeout_group with
    | None -> ()
    | Some g ->
	Unixqueue.clear cl.esys g


  (*****)

let pass_result cl call f =
  (* Stop the timer, if any : *)
  stop_retransmission_timer cl call;

  (* Change the state of the call to 'Done': *)
  call.state <- Done;

  (* pass 'f' to the call back function: *)
  try
    if !debug then prerr_endline "Netdns_client: Calling back";
    call.get_result f;
    if !debug then prerr_endline "Netdns_client: Returned from callback";
  with
    | any ->
	begin  (* pass the exception to the exception handler: *)
	  if !debug then
	    prerr_endline ("Netdns_client: Exception from callback: " ^
			     exn_to_string any);
	  try
	    cl.exception_handler any
	  with
	      _ -> ()
	end


let pass_exception cl call x =
  (* Caution! This function does not remove [call] from the set of pending
   * calls.
   *)
  if call.state <> Done then (  (* Don't call back twice *)
    if !debug then 
      prerr_endline ("Netdns_client: Passing exception " ^ exn_to_string x);
    pass_result cl call (fun () -> raise x)
  )

let pass_exception_to_all cl x =
  (* Caution! This function does not erase the set of pending calls.  *)
  if !debug then prerr_endline "Netdns_client: Passing exception to all";

  let fn_list = ref [] in
  let add_fn xid call =
    if not (List.mem_assoc xid !fn_list) then
      fn_list := (xid,call) :: !fn_list
  in

  SessionMap.iter (fun xid call -> add_fn xid call)  cl.pending_calls;
  Queue.iter      (fun call -> add_fn call.xid call) cl.waiting_calls;

  List.iter (fun (xid,call) -> pass_exception cl call x) !fn_list

  (*****)

let close ?error cl =
  if cl.ready then (
    if !debug then prerr_endline "Netdns_client: Closing";
    cl.ready <- false;
    ( match error with
	| None -> pass_exception_to_all cl Message_lost
	| Some e -> pass_exception_to_all cl e
    );
    cl.pending_calls <- SessionMap.empty;
    Queue.clear cl.waiting_calls;
    match cl.trans with
      | None -> ()
      | Some trans ->
	  cl.trans <- None;
	  cl.shutdown_connector cl trans
  )
;;

  (*****)

let check_for_input =  (* "forward declaration" *)
  ref (fun _ -> ());;

let check_for_output = (* "forward declaration" *)
  ref (fun _ -> ());;

  (*****)

let find_next_free_xid cl =
  (* We have only a xid space of 64K. This can be problematic if a lot of
   * queries are done quickly.
   *)
  let barrier_opt =
    try
      Some (Queue.peek cl.waiting_calls).xid
	(* If we again reach this xid, we are out of IDs *)
    with
      | Queue.Empty -> None in

  let rec find xid =
    if Some xid = barrier_opt then
      raise Queue_overflow;
    if SessionMap.mem xid cl.pending_calls then
      find ( (xid+1) land 0xffff )
    else
      xid
  in
  find ( (cl.next_xid+1) land 0xffff )


let add_call cl msg receiver =
  if not cl.ready then
    raise Client_is_down;

  let next_xid' =
    find_next_free_xid cl in

  let msg_hdr = msg.msg_header in
  let msg =
    { msg with
	msg_header = { msg_hdr with
			 msg_id = cl.next_xid;
			 msg_is_query = true;
			 msg_is_trunc = false
		     }
    } in

  let value =
    Netdns_message.print msg in

  ( match cl.prot with
      | Tcp ->
	  if String.length value > 65535 then
	    raise Message_too_long
      | Udp ->
	  if String.length value > 511 then
	    raise Message_too_long
  );

  let new_call =
    { arg_value = msg;
      value = value;
      get_result = receiver;
      state = Waiting;
      retrans_count = cl.max_retransmissions;
      xid = cl.next_xid;
      call_timeout = cl.timeout;
      timeout_group = None;
    }
  in

  Queue.add new_call cl.waiting_calls;
  cl.next_xid <- next_xid'; 

  !check_for_output cl
;;


let add_call_again cl call =
  (* Add a call again to the queue of waiting calls. The call is authenticated
   * again.
   *)

  if not cl.ready then
    raise Client_is_down;

  call.state <- Waiting;
  call.retrans_count <- cl.max_retransmissions;
  call.timeout_group <- None;

  Queue.add call cl.waiting_calls;

  !check_for_output cl
;;

  (*****)

let remove_pending_call cl call =
  cl.pending_calls <- SessionMap.remove call.xid cl.pending_calls;
  stop_retransmission_timer cl call
;;

  (*****)

let retransmit cl call =
  if call.state = Pending then begin
    if call.retrans_count > 0 then begin
      if !debug then prerr_endline "Netdns_client: Retransmitting";
      (* Make the 'call' waiting again *)
      Queue.add call cl.waiting_calls;
      (* Decrease the retransmission counter *)
      call.retrans_count <- call.retrans_count - 1;
      (* Check state of reources: *)
      !check_for_output cl
      (* Note: The [call] remains in state [Pending]. This prevents the [call]
       * from being added to [cl.pending_calls] again.
       *)
    end
    else begin
      (* still no answer after maximum number of retransmissions *)
      if !debug then prerr_endline "Netdns_client: Call timed out!";
      remove_pending_call cl call;
      pass_exception cl call Message_timeout;
      (* Note: The call_auth_session is dropped. *)
      (* Check state of reources: *)
      !check_for_output cl
    end
  end

  (*****)

let process_incoming_message cl message =

  (* Got a 'message' for which the corresponding 'call' must be searched: *)

  let xid =
    try
      (Netdns_message.peek_header message).msg_id
    with
	_ -> raise Message_not_processable
  in

  let call =
    try
      SessionMap.find xid cl.pending_calls
    with
	Not_found ->
	  (* Strange: Got a message with a session ID that is not pending.
	   * We assume that this is an answer of a very old message that
	   * has been completely timed out.
	   *)
	  raise Message_not_processable
  in
  assert(call.state = Pending);

  (* Exceptions in the following block are forwarded to the callback
   * function
   *)

  let result =
    try
      let response =
	Netdns_message.parse message in
      (* may raise an exception *)
      (fun () -> response)
    with
	error ->
	  (* Forward the exception [error] to the caller: *)
	  (fun () -> raise error)
  in

  pass_result cl call result;
  (* Side effect: Changes the state of [call] to [Done] *)
  remove_pending_call cl call

  (*****)

let rec handle_incoming_message cl r =
  (* Called when a complete message has been read by the transporter *)
  match r with
    | `Error e ->
	close ~error:(Communication_error e) cl

    | `Ok(s,addr) ->
	if !debug then prerr_endline "Netdns_client: Message arrived";
	( try
	    process_incoming_message cl s
	  with
	      Message_not_processable ->
		if !debug then
		  prerr_endline "Netdns_client: message not processable";
		()
	);
	(next_incoming_message cl : unit)

    | `End_of_file ->
	if !debug then prerr_endline "Netdns_client: End of file";
	close cl

and next_incoming_message cl =
  match cl.trans with
    | None -> ()
    | Some trans -> next_incoming_message' cl trans

and next_incoming_message' cl trans =
  trans # cancel_rd_polling();
  if cl.pending_calls <> SessionMap.empty && not trans#reading then (
    trans # start_reading
      ~when_done:(fun r ->
		    handle_incoming_message cl r)
      ()
  )
  else
    if !debug then prerr_endline "Netdns_client: Stopping reading"
;;


check_for_input := next_incoming_message;;

  (*****)

let rec handle_outgoing_message cl call r =
  (* Called after a complete message has been sent by the transporter *)
  match r with
    | `Error e ->
	close ~error:(Communication_error e) cl

    | `Ok () ->
	if !debug then prerr_endline "Netdns_client: message writing finished";
	if call.call_timeout = 0.0 then
	  remove_pending_call cl call;
	!check_for_input cl;
	next_outgoing_message cl

and next_outgoing_message cl =
  match cl.trans with
    | None -> ()   (* Not yet initialized *)
    | Some trans -> 
	if not trans#writing then
	  next_outgoing_message' cl trans

and next_outgoing_message' cl trans =
  let call_opt = 
    try Some(Queue.take cl.waiting_calls) with Queue.Empty -> None in

  match call_opt with
    | Some call ->
	(* If the call is already 'Done', skip it. *)
  	(* Change the state of the call. It is now 'pending': *)

	if call.state = Done then (
	  if !debug then
	    prerr_endline "Netdns_client: found call that has been done";
	  next_outgoing_message cl
	)
	else (
	  ( match call.state with
	      | Done -> assert false
	      | Waiting ->
		  cl.pending_calls <-
		    SessionMap.add call.xid call cl.pending_calls;
		  call.state <- Pending;
	      | Pending ->
		  ()
		    (* The call is already member of [pending_calls]
		     * (retransmitted)
		     *)
	  );

	  (* If there should be a timeout handler, add it: *)

	  if call.call_timeout > 0.0 then (
	    (* Note: Case call_timeout = 0.0 is handled elsewhere *)
	    (* CHECK: What happens when the timeout comes before the message
             * is fully written? (Low priority because for stream connections
             * a timeout is usually not set.)
             *)
	    stop_retransmission_timer cl call;  (* To be sure! *)
	    let g = new_group cl.esys in
	    Unixqueue.once cl.esys g call.call_timeout
	      (fun () ->
		 call.timeout_group <- None;
		 if !debug then prerr_endline "Netdns_client: Timeout handler";
		 retransmit cl call;
		 (* Maybe we have to cancel reading: *)
		 next_incoming_message cl
	      );
	    call.timeout_group <- Some g
	  );

	  (* Send the message: *)

	  if !debug then prerr_endline "Netdns_client: start_writing";
	  trans # start_writing
	    ~when_done:(fun r ->
			  handle_outgoing_message cl call r)
	    call.value
	    trans#getpeername

	);

    | None ->
	()
;;


check_for_output := next_outgoing_message ;;

  (*****)

(* Shutdown:
 * We first try an orderly shutdown. If that does not work, just inactivate
 * the transport.
 *)


let shutdown_connector cl mplex =
  if !debug then prerr_endline "Netdns_client: shutdown_connector";
  mplex # abort_rw();
  ( try
      mplex # start_shutting_down
	~when_done:(fun exn_opt ->
		      (* CHECK: Print exception? *)
		      mplex # inactivate())
	()
    with
      | _ -> mplex # inactivate()
  )

  (*****)


let mplex_of_fd ~close_inactive_descr prot fd esys =
  match prot with
    | Tcp ->
        Netdns_transport.stream_dns_multiplex_controller
          ~close_inactive_descr fd esys
    | Udp ->
        Netdns_transport.datagram_dns_multiplex_controller
          ~close_inactive_descr fd esys


class type socket_config =
object
  method multiplexing :
    close_inactive_descr:bool ->
    Rpc.protocol -> Unix.file_descr -> Unixqueue.event_system ->
      Netdns_transport.dns_multiplex_controller Uq_engines.engine
end


class default_socket_config : socket_config =
object
  method multiplexing ~close_inactive_descr prot fd esys =
    let mplex = mplex_of_fd ~close_inactive_descr prot fd esys in
    let eng = new Uq_engines.epsilon_engine (`Done mplex) esys in
    Uq_engines.when_state
      ~is_aborted:(fun () -> mplex # inactivate())
      ~is_error:(fun _ -> mplex # inactivate())
      eng;
    eng
end


let default_socket_config = new default_socket_config


type mode =
    [ `Socket_endpoint of Rpc.protocol * Unix.file_descr 
    | `Multiplexer_endpoint of Netdns_transport.dns_multiplex_controller
    | `Socket of Rpc.protocol * connector * socket_config
    ]


let rec create ?(initial_xid=0) ?(shutdown = shutdown_connector) mode esys =

  let connect_engine addr esys =
    let opts = Uq_engines.default_connect_options in
    Uq_engines.connector (`Socket(addr,opts)) esys in

  let open_socket addr prot conf =
    new Uq_engines.seq_engine
      (connect_engine addr esys)
      (fun status ->
	  if !debug then prerr_endline ("Netdns_client: Non-blocking socket connect successful ");
	 let fd = Uq_engines.client_socket status in
	 conf # multiplexing ~close_inactive_descr:true prot fd esys
      ) in

  let (prot, establish_engine) =
    match mode with
      | `Socket_endpoint(prot,fd) ->
	  let m = mplex_of_fd ~close_inactive_descr:true prot fd esys in
	  (prot, new Uq_engines.epsilon_engine (`Done m) esys)
      | `Multiplexer_endpoint(mplex) ->
	  if mplex # event_system != esys then
            failwith "Netdns_client.create: Multiplexer is attached to the wrong event system";
	  (mplex # protocol,
	   new Uq_engines.epsilon_engine (`Done mplex) esys)
       | `Socket(prot,conn,conf) ->
	   let stype = 
	     match prot with Tcp -> Unix.SOCK_STREAM | Udp -> Unix.SOCK_DGRAM in
	   (match conn with
	      | `Internet (host,port) ->
		  let addr = `Sock_inet(stype, host, port) in
		  (prot, open_socket addr prot conf)
	      |	`Descriptor fd -> 
		  let m = 
		    mplex_of_fd ~close_inactive_descr:false prot fd esys in
		  (prot, new Uq_engines.epsilon_engine (`Done m) esys)
	   )
  in

  let cl =
    { ready = true;
      trans = None;
      prot = prot;
      esys = esys;
      shutdown_connector = shutdown;
      waiting_calls = Queue.create();
      pending_calls = SessionMap.empty;
      next_xid = initial_xid;
      timeout = if prot = Udp then 15.0 else (-.1.0);
      max_retransmissions = 3;
      exception_handler = (fun e -> 
			     prerr_endline("Netdns_client callback exception: "
					   ^ exn_to_string e));
    }
  in

  Uq_engines.when_state
    ~is_done:(fun mplex ->
		if !debug then 
		  prerr_endline ("Netdns_client: Fully connected" ^
				   match prot with
				     | Rpc.Udp -> " (UDP)"
				     | Rpc.Tcp -> " (TCP)"
				);
		cl.trans <- Some mplex;
		(* Maybe we already have messages to send: *)
		!check_for_output cl
	     )
    ~is_error:(fun err ->
		 if !debug then
		   prerr_endline ("Netdns_client: Cannot connect. Error: " ^ 
				    exn_to_string err);
		 close ~error:(Communication_error err) cl
	      )
    establish_engine;

  cl
;;


  (*****)

let configure cl max_retransmission_trials timeout =
  cl.max_retransmissions <- max_retransmission_trials;
  cl.timeout <- timeout


let set_exception_handler cl xh =
  cl.exception_handler <- xh

let shut_down cl =
  if cl.ready then (
    close cl;
    if not (Unixqueue.is_running cl.esys) then
      (* assume synchronous invocation *)
      Unixqueue.run cl.esys
  )

let event_system cl =
  cl.esys


let get_socket_name cl =
  match cl.trans with
    | None -> failwith "Netdns_client.get_socket_name: not connected"
    | Some trans ->
	( match trans # getsockname with
	    | `Implied ->
		failwith "Netdns_client.get_socket_name: not applicable"
	    | `Sockaddr a -> a
	)

let get_peer_name cl =
  match cl.trans with
    | None -> failwith "Netdns_client.get_peer_name: not connected"
    | Some trans ->
	( match trans # getpeername with
	    | `Implied ->
		failwith "Netdns_client.get_peer_name: not applicable"
	    | `Sockaddr a -> a
	)

let get_protocol cl =
  cl.prot

let verbose b =
  debug := b

 
