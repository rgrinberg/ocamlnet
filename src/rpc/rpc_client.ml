(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


open Rtypes
open Xdr
open Rpc
open Rpc_common
open Rpc_packer
open Unixqueue

exception Message_not_processable


(* The following exceptions are delivered to the callback function: *)

exception Message_lost
  (* got EOF when some procedure calls were not replied or not even sent *)

exception Message_timeout
  (* After all retransmissions, there was still no reply *)

exception Communication_error of exn
  (* an I/O error happened *)

exception Client_is_down
  (* The RPC call cannot be performed because the client has been shut down
   * in the meantime. You can get this exception if you begin a new call,
   * but the connection is closed now.
   *)

exception Keep_call



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


(* The following class types are only preliminary definitions. The type
 * parameter 't is later instantiated with the type of the clients, t.
 *)

class type ['t] pre_auth_session =
object
  method next_credentials : 't -> (string * string * string * string)
  method server_rejects : server_error -> unit
  method server_accepts : string -> string -> unit
end


class type ['t] pre_auth_method =
object
  method name : string
  method new_session : unit -> 't pre_auth_session
end


type call =
      { mutable proc : string;
	mutable xdr_value : xdr_value;      (* the argument of the call *)
	mutable value : packed_value;       (* the argument of the call *)
	mutable get_result : (unit -> xdr_value) -> unit;

	mutable state : call_state;
	mutable retrans_count : int;        (* retransmission counter *)
	mutable xid : int;

	mutable call_timeout : float;
	mutable timeout_group : group option;
	  (* If a timeout handler has been set, this is the corresponding group *)

	mutable call_auth_session : t pre_auth_session;
	mutable call_auth_method : t pre_auth_method;
	  (* calls store the authentication session and the method. *)

	mutable when_sent : unit -> bool;
      }

and t =
      { mutable ready : bool;

        mutable trans : Rpc_transport.rpc_multiplex_controller option;
	mutable prog :  Rpc_program.t;
	mutable prot :  protocol;
        mutable esys :  event_system;

	mutable shutdown_connector : t -> Rpc_transport.rpc_multiplex_controller -> unit;

	mutable waiting_calls : call Queue.t;
	mutable pending_calls : call SessionMap.t;

	mutable next_xid : int;

	(* configs: *)
	mutable timeout : float;
        mutable max_retransmissions : int;

	(* authentication: *)
	mutable auth_methods : t pre_auth_method list;     (* methods to try *)
	mutable current_auth_method : t pre_auth_method;
	mutable unused_auth_sessions : t pre_auth_session list;
	  (* The [unused_auth_sessions] are the sessions that were used for
	   * previous calls and that can be reused. These sessions belong to
	   * [current_auth_method].
	   *)

	mutable exception_handler : exn -> unit;
      }

and connector =
    Inet of (string * int)                        (* Hostname, port *)
  | Internet of (Unix.inet_addr * int)
  | Unix of string                                (* path to unix dom sock *)
  | Descriptor of Unix.file_descr
  | Dynamic_descriptor of (unit -> Unix.file_descr)
  | Portmapped of string

class type auth_session = [t] pre_auth_session
class type auth_method = [t] pre_auth_method


class auth_none_session : auth_session =
object
  method next_credentials _ = ("AUTH_NONE", "", "AUTH_NONE", "")
  method server_rejects _ = ()
  method server_accepts _ _ = ()
end


class auth_none =
object
  method name = "AUTH_NONE"
  method new_session () = new auth_none_session
end

let auth_none = new auth_none

let debug = ref false

  (*****)

let set_auth_methods cl list =
  match list with
      m :: list' ->
	cl.current_auth_method <- m;
	cl.auth_methods <- list';
	cl.unused_auth_sessions <- []
    | [] ->
	invalid_arg "Rpc_client.set_auth_methods"

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
    if !debug then prerr_endline "Rpc_client: Calling back";
    call.get_result f;
    if !debug then prerr_endline "Rpc_client: Returned from callback";
  with
    | Keep_call as x ->
	if !debug then prerr_endline "Rpc_client: Keep_call";
	raise x
    | any ->
	begin  (* pass the exception to the exception handler: *)
	  if !debug then
	    prerr_endline ("Rpc_client: Exception from callback: " ^
			     Printexc.to_string any);
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
    try
      if !debug then 
	prerr_endline ("Rpc_client: Passing exception " ^ Printexc.to_string x);
      pass_result cl call (fun () -> raise x)
    with
	Keep_call -> ()          (* ignore *)
  )

let pass_exception_to_all cl x =
  (* Caution! This function does not erase the set of pending calls.  *)
  if !debug then prerr_endline "Rpc_client: Passing exception to all";

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
    if !debug then prerr_endline "Rpc_client: Closing";
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

let find_or_make_auth_session cl =
  match cl.unused_auth_sessions with
      [] ->
	cl.current_auth_method # new_session()
    | s :: other ->
	cl.unused_auth_sessions <- other;
	s
;;


(* Note: For asynchronous authentication, it would be sufficient that
 * add_call (and add_call_again) are rewritten such that they first
 * schedule the authentication request, and when the request is replied,
 * the call is scheduled.
 *)

let add_call ?(when_sent = fun () -> true) cl procname param receiver =
  if not cl.ready then
    raise Client_is_down;

  let s = find_or_make_auth_session cl in

  let (cred_flav, cred_data, verf_flav, verf_data) = s # next_credentials cl in

  let value =
    Rpc_packer.pack_call
      cl.prog
      (uint4_of_int cl.next_xid)
      procname
      cred_flav cred_data verf_flav verf_data
      param
  in

  let new_call =
    { proc = procname;
      xdr_value = param;
      value = value;
      get_result = receiver;
      state = Waiting;
      retrans_count = cl.max_retransmissions;
      xid = cl.next_xid;
      call_timeout = cl.timeout;
      timeout_group = None;
      call_auth_session = s;
      call_auth_method = cl.current_auth_method;
      when_sent = when_sent
    }
  in

  Queue.add new_call cl.waiting_calls;
  cl.next_xid <- cl.next_xid + 1;

  !check_for_output cl
;;


let add_call_again cl call =
  (* Add a call again to the queue of waiting calls. The call is authenticated
   * again.
   *)

  if not cl.ready then
    raise Client_is_down;

  let s = call.call_auth_session in

  let (cred_flav, cred_data, verf_flav, verf_data) = s # next_credentials cl in

  let value =
    Rpc_packer.pack_call
      cl.prog
      (uint4_of_int call.xid)                   (* use old XID again (CHECK) *)
      call.proc
      cred_flav cred_data verf_flav verf_data
      call.xdr_value
  in

  call.value <- value;           (* the credentials may have changed *)
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
      if !debug then prerr_endline "Rpc_client: Retransmitting";
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
      if !debug then prerr_endline "Rpc_client: Call timed out!";
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
	int_of_uint4 (Rpc_packer.peek_xid message)
      with
	_ -> raise Message_not_processable
	    (* TODO: shut down the connection. This is a serious error *)
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

    let result_opt = try
      begin match Rpc_packer.peek_auth_error message with
	( Some Auth_rejected_cred
	| Some Auth_rejected_verf
	| Some Auth_bad_cred
	| Some Auth_bad_verf) as erropt ->
	    (* Automatic retry with the same auth_session *)
	    let error = match erropt with Some x -> x | _ -> assert false in
	    call.call_auth_session # server_rejects error;
              (* may raise an exception *)
	    remove_pending_call cl call;
	    add_call_again cl call;
	    None                    (* don't pass a value back to the caller *)
	| Some Auth_too_weak ->
	    (* Automatic retry with next auth_method *)
	    if call.call_auth_method = cl.current_auth_method then begin
	      (* Switch to next best authentication method *)
	      match cl.auth_methods with
		  a :: other ->
		    cl.auth_methods <- other;
		    cl.current_auth_method <- a;
		    cl.unused_auth_sessions <- []    (* drop all old sessions *)
		| [] ->
		    (* No further authentication method. Keep the
		     * current method, but raise Auth_too_weak
		     *)
		    raise (Rpc_server Auth_too_weak)
	    end;
	    (* else: in the meantime the method has already been
	     * switched
	     *)
	    add_call cl call.proc call.xdr_value call.get_result;
	    None                     (* don't pass a value back to the caller *)
	| _ ->
	    let (xid,verf_flavour,verf_data,response) =
	      Rpc_packer.unpack_reply cl.prog call.proc message
		(* may raise an exception *)
            in
	    call.call_auth_session # server_accepts verf_flavour verf_data;
	    Some (fun () -> response)
      end
    with
	error ->
	  (* The call_auth_session is simply dropped. *)
	  (* Forward the exception [error] to the caller: *)
	  Some (fun () -> raise error)
    in

    match result_opt with
	None ->
	  (* There is no result yet *)
	  ()

      | Some result ->

          (* pass result to the user *)

	  try
	    pass_result cl call result;      (* may raise Keep_call *)
	    (* Side effect: Changes the state of [call] to [Done] *)
	    remove_pending_call cl call;
	    cl.unused_auth_sessions <-
	                       call.call_auth_session ::cl.unused_auth_sessions;
	  with
	      Keep_call ->
		call.state <- Pending


  (*****)

let rec handle_incoming_message cl r =
  (* Called when a complete message has been read by the transporter *)
  match r with
    | `Error e ->
	close ~error:(Communication_error e) cl

    | `Ok(pv,addr) ->
	if !debug then prerr_endline "Rpc_client: Message arrived";
	( try
	    process_incoming_message cl pv
	  with
	      Message_not_processable ->
		if !debug then
		  prerr_endline "Rpc_client: message not processable";
		()
	);
	(next_incoming_message cl : unit)

    | `End_of_file ->
	if !debug then prerr_endline "Rpc_client: End of file";
	close cl

and next_incoming_message cl =
  match cl.trans with
    | None -> ()
    | Some trans -> next_incoming_message' cl trans

and next_incoming_message' cl trans =
  if cl.pending_calls <> SessionMap.empty && not trans#reading then (
    trans # start_reading
      ~when_done:(fun r ->
		    handle_incoming_message cl r)
      ()
  )
  else
    if !debug then prerr_endline "Rpc_client: Stopping reading"
;;


check_for_input := next_incoming_message;;


let rec handle_outgoing_message cl call r =
  (* Called after a complete message has been sent by the transporter *)
  match r with
    | `Error e ->
	close ~error:(Communication_error e) cl

    | `Ok () ->
	if !debug then prerr_endline "Rpc_client: message writing finished";
	let cont = call.when_sent() in
	if not cont then
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
	    prerr_endline "Rpc_client: found call that has been done";
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

	  if call.call_timeout >= 0.0 then (
	    stop_retransmission_timer cl call;  (* To be sure! *)
	    let g = new_group cl.esys in
	    Unixqueue.once cl.esys g call.call_timeout
	      (fun () ->
		 call.timeout_group <- None;
		 if !debug then prerr_endline "Rpc_client: Timeout handler";
		 retransmit cl call;
	      );
	    call.timeout_group <- Some g
	  );

	  (* Send the message: *)

	  if !debug then prerr_endline "Rpc_client: start_writing";
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


(* Shutdown:
 * We first try an orderly shutdown. If that does not work, just inactivate
 * the transport.
 *)


let shutdown_connector cl mplex =
  if !debug then prerr_endline "Rpc_client: shutdown_connector";
  mplex # cancel_rw();
  ( try
      mplex # start_shutting_down
	~when_done:(fun exn_opt ->
		      (* CHECK: Print exception? *)
		      mplex # inactivate())
	()
    with
      | _ -> mplex # inactivate()
  )


let mplex_of_fd ~close_inactive_descr prot fd esys =
  match prot with
    | Tcp ->
        Rpc_transport.stream_rpc_multiplex_controller
          ~close_inactive_descr fd esys
    | Udp ->
        Rpc_transport.datagram_rpc_multiplex_controller
          ~close_inactive_descr fd esys


class type socket_config =
object
  method non_blocking_connect : bool
  method multiplexing :
    close_inactive_descr:bool ->
    protocol -> Unix.file_descr -> Unixqueue.event_system ->
      Rpc_transport.rpc_multiplex_controller Uq_engines.engine
end


class default_socket_config : socket_config =
object
  method non_blocking_connect = true
  method multiplexing ~close_inactive_descr prot fd esys =
    let mplex = mplex_of_fd ~close_inactive_descr prot fd esys in
    new Uq_engines.const_engine (`Done mplex) esys
end


class blocking_socket_config : socket_config =
object
  inherit default_socket_config
  method non_blocking_connect = false
end

let default_socket_config = new default_socket_config

type mode2 =
    [ `Socket_endpoint of protocol * Unix.file_descr 
    | `Multiplexer_endpoint of Rpc_transport.rpc_multiplex_controller
    | `Socket of protocol * connector * socket_config
    ]


class add_call_as_engine cl name v =
object(self)
  inherit [ Xdr.xdr_value ] Uq_engines.engine_mixin (`Working 0)

  initializer
    add_call cl name v
      (fun get_result ->
	 try
	   let r = get_result() in
	   self # set_state (`Done r)
	 with
	   | err ->
	       self # set_state (`Error err)
      )

  method event_system = cl.esys
  method abort() =
    (failwith "add_call_as_engine#abort: not implemented" : unit)

  (* TODO: Once we have [abort], this class can be exported *)

end


let rec create2 ?program_number ?version_number ?(initial_xid=0)
            ?(shutdown = shutdown_connector)
            mode prog0 esys =

  let prog = Rpc_program.update ?program_number ?version_number prog0 in
  let id_s =
    "program " ^ 
      (Int32.to_string 
	 (Rtypes.logical_int32_of_uint4
	    (Rpc_program.program_number prog))) in

  let non_blocking_connect =
    match mode with
      | `Socket(_,_,conf) -> conf # non_blocking_connect
      | _ -> true in

  let portmapper_engine prot host esys = 
    (* Performs GETPORT for the program on [host]. We use 
     * Rpc_portmapper_aux but not Rpc_portmapper_clnt. The latter is
     * impossible because of a dependency cycle.
     *)
    if !debug then prerr_endline "Rpc_client: starting portmapper query";
    let pm_port = Rtypes.int_of_uint4 Rpc_portmapper_aux.pmap_port in
    let pm_client = 
      create2 
	(`Socket(Rpc.Udp, Inet(host, pm_port), default_socket_config))
	Rpc_portmapper_aux.program_PMAP'V2
	esys
    in
    let v =
      Rpc_portmapper_aux._of_PMAP'V2'pmapproc_getport'arg
	{ Rpc_portmapper_aux.prog = Rpc_program.program_number prog;
	  vers = Rpc_program.version_number prog;
	  prot = ( match prot with
		     | Tcp -> Rpc_portmapper_aux.ipproto_tcp
		     | Udp -> Rpc_portmapper_aux.ipproto_udp
		 );
	  port = Rtypes.uint4_of_int 0;
	} in
    let close_deferred() =
      Unixqueue.once esys (Unixqueue.new_group esys) 0.0 
	(fun() -> close pm_client) in
    new Uq_engines.map_engine
      ~map_done:(fun r ->
		   if !debug then prerr_endline "Rpc_client: Portmapper GETPORT done";
		   let addr =
		     match pm_client.trans with
		       | None -> assert false
		       | Some trans ->
			   ( match trans # getpeername with
			       | `Implied -> assert false
			       | `Sockaddr a -> a
			   ) in
		   let port =
		     Rpc_portmapper_aux._to_PMAP'V2'pmapproc_getport'res r in
		   let port = Rtypes.int_of_uint4 port in
		   close_deferred();
		   if port = 0 then
		     `Error (Failure "Program not bound in Portmapper")
		   else
		     `Done(addr, port)
		)
      ~map_error:(fun err ->
		    if !debug then prerr_endline "Rpc_client: Portmapper GETPORT error";
		    close_deferred();
		    `Error err)
      (new add_call_as_engine pm_client "PMAPPROC_GETPORT" v)
  in

  let connect_engine addr esys =
    match addr with
      | `Portmapped(prot,host) ->
	  new Uq_engines.seq_engine
	    (portmapper_engine prot host esys)
	    (fun (sockaddr, port) ->
	       let inetaddr =
		 match sockaddr with
		   | Unix.ADDR_INET(inet, _) -> inet
		   | _ -> assert false in
	       let stype = 
		 match prot with 
		   | Tcp -> Unix.SOCK_STREAM 
		   | Udp -> Unix.SOCK_DGRAM in
	       let addr = `Sock_inet(stype, inetaddr, port) in
	       let opts = Uq_engines.default_connect_options in
	       Uq_engines.connector (`Socket(addr,opts)) esys
	    )

      | #Uq_engines.sockspec as addr ->
	  let opts = Uq_engines.default_connect_options in
	  Uq_engines.connector (`Socket(addr,opts)) esys in

  let open_socket_non_blocking addr prot conf =
    new Uq_engines.seq_engine
      (connect_engine addr esys)
      (fun status ->
	  if !debug then prerr_endline ("Rpc_client: Non-blocking socket connect successful for " ^ id_s);
	 let fd = Uq_engines.client_socket status in
	 conf # multiplexing ~close_inactive_descr:true prot fd esys
      ) in

  let open_socket_blocking addr prot conf =
    let conn_esys = Unixqueue.create_unix_event_system() in
    let c = connect_engine addr conn_esys in
    Unixqueue.run conn_esys;
    match c # state with
      | `Done status ->
	  if !debug then prerr_endline ("Rpc_client: Blocking socket connect successful for " ^ id_s);
	  let fd = Uq_engines.client_socket status in
	  conf # multiplexing ~close_inactive_descr:true prot fd esys
      | `Error err ->
	  raise err
      | _ ->
	  assert false
  in

  let open_socket =
    if non_blocking_connect then 
      open_socket_non_blocking
    else
      open_socket_blocking in

  let (prot, establish_engine) =
    match mode with
      | `Socket_endpoint(prot,fd) ->
	  let m = mplex_of_fd ~close_inactive_descr:true prot fd esys in
	  (prot, new Uq_engines.const_engine (`Done m) esys)
      | `Multiplexer_endpoint(mplex) ->
	  if mplex # event_system != esys then
            failwith "Rpc_client.create2: Multiplexer is attached to the wrong event system";
	  (mplex # protocol,
	   new Uq_engines.const_engine (`Done mplex) esys)
       | `Socket(prot,conn,conf) ->
	   let stype = 
	     match prot with Tcp -> Unix.SOCK_STREAM | Udp -> Unix.SOCK_DGRAM in
	   (match conn with
	      | Inet (host,port) ->
		  let addr = `Sock_inet_byname(stype, host, port) in
		  (prot, open_socket addr prot conf)
	      | Internet (host,port) ->
		  let addr = `Sock_inet(stype, host, port) in
		  (prot, open_socket addr prot conf)
	      | Unix path ->
		  let addr = `Sock_unix(stype, path) in
		  (prot, open_socket addr prot conf)
	      |	Descriptor fd -> 
		  let m = 
		    mplex_of_fd ~close_inactive_descr:false prot fd esys in
		  (prot, new Uq_engines.const_engine (`Done m) esys)
	      |	Dynamic_descriptor f ->
		  let fd = f() in
		  let m = 
		    mplex_of_fd ~close_inactive_descr:true prot fd esys in
		  (prot, new Uq_engines.const_engine (`Done m) esys)
	      | Portmapped host ->
		  (prot, open_socket (`Portmapped(prot,host)) prot conf)
	   )
  in

  let cl =
    { ready = true;
      trans = None;
      prog = prog;
      prot = prot;
      esys = esys;
      shutdown_connector = shutdown;
      waiting_calls = Queue.create();
      pending_calls = SessionMap.empty;
      next_xid = initial_xid;
      timeout = if prot = Udp then 15.0 else (-.1.0);
      max_retransmissions = 3;
      exception_handler = (fun _ -> ());
      auth_methods = [ ];
      current_auth_method = auth_none;
      unused_auth_sessions = []
    }
  in

  Uq_engines.when_state
    ~is_done:(fun mplex ->
		if !debug then 
		  prerr_endline ("Rpc_client: Fully connected for " ^ id_s);
		cl.trans <- Some mplex;
		(* Maybe we already have messages to send: *)
		!check_for_output cl
	     )
    ~is_error:(fun err ->
		 close cl;
		 cl.exception_handler err
	      )
    establish_engine;

  cl
;;


let create ?program_number ?version_number ?(initial_xid=0) 
           ?(shutdown = shutdown_connector)
           esys c prot prog0 =
  create2 
    ?program_number ?version_number ~initial_xid ~shutdown
    (`Socket(prot, c, (new blocking_socket_config)))
    prog0
    esys

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

let program cl =
  cl.prog


let get_socket_name cl =
  match cl.trans with
    | None -> failwith "Rpc_client.get_socket_name: not connected"
    | Some trans ->
	( match trans # getsockname with
	    | `Implied ->
		failwith "Rpc_client.get_socket_name: not applicable"
	    | `Sockaddr a -> a
	)

let get_peer_name cl =
  match cl.trans with
    | None -> failwith "Rpc_client.get_peer_name: not connected"
    | Some trans ->
	( match trans # getpeername with
	    | `Implied ->
		failwith "Rpc_client.get_peer_name: not applicable"
	    | `Sockaddr a -> a
	)

let get_protocol cl =
  cl.prot

let verbose b =
  debug := b

  (*****)

(* Now synchronous calls: *)

type result =
    No
  | Reply of xdr_value
  | Error of exn


let sync_call cl proc arg =
  let r = ref No in
  let get_result transmitter =
    try
      r := Reply (transmitter())
    with
      x ->
	r := Error x
  in
  (* push the request onto the queue: *)
  add_call cl proc arg get_result;
  (* run through the queue and process all elements: *)
  Unixqueue.run cl.esys;
  (* now a call back of 'get_result' should have happened. *)
  match !r with
    No -> failwith "Rpc_client.sync_call: internal error"
  | Reply x -> x
  | Error e -> raise e


