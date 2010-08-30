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
open Printf

exception Message_not_processable


exception Message_lost
exception Message_timeout
exception Response_dropped
exception Communication_error of exn
exception Client_is_down
exception Keep_call
exception Unbound_exception of exn

module type USE_CLIENT = sig
  type t
  val use : t -> Rpc_program.t -> unit
  val unbound_sync_call : 
        t -> Rpc_program.t -> string -> xdr_value -> xdr_value
  val unbound_async_call :
        t -> Rpc_program.t -> string -> xdr_value -> 
        ((unit -> xdr_value) -> unit) -> unit
end

let () =
  Netexn.register_printer
    (Communication_error Not_found)
    (fun e ->
       match e with
	 | Communication_error e' ->
	     "Rpc_client.Communication_error(" ^ Netexn.to_string e' ^ ")"
	 | _ -> assert false
    );
  Netexn.register_printer
    (Unbound_exception Not_found)
    (fun e ->
       match e with
	 | Unbound_exception e' ->
	     "Rpc_client.Unbound_exception(" ^ Netexn.to_string e' ^ ")"
	 | _ -> assert false
    )



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
      { mutable prog : Rpc_program.t;
	mutable proc : string;
	mutable xdr_value : xdr_value;      (* the argument of the call *)
	mutable value : packed_value;       (* the argument of the call *)
	mutable get_result : (unit -> xdr_value) -> unit;

	mutable state : call_state;
	mutable retrans_count : int;        (* retransmission counter *)
	mutable xid : int;
	mutable destination : Unix.sockaddr option;

	mutable call_timeout : float;
	mutable timeout_group : group option;
	  (* If a timeout handler has been set, this is the corresponding group *)

	mutable call_auth_session : t pre_auth_session;
	mutable call_auth_method : t pre_auth_method;
	  (* calls store the authentication session and the method. *)

	mutable batch_flag : bool
      }

and t =
      { mutable ready : bool;
	mutable nolog : bool;

        mutable trans : Rpc_transport.rpc_multiplex_controller option;
	mutable progs :  Rpc_program.t list;
	mutable prot :  protocol;
        mutable esys :  event_system;
	
	mutable est_engine : Rpc_transport.rpc_multiplex_controller Uq_engines.engine option;
	mutable shutdown_connector : t -> Rpc_transport.rpc_multiplex_controller -> (unit->unit) -> unit;

	mutable waiting_calls : call Queue.t;
	mutable pending_calls : call SessionMap.t;

	mutable next_xid : int;
	mutable used_xids : unit SessionMap.t;
	mutable last_replier : Unix.sockaddr option;

	(* configs: *)
	mutable timeout : float;
        mutable max_retransmissions : int;
	mutable next_timeout : float;
	mutable next_max_retransmissions : int;
	mutable next_destination : Unix.sockaddr option;
	mutable next_batch_flag : bool;
	mutable max_resp_length : int option;
	mutable mstring_factories : Xdr_mstring.named_mstring_factories;

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
  | W32_pipe of string
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

module Debug = struct
  let enable = ref false
  let enable_ptrace = ref false
  let ptrace_verbosity = ref `Name_abbrev_args
  let disable_for_client c = c.nolog <- true
end

let dlog0 = Netlog.Debug.mk_dlog "Rpc_client" Debug.enable
let dlogr0 = Netlog.Debug.mk_dlogr "Rpc_client" Debug.enable

let dlog cl msg =
  if not cl.nolog then dlog0 msg

let dlogr cl getmsg =
  if not cl.nolog then dlogr0 getmsg

let dlog0_ptrace = Netlog.Debug.mk_dlog "Rpc_client.Ptrace" Debug.enable_ptrace
let dlogr0_ptrace = Netlog.Debug.mk_dlogr "Rpc_client.Ptrace" Debug.enable_ptrace

let dlog_ptrace cl msg =
  if not cl.nolog then dlog0_ptrace msg

let dlogr_ptrace cl getmsg =
  if not cl.nolog then dlogr0_ptrace getmsg


let () =
  Netlog.Debug.register_module "Rpc_client" Debug.enable;
  Netlog.Debug.register_module "Rpc_client.Ptrace" Debug.enable_ptrace


let connector_of_sockaddr =
  function
    | Unix.ADDR_INET(ip,p) ->
	Internet(ip,p)
    | Unix.ADDR_UNIX s ->
	Unix s


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
    dlog cl "Calling back";
    call.get_result f;
    dlog cl "Returned from callback";
  with
    | Keep_call as x ->
	dlog cl "Keep_call";
	raise x
    | Unbound_exception x ->
	dlog cl "Unbound_exception";
	raise x
    | any ->
	begin  (* pass the exception to the exception handler: *)
	  dlogr cl (fun () -> 
		      "Exception from callback: " ^ Netexn.to_string any);
	  cl.exception_handler any
	end


let pass_exception cl call x =
  (* Caution! This function does not remove [call] from the set of pending
   * calls.
   *)
  if call.state <> Done then (  (* Don't call back twice *)
    try
      dlogr cl
	(fun () ->
	   let sx = Netexn.to_string x in
	   "Passing exception " ^ sx);
      pass_result cl call (fun () -> raise x)
    with
	Keep_call -> ()          (* ignore *)
  )

let pass_exception_to_all cl x =
  (* Caution! This function does not erase the set of pending calls.  *)
  dlog cl "Passing exception to all";

  let fn_list = ref [] in
  let add_fn xid call =
    if not (List.mem_assoc xid !fn_list) then
      fn_list := (xid,call) :: !fn_list
  in

  SessionMap.iter (fun xid call -> add_fn xid call)  cl.pending_calls;
  Queue.iter      (fun call -> add_fn call.xid call) cl.waiting_calls;

  List.iter (fun (xid,call) -> pass_exception cl call x) !fn_list

  (*****)

let close ?error ?(ondown=fun()->()) cl =
  if cl.ready then (
    dlog cl "Closing";
    cl.ready <- false;
    ( match error with
	| None -> pass_exception_to_all cl Message_lost
	| Some e -> pass_exception_to_all cl e
    );
    cl.pending_calls <- SessionMap.empty;
    cl.used_xids <- SessionMap.empty;
    Queue.clear cl.waiting_calls;
    match cl.trans with
      | None -> 
	  ondown()
      | Some trans ->
	  cl.trans <- None;
	  cl.shutdown_connector cl trans ondown
  )
  else
    ondown()
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

  (*****)

let rec next_xid cl =
  if SessionMap.mem cl.next_xid cl.used_xids then
    next_xid cl
  else (
    let xid = cl.next_xid in
    cl.next_xid <- xid + 1;
    xid
  )


let add_call_again cl call =
  (* Add a call again to the queue of waiting calls. The call is authenticated
   * again.
   *)

  if not cl.ready then
    raise Client_is_down;

  let s = call.call_auth_session in

  let (cred_flav, cred_data, verf_flav, verf_data) = s # next_credentials cl in

  let xid = next_xid cl in
  (* Get new xid. Reusing the old xid seems to be too risky - there are
     servers that cache requests by xid's
   *)

  let value =
    Rpc_packer.pack_call
      call.prog
      (uint4_of_int xid)
      call.proc
      cred_flav cred_data verf_flav verf_data
      call.xdr_value
  in

  call.xid <- xid;
  call.value <- value;           (* the credentials may have changed *)
  call.state <- Waiting;
  call.retrans_count <- cl.max_retransmissions;
  call.timeout_group <- None;

  Queue.add call cl.waiting_calls;
  cl.used_xids <- SessionMap.add xid () cl.used_xids;

  !check_for_output cl
;;

  (*****)

let remove_pending_call cl call =
  cl.pending_calls <- SessionMap.remove call.xid cl.pending_calls;
  cl.used_xids <- SessionMap.remove call.xid cl.used_xids;
  stop_retransmission_timer cl call
;;

  (*****)

let retransmit cl call =
  if call.state = Pending || call.state = Waiting then begin
    if call.retrans_count > 0 then begin
      dlog cl "Retransmitting";
      (* Make the 'call' waiting again *)
      Queue.add call cl.waiting_calls;
      cl.used_xids <- SessionMap.add call.xid () cl.used_xids;
      (* Decrease the retransmission counter *)
      call.retrans_count <- call.retrans_count - 1;
      (* Check state of reources: *)
      !check_for_output cl
      (* Note: The [call] remains in state [Pending] (if it is already). 
       * This prevents the [call]
       * from being added to [cl.pending_calls] again.
       *)
    end
    else begin
      (* still no answer after maximum number of retransmissions *)
      dlog cl "Call timed out!";
      remove_pending_call cl call;
      (* Note that we do not remove the call from waiting_calls for
         performance reasons. We simply skip it there if we find it.
         pass_exception will set call.state to Done.
       *)
      pass_exception cl call Message_timeout;
      (* Note: The call_auth_session is dropped. *)
      (* If we still try to connect the TCP socket, shut the client
         completely down:
       *)
      ( match cl.est_engine with
	  | None -> ()
	  | Some e -> 
	      e#abort();
	      close cl;
      );
      (* Check state of reources: *)
      !check_for_output cl
    end
  end


  (*****)

(* Note: For asynchronous authentication, it would be sufficient that
 * add_call (and add_call_again) are rewritten such that they first
 * schedule the authentication request, and when the request is replied,
 * the call is scheduled.
 *)

let set_timeout cl call =
  if call.call_timeout > 0.0 && call.timeout_group = None then (
    (* Note: Case call_timeout = 0.0 is handled elsewhere *)
    (* CHECK: What happens when the timeout comes before the message
     * is fully written? (Low priority because for stream connections
     * a timeout is usually not set.)
     *)
    let g = new_group cl.esys in
    Unixqueue.once cl.esys g call.call_timeout
      (fun () ->
	 call.timeout_group <- None;
	 dlog cl "Timeout handler";
	 retransmit cl call;
	 (* Maybe we have to cancel reading: *)
	 !check_for_input cl
      );
    call.timeout_group <- Some g
  )


let unbound_async_call_r cl prog procname param receiver =
  if not cl.ready then
    raise Client_is_down;
  if cl.progs <> [] then (
    let prog_id = Rpc_program.id prog in
    if not (List.exists (fun p -> Rpc_program.id p = prog_id) cl.progs) then
      failwith "Rpc_client.unbound_async_call: \
                This client is not bound to the requested program"
  );

  let (_, _, out_type) =
    try Rpc_program.signature prog procname 
    with Not_found ->
      failwith ("Rpc_client.unbound_async_call: No such procedure: " ^
		  procname) in

  if cl.next_batch_flag && Xdr.xdr_type_term out_type <> X_void then
    failwith ("Rpc_client.unbound_async_call: Cannot call in batch mode: " ^
		procname);
  
  let s = find_or_make_auth_session cl in

  let (cred_flav, cred_data, verf_flav, verf_data) = s # next_credentials cl in

  let value =
    Rpc_packer.pack_call
      prog
      (uint4_of_int cl.next_xid)
      procname
      cred_flav cred_data verf_flav verf_data
      param
  in

  let new_call =
    { prog = prog;
      proc = procname;
      xdr_value = param;
      value = value;
      get_result = receiver;
      state = Waiting;
      retrans_count = cl.next_max_retransmissions;
      xid = next_xid cl;
      destination = cl.next_destination;
      call_timeout = cl.next_timeout;
      timeout_group = None;
      call_auth_session = s;
      call_auth_method = cl.current_auth_method;
      batch_flag = cl.next_batch_flag
    }
  in

  Queue.add new_call cl.waiting_calls;
  cl.used_xids <- SessionMap.add new_call.xid () cl.used_xids;
  cl.next_timeout <- cl.timeout;
  cl.next_max_retransmissions <- cl.max_retransmissions;
  cl.next_batch_flag <- false;
  (* We keep next_destination, as required by the API. *)

  (* For TCP and timeout > 0.0 set the timeout handler immediately, so the
     timeout includes connecting
   *)
  if cl.prot = Rpc.Tcp && new_call.call_timeout > 0.0 then
    set_timeout cl new_call;


  !check_for_output cl;

  new_call


let unbound_async_call cl prog procname param receiver =
  ignore(unbound_async_call_r cl prog procname param receiver)


  (*****)


type 'a threeway =
  | Value of 'a
  | Novalue
  | Error of exn


let process_incoming_message cl message peer =

  let sock = 
    match cl.trans with
      | None -> `Implied
      | Some t -> t#getsockname in

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
	    dlogr_ptrace cl
	      (fun () ->
		 sprintf
		   "RPC <-- (sock=%s,peer=%s,xid=%d) Auth error, will repeat: %s"
		   (Rpc_transport.string_of_sockaddr sock)
		   (Rpc_transport.string_of_sockaddr peer)
		   xid
		   (Rpc.string_of_server_error error)
	      );
	    add_call_again cl call;
	    Novalue                (* don't pass a value back to the caller *)
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
	    remove_pending_call cl call;
	    dlogr_ptrace cl
	      (fun () ->
		 sprintf
		   "RPC <-- (sock=%s,peer=%s,xid=%d) Auth_too_weak, will repeat"
		   (Rpc_transport.string_of_sockaddr sock)
		   (Rpc_transport.string_of_sockaddr peer)
		   xid
	      );
	    unbound_async_call 
	      cl call.prog call.proc call.xdr_value call.get_result;
	    Novalue                (* don't pass a value back to the caller *)
	| _ ->
	    let (xid,verf_flavour,verf_data,response) =
	      Rpc_packer.unpack_reply 
		~mstring_factories:cl.mstring_factories
		call.prog call.proc message
		(* may raise an exception *)
            in
	    call.call_auth_session # server_accepts verf_flavour verf_data;
	    Value response
      end
    with
	error ->
	  (* The call_auth_session is simply dropped. *)
	  (* Forward the exception [error] to the caller: *)
	  Error error
    in

    match result_opt with
      | Novalue ->
	  (* There is no result yet *)
	  ()

      | Value result ->

          (* pass result to the user *)

	  ( try
	      dlogr_ptrace cl
		(fun () ->
		   sprintf
		     "RPC <-- (sock=%s,peer=%s,xid=%d) %s"
		     (Rpc_transport.string_of_sockaddr sock)
		     (Rpc_transport.string_of_sockaddr peer)
		     xid
		     (Rpc_util.string_of_response
			!Debug.ptrace_verbosity
			call.prog
			call.proc
			result
		     )
		);
	      let f = (fun () -> result) in
	      pass_result cl call f;      (* may raise Keep_call *)
	      (* Side effect: Changes the state of [call] to [Done] *)
	      remove_pending_call cl call;
	      cl.unused_auth_sessions <-
	        call.call_auth_session ::cl.unused_auth_sessions;
	    with
		Keep_call ->
		  call.state <- Pending
	  )

      | Error error ->
	  ( try
	      dlogr_ptrace cl
		(fun () ->
		   sprintf
		     "RPC <-- (sock=%s,peer=%s,xid=%d) Error %s"
		     (Rpc_transport.string_of_sockaddr sock)
		     (Rpc_transport.string_of_sockaddr peer)
		     xid
		     (Netexn.to_string error)
		);
	      let f = (fun () -> raise error) in
	      pass_result cl call f;      (* may raise Keep_call *)
	      (* Side effect: Changes the state of [call] to [Done] *)
	      remove_pending_call cl call;
	      cl.unused_auth_sessions <-
	        call.call_auth_session ::cl.unused_auth_sessions;
	    with
		Keep_call ->
		  call.state <- Pending
	  )


let drop_response cl message peer =
  let sock = 
    match cl.trans with
      | None -> `Implied
      | Some t -> t#getsockname in
  let xid =
    try
      int_of_uint4 (Rpc_packer.peek_xid message)
    with
	_ -> raise Message_not_processable in
  let call =
    try
      SessionMap.find xid cl.pending_calls
    with
	Not_found ->
	  raise Message_not_processable in
  assert(call.state = Pending);

  dlogr_ptrace cl
    (fun () ->
       sprintf
	 "RPC <-- (sock=%s,peer=%s) Dropping response"
	 (Rpc_transport.string_of_sockaddr sock)
	 (Rpc_transport.string_of_sockaddr peer)
    );
  
  try
    let f = (fun () -> raise Response_dropped) in
    pass_result cl call f;      (* may raise Keep_call *)
    (* Side effect: Changes the state of [call] to [Done] *)
    remove_pending_call cl call;
    cl.unused_auth_sessions <-
      call.call_auth_session :: cl.unused_auth_sessions;
  with
      Keep_call ->
	call.state <- Pending

  (*****)

let rec handle_incoming_message cl r =
  (* Called when a complete message has been read by the transporter *)
  match r with
    | `Error e ->
	close ~error:(Communication_error e) cl

    | `Ok(msg,addr) ->
	dlog cl "Message arrived";
	( try
	    ( match addr with
		| `Implied -> ()
		| `Sockaddr a ->
		    cl.last_replier <- Some a
	    );
	    ( match msg with
		| `Accept pv ->
		    process_incoming_message cl pv addr
		| `Reject pv ->
		    drop_response cl pv addr
		| _ ->
		    assert false
	    )
	  with
	      Message_not_processable ->
		dlog cl "message not processable";
		()
	);
	(next_incoming_message cl : unit)

    | `End_of_file ->
	dlog cl "End of file";
	close cl

and next_incoming_message cl =
  match cl.trans with
    | None -> ()
    | Some trans -> next_incoming_message' cl trans

and next_incoming_message' cl trans =
  trans # cancel_rd_polling();
  if cl.pending_calls <> SessionMap.empty && not trans#reading then (
    trans # start_reading
      ~before_record:(fun n _ ->
			match cl.max_resp_length with
			  | None -> `Accept
			  | Some m -> if n > m then `Reject else `Accept
		     )
      ~when_done:(fun r ->
		    handle_incoming_message cl r)
      ()
  )
  else
    dlog cl "Stopping reading";
;;


check_for_input := next_incoming_message;;


let rec handle_outgoing_message cl call r =
  (* Called after a complete message has been sent by the transporter *)
  match r with
    | `Error e ->
	close ~error:(Communication_error e) cl

    | `Ok () ->
	dlog cl "message writing finished";
	if call.batch_flag || call.call_timeout = 0.0 then (
	  try
	    if call.batch_flag then
	      pass_result cl call (fun () -> XV_void) (* may raise Keep_call *)
	    else
	      pass_exception cl call Message_timeout;
	    remove_pending_call cl call;
	  with Keep_call ->  (* no removal *)
	    ()
	);
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
	  (* That can happen for calls that timeout before they are sent *)
	  dlog cl "found call that has been done";
	  next_outgoing_message cl
	)
	else (
	  let dest =
	    match call.destination with
	      | Some d -> `Sockaddr d
	      | None -> trans#getpeername in
	  ( match call.state with
	      | Done -> assert false
	      | Waiting ->
		  cl.pending_calls <-
		    SessionMap.add call.xid call cl.pending_calls;
		  (* The xid is already in used_xids *)
		  call.state <- Pending;
		  dlogr_ptrace cl
		    (fun () ->
		       sprintf
			 "RPC --> (sock=%s,peer=%s,xid=%d) %s"
			 (Rpc_transport.string_of_sockaddr trans#getsockname)
			 (Rpc_transport.string_of_sockaddr dest)
			 call.xid
			 (Rpc_util.string_of_request
			    !Debug.ptrace_verbosity
			    call.prog
			    call.proc
			    call.xdr_value
			 )
		    )
	      | Pending ->
		  ()
		    (* The call is already member of [pending_calls]
		     * (retransmitted)
		     *)
	  );

	  (* If there should be a timeout handler, add it: *)
	  set_timeout cl call;

	  (* Send the message: *)

	  dlog cl "start_writing";
	  trans # start_writing
	    ~when_done:(fun r ->
			  handle_outgoing_message cl call r)
	    call.value
	    dest

	);

    | None ->
	()
;;


check_for_output := next_outgoing_message ;;


(* Shutdown:
 * We first try an orderly shutdown. If that does not work, just inactivate
 * the transport.
 *)


let shutdown_connector cl mplex ondown =
  dlog cl "shutdown_connector";
  mplex # abort_rw();
  ( try
      mplex # start_shutting_down
	~when_done:(fun exn_opt ->
		      ( match exn_opt with 
			  | `Ok _ -> ()
			  | `Error exn ->
			      dlogr cl
				(fun () -> 
				   sprintf "start_shutting_down: exception %s"
				     (Netexn.to_string exn))
		      );
		      mplex # inactivate();
		      ondown()
		   )
	()
    with
      | _ -> mplex # inactivate(); ondown()
  )


let mplex_of_fd ~close_inactive_descr prot fd esys =
  let preclose() =
    Netlog.Debug.release_fd fd in
  match prot with
    | Tcp ->
        Rpc_transport.stream_rpc_multiplex_controller
          ~close_inactive_descr ~preclose fd esys
    | Udp ->
        Rpc_transport.datagram_rpc_multiplex_controller
          ~close_inactive_descr ~preclose fd esys


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
    let close() =
      if close_inactive_descr then (
	Netlog.Debug.release_fd fd;
	try
	  match Netsys_win32.lookup fd with
	    | Netsys_win32.W32_pipe ph ->
		Netsys_win32.pipe_shutdown ph;
		Unix.close fd
	    | _ -> 
		()
	with Not_found ->
	  Unix.close fd 
      ) in
    let eng = 
      try
	let mplex = mplex_of_fd ~close_inactive_descr prot fd esys in
	new Uq_engines.epsilon_engine (`Done mplex) esys 
      with
	| error -> 
	    new Uq_engines.epsilon_engine (`Error error) esys in
    Uq_engines.when_state
      ~is_aborted:(fun () -> close())
      ~is_error:(fun _ -> close())
      eng;
    eng
end


class blocking_socket_config : socket_config =
object
  inherit default_socket_config
  method non_blocking_connect = false
end

let default_socket_config = new default_socket_config
let blocking_socket_config = new blocking_socket_config

type mode2 =
    [ `Socket_endpoint of protocol * Unix.file_descr 
    | `Multiplexer_endpoint of Rpc_transport.rpc_multiplex_controller
    | `Socket of protocol * connector * socket_config
    ]


class unbound_async_call cl prog name v =
  let emit = ref (fun _ -> assert false) in
  let call = unbound_async_call_r cl prog name v (fun gr -> !emit gr) in
object(self)
  inherit [ Xdr.xdr_value ] Uq_engines.engine_mixin (`Working 0) cl.esys

  initializer
    emit := (fun get_result -> 
	       try
		 let r = get_result() in
		 self # set_state (`Done r)
	       with
		 | err ->
		     self # set_state (`Error err)
	    )

  method event_system = cl.esys
  method abort() =
    match self#state with
      | `Working _ ->
	  if call.state <> Done then
	    remove_pending_call cl call;
	  call.state <- Done;
	  self # set_state `Aborted
      | _ -> ()
end


let string_of_file_descr fd =
  Int64.to_string (Netsys.int64_of_file_descr fd)

let rec internal_create initial_xid
                        shutdown
			prog_opt
                        mode esys =

  let id_s_0 =
    match mode with
      | `Socket_endpoint(_,fd) ->
	  "Socket_endpoint(_," ^ string_of_file_descr fd ^ ")"
      | `Multiplexer_endpoint ep ->
	  "Multiplexer_endpoint(" ^ 
	    string_of_int(Oo.id ep) ^ ")"
      | `Socket(_, conn, _) ->
	  let s_conn =
	    match conn with
	      | Inet(h,p) -> 
		  "inet/" ^ h ^ ":" ^ string_of_int p
	      | Internet(ip,p) -> 
		  "inet/" ^ Unix.string_of_inet_addr ip ^ ":" ^ string_of_int p
	      | Unix p ->
		  "unix/" ^ p
	      | W32_pipe p ->
		  "w32_pipe/" ^ p
	      | Descriptor fd ->
		  "fd/" ^ string_of_file_descr fd
	      | Dynamic_descriptor f ->
		  "dyn_fd"
	      | Portmapped h ->
		  "portmapped:" ^ h in
	  "Socket(_," ^ s_conn ^ ",_)" in
  let id_s =
    match prog_opt with
      | None -> id_s_0
      | Some prog ->
	  id_s_0 ^ " for program " ^ 
	    (Int32.to_string 
	       (Rtypes.logical_int32_of_uint4
		  (Rpc_program.program_number prog))) in

  let non_blocking_connect =
    match mode with
      | `Socket(_,_,conf) -> conf # non_blocking_connect
      | _ -> true in

  let cl =
    (* preliminary version of the client *)
    { ready = true;
      trans = None;
      progs = ( match prog_opt with None -> [] | Some p -> [p] );
      prot = Rpc.Udp;
      esys = esys;
      est_engine = None;
      shutdown_connector = shutdown;
      waiting_calls = Queue.create();
      pending_calls = SessionMap.empty;
      used_xids = SessionMap.empty;
      next_xid = initial_xid;
      next_destination = None;
      next_timeout = (-1.0);
      next_max_retransmissions = 0;
      next_batch_flag = false;
      max_resp_length = None;
      mstring_factories = Hashtbl.create 1;
      last_replier = None;
      timeout = (-1.0);
      max_retransmissions = 0;
      exception_handler = (fun _ -> ());
      auth_methods = [ ];
      current_auth_method = auth_none;
      unused_auth_sessions = [];
      nolog = false;
    }
  in
  Hashtbl.add cl.mstring_factories "*" Xdr_mstring.string_based_mstrings;
  

  let portmapper_engine prot host prog esys = 
    (* Performs GETPORT for the program on [host]. We use 
     * Rpc_portmapper_aux but not Rpc_portmapper_clnt. The latter is
     * impossible because of a dependency cycle.
     *)
    dlog cl "starting portmapper query";
    let pm_port = Rtypes.int_of_uint4 Rpc_portmapper_aux.pmap_port in
    let pm_prog = Rpc_portmapper_aux.program_PMAP'V2 in
    let pm_client = 
      internal_create
	0
	shutdown_connector
	(Some pm_prog)
	(`Socket(Rpc.Udp, Inet(host, pm_port), default_socket_config))
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
		   dlog cl "Portmapper GETPORT done";
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
		    dlog cl "Portmapper GETPORT error";
		    close_deferred();
		    `Error err)
      (new unbound_async_call pm_client pm_prog "PMAPPROC_GETPORT" v)
  in

  let connect_engine addr esys =
    match addr with
      | `Portmapped(prot,host) ->
	  ( match prog_opt with
	      | None ->
		  failwith 
		    "Rpc_client.unbound_create: Portmapped not supported"
	      | Some prog ->
		  new Uq_engines.seq_engine
		    (portmapper_engine prot host prog esys)
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
	  )

      | #Uq_engines.connect_address as addr ->
	  Uq_engines.connector addr esys in

  let track fd =
    Netlog.Debug.track_fd ~owner:"Rpc_client" ~descr:id_s fd in

  let open_socket_non_blocking addr prot conf =
    new Uq_engines.seq_engine
      (connect_engine addr esys)
      (fun status ->
	  dlogr  cl
	    (fun () -> 
	       "Non-blocking socket connect successful for " ^ id_s);
	 let fd = Uq_engines.client_endpoint status in
	 track fd;
	 conf # multiplexing ~close_inactive_descr:true prot fd esys
      ) in

  let open_socket_blocking addr prot conf =
    let conn_esys = Unixqueue.create_unix_event_system() in
    let c = connect_engine addr conn_esys in
    Unixqueue.run conn_esys;
    match c # state with
      | `Done status ->
	  dlogr cl
	    (fun () ->
	       "Blocking socket connect successful for " ^ id_s);
	  let fd = Uq_engines.client_endpoint status in
	  track fd;
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
	  track fd;
	  let m = mplex_of_fd ~close_inactive_descr:true prot fd esys in
	  (prot, new Uq_engines.epsilon_engine (`Done m) esys)
      | `Multiplexer_endpoint(mplex) ->
	  if mplex # event_system != esys then
            failwith "Rpc_client.create2: \
                      Multiplexer is attached to the wrong event system";
	  (mplex # protocol,
	   new Uq_engines.epsilon_engine (`Done mplex) esys)
       | `Socket(prot,conn,conf) ->
	   let stype = 
	     match prot with Tcp -> Unix.SOCK_STREAM | Udp -> Unix.SOCK_DGRAM in
	   (match conn with
	      | Inet (host,port) ->
		  let saddr = `Sock_inet_byname(stype, host, port) in
		  let addr = 
		    `Socket(saddr, Uq_engines.default_connect_options) in
		  (prot, open_socket addr prot conf)
	      | Internet (host,port) ->
		  let saddr = `Sock_inet(stype, host, port) in
		  let addr = 
		    `Socket(saddr, Uq_engines.default_connect_options) in
		  (prot, open_socket addr prot conf)
	      | Unix path ->
		  let saddr = `Sock_unix(stype, path) in
		  let addr = 
		    `Socket(saddr, 
			    Uq_engines.default_connect_options) in
		  (prot, open_socket addr prot conf)
	      | W32_pipe path ->
		  if prot <> Rpc.Tcp then
		    failwith "Rpc_client.create2: \
                              Pipe only supported for Rpc.Tcp protocol type";
		  let addr = `W32_pipe(Netsys_win32.Pipe_duplex, path) in
		  (prot, open_socket addr prot conf)
	      |	Descriptor fd -> 
		  (* no track! *)
		  let m = 
		    mplex_of_fd ~close_inactive_descr:false prot fd esys in
		  (prot, new Uq_engines.epsilon_engine (`Done m) esys)
	      |	Dynamic_descriptor f ->
		  let fd = f() in
		  track fd;
		  let m = 
		    mplex_of_fd ~close_inactive_descr:true prot fd esys in
		  (prot, new Uq_engines.epsilon_engine (`Done m) esys)
	      | Portmapped host ->
		  (prot, open_socket (`Portmapped(prot,host)) prot conf)
	   )
  in

  let timeout = if prot = Udp then 15.0 else (-.1.0) in
  let max_retransmissions = if prot = Udp then 3 else 0 in

  (* update cl: *)
  cl.prot <- prot;
  cl.est_engine <- Some establish_engine;
  cl.next_timeout <- timeout;
  cl.timeout <- timeout;
  cl.next_max_retransmissions <- max_retransmissions;
  cl.max_retransmissions <- max_retransmissions;
  cl.exception_handler <- (fun exn -> 
			     if not cl.nolog then
			       Netlog.logf `Crit
				 "Rpc_client: Uncaught exception %s"
				 (Netexn.to_string exn)
			  );

  Uq_engines.when_state
    ~is_done:(fun mplex ->
		dlogr cl
		  (fun () ->
		     sprintf 
		       "Fully connected for %s: (sock=%s,peer=%s)" 
		       id_s
		       (Rpc_transport.string_of_sockaddr mplex#getsockname)
		       (Rpc_transport.string_of_sockaddr mplex#getpeername));
		cl.trans <- Some mplex;
		cl.est_engine <- None;
		(* Maybe we already have messages to send: *)
		!check_for_output cl
	     )
    ~is_error:(fun err ->
		 cl.est_engine <- None;
		 close ~error:(Communication_error err) cl;
		 cl.exception_handler err
	      )
    ~is_aborted:(fun () ->
		   cl.est_engine <- None)
    establish_engine;

  cl
;;


let create2 ?program_number ?version_number ?(initial_xid=0)
            ?(shutdown = shutdown_connector)
            mode prog0 esys =
  
  let prog = Rpc_program.update ?program_number ?version_number prog0 in
  let cl = internal_create initial_xid shutdown (Some prog) mode esys in
  cl

let unbound_create ?(initial_xid=0) ?(shutdown = shutdown_connector) 
                   mode esys =
  internal_create initial_xid shutdown None mode esys


let bind cl prog =
  cl.progs <- prog :: cl.progs

let use cl prog =
  let prog_id = Rpc_program.id prog in
  if not(List.exists (fun p -> Rpc_program.id p = prog_id) cl.progs) then
    failwith "Rpc_client.use: This program is not bound by this client"


  (*****)

let is_up cl =
  cl.ready

let configure_next_call cl max_retransmission_trials timeout =
  cl.next_max_retransmissions <- max_retransmission_trials;
  cl.next_timeout <- timeout

let configure cl max_retransmission_trials timeout =
  cl.max_retransmissions <- max_retransmission_trials;
  cl.timeout <- timeout;
  configure_next_call cl max_retransmission_trials timeout

let set_dgram_destination cl addr_opt =
  cl.next_destination <- addr_opt

let set_exception_handler cl xh =
  cl.exception_handler <- xh

let set_batch_call cl =
  cl.next_batch_flag <- true

let set_max_response_length cl n =
  cl.max_resp_length <- Some n

let set_mstring_factories cl fac =
  cl.mstring_factories <- fac

let gen_shutdown cl is_running run ondown =
  if cl.ready then (
    let b = is_running cl.esys in
    ( match cl.est_engine with
	| None -> ()
	| Some e -> e#abort()
    );
    close ~ondown cl;
    if not b then run cl.esys
  )
  else
    ondown()

let shut_down cl =
  gen_shutdown 
    cl 
    Unixqueue.is_running 
    Unixqueue.run 
    (fun () -> ())

let sync_shutdown cl =
  gen_shutdown 
    cl 
    (fun esys -> 
       if Unixqueue.is_running esys then
	 failwith "Rpc_client.sync_shutdown: called from event loop";
       false)
    Unixqueue.run 
    (fun () -> ())
  
let trigger_shutdown cl ondown =
  gen_shutdown
    cl
    (fun _ -> true)
    Unixqueue.run
    (fun () ->
       let g = Unixqueue.new_group cl.esys in
       Unixqueue.once cl.esys g 0.0 ondown
    )


let event_system cl =
  cl.esys

let program cl =
  List.hd cl.progs

let programs cl =
  cl.progs

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

let get_sender_of_last_response cl =
  match cl.last_replier with
    | None -> failwith "Rpc_client.get_sender_of_last_response: nothing received yet or sender's address not available from transport layer"
    | Some addr -> addr

let get_protocol cl =
  cl.prot

let verbose b =
  Debug.enable := b

  (*****)

(* Now synchronous calls: *)

type 'a result =
    No
  | Reply of 'a
  | Error of exn


exception Stop_call

let synchronize esys f_async arg =
  let r = ref No in
  let get_result transmitter =
    try
      r := Reply (transmitter())
    with
      x ->
	r := Error x;
	if x = Message_timeout then 
	  raise (Unbound_exception Stop_call)
  in
  (* push the request onto the queue: *)
  let () = f_async arg get_result in
  (* run through the queue and process all elements: *)
  ( try Unixqueue.run esys with Stop_call -> ());
  (* now a call back of 'get_result' should have happened. *)
  match !r with
    No -> failwith "Rpc_client.synchronize: internal error"
  | Reply x -> x
  | Error e -> raise e



let unbound_sync_call cl prog proc arg =
  synchronize
    cl.esys
    (unbound_async_call cl prog proc)
    arg


  (*****)

(* DEPRECATED FUNCTIONS *)


let add_call cl procname param receiver =
  if not cl.ready then
    raise Client_is_down;
  match cl.progs with
    | [ prog ] ->
	unbound_async_call cl prog procname param receiver
    | _ ->
	failwith "Rpc_client.add_call [deprecated function]: \
                  The client does not have exactly one bound program"


let sync_call cl procname param  =
  if not cl.ready then
    raise Client_is_down;
  match cl.progs with
    | [ prog ] ->
	unbound_sync_call cl prog procname param 
    | _ ->
	failwith "Rpc_client.sync_call [deprecated function]: \
                  The client does not have exactly one bound program"


let create ?program_number ?version_number ?(initial_xid=0) 
           ?(shutdown = shutdown_connector)
           esys c prot prog0 =
  create2 
    ?program_number ?version_number ~initial_xid ~shutdown
    (`Socket(prot, c, (new blocking_socket_config)))
    prog0
    esys

