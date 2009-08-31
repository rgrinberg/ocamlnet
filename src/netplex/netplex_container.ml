(* $Id$ *)

open Netplex_types
open Netplex_ctrl_aux
open Printf

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netplex_container" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netplex_container" Debug.enable

let () =
  Netlog.Debug.register_module "Netplex_container" Debug.enable

let string_of_event = 
  function
    | `event_none -> "NONE"
    | `event_accept ->  "ACCEPT"
    | `event_noaccept ->  "NOACCEPT"
    | `event_received_message msg ->
	"RECEIVED_MESSAGE(" ^ msg.msg_name ^ ")"
    | `event_received_admin_message msg -> 
	"RECEIVED_ADMIN_MESSAGE(" ^ msg.msg_name ^ ")"
    | `event_shutdown -> "SHUTDOWN"
    | `event_system_shutdown -> "SYSTEM_SHUTDOWN"

let string_of_sys_id =
  function
    | `Thread id -> "thread " ^ string_of_int id
    | `Process id -> "process " ^ string_of_int id


class std_container ?(esys = Unixqueue.create_unix_event_system()) 
                    ptype sockserv =
  let ssn = sockserv # name in
object(self)
  val sys_esys = Unixqueue.create_unix_event_system()
  val mutable rpc = None
  val mutable sys_rpc = None
  val mutable nr_conns = 0
  val mutable engines = []
  val mutable vars = Hashtbl.create 10 

  method socket_service_name = ssn
  method socket_service = sockserv

  method event_system = esys

  method ptype = ptype

  method start fd_clnt sys_fd_clnt =
    (* note that logging is first possible when sys_rpc is initialized! *)
    if rpc <> None then
      failwith "#start: already started";
    let sys_id = Netplex_cenv.current_sys_id() in
    ( match ptype with
	| `Multi_processing ->
	    ( match sockserv # socket_service_config # change_user_to with
		| None -> ()
		| Some(uid,gid) ->
		    (* In Netplex_config it has already been checked whether the
                     * effective uid of the process is root. So the following 
                     * drops all privileges:
		     *)
		    Unix.setgid gid;
		    Unix.setuid uid
	    )
	| _ -> ()
    );
    (* Note: fd_clnt and sys_fd_clnt are closed by the caller *)
    let rpc_cl =
      Netplex_ctrl_clnt.Control.V1.create_client
      ~esys
      (Rpc_client.Descriptor fd_clnt)
      Rpc.Tcp in
    (* We disable logging because this is pretty uninteresting (except
       you are debugging the container/controller protocol - but there are
       special log messages for this purpose). Also sys_rpc_cl below also
       disables logging
     *)
    Rpc_client.Debug.disable_for_client rpc_cl;
    rpc <- Some rpc_cl;
    let sys_rpc_cl =
      Netplex_ctrl_clnt.System.V1.create_client
	~esys:sys_esys
	(Rpc_client.Descriptor sys_fd_clnt)
	Rpc.Tcp in
    (* We have to disable logging for this client, because this client is
       used to transmit log messages. If we did not do this, each log
       message would have another log message as follower, creating an
       infinite sequence of messages.
     *)
    Rpc_client.Debug.disable_for_client sys_rpc_cl;
    sys_rpc <- Some sys_rpc_cl;
    dlogr 
      (fun () -> 
	 sprintf "Container %d: Starting (start) in %s" 
	   (Oo.id self) (string_of_sys_id sys_id));
    self # protect "post_start_hook"
      (sockserv # processor # post_start_hook)
      (self : #container :> container);

    self # setup_polling();
    self # protect_run();
    (* protect_run returns when all events are handled. This must include
       [rpc], so the client must now be down.
     *)
    assert(rpc = None);

    self # protect "pre_finish_hook"
      (sockserv # processor # pre_finish_hook)
      (self : #container :> container);
    rpc <- None;
    dlogr
      (fun () -> sprintf "Container %d: Finishing (finish)" (Oo.id self))


  method private protect : 's. string -> ('s -> unit) -> 's -> unit =
    fun label f arg ->
      ( try
	  dlogr
	    (fun () -> 
	       sprintf "Container %d: calling hook %s" (Oo.id self) label);
	  f arg 
	with
	  | error ->
	      ( match rpc with
		  | None -> ()  (* no way to report this error *)
		  | Some r ->
		      self # log `Crit (label ^ ": Exception " ^ 
					  Netexn.to_string error)
	      )
      );
       dlogr
	 (fun () -> 
	    sprintf "Container %d: back from hook %s" (Oo.id self) label)


  method private protect_run () =
    try 
      Unixqueue.run esys
    with
      | error ->
	  self # log `Crit ("run: Exception " ^ Netexn.to_string error);
	  let b = sockserv # processor # global_exception_handler error in
	  if b then self # protect_run()


  method private setup_polling() =
    match rpc with
      | None -> ()  (* Already shut down ! *)
      | Some r ->
	  dlogr
	    (fun () -> 
	       sprintf "Container %d: Polling" (Oo.id self));
	  Netplex_ctrl_clnt.Control.V1.poll'async r nr_conns
	    (fun getreply ->
	       let continue =
		 ( try
		     let reply = getreply() in
		     dlogr
		       (fun () -> 
			  sprintf "Container %d: Got event from polling: %s"
			    (Oo.id self) (string_of_event reply));
		     ( match reply with
			 | `event_none ->
			     (* When [setup_calling] is called twice, the
                                second poll loop throws out the first poll
                                loop. The first loop gets then [`event_none],
                                and has to terminate. This is used after
                                a connection is done to gain attention from
                                the server.
			      *)
			     false
			 | `event_accept -> 
			     self # enable_accepting();
			     true
			 | `event_noaccept -> 
			     self # disable_accepting();
			     true
			 | `event_received_message msg ->
			     self # protect
			       "receive_message"
			       (sockserv # processor # receive_message
				  (self : #container :> container)
				  msg.msg_name)
			       msg.msg_arguments;
			     true
			 | `event_received_admin_message msg ->
			     self # receive_admin_message msg;
			     true
			 | `event_shutdown ->
			     self # disable_accepting();
			     self # protect
			       "shutdown"
			       (sockserv # processor # shutdown)
			       ();
			     ( try
				 Netplex_cenv.cancel_all_timers()
			       with
				   (* can happen in the admin case: *)
				 | Netplex_cenv.Not_in_container_thread -> ()
			     );
			     ( match rpc with
				 | None -> ()
				 | Some r -> 
				     rpc <- None;
				     Rpc_client.trigger_shutdown 
				       r
				       self # shutdown_extra;
				     (* We ensure that shutdown_extra is called
                                        when the client is already taken down
				      *)
			     );
			     false
			 | `event_system_shutdown ->
			     self # protect
			       "system_shutdown"
			       (sockserv # processor # system_shutdown)
			       ();
			     true
		     )
		   with
		     | Rpc_client.Message_lost ->
			 (* This can happen when the container is shut down
                          * and several [poll] calls are active at the same
                          * time.
                          *)
			 false
		     | error ->
			 self # log `Crit ("poll: Exception " ^ 
					    Netexn.to_string error);
			 true
		 ) in
	       if continue then
		 self # setup_polling()
	    )
    
  method private enable_accepting() =
    if engines = [] then (
      List.iter
	(fun (proto, fd_array) ->
	   Array.iter
	     (fun fd ->
		dlogr
		  (fun () ->
		     sprintf "Container %d: Accepting on fd %Ld"
		       (Oo.id self)
		       (Netsys.int64_of_file_descr fd));
		let acc = new Uq_engines.direct_acceptor fd esys in
		let e = acc # accept() in
		Uq_engines.when_state
		  ~is_done:(fun (fd_slave,_) ->
			      (* The first engine accepted a connection. It is
                               * possible that other descriptors are ready
                               * for being accepted at this time. By disabling
                               * accepting, we ensure that these engines
                               * are aborted and the events are ignored.
                               * It is essential that the direct_endpoint_acceptor
                               * accepts in one step so intermediate states
                               * are impossible.
                               *)
			      dlogr
				(fun () ->
				   sprintf "Container %d: Accepted as fd %Ld"
				     (Oo.id self)
				     (Netsys.int64_of_file_descr fd_slave));
			      self # disable_accepting();
			      self # accepted fd_slave proto
			   )
		  ~is_error:(fun err ->
			       self # log `Crit
				 ("accept: Exception " ^ 
				    Netexn.to_string err)
			    )
		  e;
		engines <- e :: engines
	     )
	     fd_array
	)
	sockserv#sockets
    )

  method private disable_accepting() =
    dlogr
      (fun () ->
	 sprintf "Container %d: No longer accepting" (Oo.id self));
    List.iter (fun e -> e # abort()) engines;
    engines <- [];

  method private accepted fd_slave proto =
    match rpc with
      | None -> assert false
      | Some r ->
	  Rpc_client.set_batch_call r;
	  Rpc_client.unbound_async_call
	    r Netplex_ctrl_aux.program_Control'V1 "accepted" Xdr.XV_void
	    (fun _ ->
	       nr_conns <- nr_conns + 1;
	       let when_done_called = ref false in
	       dlogr
		 (fun () -> 
		    sprintf
		      "Container %d: processing connection on fd %Ld \
                       (total conns: %d)" 
		      (Oo.id self) 
		      (Netsys.int64_of_file_descr fd_slave)
		      nr_conns
		 );
	       self # protect
		 "process"
		 (sockserv # processor # process
		    ~when_done:(fun fd ->
				  if not !when_done_called then (
				    nr_conns <- nr_conns - 1;
				    when_done_called := true;
				    self # setup_polling();
				    dlogr
				      (fun () ->
					 sprintf "Container %d: \
                                               Done with connection on fd %Ld \
                                                  (total conns %d)"
					   (Oo.id self) 
					   (Netsys.int64_of_file_descr fd_slave)
					   nr_conns);
				  )
			       )
		    (self : #container :> container)
		    fd_slave
		 )
		 proto;
	       if not !when_done_called then
			    self # setup_polling();
	    )

  method system =
    match sys_rpc with
      | None -> failwith "#system: No RPC client available"
      | Some r -> r

  method shutdown() =
    dlogr
      (fun () -> 
	 sprintf
	   "Container %d: shutdown" (Oo.id self));
    ( try
	Netplex_cenv.cancel_all_timers()
      with
	  (* can happen in the admin case: *)
	| Netplex_cenv.Not_in_container_thread -> ()
    );
    self # disable_accepting();
    ( match rpc with
	| None -> ()
	| Some r -> 
	    rpc <- None;
	    Rpc_client.trigger_shutdown 
	      r
	      self # shutdown_extra;
	    (* We ensure that shutdown_extra is called
               when the client is already taken down
	     *)
    )

  method private shutdown_extra() = 
    (* to be subclassed *)
    ()

  method log_subch subchannel level message =
    match sys_rpc with
      | None -> ()
      | Some r ->
	  ( try
	      let lev = 
		match level with
		  | `Emerg -> log_emerg
		  | `Alert -> log_alert
		  | `Crit -> log_crit
		  | `Err -> log_err
		  | `Warning -> log_warning
		  | `Notice -> log_notice
		  | `Info -> log_info
		  | `Debug -> log_debug in
	      Rpc_client.set_batch_call r;
	      Netplex_ctrl_clnt.System.V1.log r (lev,subchannel,message);
	    with
	      | error ->
		  prerr_endline("Netplex Catastrophic Error: Unable to send log message - exception " ^ Netexn.to_string error);
		  prerr_endline("Log message is: " ^ message)
	  )

  method log level message =
    self # log_subch "" level message

  method lookup service protocol =
    match sys_rpc with
      | None -> failwith "#lookup: No RPC client available"
      | Some r ->
	  Netplex_ctrl_clnt.System.V1.lookup r (service,protocol)

  method send_message pat msg_name msg_arguments =
    dlogr
      (fun () -> 
	 sprintf
	   "Container %d: send_message %s to %s" (Oo.id self) msg_name pat);
    match sys_rpc with
      | None -> failwith "#send_message: No RPC client available"
      | Some r ->
	  let msg =
	    { msg_name = msg_name;
	      msg_arguments = msg_arguments
	    } in
	  Netplex_ctrl_clnt.System.V1.send_message r (pat, msg)

  method var name =
    Hashtbl.find vars name

  method set_var name value =
    Hashtbl.replace vars name value

  method call_plugin p proc_name arg =
    dlogr
      (fun () ->
	 sprintf
	   "Container %d: call_plugin id=%d procname=%s" 
	   (Oo.id self) (Oo.id p) proc_name);
    match sys_rpc with
      | None -> failwith "#call_plugin: No RPC client available"
      | Some r ->
	  let (_, arg_ty,res_ty) = 
	    try
	      Rpc_program.signature p#program proc_name
	    with
	      | Not_found -> failwith "call_plugin: procedure not found" in
	  let arg_str = Xdr.pack_xdr_value_as_string arg arg_ty [] in
	  let res_str =
	    Netplex_ctrl_clnt.System.V1.call_plugin
	      r
	      ((Int64.of_int (Oo.id p)), proc_name, arg_str) in
	  let res = Xdr.unpack_xdr_value ~fast:true res_str res_ty [] in
	  res
	    
  method private receive_admin_message msg =
    match msg.msg_name with
      | "netplex.debug.enable" ->
	  ( try
	      let m = 
		match msg.msg_arguments with
		  | [| m |] -> m
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      Netlog.Debug.enable_module m
	    with
	      | Failure s ->
		  self # log 
		    `Err
		    ("netplex.debug.enable: " ^ s)
	  )
      | "netplex.debug.disable" ->
	  ( try
	      let m = 
		match msg.msg_arguments with
		  | [| m |] -> m
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      Netlog.Debug.disable_module m
	    with
	      | Failure s ->
		  self # log 
		    `Err
		    ("netplex.debug.disable: " ^ s)
	  )
      | _ ->
	  self # protect
	    "receive_admin_message"
	    (sockserv # processor # receive_admin_message
	       (self : #container :> container)
	       msg.msg_name)
	    msg.msg_arguments;
end


let close_pipe fd =
  (* TODO: Dup in netplex_controller.ml *)
  match Sys.os_type with
    | "Win32" ->
	( try
	    match Netsys_win32.lookup fd with
	      | Netsys_win32.W32_pipe ph ->
		  Netsys_win32.pipe_shutdown ph
	      | _ ->
		  assert false
	  with Not_found -> 
	    assert false
	)
    | _ ->
	Unix.close fd



(* The admin container is special because the esys is shared with the
   system-wide controller
 *)

class admin_container esys ptype sockserv =
object(self)
  inherit std_container ~esys ptype sockserv

  val mutable c_fd_clnt = None
  val mutable c_sys_fd_clnt = None

  method start fd_clnt sys_fd_clnt =
    if rpc <> None then
      failwith "#start: already started";
    let rpc_cl =
      Netplex_ctrl_clnt.Control.V1.create_client
	~esys
	(Rpc_client.Descriptor fd_clnt)
	Rpc.Tcp in
    Rpc_client.Debug.disable_for_client rpc_cl;
    rpc <- Some rpc_cl;
    let sys_rpc_cl =
      Netplex_ctrl_clnt.System.V1.create_client
	~esys:sys_esys
	(Rpc_client.Descriptor sys_fd_clnt)
	Rpc.Tcp in
    Rpc_client.Debug.disable_for_client sys_rpc_cl;
    sys_rpc <- Some sys_rpc_cl;
    c_fd_clnt <- Some fd_clnt;
    c_sys_fd_clnt <- Some sys_fd_clnt;
    self # setup_polling();

  method private shutdown_extra() =
    (* In the admin case, fd_clnt and sys_fd_clnt are never closed.
       Do this now. (In the non-admin case the caller does this after
       [start] returns.)
     *)
    ( match sys_rpc with
	| None -> ()
	| Some r -> 
	    Rpc_client.shut_down r;
	    sys_rpc <- None
    );
    ( match c_fd_clnt with
	| None -> ()
	| Some fd -> close_pipe fd; c_fd_clnt <- None
    );
    ( match c_sys_fd_clnt with
	| None -> ()
	| Some fd -> close_pipe fd; c_sys_fd_clnt <- None
    )
end


let create_container ptype sockserv =
  new std_container ptype sockserv

let create_admin_container esys ptype sockserv =
  new admin_container esys ptype sockserv
