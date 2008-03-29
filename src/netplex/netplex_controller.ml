(* $Id$ *)

open Netplex_types
open Netplex_ctrl_aux
open Printf


let debug_scheduling = Netplex_log.debug_scheduling
let debug_containers = Netplex_log.debug_containers

let ast_re = Pcre.regexp "[*]";;

let regexp_of_pattern s =
  let l = Netstring_pcre.split_delim ast_re s in
  Netstring_pcre.regexp
    (String.concat ".*" (List.map (fun u -> Netstring_pcre.quote u) l) ^ "$")


class type extended_socket_controller =
object
  inherit socket_controller
  method forward_message : message -> unit
  method forward_admin_message : message -> unit
end

and extended_controller =
object
  inherit controller
  method ext_services : (socket_service * extended_socket_controller * workload_manager) list
end


type ext_cont_state =
    { container : container_id;
      mutable cont_state : container_state;
      mutable rpc : Rpc_server.t;
      mutable sys_rpc : Rpc_server.t;
      mutable par_thread : par_thread;
      mutable poll_call : (Rpc_server.session * 
			     (Netplex_ctrl_aux.t_Control'V1'poll'res -> unit)
			  ) option;
      mutable messages : message Queue.t;
      mutable admin_messages : message Queue.t;
      mutable shutting_down : bool;
      mutable t_accept : float;
    }


type action =
    [ `None
	(* No container exists/no notification in progress *)
    | `Selected of ext_cont_state
	(* The scheduler selected this container for the next [accept] *)
    | `Notified of ext_cont_state
	(* The container even knows it is selected for [accept] *)
    | `Deselected of ext_cont_state
	(* The container was notified and must be actively deselected
         * because of restart/shutdown
         *)
    ]


let cap_gt cap1 cap2 =
  (* cap1 has more capacity than cap2 *)
  match cap1 with
    | `Unavailable -> false
    | `Low_quality n1 ->
	( match cap2 with
	    | `Unavailable -> true
	    | `Low_quality n2 -> n1 > n2
	    | _ -> false
	)
    | `Normal_quality n1 ->
	( match cap2 with
	    | `Unavailable -> true
	    | `Low_quality _ -> true
	    | `Normal_quality n2 -> n1 > n2
	)
;;


let debug_log controller s =
  controller # logger # log 
    ~component:"netplex.controller"
    ~level:`Debug
    ~message:s

let debug_logf controller msgf =
  Printf.kprintf (debug_log controller) msgf


class std_socket_controller ?(no_disable = false)
                            rm_service (par: parallelizer) 
                            controller sockserv wrkmng
      : extended_socket_controller =
  let name = sockserv # name in
  let esys = controller # event_system in
object(self)
  val mutable state = (`Disabled : socket_state)
  val mutable clist = []
  val mutable action = (`None : action)
  val mutable n_failures = 0
    (* The number of processes in [`Starting] state that never started to
     * poll. Used to detect massive numbers of start-up failures.
     *)

  val mutable group = Unixqueue.new_group esys
    (* The group for timers etc. *)


  initializer (
    Unixqueue.once esys group 1.0 (fun () -> self#alive_check esys group)
  )


  method state = state

  method container_state =
    List.map 
      (fun c -> 
	 ( (c.container :> container_id), 
	   c.cont_state,
	   match action with
	     | `Selected c' when c' == c -> true
	     | `Notified c' when c' == c -> true
	     | `Deselected c' when c' == c -> true
	     | _ -> false
	 )
      ) 
      clist


  method private alive_check esys g =
    (* To be called every second or so. This is a "parachute" to prevent
     * problems caused by bugs in the workload manager.
     *)
    if state = `Enabled && action = `None then (
      try
	self # adjust();
	self # schedule();
      with
	| error ->
	    controller # logger # log 
	      ~component:"netplex.controller"
	      ~level:`Crit
	      ~message:("Exception in alive_check: " ^ Printexc.to_string error)
    );
    Unixqueue.once esys g 1.0 (fun () -> self#alive_check esys g)


  method enable() =
    match state with
      | `Disabled ->
	  if !debug_scheduling then
	    debug_logf controller "Service %s: Enabling" name;
	  n_failures <- 0;
	  state <- `Enabled;
	  self # schedule()
      | `Enabled ->
	  ()
      | `Restarting true ->
	  ()
      | `Restarting false ->
	  if !debug_scheduling then
	    debug_logf controller "Service %s: Will enable after restart is complete" name;
	  state <- `Restarting true
      | _ ->
	  failwith "#enable: service is already down"

  method disable() =
    if no_disable then
      failwith "#disable: not allowed for this service";
    match state with
      | `Disabled ->
	  ()
      | `Enabled ->
	  if !debug_scheduling then
	    debug_logf controller "Service %s: Disabling" name;
	  state <- `Disabled;
	  ( match action with
	      | `None
	      | `Selected _ ->
		  action <- `None
	      | `Notified c ->
		  action <- `Deselected c;
		  self # check_for_poll_reply c
	      | `Deselected _ ->
		  ()
	  )
      | `Restarting true ->
	  if !debug_scheduling then
	    debug_logf controller "Service %s: Will disable after restart is complete" name;
	  state <- `Restarting false
      | `Restarting false ->
	  ()
      | `Down ->
	  ()

  method restart() =
    if !debug_scheduling then
      debug_logf controller "Service %s: Restarting" name;
    let flag =
      match state with
	| `Disabled -> false
	| `Enabled -> true
	| `Restarting f -> f
	| `Down ->
	    failwith "#restart: service is already down" in
    state <- `Restarting flag;
    self # stop_all_containers()

  method shutdown() =
    (* We never close the master sockets or remove socket files. That would
     * make it impossible to restart the service later.
     *)
    if !debug_scheduling then
      debug_logf controller "Service %s: Shutdown" name;
    state <- `Down;
    Unixqueue.clear esys group;
    self # stop_all_containers();

  method start_containers n =
    if !debug_scheduling then
      debug_logf controller "Service %s: Starting %d new containers" name n;
    let threads = ref [] in
    for k = 1 to n do
      let (fd_clnt, fd_srv) = 
	Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let (sys_fd_clnt, sys_fd_srv) = 
	Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let fd_list =
	fd_clnt :: sys_fd_clnt ::
	  (List.flatten
	     (List.map
		(fun (_, fd_arr) -> Array.to_list fd_arr)
		sockserv # sockets
	     )
	  ) in
      let container = sockserv # create_container par#ptype sockserv in
      if !debug_containers then
	debug_logf controller "Service %s: Container %d: Starting (pre_start)"
	  name (Oo.id container);
      sockserv # processor # pre_start_hook 
	sockserv
	(controller :> controller)
	(container :> container_id);
      let par_thread =
	par # start_thread
	  (fun par_thread ->
	     Netplex_cenv.register_cont container par_thread;
	     ( try 
		 container # start fd_clnt sys_fd_clnt;
	       with 
		 | error ->
		     (* It is difficult to get this error written to a log file *)
		     prerr_endline ("Netplex Catastrophic Error: " ^ Printexc.to_string error);
		     ()
	     );
	     Netplex_cenv.unregister_cont container par_thread;
	     Unix.close fd_clnt;  (* indicates successful termination *)
	     Unix.close sys_fd_clnt 
	  )
	  fd_list
	  sockserv#name
	  controller#logger in
      if par # ptype = `Multi_processing then (
	Unix.close fd_clnt;
	Unix.close sys_fd_clnt;
      );
      threads := par_thread :: !threads;
      let rpc =
	Rpc_server.create2 
	  (`Socket_endpoint(Rpc.Tcp, fd_srv))
	  controller#event_system in
      Rpc_server.set_exception_handler rpc
	(fun err ->
	   controller # logger # log
	     ~component:sockserv#name
	     ~level:`Crit
	     ~message:("Control server caught exception: " ^ 
			 Printexc.to_string err));
      let sys_rpc =
	Rpc_server.create2 
	  (`Socket_endpoint(Rpc.Tcp, sys_fd_srv))
	  controller#event_system in
      Rpc_server.set_exception_handler rpc
	(fun err ->
	   controller # logger # log
	     ~component:sockserv#name
	     ~level:`Crit
	     ~message:("System server caught exception: " ^ 
			 Printexc.to_string err));
      let c =
	{ container = (container :> container_id);
	  cont_state = `Starting (Unix.gettimeofday());
	  rpc = rpc;
	  sys_rpc = sys_rpc;
	  par_thread = par_thread;
	  poll_call = None;
	  messages = Queue.create();
	  admin_messages = Queue.create();
	  shutting_down = false;
	  t_accept = 0.0;
	} in
      self # bind_server rpc sys_rpc c;
      clist <- c :: clist;
      Rpc_server.set_onclose_action rpc 
	(fun _ ->
	   par_thread # watch_shutdown controller#event_system;
	   self # onclose_action c container
	);
      (* Watch the new container. If it does not call [poll] within 60 seconds,
       * drop the process/thread.
       *)
      Unixqueue.once esys group 60.0
	(fun () ->
	   let is_starting =
	     match c.cont_state with `Starting _ -> true | _ -> false in
	   if List.memq c clist && is_starting then (
	     (* After 60 seconds still starting. This is a bad process! *)
	     controller # logger # log
	       ~component:sockserv#name
	       ~level:`Crit
	       ~message:"Container process/thread does not start up within 60 seconds";
	     (* Immediate measure: Remove it from the list of containers *)
	     clist <- List.filter (fun c' -> c' != c) clist;
	     (* [watch_shutdown] will kill the process if possible *)
	     par_thread # watch_shutdown controller#event_system;
	     
	     (* No need to call onclose_action. This _will_ be done if the
              * process is finally dead.
              *)
	   )
	);
      if !debug_scheduling then
	debug_logf controller "Service %s: Started %s" 
	  name 
	  (String.concat "," (List.map (fun p -> p#info_string) !threads));
    done


  method private onclose_action c container =
    (* Called back when fd_clnt is closed by the container, i.e. when
     * the container process terminates (normally/crashing)
     *)
    let is_starting =
      match c.cont_state with `Starting _ -> true | _ -> false in
    if is_starting then
      n_failures <- n_failures + 1;
    clist <- List.filter (fun c' -> c' != c) clist;
    if !debug_containers then
      debug_logf controller "Service %s: Container %d: Finishing (post_finish)"
	name (Oo.id container);
    ( try
	sockserv # processor # post_finish_hook 
	  sockserv
	  (controller :> controller)
	  (container :> container_id)
      with
	| error ->
	    controller # logger # log
	      ~component:sockserv#name
	      ~level:`Crit
	      ~message:("post_finish_hook: Exception " ^ 
			  Printexc.to_string error)
    );
    (* Maybe we have to start new containers: *)
    self # adjust();
    (* Maybe the dead container was selected for accepting connections.
     * In this case, reschedule:
     *)
    let reschedule =
      match action with
	| `Selected c' when c == c' -> true
	| `Notified c' when c == c' -> true
	| `Deselected c' when c == c' -> true
	| _ -> false in
    if reschedule then (
      action <- `None;
      self # schedule()
	(* Note: [schedule] is a no-op if the service is not enabled *)
    );
    (* Maybe we are restarting or shutting down. If this is the last
     * container of the service, continue this action:
     *)
    self # nocontainer_action()

  method private nocontainer_action() =
    if clist = [] then (
      match state with
	| `Restarting flag ->
	    (* Set to [`Disabled] and re-enable: *)
	    state <- `Disabled;
	    if flag then self # enable()
	| `Down ->
	    rm_service 
	      (self : #extended_socket_controller
	       :> extended_socket_controller);
	| _ ->
	    ()
    )
    

  method stop_containers l =
    List.iter
      (fun c ->
	 if List.mem (c.container :> container_id) l then (
	   if !debug_scheduling then
	     debug_logf controller "Service %s: Stopping container %s" 
	       name c.par_thread#info_string;
	   c.shutting_down <- true;
	   c.cont_state <- `Shutting_down;
	   ( match action with
	       | `Notified c' when c' == c ->
		   action <- `Deselected c
	       | _ -> ()
	   );
	   self # check_for_poll_reply c
	 )
      )
      clist

  method private stop_all_containers () =
    action <- `None;
    List.iter
      (fun c ->
	 if !debug_scheduling then
	   debug_logf controller "Service %s: Stopping container %s" 
	     name c.par_thread#info_string;
	 c.shutting_down <- true;
	 c.cont_state <- `Shutting_down;
	 self # check_for_poll_reply c
      )
      clist;
    (* Maybe clist is already empty... *)
    self # nocontainer_action()


  method private adjust() =
    if n_failures >= 10 then (
      controller # logger # log
	~component:sockserv#name
	~level:`Alert
	~message:("Disabling service after 10 startup failures");
      state <- `Disabled;
    )
    else (
      try
	if !debug_scheduling then
	  debug_logf controller "Service %s: Adjusting workload" name;
	wrkmng # adjust 
	  sockserv (self : #socket_controller :> socket_controller)
      with
	| error ->
	    controller # logger # log
	      ~component:sockserv#name
	      ~level:`Crit
	      ~message:("Exception in workload manager, function adjust: " ^ 
			  Printexc.to_string error)
    )

  method private schedule() =
    (* Determine the next container that will have the chance to accept a 
     * connection
     *)
    if state = `Enabled && action = `None then (
      if clist = [] then
	self # adjust();
      let best = ref (None, `Unavailable) in
      let now = Unix.gettimeofday() in
      let have_young_starters = ref false in
      List.iter
	(fun c ->
	   match c.cont_state with
	     | `Busy -> ()  (* ignore *)
	     | `Starting t -> 
		 if now -. t < 1.0 then have_young_starters := true
	     | `Shutting_down -> ()  (* ignore *)
	     | `Accepting(n, t_last) ->
		 let cap = 
		   wrkmng # capacity 
		     (c.container :> container_id) c.cont_state in
		 if cap <> `Unavailable then (
		   match !best with
		     | None, _ -> 
			 best := (Some c, cap)
		     | Some c', cap' ->
			 if cap_gt cap cap' then
			   best := (Some c, cap)
		 )
	)
	clist;
      ( match !best with
	  | None, _ -> 
	      if !debug_scheduling then
		debug_logf controller "Service %s: All containers busy" name;
	      ()   (* All containers are busy! *)
	  | Some c, best_cap ->
	      (* If there are starting processes that are younger than 1 sec,
               * and the best container is already overloaded, we do not
               * select any container. This choice would be very bad, and
               * we do not have logic to correct it once the starting processes
               * are ready. So defer scheduling for a small period of time.
               *)
	      let bad_best_cap =
		match best_cap with `Low_quality _ -> true | _ -> false in

	      if !have_young_starters && bad_best_cap then (
		if !debug_scheduling then
		  debug_logf controller "Service %s: Not selecting any container because of temporary overload"
		    name;
	      )
	      else (
		if !debug_scheduling then
		  debug_logf controller "Service %s: Selecting %s (bad=%b)"
		    name c.par_thread#info_string bad_best_cap;
		action <- `Selected c;
		self # check_for_poll_reply c
	      )
      )
    )

  method private bind_server rpc sys_rpc c =
    Netplex_ctrl_srv.Control.V1.bind_async
      ~proc_ping:(fun _ _ reply -> reply())
      ~proc_poll:(self # poll c)
      ~proc_accepted:(self # accepted c)
      rpc;
    Netplex_ctrl_srv.System.V1.bind_async
      ~proc_ping:(fun _ _ reply -> reply())
      ~proc_lookup:(self # lookup c)
      ~proc_send_message:(self # send_message c)
      ~proc_log:(self # log c)
      sys_rpc

  method private poll c sess n reply =
    (* Last [poll] call still unreplied? If so, send EVENT_NONE: *)
    ( match c.poll_call with
	| None -> ()
	| Some (last_sess, last_reply) ->
	    if !debug_scheduling then
	      debug_logf controller "Service %s: %s <- Event_none"
		name c.par_thread#info_string;
	    last_reply `event_none
    );

    if !debug_scheduling then
      debug_logf controller "Service %s: %s -> poll(%d)"
	name c.par_thread#info_string n;

    let is_starting =
      match c.cont_state with `Starting _ -> true | _ -> false in
    c.poll_call <- Some (sess, reply);
    if is_starting then
      n_failures <- 0;
    if c.cont_state <> `Shutting_down then (
      (* If n is updated, we must call [adjust] asap. Before [schedule]! *)
      let old_state = c.cont_state in
      c.cont_state <- `Accepting(n, c.t_accept);
      ( match old_state with
	  | `Accepting(n_old, _) ->
	      if n_old <> n then self # adjust()
	  | _ ->
	      self # adjust()
      );
    );
    self # schedule();
    self # check_for_poll_reply c

  method private check_for_poll_reply c =
    match c.poll_call with
      | None -> ()
      | Some (sess, reply) ->
	  if not (Queue.is_empty c.messages) then (
	    let msg = Queue.take c.messages in
	    if !debug_scheduling then
	      debug_logf controller "Service %s: %s <- Event_received_message"
		name c.par_thread#info_string;
	    reply (`event_received_message msg);
	    c.poll_call <- None
	  )
	  else if not (Queue.is_empty c.admin_messages) then (
	    let msg = Queue.take c.admin_messages in
	    if !debug_scheduling then
	      debug_logf controller "Service %s: %s <- Event_received_admin_message"
		name c.par_thread#info_string;
	    reply (`event_received_admin_message msg);
	    c.poll_call <- None
	  )
	  else if c.shutting_down then (
	    if !debug_scheduling then
	      debug_logf controller "Service %s: %s <- Event_shutdown"
		name c.par_thread#info_string;
	    reply `event_shutdown;
	    c.poll_call <- None;
	    ( match action with
		| `Deselected c' when c' == c ->
		    action <- `None;
		    self # schedule()
		      (* Note: we have here a race condition. I think
                       * it is harmless, however:
                       * It may happen that c and the newly scheduled
                       * container accept connections in parallel.
                       *)
		| _ -> ()
	    )
	  )
	  else 
	    ( match action with
		| `Selected c' when c' == c ->
		    if !debug_scheduling then
		      debug_logf controller "Service %s: %s <- Event_accept"
			name c.par_thread#info_string;
		    reply `event_accept;
		    c.poll_call <- None;
		    action <- `Notified c;
		    self # adjust();
		    (* PROBLEM: This adjust call is bogus because the 
                     * number of connections is not yet updated.
                     *)
		| `Deselected c' when c' == c ->
		    if !debug_scheduling then
		      debug_logf controller "Service %s: %s <- Event_noaccept"
			name c.par_thread#info_string;
		    reply `event_noaccept;
		    c.poll_call <- None;
		    action <- `None;
		    self # schedule()
		| _ ->
		    ()
	    )
	      
  method private accepted c sess arg reply =
    if !debug_scheduling then
      debug_logf controller "Service %s: %s -> accepted() (won't be replied)"
	name c.par_thread#info_string;
    match action with
      | `Notified c' when c' == c ->
	  c.t_accept <- Unix.gettimeofday();
	  if c.cont_state <> `Shutting_down then
	    c.cont_state <- `Busy;
	  action <- `None;
	  (* We call [adjust] here even although this can make workload
           * management much harder, because many containers are only
           * `Busy for a short time. However, it would be possible that 
           * required containers are not started if we did not do it.
           *)
	  self # adjust();
	  self # schedule()

      | _ -> ();
	  (* This call is not replied! *)

  method private lookup c sess (srv_name, proto_name) reply =
    let path = ref None in
    List.iter
      (fun (sockserv, _, _) ->
	 if sockserv#name = srv_name then
	   List.iter
	     (fun p ->
		if p#name = proto_name && !path = None then
		  Array.iter
		    (fun addr ->
		       match addr with
			 | Unix.ADDR_UNIX p -> 
			     path := Some p
			 | _ -> ()
		    )
		    p # addresses
	     )
	     sockserv # socket_service_config # protocols
      )
      controller#services;
    reply !path

  method private send_message c sess (pat, msg) reply =
    let re = regexp_of_pattern pat in
    List.iter
      (fun (sockserv, ctrl, _) ->
	 match Netstring_pcre.string_match re sockserv#name 0 with
	   | Some _ ->
	       ctrl # forward_message msg
	   | None -> ()
      )
      controller#ext_services;


  method forward_message msg =
    List.iter
      (fun c ->
	 Queue.push msg c.messages;
	 self # check_for_poll_reply c
      )
      clist

  method forward_admin_message msg =
    match msg.msg_name with
      | "netplex.threadlist" ->
	  self # threadlist()
      | "netplex.logger.set_max_level"
	  when sockserv#name = "netplex.controller" ->
	  ( try
	      let s_level = 
		match msg.msg_arguments with
		  | [| s |] -> s
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      let level = Netplex_log.level_of_string s_level in
	      controller # logger # set_max_level level
	    with
	      | Failure s ->
		  controller # logger # log 
		    ~component:sockserv#name
		    ~level:`Err
		    ~message:("netplex.logger.set_max_level: " ^ s)
	  )
      | "netplex.debug_scheduling"
	  when sockserv#name = "netplex.controller" ->
	  ( try
	      let s_switch = 
		match msg.msg_arguments with
		  | [| s |] -> s
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      let switch = bool_of_string s_switch in
	      Netplex_log.debug_scheduling := switch
	    with
	      | Failure s ->
		  controller # logger # log 
		    ~component:sockserv#name
		    ~level:`Err
		    ~message:("netplex.debug_scheduling: " ^ s)
	  )
      | "netplex.debug_containers"
	  when sockserv#name = "netplex.controller" ->
	  ( try
	      let s_switch = 
		match msg.msg_arguments with
		  | [| s |] -> s
		  | [| |] -> failwith "Missing argument"
		  | _  -> failwith "Too many arguments" in
	      let switch = bool_of_string s_switch in
	      Netplex_log.debug_containers := switch
	    with
	      | Failure s ->
		  controller # logger # log 
		    ~component:sockserv#name
		    ~level:`Err
		    ~message:("netplex.debug_containers: " ^ s)
	  )

      | _ ->
	  List.iter
	    (fun c ->
	       Queue.push msg c.admin_messages;
	       self # check_for_poll_reply c
	    )
	    clist

  val lev_trans =
    [ log_emerg, `Emerg;
      log_alert, `Alert;
      log_crit, `Crit;
      log_err, `Err;
      log_warning, `Warning;
      log_notice, `Notice;
      log_info, `Info;
      log_debug, `Debug
    ]

  method private log c sess (lev, subchannel, message) reply =
    let level = 
      try List.assoc lev lev_trans 
      with Not_found -> `Emerg in
    controller # logger # log_subch
      ~component:sockserv#name
      ~subchannel
      ~level
      ~message
	  (* This call is not replied! *)

  method private threadlist() =
    List.iter
      (fun c ->
	 let msg = 
	   sprintf "%20s: %s (%s)%s" 
	     c.par_thread#info_string sockserv#name
	     ( match c.cont_state with
		 | `Accepting(n,_) -> string_of_int n ^ " jobs, accepting"
		 | `Busy -> "busy"
		 | `Starting _ -> "starting"
		 | `Shutting_down -> "shutdown"
	     )
	     ( match action with
		 | `Selected c' when c' == c -> " (selected)"
		 | `Notified c' when c' == c -> " (selected*)"
		 | `Deselected c' when c' == c -> " (deselected)"
		 | _ -> ""
	     ) in
	 controller # logger # log
	   ~component:"netplex.controller"
	   ~level:`Notice
	   ~message:msg
      )
      clist

end


class deferring_logger =
object(self)
  val queue = Queue.create()

  method log_subch ~component ~subchannel ~level ~message =
    Queue.push (component,subchannel,level,message) queue

  method log = self # log_subch ~subchannel:""

  method reopen() = ()

  method max_level : Netplex_types.level = `Debug

  method set_max_level (_ : Netplex_types.level) = ()

  method forward (l : logger) =
    Queue.iter
      (fun (component,subchannel,level,message) ->
	 l # log_subch ~component ~subchannel ~level ~message
      )
      queue;
    Queue.clear queue
end


class admin_par : Netplex_types.parallelizer =
  (* A special parallelizer used for the admin interface *)
object(self)
  method ptype = `Controller_attached

  method init() = ()

  method start_thread : (par_thread -> unit) -> 'x -> string -> logger -> par_thread =
    fun f l srv_name logger ->
      let pid = Unix.getpid() in
      let throbj =
	( object
	    method ptype = `Controller_attached
	    method info_string = "AttachedToCtrlProcess " ^ string_of_int pid
	    method sys_id = assert false
	    method parallelizer = (self : #parallelizer :> parallelizer)
	    method watch_shutdown _ = ()
	  end
	) in
      f throbj;
      throbj

  method current_sys_id = assert false

  method create_mem_mutex () = assert false

end


class controller_processor setup controller : processor =
  let find_services name =
    List.map
      (fun (sockserv, sockctrl, _) -> (sockserv, sockctrl))
      (List.filter
	 (fun (s,_,_) -> s#name = name) 
	 controller # ext_services)
  in
  let protect f arg =
    try
      f arg;
      `code_ok
    with
      | error ->
	  `code_error (Printexc.to_string error)
  in
object(self)
  inherit Netplex_kit.empty_processor_hooks()

  method supported_ptypes = [ `Controller_attached ]

  method process ~when_done cont fd proto =
    let rpc =
      Rpc_server.create2 (`Socket_endpoint(Rpc.Tcp, fd)) cont#event_system in
    Rpc_server.set_exception_handler rpc
      (fun err ->
	 controller # logger # log
	   ~component:"netplex.controller"
	   ~level:`Crit
	   ~message:("Admin server caught exception: " ^ 
		       Printexc.to_string err));
    Netplex_ctrl_srv.Admin.V1.bind
      ~proc_ping:(fun () -> ())
      ~proc_list:(fun () ->
		    Array.map
		      (fun (sockserv,sockctrl,_) ->
			 { srv_name = sockserv#name;
			   srv_protocols =
			     Array.map
			       (fun (proto, fdlist) ->
				  { prot_name = proto;
				    prot_ports =
				      Array.map
					(fun fd ->
					   try
					     let name = Unix.getsockname fd in
					     let domain = 
					       Unix.domain_of_sockaddr name in
					     match name, domain with
					       | Unix.ADDR_UNIX path, _ ->
						   `pf_unix path
					       | Unix.ADDR_INET(addr,port),
						   Unix.PF_INET
						   ->
						   `pf_inet
						     { inet_addr =
							 Unix.string_of_inet_addr
							   addr;
						       inet_port = port
						     }
					       | Unix.ADDR_INET(addr,port),
						     Unix.PF_INET6
						     ->
						   `pf_inet6
						     { inet6_addr =
							 Unix.string_of_inet_addr
							   addr;
						       inet6_port = port
						     }
					       | _ -> `pf_unknown
					   with
					     | _ -> `pf_unknown
					)
					fdlist
				  }
			       )
			       (Array.of_list sockserv#sockets);
			   srv_nr_containers =
			     List.length (sockctrl # container_state);
			   srv_state = 
			     ( match sockctrl # state with
				 | `Enabled -> state_enabled
				 | `Disabled -> state_disabled
				 | `Restarting _ -> state_restarting
				 | `Down -> state_down
			     )
			 }
		      )
		      (Array.of_list controller#services)
		 )
      ~proc_enable:(protect 
		      (fun name -> 
			 List.iter
			   (fun (_, ctrl) -> ctrl # enable())
			   (find_services name))
		   )
      ~proc_disable:(protect
		       (fun name -> 
			 List.iter
			   (fun (_, ctrl) -> ctrl # disable())
			   (find_services name))
		    )
      ~proc_restart:(protect 
		       (fun name ->
			  List.iter
			    (fun (_, ctrl) -> ctrl # restart())
			    (find_services name))
		    )
      ~proc_restart_all:(protect (fun () ->
				    controller # restart()))
      ~proc_shutdown:(protect (fun () ->
				 controller # shutdown()))
      ~proc_reopen_logfiles:(protect (fun () ->
					controller # logger # reopen()))
      ~proc_send_admin_message:(fun (pat, msg) ->
				  let re = regexp_of_pattern pat in
				  List.iter
				    (fun (sockserv, ctrl, _) ->
				       match
					 Netstring_pcre.string_match 
					   re sockserv#name 0
				       with
					 | Some _ ->
					     ctrl # forward_admin_message msg
					 | None -> ()
				    )
				    controller#ext_services
			       )
      rpc;
    Rpc_server.set_onclose_action rpc (fun _ -> 
					 when_done();
				      );
    setup rpc

  method global_exception_handler exn = true

end ;;


let try_mkdir f =
  try
    Unix.mkdir f 0o777
  with
    | Unix.Unix_error(Unix.EEXIST,_,_) -> ()
;;


class controller_sockserv setup controller : socket_service =
  let processor = new controller_processor setup controller in
  let dir = controller#controller_config#socket_directory in
  let dir' = Filename.concat dir "netplex.controller" in
  let socket_name = Filename.concat dir' "admin" in
  let () = try_mkdir dir in
  let () = try_mkdir dir' in
  let config : socket_service_config = 
    ( object
	method name = "netplex.controller"
	method protocols =
	  [ object
	      method name = "admin"
	      method addresses = [| Unix.ADDR_UNIX socket_name |]
	      method lstn_backlog = 50
	      method lstn_reuseaddr = true
	      method so_keepalive = true
	      method configure_slave_socket _ = ()
	    end
	  ]
	method change_user_to = None
      end
    ) in
  let sockserv' = Netplex_sockserv.create_socket_service processor config in
object(self)
  method name = sockserv' # name
  method sockets = sockserv' # sockets
  method socket_service_config = sockserv' # socket_service_config
  method processor = processor
  method create_container p s =
    Netplex_container.create_admin_container controller#event_system p s
end


class std_controller (par : parallelizer) (config : controller_config) 
       : extended_controller =
  let dl = new deferring_logger in
object(self)
  val mutable logger = (dl :> logger)
  val esys = Unixqueue.create_unix_event_system()
  val mutable services = []
  val mutable shutting_down = false
  val mutable admin_setups = []

  initializer (
    par # init();
    let l = config # create_logger (self : #controller :> controller) in
    logger <- l;
    dl # forward l;
      (* Forward messages sent to the logger during [create_logger]. *)
    let my_sockserv = 
      new controller_sockserv 
	(fun rpc ->
	   List.iter (fun f -> f rpc) admin_setups
	)
	(self : #extended_controller :> extended_controller) in
    let my_wrkmng =
      Netplex_workload.create_constant_workload_manager 1 in
    (* Cannot use [add_service] because we must use the special parallelizer *)
    let my_sockctrl = 
      new std_socket_controller 
	~no_disable:true
	self#rm_service
	(new admin_par)
	(self : #extended_controller :> extended_controller)
	my_sockserv 
	my_wrkmng in
    services <- (my_sockserv, my_sockctrl, my_wrkmng) :: services;
    my_wrkmng # hello (self : #controller :> controller);
    my_sockctrl # enable();

  )

  method ptype = par # ptype

  method controller_config = config

  method services = 
    ( services 
      :> (socket_service * socket_controller * workload_manager) list )

  method ext_services =
    services

  method add_service sockserv wrkmng =
    if not (List.mem par#ptype sockserv#processor#supported_ptypes) then
      failwith "#add_service: the parallelization type is not supported";
    if shutting_down then
      failwith "#add_service: controller is shutting down";
    let sockctrl = 
      new std_socket_controller 
	self#rm_service
	par
	(self : #extended_controller :> extended_controller)
	sockserv 
	wrkmng in
    services <- (sockserv, sockctrl, wrkmng) :: services;
    wrkmng # hello (self : #controller :> controller);
    sockserv # processor # post_add_hook sockserv;
    sockctrl # enable();

  method add_admin setup =
    admin_setups <- setup :: admin_setups

  method private rm_service sockctrl =
    let sockserv = ref None in
    services <- 
      (List.filter 
         (fun (s, c, _) -> 
	    if c = sockctrl then (
	      sockserv := Some s;
	      false
	    )
	    else
	      true
	 ) 
         services);
    match !sockserv with
      | None -> ()   (* strange *)
      | Some s -> s # processor # post_rm_hook s

  method logger = logger

  method event_system = esys

  method restart() =
    if shutting_down then
      failwith "#restart: controller is shutting down";
    List.iter
      (fun (_, ctrl, _) ->
	 ctrl # restart()
      )
      services


  method shutdown() =
    shutting_down <- true;
    List.iter
      (fun (_, ctrl, wrkmng) ->
	 ctrl # shutdown();
	 wrkmng # shutdown();
      )
      services
end


let create_controller par config =
  (new std_controller par config :> controller)


let default_socket_directory = "/tmp/.netplex"

let default_create_logger _ = Netplex_log.channel_logger stderr

let extract_config (loggers : logger_factory list) (cf : config_file) =
  match cf # resolve_section cf#root_addr "controller" with
    | [] ->
	(* Create a default configuration: *)
	( object
	    method socket_directory = default_socket_directory
	    method create_logger = default_create_logger
	  end
	)
    | [ ctrladdr ] ->
	cf # restrict_subsections ctrladdr [ "logging" ];
	cf # restrict_parameters ctrladdr [ "socket_directory"; "max_level" ];
	let socket_directory =
	  try 
	    cf # string_param 
	      (cf # resolve_parameter ctrladdr "socket_directory")
	  with
	    | Not_found -> default_socket_directory in
	let create_logger ctrl =
	  ( match cf # resolve_section ctrladdr "logging" with
	      | [] ->
		  default_create_logger ctrl
	      | [ logaddr ] ->
		  let typ =
		    try 
		      cf # string_param
			(cf # resolve_parameter logaddr "type") 
		    with
		      | Not_found ->
			  failwith "Parameter 'type' in 'logging' section is missing" in
		  let logger =
		    try
		      List.find (fun l -> l#name = typ) loggers 
		    with
		      | Not_found ->
			  failwith ("Logging type not found: " ^ typ) in
		  logger # create_logger cf logaddr ctrl
	      | _ ->
		  failwith "More than one 'logging' section"
	  ) in
	let max_level_opt =
	  try
	    let s = 
	      cf # string_param
		(cf # resolve_parameter ctrladdr "max_level") in
	    if String.lowercase s = "all" then
	      Some `Debug
	    else
	      Some(Netplex_log.level_of_string s)
	  with
	    | Not_found -> None
	    | _ -> 
		failwith ("In section " ^ cf # print ctrladdr ^ ": Bad max_level parameter value")
	in
	( object
	    method socket_directory = socket_directory
	    method create_logger ctrl = 
	      let l = create_logger ctrl in
	      ( match max_level_opt with
		  | None -> ()
		  | Some max_level ->
		      l # set_max_level max_level
	      );
	      l
	  end
	)
    | _ ->
	failwith "More than one 'controller' section"
;;
