(* $Id$ *)

open Netplex_types
open Netplex_ctrl_aux


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
    | `Selected of ext_cont_state
    | `Notified of ext_cont_state
    | `Deselected of ext_cont_state
    ]


class std_socket_controller rm_service (par: parallelizer) 
                            controller sockserv wrkmng
      : extended_socket_controller =
object(self)
  val mutable state = (`Disabled : socket_state)
  val mutable clist = []
  val mutable action = (`None : action)

  method state = state

  method container_state =
    List.map (fun c -> ( (c.container :> container_id), c.cont_state)) clist

  method enable() =
    match state with
      | `Disabled ->
	  state <- `Enabled;
	  self # schedule()
      | `Enabled ->
	  ()
      | `Restarting true ->
	  ()
      | `Restarting false ->
	  state <- `Restarting true
      | _ ->
	  failwith "#enable: service is already down"

  method disable() =
    match state with
      | `Disabled ->
	  ()
      | `Enabled ->
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
	  state <- `Restarting false
      | `Restarting false ->
	  ()
      | `Down ->
	  ()

  method restart() =
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
    (* TODO: Close socket/remove socket file *)
prerr_endline "DOWN";
    state <- `Down;
    self # stop_all_containers()

  method start_containers n =
    for k = 1 to n do
      let (fd_clnt, fd_srv) = 
	Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
      let fd_list =
	fd_clnt ::
	  (List.flatten
	     (List.map
		(fun (_, fd_arr) -> Array.to_list fd_arr)
		sockserv # sockets
	     )
	  ) in
      let container = sockserv # create_container sockserv in
      (* CHECK: ptype *)
      sockserv # pre_start_hook 
	(controller :> controller)
	(container :> container_id);
      par # start_thread
	(fun () ->
	   ( try 
	       container # start fd_clnt
	     with 
	       | error ->
		   (* It is difficult to get this error written to a log file *)
		   ()
	   );
	   Unix.close fd_clnt  (* indicates successful termination *)
	)
	()
	fd_list;
      if par # ptype = `Multi_processing then 
	Unix.close fd_clnt;
      let rpc =
	Rpc_server.create2 
	  (`Socket_endpoint(Rpc.Tcp, fd_srv))
	  controller#event_system in
      let c =
	{ container = (container :> container_id);
	  cont_state = `Starting;
	  rpc = rpc;
	  poll_call = None;
	  messages = Queue.create();
	  admin_messages = Queue.create();
	  shutting_down = false;
	  t_accept = 0.0;
	} in
      self # bind_server rpc c;
      Rpc_server.set_onclose_action rpc 
	(fun _ ->
	   (* Called back when fd_clnt is closed by the container *)
	   clist <- List.filter (fun c' -> c' != c) clist;
	   sockserv # post_finish_hook 
	     (controller :> controller)
	     (container :> container_id);
	   wrkmng # adjust 
	     sockserv (self : #socket_controller :> socket_controller);
	   let reschedule =
	     match action with
	       | `Selected c' when c == c' -> true
	       | `Notified c' when c == c' -> true
	       | `Deselected c' when c == c' -> true
	       | _ -> false in
	   if reschedule then (
	     action <- `None;
	     self # schedule()
	   );
	   if clist = [] then (
	     match state with
	       | `Restarting flag ->
		   state <- `Disabled;
		   if flag then self # enable()
	       | `Down ->
		   rm_service 
		     (self : #extended_socket_controller
		      :> extended_socket_controller);
	       | _ ->
		   ()
	   )
	);
      clist <- c :: clist
    done

  method stop_containers l =
    List.iter
      (fun c ->
	 if List.mem (c.container :> container_id) l then (
	   c.shutting_down <- true;
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
	 c.shutting_down <- true;
	 self # check_for_poll_reply c
      )
      clist
    

  method private schedule() =
    (* Determine the next container that will have the chance to accept a 
     * connection
     *)
    if state = `Enabled && action = `None then (
      if clist = [] then
	wrkmng # adjust 
	  sockserv (self : #socket_controller :> socket_controller);
      let best = ref None in
      List.iter
	(fun c ->
	   match c.cont_state with
	     | `Busy -> ()  (* ignore *)
	     | `Starting -> ()  (* ignore *)
	     | `Accepting(n, t_last) ->
		 ( match !best with
		     | None -> best := Some c
		     | Some c' ->
			 ( match c'.cont_state with
			     | `Accepting(n', t_last') ->
				 if n < n' || (n = n' && t_last < t_last') then
				   best := Some c
			     | _ -> assert false
			 )
		 )
	)
	clist;
      ( match !best with
	  | None -> 
	      ()   (* All containers are busy! *)
	  | Some c ->
prerr_endline "scheduled";
	      action <- `Selected c;
	      self # check_for_poll_reply c
      )
    )

  method private bind_server rpc c =
    Netplex_ctrl_srv.Control.V1.bind_async
      ~proc_ping:(fun _ _ reply -> reply())
      ~proc_lookup:(self # lookup c)
      ~proc_send_message:(self # send_message c)
      ~proc_poll:(self # poll c)
      ~proc_accepted:(self # accepted c)
      ~proc_log:(self # log c)
      rpc


  method private poll c sess n reply =
    (* Last [poll] call still unreplied? If so, send EVENT_NONE: *)
    ( match c.poll_call with
	| None -> ()
	| Some (last_sess, last_reply) ->
	    last_reply `event_none
    );
    c.poll_call <- Some (sess, reply);
    c.cont_state <- `Accepting(n, c.t_accept);
    self # schedule();
    self # check_for_poll_reply c

  method private check_for_poll_reply c =
    match c.poll_call with
      | None -> ()
      | Some (sess, reply) ->
	  if not (Queue.is_empty c.messages) then (
	    let msg = Queue.take c.messages in
	    reply (`event_received_message msg);
	    c.poll_call <- None
	  )
	  else if not (Queue.is_empty c.admin_messages) then (
	    let msg = Queue.take c.admin_messages in
	    reply (`event_received_admin_message msg);
	    c.poll_call <- None
	  )
	  else if c.shutting_down then (
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
		    reply `event_accept;
		    c.poll_call <- None;
		    action <- `Notified c;
		    wrkmng # adjust 
		      sockserv (self : #socket_controller :> socket_controller)
		| `Deselected c' when c' == c ->
		    reply `event_noaccept;
		    c.poll_call <- None;
		    action <- `None;
		    self # schedule()
		| _ ->
		    ()
	    )
	      
  method private accepted c sess arg reply =
prerr_endline "got 'accepted'";
    match action with
      | `Notified c' when c' == c ->
	  c.t_accept <- Unix.gettimeofday();
	  c.cont_state <- `Busy;
	  action <- `None;
	  self # schedule()
      | _ -> ();
prerr_endline "'accepted' mismatch"
	  (* This call is not replied! *)

  method private lookup c sess (srv_name, proto_name) reply =
    let path = ref None in
    List.iter
      (fun (sockserv, _, _) ->
	 List.iter
	   (fun p ->
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

  method private log c sess (lev, message) reply =
    let level = 
      try List.assoc lev lev_trans 
      with Not_found -> `Emerg in
    controller # logger # log 
      ~component:sockserv#name
      ~level
      ~message
	  (* This call is not replied! *)

end


class deferring_logger =
object(self)
  val queue = Queue.create()

  method log ~component ~level ~message =
    Queue.push (component,level,message) queue

  method reopen() = ()

  method max_level : Netplex_log.level = `Debug

  method set_max_level (_ : Netplex_log.level) = ()

  method forward (l : Netplex_log.logger) =
    Queue.iter
      (fun (component,level,message) ->
	 l # log ~component ~level ~message
      )
      queue;
    Queue.clear queue
end


class admin_par : Netplex_types.parallelizer =
  (* A fake parallelizer used for the admin interface *)
object
  method ptype = `Multi_threading
    (* This is a lie! *)

  method init() = ()

  method start_thread : 't . ('t -> unit) -> 't -> 'x -> unit =
    fun f arg l ->
      f arg

end


class controller_processor controller : processor =
  let find_service name =
    let (sockserv, sockctrl, _) =
      List.find (fun (s,_,_) -> s#name = name) controller # ext_services in
    (sockserv, sockctrl) 
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
  method process ~when_done cont fd proto =
    let rpc =
      Rpc_server.create2 (`Socket_endpoint(Rpc.Tcp, fd)) cont#event_system in
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
			 let (_, ctrl) = find_service name in (* or Not_found *)
			 ctrl # enable()
		      ))
      ~proc_disable:(protect
		       (fun name -> 
			  let (_, ctrl) = find_service name in (* or Not_found *)
			  ctrl # disable()
		       ))
      ~proc_restart:(protect 
		       (fun name ->
			  let (_, ctrl) = find_service name in (* or Not_found *)
			  ctrl # restart()
		       ))
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
					     ctrl # forward_message msg
					 | None -> ()
				    )
				    controller#ext_services
			       )
      rpc;
    Rpc_server.set_onclose_action rpc (fun _ -> when_done())

  method receive_message _ _ _ = ()

  method receive_admin_message _ _ _ = ()

  method shutdown() = ()

end ;;


let try_mkdir f =
  try
    Unix.mkdir f 0o777
  with
    | Unix.Unix_error(Unix.EEXIST,_,_) -> ()
;;


class controller_sockserv controller : socket_service =
  let processor = new controller_processor controller in
  let dir = controller#controller_config#socket_directory in
  let dir' = Filename.concat dir "netplex.controller" in
  let socket_name = Filename.concat dir' "admin" in
  let () = try_mkdir dir in
  let () = try_mkdir dir' in
  let config : socket_service_config = 
    ( object
	method name = "netplex.controller"
	method supported_ptypes = []
	method protocols =
	  [ object
	      method name = "admin"
	      method addresses = [| Unix.ADDR_UNIX socket_name |]
	      method lstn_backlog = 5
	      method lstn_reuseaddr = true
	      method configure_slave_socket _ = ()
	    end
	  ]
      end
    ) in
  let sockserv' = Netplex_sockserv.create_socket_service processor config in
object(self)
  method name = sockserv' # name
  method sockets = sockserv' # sockets
  method socket_service_config = sockserv' # socket_service_config
  method pre_start_hook _ _ = ()
  method post_start_hook _ = ()
  method pre_finish_hook _ = ()
  method post_finish_hook _ _ = ()
  method processor = processor
  method create_container s =
    Netplex_container.create_admin_container controller#event_system s
end


class std_controller (par : parallelizer) (config : controller_config) 
       : extended_controller =
  let dl = new deferring_logger in
object(self)
  val mutable logger = (dl :> Netplex_log.logger)
  val esys = Unixqueue.create_unix_event_system()
  val mutable services = []
  val mutable shutting_down = false

  initializer (
    par # init();
    let l = config # create_logger (self : #controller :> controller) in
    logger <- l;
    dl # forward l;
      (* Forward messages sent to the logger during [create_logger]. *)
    let my_sockserv = 
      new controller_sockserv 
	(self : #extended_controller :> extended_controller) in
    let my_wrkmng =
      Netplex_workload.create_constant_workload_manager 1 in
    (* Cannot use [add_service] because we must use the special parallelizer *)
    let my_sockctrl = 
      new std_socket_controller 
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
    (* TODO: Check supported_ptypes *)
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
    sockctrl # enable();

  method private rm_service sockctrl =
    services <- List.filter (fun (_, c, _) -> c <> sockctrl) services

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
      (fun (_, ctrl, _) ->
	 ctrl # shutdown()
      )
      services
end


let create_controller par config =
  (new std_controller par config :> controller)
