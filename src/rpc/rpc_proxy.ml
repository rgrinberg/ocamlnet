(* $Id$ *)

module ReliabilityCache = struct

  type rcache_policy =
      [ `Independent
      | `Failing_port_disables_host of int
      | `Any_failing_port_disables_host
      | `None
      ]

  type rcache_config =
      { rcache_policy : rcache_policy;
	rcache_disable_timeout_min : float;
	rcache_disable_timeout_max : float;
	rcache_threshold : int;
	rcache_hook : rcache -> unit;
      }

  and entry =
      { mutable error_counter : int;
	mutable disabled_until : float option;
	mutable disable_timeout : float;
      }	

  and host_entry =
      { mutable host_disabled_until : float option;
	mutable host_socks : (Unix.sockaddr, unit) Hashtbl.t
      }	

  and rcache =
      { mutex : Netsys_oothr.mutex;
	config : rcache_config;
	ports : (Unix.sockaddr, entry) Hashtbl.t;
	hosts : (Unix.inet_addr, host_entry) Hashtbl.t
      }

  let create_rcache_config ?(policy = `None)
                           ?(disable_timeout_min = 1.0)
			   ?(disable_timeout_max = 64.0)
			   ?(threshold = 1)
			   ?(hook = fun _ -> ())
			   () =
    { rcache_policy = policy;
      rcache_disable_timeout_min = disable_timeout_min;
      rcache_disable_timeout_max = disable_timeout_max;
      rcache_threshold = threshold;
      rcache_hook = hook;
    }

  let create_rcache cfg =
    { mutex = !Netsys_oothr.provider # create_mutex ();
      config = cfg;
      ports = Hashtbl.create 10;
      hosts = Hashtbl.create 10
    }

  let rcache_config rc = rc.config

  let get_entry rc sa =
    try Hashtbl.find rc.ports sa
    with Not_found ->
      let new_e =
	{ error_counter = 0;
	  disabled_until = None;
	  disable_timeout = rc.config.rcache_disable_timeout_min
	} in
      Hashtbl.add rc.ports sa new_e;
      new_e

  let get_host_entry rc ip =
    try Hashtbl.find rc.hosts ip
    with Not_found ->
      let new_e =
	{ host_disabled_until = None;
	  host_socks = Hashtbl.create 1;
	} in
      Hashtbl.add rc.hosts ip new_e;
      new_e


  let ip_of_sa sa =
    match sa with
      | Unix.ADDR_INET(ip,_) -> ip
      | Unix.ADDR_UNIX _ -> Unix.inet_addr_loopback


  let local_ip ip =
    ip = Unix.inet_addr_loopback || ip = Unix.inet6_addr_loopback


  let incr_rcache_error_counter rc sa =
    Netsys_oothr.serialize rc.mutex
      (fun () ->
	 let e = get_entry rc sa in
	 e.error_counter <- e.error_counter + 1;
	 let now = Unix.gettimeofday() in
	 if e.error_counter >= rc.config.rcache_threshold then (
	   let disable_p =
	     rc.config.rcache_policy <> `None &&
	       match e.disabled_until with
		 | None -> true
		 | Some t -> t < now in
	   if disable_p then (
	     let until = now +. e.disable_timeout in
	     e.disabled_until <- 
	       Some until;
	     e.disable_timeout <- 
	       min 
	         (2. *. e.disable_timeout) 
	         rc.config.rcache_disable_timeout_max;
	     let ip = ip_of_sa sa in
	     let disable_host_p =
	       not(local_ip ip)
	       && (match rc.config.rcache_policy with
		     | `None -> assert false
		     | `Independent -> false
		     | `Failing_port_disables_host p -> 
			 ( match sa with
			     | Unix.ADDR_INET(_,p') -> p=p'
			     | _ -> false
			 )
		     | `Any_failing_port_disables_host -> true
		  ) in
	     if disable_host_p then (
	       let he = get_host_entry rc ip in
	       let t1 =
		 match he.host_disabled_until with
		   | None -> until
		   | Some t -> max t until in
	       he.host_disabled_until <- Some t1;
	       Hashtbl.replace he.host_socks sa ();
	     )
	   )
	 )
      )
      ()

  let reset_rcache_error_counter rc sa =
    Netsys_oothr.serialize rc.mutex
      (fun () ->
	 if Hashtbl.mem rc.ports sa then (
	   Hashtbl.remove rc.ports sa;
	   let ip = ip_of_sa sa in
	   if not(local_ip ip) then (
	     let now = Unix.gettimeofday() in
	     let he = get_host_entry rc ip in
	     Hashtbl.remove he.host_socks sa;
	     let t1_opt = 
	       Hashtbl.fold
		 (fun sa1 _ acc ->
		    let e = get_entry rc sa1 in
		    if e.error_counter >= rc.config.rcache_threshold then
		      match e.disabled_until with
			| None -> acc
			| Some t -> 
			    if t < now then None else (
			      match acc with
				| None -> Some t
				| Some t_acc -> Some(max t t_acc)
			    )
		    else
		      acc
		 )
		 he.host_socks
		 None in
	     match t1_opt with
	       | None ->
		   Hashtbl.remove rc.hosts ip
	       | Some t1 ->
		   he.host_disabled_until <- Some t1
	   )
	 )
      )
      ()

  let host_is_enabled_for rc ip now =
    try
      let he = Hashtbl.find rc.hosts ip in
      match he.host_disabled_until with
	| None -> true
	| Some t1 -> t1 < now
    with
      | Not_found -> true

  let host_is_enabled rc ip =
    rc.config.rcache_hook rc;
    Netsys_oothr.serialize rc.mutex
      (fun () ->
	 host_is_enabled_for rc ip (Unix.gettimeofday())
      )
      ()

  let sockaddr_is_enabled rc sa =
    rc.config.rcache_hook rc;
    Netsys_oothr.serialize rc.mutex
      (fun () ->
	 let now = Unix.gettimeofday() in
	 let ip = ip_of_sa sa in
	 host_is_enabled_for rc ip now
	 && ( try
		let e = Hashtbl.find rc.ports sa in
		match e.disabled_until with
		  | None -> true
		  | Some t1 -> t1 < now
	      with
		| Not_found -> true
	    )
      )
      ()

end



module ManagedClient = struct

  exception Service_unavailable

  type mclient_config =
      { mclient_rcache : ReliabilityCache.rcache;
	mclient_socket_config : Rpc_client.socket_config;
	mclient_idle_timeout : float;
	mclient_programs : Rpc_program.t list;
	mclient_msg_timeout : float;
	mclient_exception_handler : (exn -> unit) option;
	mclient_auth_methods : Rpc_client.auth_method list;
	mclient_initial_ping : bool;
      }

  type state = [ `Down | `Connecting | `Up of Unix.sockaddr option]

  type up_state =
      { client : Rpc_client.t;
	mutable idle_timer : Unixqueue.group option;
	mutable unavailable : bool;
      }

  type conn_state =
      { c_client : Rpc_client.t;
	mutable c_when_up : (up_state -> unit) list;
	mutable c_when_fail : (exn -> unit) list;
	mutable c_unavailable : bool;
      }

  type extended_state =
      [ `Down
      | `Connecting of conn_state
      | `Up of up_state
      ]

  type mclient =
      { config : mclient_config;
	conn : Rpc_client.connector;
	esys : Unixqueue.event_system;
	null_proc_name : string;
	mutable estate : extended_state;
	mutable next_batch_call : bool;
	mutable pending_calls : int;
      }
	
  type t = mclient
      (* for USE_CLIENT compatibility *)

  let default_rcache =
    ReliabilityCache.create_rcache
      (ReliabilityCache.create_rcache_config ())

  let get_null_proc_name config =
     if config.mclient_initial_ping then
	match Rpc_program.null_proc_name (List.hd config.mclient_programs) with
	  | None -> failwith "Rpc_proxy.ManagedClient.create_mclient_config: \
                              The program does not have a null procedure"
	  | Some n -> n 
      else
	""

  let create_mclient_config
        ?(rcache = default_rcache)
	?(socket_config = Rpc_client.default_socket_config)
	?(idle_timeout = (-1.0))
	?(programs = [])
	?(msg_timeout = (-1.0))
	?exception_handler
	?(auth_methods = [])
	?(initial_ping = false)
	() =
    if initial_ping && programs = [] then
      failwith
	"Rpc_proxy.ManagedClient.create_mclient_config: \
         need a program for initial ping";
    let config =
      { mclient_rcache = rcache;
	mclient_socket_config = socket_config;
	mclient_idle_timeout = idle_timeout;
	mclient_programs = programs;
	mclient_msg_timeout = msg_timeout;
	mclient_exception_handler = exception_handler;
	mclient_auth_methods = auth_methods;
	mclient_initial_ping = initial_ping;
      } in
    ignore(get_null_proc_name config);
    config


  let sockaddr_of_conn conn =
    match conn with
      | Rpc_client.Internet(ip,p) ->
	  Unix.ADDR_INET(ip,p)
      | Rpc_client.Unix p ->
	  Unix.ADDR_UNIX p
      | _ ->
	  failwith
	    "Rpc_proxy.ManagedClient.create_mclient: Unsupported connector"

  let create_mclient config conn esys =
    ignore(sockaddr_of_conn conn);
    let null_proc_name = get_null_proc_name config in
    { config = config;
      conn = conn;
      esys = esys;
      estate = `Down;
      next_batch_call = false;
      null_proc_name = null_proc_name;
      pending_calls = 0
    }
        
  let mclient_state mc =
    match mc.estate with
      | `Down -> `Down
      | `Connecting _ -> `Connecting
      | `Up up -> `Up (try Some(Rpc_client.get_socket_name up.client) 
		       with _ -> None)

  let pending_calls mc =
    mc.pending_calls

  let if_up mc f =
    match mc.estate with
      | `Down
      | `Connecting _ -> ()
      | `Up up -> f up

  let if_connecting_or_up mc f =
    match mc.estate with
      | `Down -> ()
      | `Connecting c -> f c.c_client
      | `Up up -> f up.client

  let cancel_idle_timer mc up =
    match up.idle_timer with
      | None -> ()
      | Some g -> Unixqueue.clear mc.esys g

  let shut_down mc =
    if_connecting_or_up mc
      (fun client -> Rpc_client.shut_down client);
    if_up mc
      (fun up -> cancel_idle_timer mc up);
    mc.estate <- `Down

  let sync_shutdown mc =
    if_connecting_or_up mc
      (fun client -> Rpc_client.sync_shutdown client);
    if_up mc
      (fun up -> cancel_idle_timer mc up);
    mc.estate <- `Down

  let trigger_shutdown mc f =
    if_connecting_or_up mc
      (fun client -> Rpc_client.trigger_shutdown client f);
    if_up mc
      (fun up -> cancel_idle_timer mc up);
    mc.estate <- `Down

  let enforce_unavailability mc =
    ( match mc.estate with
	| `Down -> ()
	| `Connecting c ->
	    c.c_unavailable <- true
	| `Up up ->
	    up.unavailable <- true
    );
    trigger_shutdown mc (fun () -> ())

  let set_batch_call mc =
    mc.next_batch_call <- true

  let use mc prog =
    let prog_id = Rpc_program.id prog in
    if not(List.exists 
	     (fun p -> Rpc_program.id p = prog_id) 
	     mc.config.mclient_programs) 
    then
      failwith "Rpc_proxy.ManagedClient.use: \
                This program is not bound by this client"

  let reconcile_state mc =
    if_connecting_or_up mc
      (fun client ->
	 if not(Rpc_client.is_up client) then
	   mc.estate <- `Down
      )

  let create_up mc client =
    { client = client;
      idle_timer = None;
      unavailable = false
    }

  let maybe_start_idle_timer mc =
    if_up mc
      (fun up ->
	 cancel_idle_timer mc up;
	 let tmo = mc.config.mclient_idle_timeout in
	 if tmo = 0.0 then (
	   (* Stop client immediately again! *)
	   Rpc_client.trigger_shutdown up.client (fun () -> ());
	   mc.estate <- `Down
	 )
	 else
	   if tmo > 0.0 then (
	     (* We start a weak timer *)
	     let g = Unixqueue.new_group mc.esys in
	     Unixqueue.weak_once mc.esys g tmo
	       (fun () ->
		  Rpc_client.trigger_shutdown up.client (fun () -> ());
		  mc.estate <- `Down
	       );
	     up.idle_timer <- Some g
	   )
      )


  let do_initial_ping mc client when_up when_fail =
    (* Arrange that the initial ping is sent. When it arrives,
       when_up will be called, and when_fail on error
     *)
    let prog = List.hd mc.config.mclient_programs in
    let cstate =
      { c_client = client;
	c_when_up = [when_up];
	c_when_fail = [when_fail];
	c_unavailable = false;
      } in
    ( try
	Rpc_client.unbound_async_call
	  client 
	  prog
	  mc.null_proc_name
	  Xdr.XV_void
	  (fun get_reply ->
	     try
	       let _ = get_reply() in   (* or exn *)
	       let g = Unixqueue.new_group mc.esys in
	       let up = create_up mc client in
	       mc.estate <- `Up up;
	       List.iter
		 (fun f_up -> 
		    Unixqueue.once mc.esys g 0.0 (fun () -> f_up up))
		 cstate.c_when_up
	     with error -> 
	       Rpc_client.trigger_shutdown client (fun () -> ());
	       let g = Unixqueue.new_group mc.esys in
	       mc.estate <- `Down;
	       let error =
		 if error = Rpc_client.Message_lost && cstate.c_unavailable then
		   Service_unavailable
		 else
		   error in
	       List.iter
		 (fun f_fail -> 
		    Unixqueue.once mc.esys g 0.0 (fun () -> f_fail error))
		 cstate.c_when_fail
	  )
      with
	| error ->
	    when_fail error
    );
    mc.estate <- `Connecting cstate

  let bring_up mc when_up when_fail =
    (* Check availability: *)
    let sa = sockaddr_of_conn mc.conn in
    if not (ReliabilityCache.sockaddr_is_enabled 
	      mc.config.mclient_rcache
	      sa) 
    then
      when_fail Service_unavailable
    else (
      match mc.estate with
	| `Down ->
	    (* Create a new client and initialize it *)
	    let mode2 = 
	      `Socket(Rpc.Tcp, mc.conn, mc.config.mclient_socket_config) in
	    let client = 
	      Rpc_client.unbound_create mode2 mc.esys in
	    (* The client remains unbound. We check program compatibility here
	     *)
	    Rpc_client.configure client 0 mc.config.mclient_msg_timeout;
	    ( match mc.config.mclient_exception_handler with
		| None -> ()
		| Some eh -> Rpc_client.set_exception_handler client eh
	    );
	    if mc.config.mclient_auth_methods <> [] then
	      Rpc_client.set_auth_methods client mc.config.mclient_auth_methods;
	    if mc.config.mclient_initial_ping then
	      do_initial_ping mc client when_up when_fail
	    else (
	      (* Easier: just claim that the client is up *)
	      let up = create_up mc client in
	      mc.estate <- `Up up;
	      when_up up
	    )

	| `Connecting c ->
	    (* We only have to arrange that when_up/when_fail is called *)
	    c.c_when_up <- when_up :: c.c_when_up;
	    c.c_when_fail <- when_fail :: c.c_when_fail
	      
	| `Up up ->
	    when_up up
    )

  let unbound_async_call mc prog procname param receiver =
    use mc prog;
    reconcile_state mc;
    let batch_flag = mc.next_batch_call in
    mc.next_batch_call <- false;
    bring_up mc
      (fun up ->
	 cancel_idle_timer mc up;
	 if batch_flag then
	   Rpc_client.set_batch_call up.client;
	 try
	   Rpc_client.unbound_async_call up.client prog procname param
	     (fun get_reply ->
		mc.pending_calls <- mc.pending_calls - 1;
		if mc.pending_calls = 0 then
		  maybe_start_idle_timer mc;
		receiver 
		  (fun () ->
		     try
		       get_reply()
		     with
		       | Rpc_client.Message_lost when up.unavailable ->
			   raise Service_unavailable
		  )
	     )
	 with
	   | error ->
	       mc.pending_calls <- mc.pending_calls - 1;
	       receiver (fun () -> raise error)
      )
      (fun error ->
	 mc.pending_calls <- mc.pending_calls - 1;
	 receiver (fun () -> raise error)
      );
    mc.pending_calls <- mc.pending_calls + 1


  let unbound_sync_call mc prog procname param =
    Rpc_client.synchronize
      mc.esys
      (unbound_async_call mc prog procname)
      param
end


(*
module ManagedSet = struct
  
end
 *)
