(* $Id$ *)

open Netplex_types
open Netplex_ctrl_aux

class std_container sockserv : container =
object(self)
  val esys = Unixqueue.create_unix_event_system()
  val mutable rpc = None
  val mutable nr_conns = 0
  val mutable engines = []

  method socket_service = sockserv

  method event_system = esys

  method start fd_clnt =
    if rpc <> None then
      failwith "#start: already started";
    rpc <-
      Some(Netplex_ctrl_clnt.Control.V1.create_client
	     ~esys
	     (Rpc_client.Descriptor fd_clnt)
	     Rpc.Tcp);
    self # protect "post_start_hook"
      (sockserv # post_start_hook) (self : #container :> container);
    self # setup_polling();
    self # protect "run" Unixqueue.run esys;
    self # protect "pre_finish_hook"
      (sockserv # pre_finish_hook) (self : #container :> container);
    rpc <- None

  method private protect : 's. string -> ('s -> unit) -> 's -> unit =
    fun label f arg ->
      try
	f arg 
      with
	| error ->
	    ( match rpc with
		| None -> ()  (* no way to report this error *)
		| Some r ->
		    self # log `Err (label ^ ": Exception " ^ 
				       Printexc.to_string error)
	    )

  method private setup_polling() =
    match rpc with
      | None -> assert false
      | Some r ->
	  Netplex_ctrl_clnt.Control.V1.poll'async r nr_conns
	    (fun getreply ->
	       let continue =
		 ( try
		     let reply = getreply() in
		     ( match reply with
			 | `event_none ->
			     true
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
			     self # protect
			       "receive_admin_message"
			       (sockserv # processor # receive_admin_message
				  (self : #container :> container)
				  msg.msg_name)
			       msg.msg_arguments;
			     true
			 | `event_shutdown ->
			     self # disable_accepting();
			     self # protect
			       "shutdown"
			       (sockserv # processor # shutdown)
			       ();
			     Rpc_client.shut_down r;
			     false
		     )
		   with
		     | error ->
			 self # log `Err ("poll: Exception " ^ 
					    Printexc.to_string error);
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
		let acc = new Uq_engines.direct_socket_acceptor fd esys in
		let e = acc # accept() in
		Uq_engines.when_state
		  ~is_done:(fun (fd_slave,_) ->
			      self # accepted fd_slave proto
			   )
		  ~is_error:(fun err ->
			       self # log `Err
				 ("accept: Exception " ^ 
				    Printexc.to_string err)
			    )
		  e;
		engines <- e :: engines
	     )
	     fd_array
	)
	sockserv#sockets
    )

  method private disable_accepting() =
    List.iter (fun e -> e # abort()) engines;
    engines <- [];

  method private accepted fd_slave proto =
    match rpc with
      | None -> assert false
      | Some r ->
	  self # disable_accepting();
	  Rpc_client.add_call
	    ~when_sent:(fun () ->
			  nr_conns <- nr_conns + 1;
			  self # protect
			    "process"
			    (sockserv # processor # process
			       ~when_done:(fun fd ->
					     nr_conns <- nr_conns - 1
					  )
			       (self : #container :> container)
			       fd_slave
			    )
			    proto;
			  self # setup_polling();
			  false
		       )
	    r
	    "accepted"
	    Xdr.XV_void
	    (fun _ -> ())

  method ctrl =
    match rpc with
      | None -> failwith "#ctrl: No RPC client available"
      | Some r -> r

  method shutdown() =
    self # disable_accepting();
    match rpc with
      | None -> ()
      | Some r -> Rpc_client.shut_down r

  method log level message =
    match rpc with
      | None -> ()
      | Some r ->
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
	  Rpc_client.add_call
	    ~when_sent:(fun () -> false)
	    r
	    "log"
	    (_of_Control'V1'log'arg(lev,message))
	    (fun _ -> ())

end


let create_container sockserv =
  new std_container sockserv

