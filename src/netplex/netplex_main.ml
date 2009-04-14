(* $Id$ *)

open Netplex_types
open Printf

type cmdline_config =
    { mutable config_filename : string;
      mutable pidfile : string option;
      mutable foreground : bool;
    }

let create ?(config_filename = "/etc/netplex.conf")
           ?(pidfile = None)
           ?(foreground = false) () =
  { config_filename = config_filename;
    pidfile = pidfile;
    foreground = foreground
  }


let args ?(defaults = create()) () =
  let config =
    (* copy of defaults: *)
    { defaults with foreground = defaults.foreground  } in

  let spec =
    [ "-conf",
      (Arg.String (fun s -> config.config_filename <- s)),
      "<file>  Read this configuration file";
      
      "-pid",
      (Arg.String (fun s -> config.pidfile <- Some s)),
      "<file>  Write this PID file";
      
      "-fg",
      (Arg.Unit (fun () -> config.foreground <- true)),
      "  Start in the foreground and do not run as daemon";
    ] in
  (spec, config)
;;


let config_filename cf = cf.config_filename

let pidfile cf = cf.pidfile

let foreground cf = cf.foreground

let daemon f arg =
  (* Double fork to avoid becoming a pg leader *)
  match Unix.fork() with
    | 0 ->
        ( match Unix.fork() with
            | 0 ->
                let _ = Unix.setsid() in (* Start new session/get rid of tty *)
                (* Assign stdin/stdout to /dev/null *)
                Unix.close Unix.stdin;
                ignore(Unix.openfile "/dev/null" [ Unix.O_RDONLY ] 0);
                Unix.close Unix.stdout;
                ignore(Unix.openfile "/dev/null" [ Unix.O_WRONLY ] 0);
                (* Keep stderr open: error messages should appear *)
                f arg
            | _ ->
                ()
        )
    | _ ->
        ()
;;


let rec run ctrl =
  try
    Unixqueue.run ctrl#event_system
  with
    | error ->
	ctrl # logger # log
	  ~component:"netplex.controller"
	  ~level:`Crit
	  ~message:("Uncaught exception: " ^ Netexn.to_string error);
	run ctrl
;;


let startup ?(late_initializer = fun _ _ -> ())
            ?(config_parser = Netplex_config.read_config_file)
            par c_logger_cf c_wrkmg_cf c_proc_cf cf =
  let config_file = config_parser cf.config_filename in
  
  let netplex_config =
    Netplex_config.read_netplex_config
      par#ptype
      c_logger_cf c_wrkmg_cf c_proc_cf 
      config_file in

  let maybe_daemonize =
    (if cf.foreground then
       (fun f arg -> f arg)
     else
       daemon) in
  maybe_daemonize
    (fun () ->
       Unix.chdir "/";
       let remove_pid_file =
	 match cf.pidfile with
	   | Some file ->
               let f = open_out file in
               fprintf f "%d\n" (Unix.getpid());
               close_out f;
               (fun () ->
		  try Sys.remove file with _ -> ())
	   | None ->
               (fun () -> ())
       in
       try
	 let controller_config = netplex_config # controller_config in
	 
	 let controller = 
	   Netplex_controller.create_controller par controller_config in

	 let processors =
	   List.map
	     (fun (sockserv_cfg, 
		   (procaddr, c_proc_cfg), 
		   (wrkmngaddr, c_wrkmng_cfg)
		  ) ->
		c_proc_cfg # create_processor
		  controller_config config_file procaddr)
	     netplex_config#services in
	 (* An exception while creating the processors will prevent the
          * startup of the whole system!
	  *)

	 let services =
	   List.map2
	     (fun (sockserv_cfg, 
		   (procaddr, c_proc_cfg), 
		   (wrkmngaddr, c_wrkmng_cfg)
		  ) 
  		  processor ->
		try
		  let wrkmng =
		    c_wrkmng_cfg # create_workload_manager
		      controller_config config_file wrkmngaddr in
		  let sockserv = 
		    Netplex_sockserv.create_socket_service processor sockserv_cfg in
		  Some (sockserv, wrkmng)
		with
		  | error ->
		      (* An error while creating the sockets is quite
                       * problematic. We do not add the service, but we cannot
                       * prevent the system startup at that late point in time
                       *)
		      controller # logger # log
			~component:"netplex.controller"
			~level:`Crit
			~message:("Uncaught exception preparing service " ^ 
				    sockserv_cfg#name ^ ": " ^ 
				    Netexn.to_string error);
		      None
	     )
	     netplex_config#services
	     processors in

	 List.iter
	   (function
	      | Some(sockserv,wrkmng) ->
		  ( try
		      controller # add_service sockserv wrkmng
		    with
		      | error ->
			  (* An error is very problematic now... *)
			  controller # logger # log
			    ~component:"netplex.controller"
			    ~level:`Crit
			    ~message:("Uncaught exception adding service " ^ 
					sockserv#name ^ ": " ^ 
					Netexn.to_string error);
		  )
	      | None ->
		  ()
	   )
	   services;

	 ( try
	     late_initializer config_file controller
	   with
	     | error ->
		 (* An error is ... *)
		 controller # logger # log
		   ~component:"netplex.controller"
		   ~level:`Crit
		   ~message:("Uncaught exception in late initialization: " ^ 
			       Netexn.to_string error);
	 );

	 run controller

       with
	 | error ->
             remove_pid_file();
             raise error
    )
    ()
;;
