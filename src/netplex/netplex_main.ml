(* $Id$ *)

open Netplex_types
open Printf

type cmdline_config =
    { mutable config_filename : string;
      mutable pidfile : string option;
      mutable foreground : bool;
    }

let args () =
  let config =
    { config_filename = "/etc/netplex.conf";
      pidfile = None;
      foreground = false;
    } in

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
	  ~message:("Uncaught exception: " ^ Printexc.to_string error);
	run ctrl
;;


let startup ?(late_initializer = fun _ _ -> ())
            par c_logger_cf c_wrkmg_cf c_proc_cf cf =
  let config_file =
    Netplex_config.read_config_file cf.config_filename in
  
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
	 let ptype = netplex_config # ptype in
	 let controller_config = netplex_config # controller_config in
	 
	 let controller = 
	   Netplex_controller.create_controller par controller_config in

	 List.iter
	   (fun (sockserv_cfg, 
		 (procaddr, c_proc_cfg), 
		 (wrkmngaddr, c_wrkmng_cfg)
		) ->
	      let processor =
		c_proc_cfg # create_processor
		  controller_config config_file procaddr in
	      let wrkmng =
		c_wrkmng_cfg # create_workload_manager
		  controller_config config_file wrkmngaddr in
	      let sockserv = 
		Netplex_sockserv.create_socket_service processor sockserv_cfg in
	      controller # add_service sockserv wrkmng
	   )
	   netplex_config#services;

	 late_initializer config_file controller;

	 run controller

       with
	 | error ->
             remove_pid_file();
             raise error
    )
    ()
     
;;
