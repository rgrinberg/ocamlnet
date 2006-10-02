(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


open Netcgi_types
open Netcgi
open Netcgi_jserv
open Netcgi_env
open Netmime

module U = Unix

type request_handler =
    { req_activate : cgi_activation -> unit;
      req_processing : string -> mime_header -> argument_processing;
      req_operating_type : operating_type;
    }

type server_type =
    [ `Sequential   of (string * request_handler) list
    | `Forking      of int * ((string * request_handler) list)
    | `Process_pool of int * ((string * request_handler) list)
    ]

type protocol_type =
    [ `Ajp_1_2
    ]

type jserv_config =
    { js_backlog : int;
      js_reuseaddr : bool;
      js_cgiconfig : cgi_config;
      js_init_process : unit -> unit;
      js_fini_process : unit -> unit;    
      js_idle_worker : unit -> unit;
      js_idle_master : unit -> unit;
    }


let rec restart f arg =
  try f arg
  with
      U.Unix_error(U.EINTR,_,_) ->
	restart f arg
;;


let std_config =
  { js_backlog = 10;
    js_reuseaddr = true;
    js_cgiconfig = Netcgi_env.default_config;
    js_init_process = (fun () -> ());
    js_fini_process = (fun () -> ());
    js_idle_worker = (fun () -> ());
    js_idle_master = (fun () -> ());
  }


let get_allowed_hosts props =
  let l = 
    List.map snd ( List.filter (fun (n,v) -> n = "security.allowHost") props ) 
  in
  List.map
    (fun a ->
       try Unix.inet_addr_of_string a 
       with Failure _ ->
	 begin try
	   (Unix.gethostbyname a).Unix.h_addr_list.(0)
	 with
	     Not_found ->
	       failwith ("Host not found: " ^ a)
	 end
    )
    l
;;


let get_jk_servletSubString props =
  try
    Some (List.assoc "jakarta.servletSubString" props)
  with
      Not_found -> None
;;


let get_https props =
  try
    let https = List.assoc "ocamlnet.https" props in
    https = "true"
  with
      Not_found -> false
;;


let mk_rhtab rhlist =
  let rhtab = Hashtbl.create (List.length rhlist) in
  List.iter (fun (name,rh) -> Hashtbl.add rhtab name rh) rhlist;
  rhtab
;;


let log_error_message servlet message =
  Printf.eprintf "[%s] [error] [ocamlnet servlet] servlet=%s %s\n"
    (Netdate.format "%a %b %e %T %Y " (Netdate.create ~zone:Netdate.localzone
					 (U.time())))
    servlet
    message;
  flush stderr
;;


let logger = ref log_error_message ;;


let catch f arg =
  try f arg
  with
      error -> 
	!logger "?" ("Uncaught exception: " ^ Printexc.to_string error)
;;


let error_message env number name servlet message =
  if env # output_state = `Start then begin
    env # set_output_header_fields
      [ "Status", (string_of_int number ^ " " ^ name);
	"Content-type", "text/html"
      ];
    env # send_output_header();
  end
  else begin
    env # output_ch # output_string
      "</BODY></HTML>\n";
  end;
  env # output_ch # output_string
    ("<HTML><HEAD><TITLE>" ^ name ^ "</TITLE></HEAD>\n" ^
     "<BODY><H1>" ^ name ^ "</H1>\n<P>" ^ message ^ "\n</BODY></HTML>\n");
  env # output_ch # flush();
  !logger servlet message
;;

let service_unavailable outch =
  let number = 503 in
  let name = "Service Unavailable" in
  let message = 
    "This server is currently overloaded and refuses to process further requests. Please try again later." in
	       
  outch # output_string
    ("Status: " ^ (string_of_int number ^ " " ^ name) ^ 
     "\nContent-Type: text/html\n\n");
  outch # output_string
    ("<HTML><HEAD><TITLE>" ^ name ^ "</TITLE></HEAD>\n" ^
     "<BODY><H1>" ^ name ^ "</H1>\n<P>" ^ message ^ "\n</BODY></HTML>\n");
  outch # flush();
  !logger "?" message
;;


let do_request config rhtab jk_servletSubString https auth inch outch = 
  Netcgi_jserv_ajp12.serve_connection
    ~config:config.js_cgiconfig
    ?jk_servletSubString
    ~https
    (fun zone servlet env ->
       let servlet_s =
	 match servlet with
	     Some s -> s
	   | None -> ""
       in
       let handler =
	 try Some(Hashtbl.find rhtab servlet_s)
	 with Not_found -> None
       in
       match handler with
	   Some h ->
	     begin try
	       let cgi = new std_activation 
			   ~processing:h.req_processing
			   ~operating_type:h.req_operating_type
			   ~env () in
	       h.req_activate cgi
	     with
		 error ->
		   error_message env 500 "Server Error" servlet_s
		     ("Uncaught exception: " ^ Printexc.to_string error)
	     end
	 | None ->
	     (* Generate an error message *)
	     error_message env 404 "Not found" servlet_s "Netcgi: Servlet not found"
    )
    auth
    inch
    outch
;;


let run_sequential_server 
      ?(config = std_config) rhlist prttype props auth addr port =
  let rhtab = mk_rhtab rhlist in
  let t_w = ref 0.0 in

  let rec select_accept sel sock =
    let sel',_,_ = restart (Unix.select (sock::sel) [] []) 1.0 in
    let t = Unix.gettimeofday() in
    if t -. !t_w >= 1.0 then (
      catch config.js_idle_worker ();
      config.js_idle_master();
      t_w := t
    );
    if sel' = [] then
      select_accept sel sock
    else
      let sel'' = List.filter (fun fd -> fd <> sock) sel' in
      let slave =
	if List.mem sock sel' then
	  Some(fst(restart Unix.accept sock))
	else
	  None in
      (sel'', slave)
  in

  config.js_init_process();
  server
    ~backlog:config.js_backlog
    ~reuseaddr:config.js_reuseaddr
    ~allow_hosts:(get_allowed_hosts props)
    ~select_accept
    (fun srv -> do_request config rhtab 
                           (get_jk_servletSubString props)
                           (get_https props)
    )
    auth 
    addr
    port;
  config.js_fini_process()
;;


exception Continue;;

let run_forking_server 
      ?(config = std_config) maxload rhlist prttype props auth addr port =
  let rhtab = mk_rhtab rhlist in
  let processtab = Hashtbl.create 50 in
  let processnum = ref 0 in
  let wait_time = ref 0.0 in
  let jk_sSS = get_jk_servletSubString props in
  let https = get_https props in

  let wait_for_children() =
    let to_delete = ref [] in
    Hashtbl.iter
      (fun pid _ ->
	 let rpid, _ = U.waitpid [ U.WNOHANG ] pid in
	 if rpid = pid then
	   to_delete := pid :: !to_delete
      )
      processtab;
    List.iter
      (fun pid ->
	 Hashtbl.remove processtab pid;
	 decr processnum
      )
      !to_delete;
    config.js_idle_master();
    wait_time := U.time()
  in

  let rec select_accept dl master =
    try
      let dl',_,_ = U.select (master::dl) [] [] (1.0) in
      if U.gettimeofday() -. !wait_time > 1.0 then wait_for_children();
      if dl' = [] then raise Continue;
      let dl'' = List.filter (fun fd -> fd <> master) dl' in
      let slave =
	if List.mem master dl' then
	  Some(fst(restart U.accept master))
	else
	  None in
      (dl', slave)
    with
	U.Unix_error(U.EINTR,_,_) ->
	  wait_for_children();
	  select_accept dl master
      | Continue ->
	  select_accept dl master
  in

  server
    ~backlog:config.js_backlog
    ~reuseaddr:config.js_reuseaddr
    ~select_accept
    ~allow_hosts:(get_allowed_hosts props)
    (fun srv auth inch outch ->
       if !processnum >= maxload then begin
	 service_unavailable outch;
	 catch outch#close_out ();
	 catch inch#close_in ();
       end
       else
	 try
	   begin match U.fork() with
	       0 ->
		 begin try
		   config.js_init_process();
		   do_request config rhtab jk_sSS https auth inch outch;
		 with 
		     Signal_shutdown ->
		       catch signal_shutdown srv
		   | Signal_restart ->
		       catch signal_restart srv
		   | error ->
		       catch (fun _ -> raise error) ();
		 end;
		 catch config.js_fini_process ();
		 exit 0;
	     | n ->
		 Hashtbl.add processtab n ();
		 incr processnum;
		 catch outch#close_out ();
		 catch inch#close_in ();
	   end
	 with 
	     error ->
	       !logger "?" ("Uncaught exception: " ^ Printexc.to_string error)
    )
    auth 
    addr
    port;

  while !processnum > 0 do
    wait_for_children();
    U.sleep 1;
  done
;;

(**********************************************************************
 * `Process_pool
 *
 * The master process starts N subprocesses (N is a fixed number).
 * All subprocesses share the master socket that was returned by Unix.socket.
 * At every moment, only one of the subprocesses waits for new connections
 * arriving at the master socket - this is enforced by mutual exclusion.
 * (The same trick is used as in Apache httpd: Instead of calling
 * the [accept] syscall by every subprocess only one subprocess is selected
 * in advance that will get the next connection. This avoids that the kernel
 * wakes up all subprocesses, as it is done in many implementations of
 * [accept].)
 *
 * While the subprocesses accept the connections and call the request
 * handlers, the master process simply waits until all subprocesses 
 * terminate. (Termination is discussed later.)
 *
 * The processes communicate by a message board (that is simply a 
 * regular file), and by a control pipe.
 *
 * The message board consists of a number of bytes, and every byte
 * represents a message. There are currently only two defined byte
 * positions. Furthermore, the bytes can be locked (Unix.lockf).
 * The byte at position 0 contains the character 'a' when one of the
 * processes currently waits for a new connection, and '-' if no
 * process currently waits. This byte is written by the subprocess that
 * is selected to be waiting, but not interpreted by anybody. (So it is
 * only for informational purposes, e.g. the admin can check how busy
 * the jserv engine is.) The byte at position 0 is also used as mutual
 * exclusion lock for the right to wait for connections. The selected subprocess
 * has a write lock for this byte, and once it has it, the subprocess waits
 * for the next connection. When the connection is accepted, the lock is
 * released, and one of the other processes can get it.
 *
 * The byte at position 1 contains either the character 'n' or 's',
 * depending on the current mode. 'n' means normal operation. 's' means
 * that the server is currently shutting down. The value 's' is written
 * by the master process, and the subprocesses are checking byte 1 from
 * time to time. When a subprocess finds the 's' it closes all descriptors
 * and exits.
 *
 * Finally, there is the control pipe. The pipe is read by the master
 * process, and it can be written by all subprocesses. On the one hand,
 * this pipe transports the shutdown and restart notifications that
 * arrive at a subprocess from the subprocess to the master process (which
 * then sets the 's' character in byte 1 of the message board). On the 
 * other hand, this pipe is used to detect when all subprocesses have
 * terminated. In this case, the master process sees the EOF condition.
 *
 * There are three operation modes: startup, normal operation, shutdown.
 *
 * STARTUP:
 * - The master process creates the master socket and the control pipe
 * - The master process creates the message board and initializes it
 * - The master process forks N times to create the subprocesses
 * - The subprocesses can now start getting the exclusive lock on byte 0
 *   of the message board, and once they start doing so, they are in
 *   normal mode
 *
 * NORMAL OPERATION:
 * - The master process waits for notifications arriving on the control
 *   pipe (coming from one of the subprocesses). These messages are only
 *   meaningful when shutting down.
 * - The subprocesses try to get the exclusive lock on byte 0 of the
 *   message board. The subprocess that gets it, continues and waits for
 *   the next connection, and accepts it. When the connection is accepted
 *   the lock is released, so one of the other subprocesses gets the lock.
 *   The connection is served, and when everything is done, the subprocess
 *   attempts to get the lock again.
 * - At the same time, the subprocess that waits for arriving connections
 *   checks byte 1 of the message board from time to time, and when there
 *   is an 's', it is already in shutdown mode.
 *
 * SHUTDOWN:
 * - One of the subprocesses accepts a new connection, and this connection
 *   does not contain a user request, but the shutdown notification (from
 *   the web server). The subprocess now tells the other processes about
 *   this notification, and this works as follows.
 * - The subprocess sends the shutdown message over the control pipe to
 *   the master process. (After that, the subprocess can continue accepting
 *   connections.)
 * - The master process now sets byte 1 of the message board to 's'. This
 *   informs the subprocesses (one after another, see below), and the 
 *   master process waits until all subprocesses are terminated. The criterion
 *   is that the control pipe is at EOF because all potential writers have
 *   closed the pipe.
 * - First, only the subprocess that currently waits for connections sees
 *   the 's'. However, it releases the lock immediately, and terminates.
 *   The next subprocess gets the lock, sees the 's', releases the lock
 *   and terminates, and so on, until all subprocess have gracefully
 *   terminated themselves.
 * - Finally, the master process waits (in the sense of Unix.waitpid) until
 *   all subprocesses really terminate.
 *)


let make_message_board() =
  (* Create a unique file name, and return the pair of the name and the
   * open descriptor
   *)
  let tmp_directory = "/tmp" in
  let tmp_prefix = "netcgi-jserv-board" in
  let limit = 1000 in
  let rec try_creation n =
    try
      let fn =
        Filename.concat
          tmp_directory
          (tmp_prefix ^ "-" ^ (string_of_int n))
      in
      let fd =
        Unix.openfile fn [ Unix.O_RDWR; Unix.O_CREAT; Unix.O_EXCL ] 0o600 in
      (fn, fd)
    with
        Unix.Unix_error(Unix.EEXIST,_,_) ->
	  if n > limit then
	    failwith ("Netcgi_jserv_app.run_process_pool_server: too many tempfiles " ^ (Filename.concat tmp_directory (tmp_prefix ^ "-*")));
	  try_creation (n+1)
      | Unix.Unix_error(e,_,_) ->
          raise (Sys_error("Cannot create a temporary file in the directory " ^
			   tmp_directory ^ ": " ^ Unix.error_message e))
  in
  try_creation 0
;;


let run_process_pool_server 
      ?(config = std_config) n_children rhlist prttype props auth addr port =
  let rhtab = mk_rhtab rhlist in
  let files = ref [] in
  let delete = ref [] in
  let jk_sSS = get_jk_servletSubString props in
  let https = get_https props in

  let cleanup() =
    (* Close files and delete files before leaving this function *)
    List.iter (fun fd -> U.close fd) !files;
    List.iter (fun fn -> try Sys.remove fn with _ -> ()) !delete
  in

  let allow_hosts = get_allowed_hosts props in

  begin try
    (* Create the message board: *)
    let msg_board_name, msg_board_master = make_message_board() in
    files := msg_board_master :: !files;
    delete := msg_board_name :: !delete;

    (* Important note:
     * After fork() the processes share the file descriptor msg_board_master.
     * Unfortunately, even the current file position is shared, so
     * lseek changes the position of all processes. dup does not help.
     * So we open the msg_board for every process anew. Locks are per
     * file.
     *)

    (* Layout of the message board:
     * byte 0: "a" = a process waits for a new connection (a = accept)
     *         "-" = no process waits currently
     *         " " = during startup
     *         This byte is set by the subprocesses. This byte is locked
     *         while a subprocess has the right to wait for a new connection.
     * byte 1: "n" = normal operating mode 
     *         "s" = shutdown requested
     *         This byte is set by the main process, and it is tested by the
     *         subprocesses.
     *)
    let write_msg_board msg_board pos byte =
      let buf = String.create 1 in
      buf.[0] <- byte;
      restart
	(fun () -> 
	   ignore(U.lseek msg_board pos U.SEEK_SET);
	   ignore(U.write msg_board buf 0 1))
	()
    in

    let read_msg_board msg_board pos =
      let buf = String.create 1 in
      restart
	(fun () -> 
	   ignore(U.lseek msg_board pos U.SEEK_SET);
	   ignore(U.read msg_board buf 0 1))
	();
      buf.[0]
    in

    let unlock_msg_board msg_board pos =
      restart
	(fun () ->
	   ignore(U.lseek msg_board pos U.SEEK_SET);
	   ignore(U.lockf msg_board U.F_ULOCK 1))
	()
    in

    let lock_msg_board msg_board pos timeout f arg =
      (* locks position [pos] of the message board, calls f arg, and unlocks
       * the position. The latter also happens when an exception is raised.
       *)
      (* [timeout] should be called every second. To do this, we install a 
       * signal handler for SIGALRM, and start an itimer
       *)
      let old_handler = 
	Sys.signal Sys.sigalrm (Sys.Signal_handle (fun _ -> ())) in
      ignore(
	U.setitimer U.ITIMER_REAL { U.it_interval = 1.0; U.it_value = 1.0 });
      (* Now every second the lockf function will be interrupted and raise
       * an EINTR condition.
       *)
      restart
	(fun () ->
	   timeout();
	   ignore(U.lseek msg_board pos U.SEEK_SET);
	   ignore(U.lockf msg_board U.F_LOCK 1))
	();
      (* Stop the timer *)
      ignore(
	U.setitimer U.ITIMER_REAL { U.it_interval = 0.0; U.it_value = 0.0 });
      (* Activate the old signal handler *)
      Sys.set_signal Sys.sigalrm old_handler;
      try
	let r = f arg in
	unlock_msg_board msg_board pos;
	r
      with
	  error -> unlock_msg_board msg_board pos; raise error
    in

    let select_accept msg_board timeout dl master =
      (* Locks the byte 0 of the message board, and if this process gets the
       * lock, the U.select/U.accept will be executed as demanded. Furthermore,
       * it is checked whether there is a shutdown in this case.
       * timeout: A function that should be called every second (or more
       * often)
       *)
      lock_msg_board msg_board 0 
	timeout
	(fun _ ->
	   write_msg_board msg_board 0 'a';
	   let result_dl = ref [] in
	   let result_slave = ref None in
	   while !result_dl = [] && !result_slave = None do
	     timeout();
	     let dl', _, _ = restart (U.select (master::dl) [] []) 0.1 in
	     if read_msg_board msg_board 1 = 's' then (
	       write_msg_board msg_board 0 '-';
	       raise Signal_shutdown;
	     );
	     result_dl := List.filter (fun fd -> fd <> master) dl';
	     if (List.mem master dl') then
	       result_slave := Some(fst(restart U.accept master));
	   done;
	   write_msg_board msg_board 0 '-';
	   (!result_dl, !result_slave)
	)
	()
    in
    
    let create_child master ctrl_pipe_r ctrl_pipe_w =
      match restart U.fork() with
	  0 ->
	    begin try
	      (* Initialize the new process: *)
	      U.close ctrl_pipe_r;
	      config.js_init_process();
	      (* Timeout handling: *)
	      let time_idle = ref 0.0 in
	      let timeout() =
		let t = Unix.gettimeofday() in
		if t -. !time_idle >= 1.0 then (
		  catch config.js_idle_worker ();
		  time_idle := t;
		)
	      in
	      (* open the message board: *)
	      let msg_board = 
		Unix.openfile msg_board_name [ Unix.O_RDWR ] 0o000 in
	      (* Continue with the main loop: *)
	      server_loop 
	        ~controlpipe:(`Descriptor ctrl_pipe_w)
	        ~select_accept: (select_accept msg_board timeout)
	        ~allow_hosts
		(fun srv auth inch outch -> 
		   try
		     do_request config rhtab jk_sSS https auth inch outch
		   with 
		       Signal_shutdown ->
			 catch signal_shutdown srv  (* notifies main process *)
		     | Signal_restart ->
			 catch signal_restart srv   (* notifies main process *)
		     | error ->
			 catch (fun _ -> raise error) ();
		)
		auth
		master;
	      (* The main loop returns on shutdown. *)
	      config.js_fini_process();
	      exit 0;
	    with
		error ->
		  catch (fun _ -> raise error) ();
		  exit 2;
	    end
	| n ->
	    n
    in
    
    (* Initialize the message board *)
    write_msg_board msg_board_master 0 ' ';
    write_msg_board msg_board_master 1 'n';

    (* Create the control pipe: The subprocesses can write messages into it,
     * and the main process reads from it. When all writing end points of the
     * pipe are closed, all subprocesses have terminated.
     *)
    let ctrl_pipe_r, ctrl_pipe_w = restart U.pipe() in
    files := ctrl_pipe_r :: ctrl_pipe_w :: !files;

    (* Create the server socket: *)
    let master = server_init 
	           ~backlog:config.js_backlog
	           ~reuseaddr:config.js_reuseaddr
		   addr
		   port in
    files := master :: !files;

    (* Create the subprocesses: *)
    let pid_list = ref [] in
    for i = 1 to n_children do
      let pid = create_child master ctrl_pipe_r ctrl_pipe_w in
      pid_list := pid :: !pid_list
    done;
    
    (* Close ctrl_pipe_w such that EOF detection works: *)
    U.close ctrl_pipe_w;
    files := List.filter (fun fd -> fd <> ctrl_pipe_w) !files;

    (* Wait for messages on the control pipe, or EOF: *)
    let continuing = ref true in
    while !continuing do
      try
        while restart (U.select [ctrl_pipe_r] [] []) 1.0 = ([],[],[]) do
	  config.js_idle_master()
        done;
        read_control_pipe ctrl_pipe_r
      with
	  Signal_restart -> ()       (* not yet handled *)
	| Signal_shutdown ->
	    (* Modify the message board to notify the children: *)
	    write_msg_board msg_board_master 1 's'
	| End_of_file ->
	    (* All children have signaled that they will terminate. *)
	    continuing := false
    done;

    (* Wait for the children: *)
    List.iter
      (fun pid ->
	 ignore(restart(U.waitpid []) pid)
      )
      !pid_list;

    cleanup()
  with
      error ->
	cleanup();
	raise error
  end
;;


let run ?config srvtype prttype props auth addr port =
  match srvtype with
      `Sequential rhlist ->
	run_sequential_server ?config rhlist prttype props auth addr port
    | `Forking (maxload,rhlist) ->
	run_forking_server ?config maxload rhlist prttype props auth addr port
    | `Process_pool (pool_size, rhlist) ->
	run_process_pool_server 
	  ?config pool_size rhlist prttype props auth addr port
;;
