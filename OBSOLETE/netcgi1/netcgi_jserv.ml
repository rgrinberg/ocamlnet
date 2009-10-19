(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


(* Generic JSERV stuff *)

open Netchannels
module U = Unix

(**********************************************************************
 * TCP SERVER SOCKET
 **********************************************************************)

let rec restart f arg =
  try f arg
  with
      U.Unix_error(U.EINTR,_,_) ->
	restart f arg
;;


type t =
    { mutable closed : bool;
      master_socket : U.file_descr;
      ctrl_pipe_r : U.file_descr option;
      ctrl_pipe_w : U.file_descr option;
    }
;;


type auth =
    { auth_challenge_length : int;
      auth_secret : string;
    }
;;


type controlpipe = [ `None | `Descriptor of Unix.file_descr | `Allocate ]
;;


exception Server_is_down
exception Signal_shutdown
exception Signal_restart
exception Continue


let read_control_pipe fd =
  let buf = String.create 1 in
  let n = restart (U.read fd buf 0) 1 in
  if n = 0 then raise End_of_file;
  match buf.[0] with
      'R' -> raise Signal_restart
    | 'S' -> raise Signal_shutdown
    | _   -> failwith "Netcgi_jserv.read_control_pipe: unknown message"
;;


let signal_restart srv =
  match srv.ctrl_pipe_w with
      Some p ->
	if srv.closed then raise Server_is_down;
	( try
	    let _ = restart (U.single_write p "R" 0) 1 in
	    ()
	  with
	      U.Unix_error(U.EPIPE,_,_) -> raise Server_is_down
	)
    | None ->
	failwith "Netcgi_jserv.signal_restart: no control pipe"
;;


let signal_shutdown srv =
  match srv.ctrl_pipe_w with
      Some p ->
	if srv.closed then raise Server_is_down;
	( try
	    let _ = restart (U.single_write p "S" 0) 1 in
	    ()
	  with
	      U.Unix_error(U.EPIPE,_,_) -> raise Server_is_down
	)
    | None ->
	failwith "Netcgi_jserv.signal_shutdown: no control pipe"
;;


let server_init 
      ?(backlog = 10) 
      ?(reuseaddr = true)
      addr
      port =
  let dom = Netsys.domain_of_inet_addr addr in
  let master = U.socket dom U.SOCK_STREAM 0 in
  U.setsockopt master U.SO_REUSEADDR reuseaddr;
  U.bind master (U.ADDR_INET(addr,port));
  U.listen master backlog;
  master
;;


let server_loop
      ?(controlpipe = (`Allocate : controlpipe))
      ?(onrestart = fun _ -> ())
      ?(onshutdown = fun _ -> ())
      ?(select_accept = 
	  (fun sel sock -> 
             let sel',_,_ = restart (Unix.select (sock::sel) [] []) (-1.0) in 
             let sel'' = List.filter (fun fd -> fd <> sock) sel' in
             let slave = 
               if List.mem sock sel' then 
		 Some(fst(restart Unix.accept sock))
               else
		 None in
             (sel'', slave)
	  )
       )
      ?(allow_hosts = [])
      onconnect
      auth master =
  let ctrl_pipe_r, ctrl_pipe_w = 
    match controlpipe with
	`Allocate -> 
	  let r,w = U.pipe() in (Some r, Some w)
      | `None -> 
	  (None, None)
      | `Descriptor fd -> 
	  (None, Some fd)
  in
  let srv =
    { closed = false;
      master_socket = master;
      ctrl_pipe_r = ctrl_pipe_r;
      ctrl_pipe_w = ctrl_pipe_w;
    }
  in
  let listening = ref true in
  while !listening do
    try
      let select_list =
	match ctrl_pipe_r with
	    Some fd -> [ fd ]
	  | None    -> [ ]
      in
      let ready_list, slave_opt = select_accept select_list master in
      
      begin match slave_opt with
	  Some sock ->
	    (* Check whether the connection is allowed: *)
	    if allow_hosts <> [] then begin
	      let peer = U.getpeername sock in
	      ( match peer with
		    U.ADDR_UNIX _ -> ()
		  | U.ADDR_INET (peer_addr,_) ->
		      if not (List.mem peer_addr allow_hosts) then begin
			U.close sock;
			raise Continue
		      end
	      );
	    end;

	    (* Process the connection: *)
	    let inch = U.in_channel_of_descr sock in
	    let outch = U.out_channel_of_descr sock in
	    onconnect 
	      srv auth (new input_channel inch) (new output_channel outch);
	| None ->
	    ()
      end;
      
      match ctrl_pipe_r with
	  Some fd ->
	    if List.memq fd ready_list then begin
	      (* Read exactly one byte from the control pipe *)
	      read_control_pipe fd
	    end;
	| None ->
	    ()
    with
	Signal_shutdown ->
	  srv.closed <- true;
	  ( match ctrl_pipe_r with
		Some fd -> U.close fd;  (* Closed early to prevent deadlocks *)
	      | None    -> ()
	  );
	  onshutdown srv;
	  listening := false
      | Signal_restart ->
	  onrestart srv
      | Continue ->
	  ()
  done;

  U.close master;
  ( match ctrl_pipe_w with
	Some fd -> U.close fd
      | None    -> ()
  )
;;


let server ?backlog ?reuseaddr ?controlpipe ?onrestart ?onshutdown ?select_accept
           ?allow_hosts
           handler auth addr port =
  let socket = server_init ?backlog ?reuseaddr addr port in
  server_loop ?controlpipe ?onrestart ?onshutdown ?select_accept ?allow_hosts
              handler auth socket
;;


(**********************************************************************
 * CRYPTOGRAPHICALLY SECURE PSEUDO RANDOM NUMBER GENERATOR 
 **********************************************************************)

(* Bases on the RC4 cipher *)

(* Usage: Call [prng_init] once with the initial key, and call [random_8bits]
 * as often as necessary 
 *)

let prng_lock = ref (fun () -> ());;
let prng_unlock = ref (fun () -> ());;

let prng_init = ref false;;
let prng_s = Array.make 256 0;;     (* S-box *)
let prng_i = ref 0;;
let prng_j = ref 0;;

let random_8bits() =
  if not !prng_init then failwith "Netcgi_jserv.random_8bits: uninitialized";
  !prng_lock();
  prng_i := (!prng_i + 1) land 0xff;
  prng_j := (!prng_j + prng_s.( !prng_i )) land 0xff;
  (* Swap prng_s.(!prng_i) and prng_s(!prng_j): *)
  let h = prng_s.( !prng_i ) in
  prng_s.( !prng_i ) <- prng_s.( !prng_j );
  prng_s.( !prng_j ) <- h;
  let r = prng_s.( (prng_s.( !prng_i ) + prng_s.( !prng_j )) land 0xff ) in
  !prng_unlock();
  r
;;


let prng_init ?(lock=(fun()->())) ?(unlock=(fun()->())) key =
  assert(key <> "");
  let k = Array.make 256 0 in
  for p = 0 to 255 do prng_s.(p) <- p done;
  let q = ref 0 in
  for p = 0 to 255 do
    k.(p) <- Char.code key.[ !q ];
    incr q;
    if (!q >= String.length key) then q := 0
  done;
  prng_j := 0;
  for p = 0 to 255 do
    prng_j := ( !prng_j + prng_s.(p) + k.(p) ) land 0xff;
    (* Swap s.(p) and s(!j): *)
    let h = prng_s.(p) in
    prng_s.( p ) <- prng_s.( !prng_j );
    prng_s.( !prng_j ) <- h;
  done;
  prng_i := 0;
  prng_j := 0;
  prng_init := true;
  prng_lock := lock;
  prng_unlock := unlock;
  ()
;;


let prng_init_from_file ?lock ?unlock ?(length = 256) filename =
  (* Read length bytes from the file, and use this as key *)
  let f = open_in_bin filename in
  try
    let buf = String.make length 'X' in
    let _ = input f buf 0 length in
    prng_init ?lock ?unlock buf;
    close_in f
  with
    | End_of_file -> 
	close_in f; failwith "prng_init_from_file"
    | err -> 
	close_in f; raise err
;;


(**********************************************************************
 * PROPERTY FILES
 **********************************************************************)

let line_re = Netstring_pcre.regexp 
		"^[ \t]*(.*[^ \t])[ \t]*$";;

let equation_re = Netstring_pcre.regexp
		    "^([^=]+)=(.*)$";;

let parse_properties file =
  let props = ref [] in
  let f = open_in file in
  try
    while true do
      let line = input_line f in
      match Netstring_pcre.string_match line_re line 0 with
	  Some r ->
	    let text = Netstring_pcre.matched_group r 1 line in
	    if text.[0] <> '#' then begin  (* Ignore comments *)
	      match Netstring_pcre.string_match equation_re text 0 with
		  Some eq ->
		    let name = Netstring_pcre.matched_group eq 1 text in
		    let value = Netstring_pcre.matched_group eq 2 text in
		    props := (name,value) :: !props
		| None ->
		    (* No equation: Ignore silently *)
		    ()
	    end
	| None ->
	    (* The line is empty --> ignore *)
	    ()
    done;
    assert false
  with
      End_of_file ->
	close_in f;
	List.rev !props
    | err ->
	close_in f;
	raise err
;;


(**********************************************************************
 * JVM-COMPATIBLE MAIN PROGRAM
 **********************************************************************)


let read_file filename =
  with_in_obj_channel
    (new input_channel (open_in_bin filename))
    string_of_in_obj_channel
;;
    

let jvm_emu_main est_server =
  let anon_args = ref [] in
  let name = Sys.argv.(0) in
  Arg.parse
      [ "-classpath", Arg.String (fun _ -> ()),
	           "<path>           This option is ignored";
      ]
      (fun s -> anon_args := !anon_args @ [s])
      ("usage: " ^ name ^ " [ options ] classname propertyfile");
  if List.length !anon_args <> 2 then (
    prerr_endline "Bad number of arguments. Try -help to see usage information.";
    exit 1;
  );
  let props = parse_properties (List.nth !anon_args 1) in
  let addr_str = 
    try List.assoc "bindaddress" props 
    with Not_found -> "localhost" in
  let port_str =
    try List.assoc "port" props
    with Not_found -> "8007" in
  
  let addr =
    if addr_str = "*" then
      U.inet_addr_any
    else
      try U.inet_addr_of_string addr_str
      with
	  _ ->
	    try (U.gethostbyname addr_str).U.h_addr_list.(0)
	    with
		Not_found ->
		  prerr_endline "Cannot resolve bindaddress";
		  exit 1
  in
  let port =
    try int_of_string port_str
    with
	_ ->
	  prerr_endline "The port must be a number";
	  exit 1
  in
  let do_auth =
    try List.assoc "security.authentication" props = "true"
    with Not_found -> true
  in
  let auth =
    if do_auth then 
      Some 
	{ auth_challenge_length =
	    ( try 
		int_of_string 
		  (try List.assoc "security.challengeSize" props 
		   with Not_found -> "5")
	      with _ ->
		prerr_endline "security.challengeSize must be a number";
		exit 1
	    );
	  auth_secret =
	    read_file
	      ( try
		  List.assoc "security.secretKey" props
		with Not_found ->
		  prerr_endline "security.secretKey is missing";
		  exit 1
	      );
	}
    else
      None
  in

  (* Set up signal handlers. SIGPIPE is ignored, so broken pipes are reported
   * by the system call as EPIPE, and no longer cause program termination.
   * SIGCHLD is set to an empty handler. The effect is that whenever a
   * child terminates, the next blocking system call will return EINTR.
   * Note that for SIGCHLD an empty handler has a different meaning than
   * ignoring the signal!
   *)
  (* Now using the Netsys_signal framework: *)
  Netsys_signal.register_handler
    ~library:"netcgi1"
    ~name:"Jserv emulation"
    ~signal:Sys.sigchld
    ~callback:(fun _ -> ())
    ();
  (* Note: the handler for sigpipe is set by Netsys_signal anyway *)

  est_server props auth addr port
;;
