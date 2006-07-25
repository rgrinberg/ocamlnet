(* $Id$ *)

open Netplex_types
open Printf

module A = Proto_aux
module C = Proto_clnt.P.V1


let rec upto n =
  if n = 0 then
    []
  else
    n :: (upto (n-1))


let tests = ref [];;

let register_test name f =
  tests := (name,f) :: !tests
;;


let register_rpc_test name socketname f =
  register_test name
    (fun () ->
       let client = C.create_client (Rpc_client.Unix socketname) Rpc.Tcp in
       ( try
	   let r = f client in
	   Rpc_client.shut_down client;
	   r
	 with
	   | error ->
	       Rpc_client.shut_down client;
	       raise error
       )
    )
;;

(**********************************************************************)

let register_ping() =
  register_rpc_test "ping1" "socket1"
    (fun client ->
       C.ping client();
       true
    );
  register_rpc_test "ping2" "socket2"
    (fun client ->
       C.ping client();
       true
    )
;;

(**********************************************************************)

let register_accept_seq() =
  (* 5 consecutive [accept]s plus one call to [hard_work] for each conn. *)
  register_test "accept_seq"
    (fun () ->
       let n = ref 0 in
       let l = [ 1; 2; 3; 4; 5 ] in
       let esys = Unixqueue.create_unix_event_system() in
       let clients =
	 List.map
	   (fun k ->
	      C.create_client2 
		~esys
		(`Socket(Rpc.Tcp, 
			 Rpc_client.Unix "socket2",
			 Rpc_client.default_socket_config))
	   ) l in
       List.iter
	 (fun client ->
	    C.hard_work'async
	      client
	      ()
	      (fun get_reply ->
		 let p = get_reply() in
		 n := !n + p)
	 ) 
	 clients;
       Unixqueue.run esys;
       List.iter Rpc_client.shut_down clients;
       !n = 5
    )
;;

(**********************************************************************)

let register_accept_par() =
  (* 50 parallel [accept]s plus one call to [hard_work] for each conn. *)
  register_test "accept_par"
    (fun () ->
       let n = ref 0 in
       let l = upto 50 in
       let esys = Unixqueue.create_unix_event_system() in
       let clients =
	 List.map
	   (fun k ->
	      C.create_client2 
		~esys
		(`Socket(Rpc.Tcp, 
			 Rpc_client.Unix "socket1",
			 Rpc_client.default_socket_config))
	   ) l in
       List.iter
	 (fun client ->
	    C.hard_work'async
	      client
	      ()
	      (fun get_reply ->
		 let p = get_reply() in
		 n := !n + p)
	 ) 
	 clients;
       Unixqueue.run esys;
       List.iter Rpc_client.shut_down clients;
       !n = 50
    )
;;


(**********************************************************************)

let register_accept_seq_exit() =
  (* 5 consecutive [accept]s plus one call to [hard_work] for most conn.,
   * and a call to [exit] for one conn.
   *)
  register_test "accept_seq_exit"
    (fun () ->
       let n = ref 0 in
       let l = [ 1; 2; 3; 4; 5 ] in
       let esys = Unixqueue.create_unix_event_system() in
       let k_clients =
	 List.map
	   (fun k ->
	      k, (C.create_client2 
		    ~esys
		    (`Socket(Rpc.Tcp, 
			     Rpc_client.Unix "socket2",
			     Rpc_client.default_socket_config))
		 )
	   ) l in
       List.iter
	 (fun (k,client) ->
	    if k = 3 then
	      C.exit'async
		client
		()
		(fun get_reply -> ())
	    else
	      C.hard_work'async
		client
		()
		(fun get_reply ->
		   let p = get_reply() in
		   n := !n + p)
	 ) 
	 k_clients;
       Unixqueue.run esys;
       List.iter Rpc_client.shut_down (List.map snd k_clients);
       !n = 4
    )
;;

(**********************************************************************)

let main() =
  (* Register tests: *)

  let do_register() =
    register_ping();
    register_accept_seq();
    register_accept_par();
    register_accept_seq_exit();
  in

  (* Parse command line: *)

  let tests_to_do = ref [] in
  let print_test_names = ref false in
  let args = ref [] in
  Arg.parse
    [ "-print", Arg.Set print_test_names,
      "                Print the available tests";
    ]
    (fun s -> args := !args @ [s])
    "Usage: protoclient [ options ] test ...";

  do_register();

  List.iter
    (fun s ->
       match s with
           "all" ->
             tests_to_do := List.rev !tests
         | name ->
             let f =
               try List.assoc name !tests
               with Not_found ->
                 raise(Arg.Bad("No such test: " ^ name))
             in
             tests_to_do := !tests_to_do @ [ name, f ]
    )
    !args;

  if !print_test_names then begin
    printf "Available tests:\n";
    List.iter
      (fun (name,_) ->
         printf "- %s\n" name;
      )
      (List.rev !tests);
    printf "Or use the keyword 'all' to select all tests.\n";
    flush stdout;
    exit 0
  end;

  if !tests_to_do = [] then begin
    prerr_endline "No tests selected.";
    exit 1
  end;

  Sys.set_signal Sys.sigpipe Sys.Signal_ignore;

  List.iter
    (fun (name,f) ->
       printf "Starting test %s: " name;
       flush stdout;
       try
         match f() with
             true  -> printf "passed\n"; flush stdout
           | false -> printf "failed\n"; flush stdout
       with
           err ->
             printf "exception occurred: %s\n" (Printexc.to_string err);
             flush stdout
    )
    !tests_to_do
;;


main();;
