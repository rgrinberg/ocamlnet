(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Printf

module A = Proto_aux;;
module C = Proto_clnt.PROTO.V1;;

let esys = Unixqueue.create_unix_event_system();;

let tcp_port = ref 0;;
let udp_port = ref 0;;
let unix_path = ref "";;
let enable_ssl = ref false ;;

let tests = ref [];;

let register_test name f =
  tests := (name,f) :: !tests
;;


let register_test_triple ?(tcp=true) ?(udp=true) ?(unix=true) name f =
  let socket_config =
    if !enable_ssl then
      let ctx = Ssl.create_client_context Ssl.TLSv1 in
      Rpc_ssl.ssl_client_socket_config ctx
    else
      Rpc_client.default_socket_config in
  if tcp then begin
    register_test ("tcp_" ^ name)
      (fun () ->
	 let client = C.create_client2 ~esys
	              (`Socket(Rpc.Tcp, 
			       Rpc_client.Inet("localhost", !tcp_port),
			       socket_config))
	 in
	 f client
      )
  end;
  if udp && not !enable_ssl then begin
    register_test ("udp_" ^ name)
      (fun () ->
	 let client = C.create_client2 ~esys
	              (`Socket(Rpc.Udp, 
			       Rpc_client.Inet("localhost", !udp_port),
			       socket_config))
	 in
	 f client
      )
  end;
  if unix then begin
    register_test ("unix_" ^ name)
      (fun () ->
	 let client = C.create_client2 ~esys
	              (`Socket(Rpc.Tcp, 
			       Rpc_client.Unix !unix_path,
			       socket_config))
	 in
	 f client
      )
  end
;;


(**********************************************************************)
(* ping *)

let register_ping() =
  register_test_triple "ping"
    (fun client ->
       C.ping client ();
       Rpc_client.shut_down client;
       true
    )
;;

(**********************************************************************)
(* revert *)


let register_revert() =
  register_test_triple "short_revert"
    (fun client ->
       let s = C.revert client "Remote Procedure Call" in
       Rpc_client.shut_down client;
       s = "llaC erudecorP etomeR"
    );

  let medium = String.create 4000 in
  for i = 0 to 3999 do medium.[i] <- Char.chr (i land 0xff) done;
  let rmedium = String.create 4000 in
  for i = 0 to 3999 do rmedium.[3999-i] <- Char.chr (i land 0xff) done;

  register_test_triple "medium_revert"
    (fun client ->
       let s = C.revert client medium in
       Rpc_client.shut_down client;
       s = rmedium
    );

  let large = String.create 40000 in
  for i = 0 to 39999 do large.[i] <- Char.chr (i land 0xff) done;
  let rlarge = String.create 40000 in
  for i = 0 to 39999 do rlarge.[39999-i] <- Char.chr (i land 0xff) done;

  register_test_triple ~udp:false "large_revert"
    (fun client ->
       let s = C.revert client large in
       Rpc_client.shut_down client;
       s = rlarge
    );

  register_test_triple ~udp:false "double_large_revert"
    (fun client ->
       let s = C.revert client large in
       let s' = C.revert client large in
       Rpc_client.shut_down client;
       s = rlarge && s' = rlarge
    );
;;

(**********************************************************************)
(* batch_in *)


let register_batch_in () =
  let test_batch_in client =
    Rpc_client.configure client 0 0.0;  (* configure immediate timeout *)
    C.batch_in'async client (true,false,"Remote ") (fun _ -> ());
    C.batch_in'async client (false,false,"Procedure ") (fun _ -> ());
    Rpc_client.configure client 0 (-1.0);  (* disable timeout *)
    let response = C.batch_in client (false,true,"Call") in
    Rpc_client.shut_down client;
    response = "Remote Procedure Call"
  in
  register_test_triple ~udp:false "batch_in" test_batch_in
;;

(**********************************************************************)
(* batch_out *)

let register_batch_out () =
  let test_batch_out client =
    let b = Buffer.create 100 in
    C.batch_out'async
      client
      "Remote Procedure Call"
      (fun get_response ->
	 let r = get_response() in
	 match r with
	     None -> ()
	   | Some s -> Buffer.add_string b s; raise Rpc_client.Keep_call
      );
    Unixqueue.run esys;
    Rpc_client.shut_down client;
    Buffer.contents b = "Remote Procedure Call"
  in
  register_test_triple ~udp:false "batch_out" test_batch_out
;;

(**********************************************************************)
(* retransmit *)

let register_retransmit () =
  let test_retransmit client =
    Rpc_client.configure client 1 1.0;  (* one retransmission after 1 second *)
    C.retransmit1 client ();     (* reset *)
    C.retransmit2 client ();     (* the test *)
    Rpc_client.shut_down client;
    true
  in
  register_test_triple "retransmit" test_retransmit
;;

(**********************************************************************)
(* dropping_filter *)

let register_dropping_filter () =
  let test_filter client =
    C.install_dropping_filter client ();
    Rpc_client.configure client 0 1.0;  (* no retransmission timeout 1 second *)
    let as_expected = ref false in
    C.ping'async client ()
      (fun get_resp ->
	 try get_resp() with
	     Rpc_client.Message_timeout -> as_expected := true
      );
    Unixqueue.run esys;
    Rpc_client.shut_down client;
    !as_expected
  in
  register_test_triple "dropping_filter" test_filter;

  let test_filter_large client =
    C.install_dropping_filter client ();
    Rpc_client.configure client 0 1.0;  (* no retransmission timeout 1 second *)
    let as_expected = ref false in
    let large = String.make 40000 'X' in
    C.revert'async client large
      (fun get_resp ->
	 try ignore(get_resp()) with
	     Rpc_client.Message_timeout -> as_expected := true
      );
    Unixqueue.run esys;
    Rpc_client.shut_down client;
    !as_expected
  in
  register_test_triple ~udp:false "large_dropping_filter" test_filter_large;

;;

(**********************************************************************)
(* rejecting_filter *)

let register_rejecting_filter () =
  let test_filter client =
    C.install_rejecting_filter client ();
    let as_expected = ref false in
    C.ping'async client ()
      (fun get_resp ->
	 try get_resp() with
	     Rpc.Rpc_server Rpc.Auth_too_weak -> as_expected := true
      );
    Unixqueue.run esys;
    Rpc_client.shut_down client;
    !as_expected
  in
  register_test_triple "rejecting_filter" test_filter
;;


(**********************************************************************)
(* denying_filter *)

let register_denying_filter () =
  let test_filter client =
    C.install_denying_filter client ();
    let as_expected = ref false in
    C.ping'async client ()
      (fun get_resp ->
	 try get_resp() with
	     Rpc_client.Message_lost -> as_expected := true
      );
    Unixqueue.run esys;
    Rpc_client.shut_down client;
    !as_expected
  in
  register_test_triple ~udp:false "denying_filter" test_filter
;;

(**********************************************************************)
(* dropping_filter_with_limit *)

let register_dropping_filter_with_limit () =
  let test_filter client =
    Rpc_client.configure client 0 1.0;  (* no retransmission timeout 1 second *)
    let as_expected = ref false in
    C.install_dropping_filter_with_limit client ();
    (* The RPC header has 40 bytes (for AUTH_NULL). A string of 36 bytes
     * fits into the 80 bytes message limit (36 bytes + 1 length word = 40
     * bytes); but 37 bytes do not fit.
     *)
    let msg1 = String.make 36 'X' in
    ignore(C.revert client msg1);
    C.install_dropping_filter_with_limit client ();
    let msg2 = String.make 37 'X' in
    C.revert'async client msg2
      (fun get_resp ->
	 try ignore(get_resp()) with
	     Rpc_client.Message_timeout -> as_expected := true
      );
    Unixqueue.run esys;
    Rpc_client.shut_down client;
    !as_expected
  in
  register_test_triple "dropping_filter_with_limit" test_filter
;;

(**********************************************************************)
(* auth_sys *)

let register_auth_sys () =
  let test_filter client =
    let identity1 = `This_user(50,51,[|52;53|],"localhost") in
    let m1 = Rpc_auth_sys.client_auth_method ~identity:identity1 () in
    Rpc_client.set_auth_methods client [ m1 ];
    C.auth_sys client ();
    let identity2 = `This_user(52,51,[|52;53|],"localhost") in
    let m2 = Rpc_auth_sys.client_auth_method ~identity:identity2 () in
    Rpc_client.set_auth_methods client [ m2 ];
    try
      ignore(C.auth_sys client ()); false
    with
	Rpc.Rpc_server Rpc.Auth_rejected_cred -> true
  in
  register_test_triple ~unix:false "auth_sys" test_filter
;;

(**********************************************************************)
(* auth_local *)

let register_auth_local () =
  let f() =
    let client = C.create_client ~esys
		 (Rpc_client.Unix !unix_path)
		   Rpc.Tcp
    in
    C.enable_auth_local client ();
    Rpc_client.shut_down client;
    let client' = C.create_client ~esys
		  (Rpc_client.Unix !unix_path)
		    Rpc.Tcp
    in
    let r = C.auth_local client' () in
    Rpc_client.shut_down client';
    printf "(user=%s) " r;
    r = (sprintf "%d.%d@localhost" (Unix.geteuid()) (Unix.getegid()))
  in
  register_test ("unix_auth_local") f;
;;

(**********************************************************************)
(* auth_dh *)

let register_auth_dh () =
  let test_filter client =
    let uid = "unix." ^ string_of_int (Unix.geteuid()) ^ "@" ^
	      Rpc_auth_dh.domainname() in
    Rpc_client.set_auth_methods client
      [ Rpc_auth_dh.client_auth_method uid ];
    let r = C.auth_dh client() in
    printf "(user=%s) " r;
    Rpc_client.shut_down client;
    r = uid
  in
  register_test_triple "auth_dh" test_filter
;;


(**********************************************************************)

let main() =
  (* Register tests: *)

  let do_register() =
    register_ping();
    register_revert();
    register_batch_in();
    register_batch_out();
    register_retransmit();
    register_dropping_filter();
    register_rejecting_filter();
    register_denying_filter();
    register_dropping_filter_with_limit();
    register_auth_sys();
    register_auth_local();
    register_auth_dh();
  in    

  (* Parse command line: *)

  let tests_to_do = ref [] in
  let print_test_names = ref false in
  let args = ref [] in
  Arg.parse
    [ "-tcp_port", Arg.Int (fun n -> tcp_port := n),
      "<n>          Set the TCP port of the test server";
      "-udp_port", Arg.Int (fun n -> udp_port := n),
      "<n>          Set the UDP port of the test server";
      "-unix_path", Arg.String (fun s -> unix_path := s),
      "<path>      Set the path name of the Unix domain socket";
      "-ssl", Arg.Set enable_ssl,
      "                  Enable SSL (incompatible with UDP)";
      "-print", Arg.Set print_test_names,
      "                Print the available tests";
    ]
    (fun s -> args := !args @ [s])
    "Usage: protoclient [ options ] test ...";

  if !enable_ssl then
    Ssl.init();

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
