
open Proto_aux;;

let proc_do_something arg = arg ;;
  (* Test procedure: the identity function *)

let test_001() =
  (* set up test environment: *)

  let bipipe1, bipipe2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in

  let esys = Unixqueue.create_unix_event_system() in
  let srv_conn = Rpc_server.Descriptor bipipe1 in
  let srv = Proto_srv.P.V.create_server
	      ~proc_do_something srv_conn Rpc.Tcp Rpc.BiPipe esys in
  let clnt_conn = Rpc_client.Descriptor bipipe2 in
  let clnt = Proto_clnt.P.V.create_client
	       ~esys clnt_conn Rpc.Tcp in
  
  let arg = 
    { i = Int32.of_int 0;
      s = "Hello world";
      ia = Array.map Int32.of_int 
	     [| 0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16;
		17; 18; 19 |];
      sa = [| "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	      "01234567890123456789";
	   |];
      abc = Some(`a { a1 = "a1"; a2 = "a2" });
    }
 in

  Proto_clnt.P.V.do_something'async
    clnt
    arg
    (fun get_result ->
       let result = get_result() in
(*
       if result = arg then
	 prerr_endline "Test passed!"
       else
	 prerr_endline "Test not passed!";
*)
       Unix.close bipipe2;
    );

  (* Start the event queue, and see what happens: *)
  Unixqueue.run esys;
  Unix.close bipipe1;
;;
