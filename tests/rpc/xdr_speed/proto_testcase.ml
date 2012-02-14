
open Proto_aux;;

let proc_do_something arg = arg ;;
  (* Test procedure: the identity function *)

let some_str_value =
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


let some_arr_value =
  Array.init 1000 (fun k -> { some_str_value with i = Int32.of_int k })

let t_str = Xdr.validate_xdr_type xdrt_str;;
let t_arr = Xdr.validate_xdr_type xdrt_arr;;

let test_001() =
  let v = _of_str some_str_value in
  let s = Xdr.pack_xdr_value_as_string v t_str [] in
  let v' = Xdr.unpack_xdr_value ~xv_version:`Ocamlrpcgen s t_str [] in
  let _x = _to_str v in
  ()

let test_002() =
  let v = _of_arr some_arr_value in
  let s = Xdr.pack_xdr_value_as_string v t_arr [] in
  let v' = Xdr.unpack_xdr_value ~xv_version:`Ocamlrpcgen s t_arr [] in
  let _x = _to_arr v in
  ()


let test_010() =
  (* set up test environment: *)

  let bipipe1, bipipe2 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in

  let esys = Unixqueue.create_unix_event_system() in
  let srv_conn = Rpc_server.Descriptor bipipe1 in
  let srv = Proto_srv.P.V.create_server
	      ~proc_do_something srv_conn Rpc.Tcp Rpc.BiPipe esys in
  let clnt_conn = Rpc_client.Descriptor bipipe2 in
  let clnt = Proto_clnt.P.V.create_client
	       ~esys clnt_conn Rpc.Tcp in
  let arg = some_str_value in

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
