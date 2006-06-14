#require "rpc";;
#require "cryptgps";;
#directory "../../../src/rpc-auth-dh";;
#load "rpc_auth_dh.cma";;
#load "adder.cma";;
open Adder_clnt.ADDER.V1;;
open Adder_srv.ADDER.V1;;
let auth = Rpc_auth_dh.client_auth_method "unix.500@homenet";;
let test() =
  let c = create_portmapped_client "localhost" Rpc.Tcp in
  Rpc_client.set_auth_methods c [auth];
  add c (3,4)
;;
let speed() =
  let c = create_portmapped_client "localhost" Rpc.Tcp in
  Rpc_client.set_auth_methods c [auth];
  let start = Unix.gettimeofday() in
  for i = 1 to 1000 do
    ignore(add c (3,4));
  done;
  let stop = Unix.gettimeofday() in
  stop -. start
;;
let server() =
  let esys = Unixqueue.create_unix_event_system() in
  let s = create_async_server
	    ~proc_add:(fun session (a,b) reply -> 
			 prerr_endline("User: " ^ Rpc_server.get_user session);
			 reply (a+b))
	    Rpc_server.Portmapped
	    Rpc.Tcp
	    Rpc.Socket
	    esys in
  let auth = Rpc_auth_dh.server_auth_method() in
  Rpc_server.set_auth_methods s [auth];
  Unixqueue.run esys
;;

