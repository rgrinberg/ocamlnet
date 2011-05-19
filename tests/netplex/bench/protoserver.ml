(* $Id$ *)

open Netplex_types
open Printf


let configure _ _ = ()


let proc_hard_work() =
  Unix.sleep 1;
  1

let proc_fail() =
  failwith "proc_fail"

let proc_exit() =
  exit 3

let setup rpc () =
  Proto_srv.P.V1.bind
    ~proc_ping:(fun () -> ())
    ~proc_hard_work
    ~proc_fail
    ~proc_exit
    rpc
;;


let proto_factory (non_responsive,cont_fail) =
  let hooks _ =
    ( object
	inherit Netplex_kit.empty_processor_hooks ()

	method post_start_hook _ =
	  if non_responsive then
	    Unix.sleep 1000000;
	  if cont_fail then
	    exit 3
      end
    ) in

  Rpc_netplex.rpc_factory
    ~configure
    ~name:"proto"
    ~setup
    ~hooks
    ()

let start() =
  let non_responsive = ref false in
  let cont_fail = ref false in

  let opts, cmdconf = Netplex_main.args() in
  Arg.parse 
    ( opts @
	[ "-non-responsive", Arg.Set non_responsive,
	  "  Force that the containers sleep instead of starting up";

	  "-cont-failure", Arg.Set cont_fail,
	  "  Force that the containers fail instead of starting up";
	]
    )
    (fun s -> raise(Arg.Bad ("Unknown arg: " ^ s))) 
    "usage: protoserver";
  let par = Netplex_mp.mp() in
  Netplex_main.startup
    par
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    [ proto_factory (!non_responsive, !cont_fail) ]
    cmdconf
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
(*
Netplex_log.debug_scheduling := true;
Rpc_netplex.debug_rpc_service := true;
 *)
start();;
