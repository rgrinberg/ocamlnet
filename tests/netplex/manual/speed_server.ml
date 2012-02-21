
let factory() =
  Rpc_netplex.rpc_factory
    ~configure:(fun _ _ -> ())
    ~name:"speed"
    ~setup:(fun rpc () ->
	      Speed_proto_srv.P.V1.bind
		~proc_ping:(fun () -> ())
		rpc
	   )
    ()



let start() =
  let opts, cmdconf = Netplex_main.args() in
  Arg.parse 
    ( opts
    )
    (fun s -> raise(Arg.Bad ("Unknown arg: " ^ s))) 
    "usage: protoserver";
  let par = Netplex_mp.mp() in
  Netplex_main.startup
    par
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    [ factory() ]
    cmdconf
;;

(*
Netplex_container.Debug.enable := true;
Netplex_controller.Debug.enable := true;
*)
Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
(*
Netplex_log.debug_scheduling := true;
Rpc_netplex.debug_rpc_service := true;
 *)
start();;
