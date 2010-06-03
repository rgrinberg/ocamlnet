(* $Id$ *)

(* This test does not finish - it is expected to end with the message
   "Disabling service after 10 startup failures"
 *)

let factory =
  ( object
      method name = "test"
      method create_processor ctrlcfg cf addr =
	let hooks = new Netplex_kit.empty_processor_hooks() in
	( object
	    inherit Netplex_kit.processor_base hooks
            method process ~when_done container fd proto_name =
	      Unix.close fd; when_done()
            method supported_ptypes = [ `Multi_processing ]
	    method post_add_hook socksrv ctrl =
	      ctrl # add_plugin Netplex_mutex.plugin
	    method post_start_hook cont =
	      let m = Netplex_mutex.access "m" in
	      Random.self_init();
	      let w = Random.float 1.0 in
	      Netsys.sleep w;
	      for n = 1 to 10 do
		Netplex_mutex.lock m;
		Netsys.sleep 0.01;
		Netplex_mutex.unlock m
	      done;
	      if Random.int 2 = 0 then
		Netplex_mutex.lock m;
	      Netlog.logf `Info "Shutdown";
	      cont # shutdown();
	  end
	)
    end
  )

let main () =
  let opts, cmdconf = Netplex_main.args() in
  Arg.parse 
    opts
    (fun s -> raise(Arg.Bad ("Unknown arg: " ^ s))) 
    "usage: server";
  let par = Netplex_mp.mp() in
  Netplex_main.startup
    par
    Netplex_log.logger_factories
    Netplex_workload.workload_manager_factories
    [ factory ]
    cmdconf


let () = 
  main()
