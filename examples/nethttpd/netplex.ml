(* Note: start program with option "-conf netplex.cfg" *)

let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in
  Arg.parse 
    opt_list
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";
  let parallelizer = Netplex_mp.mp() in  (* multi-processing *)
  let nethttpd_factory = 
    Nethttpd_plex.netplex_factory() in
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ nethttpd_factory ]           (* make this nethttpd available *)
    cmdline_cfg
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start();;
