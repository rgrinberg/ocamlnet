(* $Id$ *)

(* This is a RPC server built from the Netplex and RPC components.
 * It is configured in the netplex.cfg file.
 * Note: start program with option "-conf netplex.cfg"
 *
 * This program iterates through the directories every time a search query
 * is solved. One can imagine to improve that, but this is only a simple
 * example without too much optimization.
 *)

(**********************************************************************)
(* The "find" procedure                                               *)
(**********************************************************************)

let proc_find root_dir searched_name =
  
  let rec iterate_dir dir =
    let f = Unix.opendir dir in
    try
      let r = iterate_next dir f in
      Unix.closedir f;
      r
    with
      | End_of_file ->
	  Unix.closedir f;
	  None
      | Unix.Unix_error((Unix.EACCES | Unix.EPERM), _, _) ->
	  (* ignore these *)
	  Unix.closedir f;
	  None
      | error ->
	  Unix.closedir f;
	  raise error

  and iterate_next dir f =
    let file = Unix.readdir f in
    if file <> "." && file <> ".." then (
      let fullname = Filename.concat dir file in
      if file = searched_name then
	Some fullname
      else (
	try
	  let s = Unix.lstat fullname in
	  if s.Unix.st_kind = Unix.S_DIR then
	    let r = iterate_dir fullname in
	    match r with
	      | None -> iterate_next dir f
	      | Some _ -> r
	  else
	    iterate_next dir f
	with
	  | Unix.Unix_error(_,_,_) ->  (* lstat error *)
	      (* ignore *)
	      iterate_next dir f
      )
    )
    else
      iterate_next dir f

  in

  match iterate_dir root_dir with
    | None -> `not_found
    | Some fullname -> `found fullname
;;


(**********************************************************************)
(* Create the RPC server                                              *)
(**********************************************************************)

let configure cf addr =
  let root_dir =
    try
      cf # string_param (cf # resolve_parameter addr "root_dir")
    with
      | Not_found ->
	  failwith "Required parameter root_dir is missing!" in
  root_dir
;;


let setup srv root_dir =
  Finder_service_srv.Finder.V1.bind 
    ~proc_ping:(fun () -> ())
    ~proc_find:(proc_find root_dir)
    srv
;;


let start() =
  let (opt_list, cmdline_cfg) = Netplex_main.args() in

  let use_mt = ref false in

  let opt_list' =
    [ "-mt", Arg.Set use_mt,
      "  Use multi-threading instead of multi-processing"
    ] @ opt_list in

  Arg.parse
    opt_list'
    (fun s -> raise (Arg.Bad ("Don't know what to do with: " ^ s)))
    "usage: netplex [options]";
  let parallelizer =
    if !use_mt then
      Netplex_mt.mt()     (* multi-threading *)
    else
      Netplex_mp.mp() in  (* multi-processing *)
  let finder_factory =
    Rpc_netplex.rpc_factory
      ~configure
      ~name:"finder"
      ~setup
      () in
  Netplex_main.startup
    parallelizer
    Netplex_log.logger_factories   (* allow all built-in logging styles *)
    Netplex_workload.workload_manager_factories (* ... all ways of workload management *)
    [ finder_factory ]           (* make this service type available *)
    cmdline_cfg
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start();;
