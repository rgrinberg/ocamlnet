(* $Id$ *)

open Netplex_types

let debug_log logger_lz s =
  (Lazy.force logger_lz) # log 
    ~component:"netplex.controller"
    ~level:`Debug
    ~message:s

let debug_logf logger_lz msgf =
  Printf.kprintf (debug_log logger_lz) msgf



class constant_workload_manager num_threads : workload_manager =
object(self)
  method hello controller =
    ()
      (* TODO: Announce the availability of admin messages *)

  method shutdown() =
    ()
      (* TODO *)

  method adjust sockserv sockctrl =
    match sockctrl # state with
      | `Enabled ->
	  let l = sockctrl # container_state in
	  let n = List.length l in
	  if n < num_threads then (
	    sockctrl # start_containers (num_threads - n)
	  )
      | _ ->
	  ()

  method capacity cid s =
    match s with
      | `Accepting(n,_) -> `Normal_quality (max_int - n)
      | `Busy -> `Unavailable
      | `Starting _ -> `Unavailable
      | `Shutting_down -> `Unavailable

end


let create_constant_workload_manager n =
  new constant_workload_manager n


let constant_workload_manager_factory =
object
  method name = "constant"
  method create_workload_manager ctrl_cfg cf addr =
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr [ "type"; "jobs"; "threads" ];
    let n =
      try
	cf # int_param
	  (cf # resolve_parameter addr "threads")
      with
	| Not_found ->
	    ( try
		cf # int_param
		  (cf # resolve_parameter addr "jobs")
		  (* Accept [jobs] for some time *)
	      with
		| Not_found ->
		    failwith ("Constant workload manager needs parameter 'threads'")
	    ) in
    if n < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".threads must be > 0");
    create_constant_workload_manager n
end



class type dynamic_workload_config =
object
  method max_jobs_per_thread : int
  method recommended_jobs_per_thread : int
  method min_free_job_capacity : int
  method max_free_job_capacity : int
  method inactivity_timeout : int
  method max_threads : int
end


module ContId = struct
  type t = container_id
  let (compare : t -> t -> int) = Pervasives.compare
end


module ContMap = Map.Make(ContId)


class dynamic_workload_manager config : workload_manager =
object(self)
  val mutable esys = lazy(assert false)
  val mutable logger = lazy(assert false)

  val mutable inactivated_conts = ContMap.empty
    (* Maps container_ids to Unixqueue groups. These containers are scheduled
     * for inactivation, and are going to be shut down. The group, if present,
     * refers to the inactivity timer.
     *
     * Note that these containers may be idle or busy. Inactivation means
     * that they won't be selected by the scheduler again, and that it is
     * hoped that they become idle soon. It is possible that these containers
     * are reactivated again if the load goes up.
     *
     * When these containers are finally idle, the inactivity timer is started.
     * If they remain idle, they will be shut down when the timer expires.
     *)

  method hello controller =
    esys <- lazy(controller # event_system);
    logger <- lazy(controller # logger);
    ()

  method shutdown() =
    ContMap.iter
      (fun _ g_opt -> 
	 match g_opt with
	   | None -> ()
	   | Some g -> Unixqueue.clear (Lazy.force esys) g)
      inactivated_conts;
    inactivated_conts <- ContMap.empty;
    ()

  method adjust (sockserv : socket_service) (sockctrl : socket_controller) =
    match sockctrl # state with
      | `Enabled ->
	  (* Determine total capacity of the current Netplex state: *)
	  let container_state = sockctrl # container_state in
	  let all_threads = List.length container_state in
	  let number_threads = 
	    List.length
	      (List.filter
		 (fun (cid,_) -> not (ContMap.mem cid inactivated_conts))
		 container_state) in
	  let active_threads =
	    List.length
	      (List.filter 
		 (fun (cid,s) -> 
		    not (ContMap.mem cid inactivated_conts) 
		    && s <> `Shutting_down) 
		 container_state) in
	  let total_cap = 
	    config#recommended_jobs_per_thread * active_threads in

	  (* Determine used capacity: *)
	  let used_cap =
	    List.fold_left
	      (fun acc (cid,s) ->
		 if ContMap.mem cid inactivated_conts then
		   acc
		 else
		   match s with
		     | `Accepting(n,_) -> 
			 acc + (min n config#recommended_jobs_per_thread)
		     | `Busy -> acc + config#recommended_jobs_per_thread
		     | `Starting _ -> acc
		     | `Shutting_down -> acc)
	      0
	      container_state in

	  (* Free capacity: *)
	  let free_cap = total_cap - used_cap in

	  if !Netplex_log.debug_scheduling then (
	    debug_logf logger
	      "Dyn workload mng %s: total_threads=%d avail_threads=%d total_cap=%d used_cap=%d"
	      sockserv#name all_threads active_threads
	      total_cap used_cap
	  );

	  (* Now decide... *)
	  if free_cap < config#min_free_job_capacity then (
	    let needed_cap = config#min_free_job_capacity - free_cap in
	    let needed_threads = 
	      (needed_cap - 1 ) / config#recommended_jobs_per_thread + 1 in
	    let needed_threads' =
	      max 0 ((min (all_threads+needed_threads) config#max_threads) -
		       all_threads) in
	    self # activate_containers sockserv sockctrl needed_threads'
	  )
	  else 
	    if free_cap > config#max_free_job_capacity then (
	      let exceeding_cap = free_cap - config#max_free_job_capacity in
	      let exceeding_threads = 
		exceeding_cap / config # recommended_jobs_per_thread in
	      if exceeding_threads > 0 then (
		(* Try to stop exceeding_thread containers. Look for
                 * the containers with the least numbers of jobs.
                 *)
		let weight s =
		  match s with
		    | `Accepting(n,_) -> n
		    | `Starting _ -> 0
		    | `Busy -> max_int
		    | `Shutting_down -> max_int in
		let sorted_conts =
		  List.sort
		    (fun (cid1,s1) (cid2,s2) ->
		       let n1 = weight s1 in
		       let n2 = weight s2 in
		       n1 - n2)
		    container_state in
		let n = ref 0 in
		List.iter
		  (fun (cid,s) ->
		     let already_inactivated =
		       ContMap.mem cid inactivated_conts in
		     if !n < exceeding_threads && not already_inactivated then (
		       match s with
			 | `Accepting(_,_)
			 | `Starting _ ->
			     incr n;
			     self # inactivate_container sockserv sockctrl cid
			 | _ -> ()
		     )
		  )
		  sorted_conts
	      )
	    );

	  self # inactivation_check sockserv sockctrl

      | _ ->
	  self # inactivation_check sockserv sockctrl


  method private activate_containers sockserv sockctrl n =
    let n = ref n in
    (* First re-activate the inactivated containers: *)
    let l = ref [] in
    ContMap.iter
      (fun cid g_opt ->
	 if !n > 0 then (
	   decr n;
	   l := cid :: !l;
	   match g_opt with
	     | None -> ()
	     | Some g ->
		 Unixqueue.clear (Lazy.force esys) g;
	 )
      )
      inactivated_conts;
    List.iter
      (fun cid ->
	 inactivated_conts <- ContMap.remove cid inactivated_conts)
      !l;
    if !Netplex_log.debug_scheduling && !l <> [] then (
      debug_logf logger
	"Dyn workload mng %s: Reclaiming %d inactivated containers"
	sockserv#name (List.length !l)
    );
    (* If needed, start further containers: *)
    if !n > 0 then
      sockctrl # start_containers !n

	(* Note that the activation may not do enough because inactivated
         * containers can be quite busy. The next [adjust] call will fix
         * this.
         *)


  method private inactivate_container sockserv sockctrl cid =
    inactivated_conts <- ContMap.add cid None inactivated_conts;

    if !Netplex_log.debug_scheduling then (
      debug_logf logger
	"Dyn workload mng %s: Inactivating 1 container"
	sockserv#name
    );


  method private inactivation_check sockserv sockctrl =
    (* Check whether there are inactivated containers without timer that
     * have become idle in the meantime. For these containers, start the
     * inactivation timer.
     *)
    let container_state = sockctrl # container_state in
    List.iter
      (fun (cid, s) ->
	 try
	   let g_opt = ContMap.find cid inactivated_conts in
	   match (g_opt, s) with
	     | None, `Accepting(0,_) ->
		 if !Netplex_log.debug_scheduling then (
		   debug_logf logger
		     "Dyn workload mng %s: Inactivated container becomes idle"
		     sockserv#name
		 );
		 let esys = Lazy.force esys in
		 let g = Unixqueue.new_group esys in
		 Unixqueue.once
		   esys g (float config#inactivity_timeout)
		   (fun () ->
		      inactivated_conts <- ContMap.remove cid inactivated_conts;
		      sockctrl # stop_containers [cid]
		   );
		 inactivated_conts <- ContMap.add cid (Some g) inactivated_conts
	     | _ ->
		 ()
	 with
	   | Not_found -> ()
      )
      container_state



  method capacity cid s =
    if ContMap.mem cid inactivated_conts then 
      `Unavailable  (* because we want to shut cid down *)
    else
      match s with
	| `Accepting(n,_) ->
	    if n < config # max_jobs_per_thread then
	      if n < config # recommended_jobs_per_thread then
		`Normal_quality (config # recommended_jobs_per_thread - n)
	      else
		`Low_quality (config # max_jobs_per_thread - n)
	    else
	      `Unavailable

	| `Busy -> `Unavailable
	| `Starting _ -> `Unavailable
	| `Shutting_down -> `Unavailable


end


let create_dynamic_workload_manager config =
  new dynamic_workload_manager config


let dynamic_workload_manager_factory =
object
  method name = "dynamic"
  method create_workload_manager ctrl_cfg cf addr =
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr [ "type"; "max_jobs_per_thread";
				    "recommended_jobs_per_thread";
				    "min_free_jobs_capacity";
				    "max_free_jobs_capacity";
				    "inactivity_timeout";
				    "max_threads"
				  ];
    let max_jobs_per_thread =
      try
	cf # int_param
	  (cf # resolve_parameter addr "max_jobs_per_thread")
      with
	| Not_found ->
	    1 in
    let recommended_jobs_per_thread =
      try
	cf # int_param
	  (cf # resolve_parameter addr "recommended_jobs_per_thread")
      with
	| Not_found ->
	    max_jobs_per_thread in
    let min_free_job_capacity =
      try
	cf # int_param
	  (cf # resolve_parameter addr "min_free_jobs_capacity")
      with
	| Not_found ->
	    failwith "Dynamic workload manager needs parameter 'min_free_jobs_capacity'" in
    let max_free_job_capacity =
      try
	cf # int_param
	  (cf # resolve_parameter addr "max_free_jobs_capacity")
      with
	| Not_found ->
	    failwith "Dynamic workload manager needs parameter 'max_free_jobs_capacity'" in
    let max_threads =
      try
	cf # int_param
	  (cf # resolve_parameter addr "max_threads")
      with
	| Not_found ->
	    failwith "Dynamic workload manager needs parameter 'max_threads'" in
    let inactivity_timeout =
      try
	cf # int_param
	  (cf # resolve_parameter addr "inactivity_timeout")
      with
	| Not_found ->
	    15 in

    if max_jobs_per_thread < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".max_jobs_per_thread must be > 0");
    if recommended_jobs_per_thread < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".recommended_jobs_per_thread must be > 0");
    if recommended_jobs_per_thread > max_jobs_per_thread then
      failwith ("Parameter " ^ cf#print addr ^ ".recommended_jobs_per_thread must be <= max_jobs_per_thread");
    if min_free_job_capacity < 0 then
      failwith ("Parameter " ^ cf#print addr ^ ".min_free_job_capacity must be >= 0");
    if max_free_job_capacity < min_free_job_capacity then
      failwith ("Parameter " ^ cf#print addr ^ ".max_free_job_capacity must be >= min_free_job_capacity");
    if max_threads < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".max_threads must be > 0");
    if inactivity_timeout < 1 then
      failwith ("Parameter " ^ cf#print addr ^ ".inactivity_tieout must be > 0");

    let cfg =
      ( object
	  method max_jobs_per_thread = max_jobs_per_thread
	  method recommended_jobs_per_thread = recommended_jobs_per_thread
	  method min_free_job_capacity = min_free_job_capacity
	  method max_free_job_capacity = max_free_job_capacity
	  method inactivity_timeout = inactivity_timeout
	  method max_threads = max_threads
	end
      ) in

    create_dynamic_workload_manager cfg
end


let workload_manager_factories =
  [ constant_workload_manager_factory;
    dynamic_workload_manager_factory
  ]
