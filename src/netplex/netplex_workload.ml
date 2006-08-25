(* $Id$ *)

open Netplex_types

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
    (* Maps container_ids to Unixqueue groups. These containers are inactive
     * (they do not have jobs), and are going to be shut down. The group
     * refers to the inactivity timer.
     *)

  method hello controller =
    esys <- lazy(controller # event_system);
    logger <- lazy(controller # logger);
    ()
      (* TODO: Announce the availability of admin messages *)

  method shutdown() =
    ContMap.iter
      (fun _ g -> Unixqueue.clear (Lazy.force esys) g)
      inactivated_conts;
    inactivated_conts <- ContMap.empty;
    ()

  method adjust sockserv sockctrl =
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
	    (Lazy.force logger) # log 
	      ~component:"netplex.controller"
	      ~level:`Debug
	      ~message:(Printf.sprintf
			  "Workload mng for %s: number_threads=%d active_threads=%d total_cap=%d used_cap=%d"
			  sockserv#name number_threads active_threads
			  total_cap used_cap)
	  );

	  (* Now decide... *)
	  if free_cap < config#min_free_job_capacity then (
	    let needed_cap = config#min_free_job_capacity - free_cap in
	    let needed_threads = 
	      (needed_cap - 1 ) / config#recommended_jobs_per_thread + 1 in
	    let needed_threads' =
	      max 0 ((min (all_threads+needed_threads) config#max_threads) -
		       all_threads) in
	    self # activate_containers sockctrl needed_threads'
	  )
	  else 
	    if free_cap > config#max_free_job_capacity then (
	      let exceeding_cap = free_cap - config#max_free_job_capacity in
	      let exceeding_threads = 
		exceeding_cap / config # recommended_jobs_per_thread in
	      if exceeding_threads > 0 then (
		(* Try to stop exceeding_thread containers. First look
                 * for idle containers.
                 *)
		let idle_conts =
		  List.filter
		    (fun (cid, s) ->
		       not (ContMap.mem cid inactivated_conts) &&
			 match s with
			   | `Accepting(0,_) -> true
			   | _ -> false)
		    container_state in
		let n = ref 0 in
		List.iter
		  (fun (cid,_) ->
		     if !n < exceeding_threads then (
		       incr n;
		       self # inactivate_container sockctrl cid
		     )
		  )
		  idle_conts
	      )
	    )

      | _ ->
	  ()

  method private activate_containers sockctrl n =
    let n = ref n in
    (* First re-activate the inactivated containers: *)
    let l = ref [] in
    ContMap.iter
      (fun cid g ->
	 if !n > 0 then (
	   l := cid :: !l;
	   Unixqueue.clear (Lazy.force esys) g;
	 )
      )
      inactivated_conts;
    List.iter
      (fun cid ->
	 inactivated_conts <- ContMap.remove cid inactivated_conts)
      !l;
    (* If needed, start further containers: *)
    if !n > 0 then
      sockctrl # start_containers !n


  method private inactivate_container sockctrl cid =
    let esys = Lazy.force esys in
    let g = Unixqueue.new_group esys in
    inactivated_conts <- ContMap.add cid g inactivated_conts;
    Unixqueue.once
      esys g (float config#inactivity_timeout)
      (fun () ->
	 inactivated_conts <- ContMap.remove cid inactivated_conts;
	 sockctrl # stop_containers [cid]
      )

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
