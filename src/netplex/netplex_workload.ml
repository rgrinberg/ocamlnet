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

end


let create_constant_workload_manager n =
  new constant_workload_manager n


class type dynamic_workload_config =
object
  method max_jobs_per_thread : int
  method min_free_job_capacity : int
  method max_free_job_capacity : int
  method max_threads : int
end


class dynamic_workload_manager config : workload_manager =
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
	  (* Determine total capacity of the current Netplex state: *)
	  let container_state = sockctrl # container_state in
	  let number_threads = List.length container_state in
	  let active_threads =
	    List.length
	      (List.filter 
		 (fun (_,s) -> s <> `Shutting_down) 
		 container_state) in
	  let total_cap = config#max_jobs_per_thread * active_threads in
	  (* Determine used capacity: *)
	  let used_cap =
	    List.fold_left
	      (fun acc (_,s) ->
		 match s with
		   | `Accepting(n,_) -> 
		       acc + (min n config#max_jobs_per_thread)
		   | `Busy -> acc + config#max_jobs_per_thread
		   | `Starting -> acc
		   | `Shutting_down -> acc)
	      0
	      container_state in
	  (* Free capacity: *)
	  let free_cap = total_cap - used_cap in
	  (* Now decide... *)
	  if free_cap < config#min_free_job_capacity then (
	    let needed_cap = config#min_free_job_capacity - free_cap in
	    let needed_threads = 
	      (needed_cap - 1 ) / config#max_jobs_per_thread + 1 in
	    let needed_threads' =
	      max 0 ((min (number_threads+needed_threads) config#max_threads) -
		       number_threads) in
	    sockctrl # start_containers needed_threads'
	  )
	  else 
	    if free_cap > config#max_free_job_capacity then (
	      let exceeding_cap = free_cap - config#max_free_job_capacity in
	      if exceeding_cap >= config # max_jobs_per_thread then (
		(* Find the container with the lowest number of jobs: *)
		let (best_id, best_n) =
		  List.fold_left
		    (fun (best_id, best_n) (id, s) ->
		       match s with
			 | `Accepting(n,_) -> 
			     if n < best_n then (id, n) else (best_id, best_n)
			 | _ ->
			     (best_id, best_n)
		    )
		    ( match List.hd container_state with
			| (id, `Accepting(n,_)) -> (id, n)
			| (id, `Busy) -> (id, config # max_jobs_per_thread)
			| (id, _) -> (id, max_int)
		    )
		    container_state in
		if best_n < max_int then
		  sockctrl # stop_containers [best_id]
	      )
	    )

      | _ ->
	  ()
end


let create_dynamic_workload_manager config =
  new dynamic_workload_manager config
