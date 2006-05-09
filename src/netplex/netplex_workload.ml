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
