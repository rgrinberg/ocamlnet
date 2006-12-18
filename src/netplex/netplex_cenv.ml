(* $Id$ *)

open Netplex_types

exception Not_in_container_thread

let cont_of_thread = Hashtbl.create 10

let register_par par =
  if not (Hashtbl.mem cont_of_thread par#ptype) then (
    let (lock, unlock) = par # create_mem_mutex() in
    let m = Hashtbl.create 10 in
    Hashtbl.add cont_of_thread par#ptype (lock, unlock, par, m)
  )
;;


let register_cont cont thread =
  if thread#ptype <> `Controller_attached then (
    let (lock, unlock, par, m) =
      try Hashtbl.find cont_of_thread thread#ptype
      with Not_found -> 
	failwith "Netplex_cenv.register_cont: Unknown parallelizer type" in
    
    lock();
    Hashtbl.replace m thread#sys_id cont;
    unlock()
  )
;;


let unregister_cont cont thread =
  if thread#ptype <> `Controller_attached then (
    let (lock, unlock, par, m) =
      try Hashtbl.find cont_of_thread thread#ptype
      with Not_found -> 
	failwith "Netplex_cenv.unregister_cont: Unknown parallelizer type" in
    
    lock();
    Hashtbl.remove m thread#sys_id;
    unlock()
  )
;;


exception Found of container

let self_cont() =
  (* We do not know the parallelizer, so simply try them one after the other *)
  try
    Hashtbl.iter
      (fun ptype (lock, unlock, par, m) ->
	 let my_sys_id = par # current_sys_id in
	 lock();
	 try
	   let cont = Hashtbl.find m my_sys_id in
	   unlock();
	   raise(Found cont)
	 with
	   | Not_found ->
	       unlock();
      )
      cont_of_thread;
    raise Not_in_container_thread
  with
    | Found cont -> cont
;;


let log level msg =
  let cont = self_cont() in
  cont # log level msg

let logf level fmt =
  Printf.ksprintf (log level) fmt
