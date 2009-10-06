(* $Id$ *)

(* POSIX semaphores: A better implementation would use POSIX semaphores.
   For each Netplex semaphore <name> we define
   - One POSIX semaphore <prefix>_<name>
   - One POSIX semaphore <prefix>_<name>_isprotected (only for
     storing the protected attribute)
   - If the semaphore is protected, another set of counters
     <prefix>_<name>_<container> for every container. These cannot be
     semaphores, because negative values are possible. A shared memory
     segment is possible (memory cells are managed by the controller,
     and RPC calls are used for this).

   The Netplex operations map nicely to POSIX operations:
   - Netplex create: sem_open with O_CREAT and O_EXCL
   - Netplex increment: sem_post. If the semaphore is protected, the
     container-specific counter is also incremented (and created with
     value 0 if not existing).
   - Netplex decrement w/o wait: sem_trywait. If the decrement is successful,
     the container-specific counter is also decremented.
   - Netplex decrement with wait: sem_wait. If the decrement is successful,
     the container-specific counter is also decremented.

   If the container crashes, the controller looks at the container-specific
   counter, and calls sem_post or sem_trywait as often as the counter says:
   sem_post for negative values, and sem_trywait for positive values.

 *)


open Netplex_types

let int64_incr v =
  v := Int64.succ !v

let int64_decr v =
  v := Int64.pred !v


let release = ref (fun () -> ())


let plugin : plugin =
  ( object(self)
      val mutable semaphores = Hashtbl.create 50
      val mutable containers = Hashtbl.create 50

      initializer (
	release :=
	  (fun () -> 
	     semaphores <- Hashtbl.create 1;
	     containers <- Hashtbl.create 1
	  )
      )

      method program = 
	Netplex_ctrl_aux.program_Semaphore'V1

      method ctrl_added _ =
	()

      method ctrl_unplugged ctrl =
	List.iter
	  (fun cid ->
	     self # ctrl_container_finished ctrl cid false
	  )
	  ctrl#containers

      method ctrl_receive_call ctrl cid procname procarg_val reply =
	match procname with
	  | "ping" ->
	      reply(Some(Netplex_ctrl_aux._of_Semaphore'V1'ping'res ()))
		
	  | "increment" ->
	      let sem_name = 
		Netplex_ctrl_aux._to_Semaphore'V1'increment'arg procarg_val in
	      let r = 
		self # increment ctrl cid sem_name in
	      reply(Some(Netplex_ctrl_aux._of_Semaphore'V1'increment'res r))
		    
	  | "decrement" ->
	      let proc_reply v =
	      	let v' = 
		  Netplex_ctrl_aux._of_Semaphore'V1'decrement'res v in
		reply(Some v') in
	      let (sem_name, wait_flag) =
		Netplex_ctrl_aux._to_Semaphore'V1'decrement'arg procarg_val in
	      self # decrement_async ctrl cid sem_name wait_flag proc_reply

	  | "get" ->
	      let sem_name =
		Netplex_ctrl_aux._to_Semaphore'V1'get'arg procarg_val in
	      let (sem, _, _) = self # get_sem ctrl sem_name in
	      reply(Some(Netplex_ctrl_aux._of_Semaphore'V1'get'res !sem))

	  | "create" ->
	      let (sem_name, init_val, protected) = 
		Netplex_ctrl_aux._to_Semaphore'V1'create'arg procarg_val in
	      let r =
		snd(self # get_or_create_sem
		      ctrl sem_name init_val protected) in
	      reply(Some(Netplex_ctrl_aux._of_Semaphore'V1'create'res r))
		    
	  | _ ->
	      failwith "Unknown procedure"

      method private increment ctrl cid sem_name =
	let (sem, protected, waiting) = self # get_sem ctrl sem_name in
	let cont_sem = self # get_cont_sem cid sem_name protected in
	int64_incr sem;
	int64_incr cont_sem;
	if !sem = 1L then (
	  if not (Queue.is_empty waiting) then (
	    let waiting_reply = Queue.take waiting in
	    self#really_decrement sem cont_sem protected;
	    waiting_reply 0L
	  )
	);
	!sem

      method private decrement_async ctrl cid sem_name wait_flag reply =
	let (sem, protected, waiting) = self # get_sem ctrl sem_name in
	let cont_sem = self # get_cont_sem cid sem_name protected in
	if !sem > 0L then (
	  self#really_decrement sem cont_sem protected;
	  reply !sem
	)
	else (
	  if wait_flag then
	    Queue.push reply waiting
	  else 
	    reply (-1L)
	)

      method private really_decrement sem cont_sem protected =
	assert(!sem > 0L);
	int64_decr sem;
	if protected then
	  int64_decr cont_sem

      method private get_or_create_sem ctrl sem_name init_val protected =
	try 
	  (Hashtbl.find semaphores (ctrl, sem_name), false)
	with Not_found -> 
	  let waiting = Queue.create() in
	  let new_sem = (ref init_val, protected, waiting) in
	  Hashtbl.add semaphores (ctrl, sem_name) new_sem;
	  (new_sem, true)

      method private get_sem ctrl sem_name =
	fst(self # get_or_create_sem ctrl sem_name 0L true)

      method private get_cont_sem cid sem_name protected =
	if protected then (
	  let ht =
	    try Hashtbl.find containers cid
	    with Not_found -> 
	      let new_ht = Hashtbl.create 1 in
	      Hashtbl.add containers cid new_ht;
	      new_ht in
	  try
	    Hashtbl.find ht sem_name
	  with Not_found ->
	    let new_sem = ref 0L in
	    Hashtbl.add ht sem_name new_sem;
	    new_sem
	)
	else (ref 0L)

      method ctrl_container_finished ctrl cid _ =
	try
	  let ht = Hashtbl.find containers cid in  (* or Not_found *)
	  Hashtbl.iter
	    (fun sem_name value ->
	       let (sem, _, waiting) = self # get_sem ctrl sem_name in
	       let zero_flag = (!sem = 0L) in
	       sem := Int64.sub !sem !value;
	       if !sem < 0L then sem := 0L;
	       if zero_flag && !sem > 0L then (
		 let v = ref !sem in
		 while not(Queue.is_empty waiting) && !v > 0L do
		   let waiting_reply = Queue.take waiting in
		   self#really_decrement sem (ref 0L) false;
		   waiting_reply 0L;
		   int64_decr v
		 done
	       )
	    )
	    ht;
	  Hashtbl.remove containers cid
	with
	  | Not_found -> ()
	  
     end
  )

let () =
  (* Release memory after [fork]: *)
  Netsys_posix.register_post_fork_handler
    (object
       method name = "Netplex_semaphore"
       method run () = !release()
     end
    )


let increment sem_name =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Semaphore'V1'increment'res
    (cont # call_plugin plugin "increment" 
       (Netplex_ctrl_aux._of_Semaphore'V1'increment'arg sem_name))


let decrement ?(wait=false) sem_name =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Semaphore'V1'decrement'res
    (cont # call_plugin plugin "decrement" 
       (Netplex_ctrl_aux._of_Semaphore'V1'decrement'arg (sem_name, wait)))


let get sem_name =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Semaphore'V1'get'res
    (cont # call_plugin plugin "get" 
       (Netplex_ctrl_aux._of_Semaphore'V1'get'arg sem_name))


let create ?(protected=false) sem_name init_val =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Semaphore'V1'create'res
    (cont # call_plugin plugin "create" 
       (Netplex_ctrl_aux._of_Semaphore'V1'create'arg 
	  (sem_name, init_val, protected)))
