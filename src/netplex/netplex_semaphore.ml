(* $Id$ *)

open Netplex_types

let int64_incr v =
  v := Int64.succ !v

let int64_decr v =
  v := Int64.pred !v


let plugin : plugin =
  ( object(self)
      val mutable semaphores = Hashtbl.create 50
      val mutable containers = Hashtbl.create 50

      method program = 
	Netplex_ctrl_aux.program_Semaphore'V1

      method ctrl_added _ =
	()

      method ctrl_receive_call ctrl cid procname procarg_val =
	match procname with
	  | "ping" ->
	      Netplex_ctrl_aux._of_Semaphore'V1'ping'res ()

	  | "increment" ->
	      self # increment ctrl cid procarg_val

	  | "protected_increment" ->
	      self # protected_increment ctrl cid procarg_val

	  | "decrement" ->
	      self # decrement ctrl cid procarg_val

	  | _ ->
	      failwith "Unknown procedure"

      method private increment ctrl cid sem_name_val =
	let sem_name = Netplex_ctrl_aux._to_Semaphore'V1'increment'arg in
	let sem = self # get_sem ctrl sem_name in
	int64_incr sem;
	Netplex_ctrl_aux._of_Semaphore'V1'increment'res !sem

      method private protected_increment ctrl cid sem_name_val =
	let sem_name = 
	  Netplex_ctrl_aux._to_Semaphore'V1'protected_increment'arg in
	let sem = self # get_sem ctrl sem_name in
	int64_incr sem;
	let cont_sem = self # get_cont_sem cid sem_name true in
	int64_incr cont_sem;
	Netplex_ctrl_aux._of_Semaphore'V1'protected_increment'res !sem

      method private decrement ctrl cid sem_name_val =
	let sem_name = Netplex_ctrl_aux._to_Semaphore'V1'decrement'arg in
	let sem = self # get_sem ctrl sem_name in
	let do_decr = !sem > 0L in
	if do_decr then int64_decr sem;
	let cont_sem = self # get_cont_sem cid sem_name false in
	if do_decr then int64_decr cont_sem;
	Netplex_ctrl_aux._of_Semaphore'V1'decrement'res !sem

      method private get_sem ctrl sem_name =
	try Hashtbl.find semaphores (ctrl, sem_name)
	with Not_found -> 
	  let new_sem = ref 0L in
	  Hashtbl.add semaphores (ctrl, sem_name) new_sem;
	  new_sem

      method private get_cont_sem cid sem_name create_flag =
	let ht =
	  try Hashtbl.find containers cid
	  with Not_found -> 
	    let new_ht = Hashtbl.create 1 in
	    if create_flag then
	      Hashtbl.add containers cid new_ht;
	    new_ht in
	try
	  Hashtbl.find ht sem_name
	with Not_found ->
	  let new_sem = ref 0L in
	  Hashtbl.add ht sem_name new_sem;
	  new_sem

      method ctrl_container_finished ctrl cid =
	try
	  let ht = Hashtbl.find containers cid in  (* or Not_found *)
	  Hashtbl.iter
	    (fun sem_name value ->
	       let sem = self # get_sem ctrl sem_name in
	       sem := Int64.sub !sem !value;
	       if !sem < 0L then sem := 0L
	    )
	    ht;
	  Hashtbl.remove containers cid
	with
	  | Not_found -> ()
	  
     end
  )


let increment ?(protected=false) sem_name =
  let cont = Netplex_cenv.self_cont() in
  if protected then 
    Netplex_ctrl_aux._to_Semaphore'V1'protected_increment'res
      (cont # call_plugin plugin "protected_increment" 
	 (Netplex_ctrl_aux._of_Semaphore'V1'protected_increment'arg sem_name))
  else
    Netplex_ctrl_aux._to_Semaphore'V1'increment'res
      (cont # call_plugin plugin "increment" 
	 (Netplex_ctrl_aux._of_Semaphore'V1'increment'arg sem_name))


let decrement sem_name =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Semaphore'V1'decrement'res
    (cont # call_plugin plugin "decrement" 
       (Netplex_ctrl_aux._of_Semaphore'V1'decrement'arg sem_name))
