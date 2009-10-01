(* $Id$ *)

(* pset # dispose: we try to call it when there are no more operations in
   the queue. This is tested when
    - the resource is removed (and we are not waiting)
    - the group is cleared (and we are not waiting)
    - after every wait
   Note that we must not call [dispose] while [wait] is running!
 *)


open Unixqueue_util
open Printf

module Float = struct
  type t = float
  let compare : float -> float -> int = Pervasives.compare
end


module FloatMap = Map.Make(Float)


let min_key m = 
  let k = ref None in
  ( try
      FloatMap.iter
	(fun t _ -> k := Some t; raise Exit)
	m
    with Exit -> ()
  );
  match !k with
    | Some min -> min
    | None -> raise Not_found


let ops_until tmax m =
  (* Look into the FloatMap m, and return all ops for which
     t <= tmax
   *)
  let l = ref [] in
  ( try
      FloatMap.iter
	(fun t ops ->
	   if t > tmax then raise Exit;
	   l := (t,ops) :: !l
	)
	m
    with
      | Exit -> ()
  );
  !l


exception Term of Unixqueue_util.group
  (* Extra (Term g) is now the group termination event for g *)

exception Keep_alive
  (* Sometimes used to keep the event system alive *)

exception Exit_loop



let pset_set (pset:Netsys_pollset.pollset) fd (i,o,p) =
  if not i && not o && not p then
    pset # remove fd
  else
    pset # add fd (Netsys_posix.poll_req_events i o p)


let pset_find (pset:Netsys_pollset.pollset) fd =
  try Netsys_posix.poll_req_triple(pset#find fd)
  with
    | Not_found -> (false,false,false)



let op_of_event ev =
  match ev with
    | Unixqueue.Input_arrived(_,fd)    -> Unixqueue.Wait_in fd
    | Unixqueue.Output_readiness(_,fd) -> Unixqueue.Wait_out fd
    | Unixqueue.Out_of_band(_,fd)      -> Unixqueue.Wait_oob fd
    | Unixqueue.Timeout(_,op)          -> op
    | _ -> assert false


let while_locked mutex f =
  Netsys_oothr.serialize mutex f ()


let escape_lock mutex f =
  mutex # unlock();
  let r = 
    try f ()
    with e -> mutex # lock(); raise e in
  mutex # lock();
  r
 

(* A little encapsulation so we can easily identify handlers by Oo.id *)
class ohandler (h:handler) = 
object
  method run esys eq ev =
    h esys eq ev
end



class pollset_event_system (pset : Netsys_pollset.pollset) =
  let mtp = !Netsys_oothr.provider in
object(self)
  val mutable sys = 
    lazy (assert false)   (* initialized below *)
(*
  val mutable ops_of_group = 
    (Hashtbl.create 10 : (group, operation list) Hashtbl.t)
  -- would be for [clear] only
 *)
  val mutable tmo_of_op =
    (Hashtbl.create 10 : (operation, float * float * group) Hashtbl.t)
      (* first number: duration of timeout (or -1)
         second number: point in time (or -1)
       *)
  val mutable ops_of_tmo =
    (FloatMap.empty : operation list FloatMap.t)
  val mutable strong_op =
    (Hashtbl.create 10 : (operation, unit) Hashtbl.t)
      (* contains all non-weak ops *)

  val mutable aborting = false
  val mutable close_tab = (Hashtbl.create 10 : (Unix.file_descr, (group * (Unix.file_descr -> unit))) Hashtbl.t)
  val mutable abort_tab = ([] : (group * (group -> exn -> unit)) list)
  val mutable handlers = (Hashtbl.create 10 : (group, ohandler list) Hashtbl.t)
  val mutable handled_groups = 0
            (* the number of keys in [handlers] *)

  val mutable waiting = false

  val mutex = mtp # create_mutex()


  initializer (
    let equeue_sys = Equeue.create ~string_of_event self#source in
    sys <- lazy equeue_sys;
    ignore(Lazy.force sys)
  )


  method private source  _sys =
    (* locking: the lock is not held when called, because we are called back
       from Equeue
     *)
    assert(Lazy.force sys == _sys);

    let locked = ref true in   (* keep track of locking state *)
    mutex # lock();
    
    let t0 = Unix.gettimeofday() in
    let tmin = try min_key ops_of_tmo with Not_found -> (-1.0) in
    let delta = if tmin < 0.0 then (-1.0) else max (tmin -. t0) 0.0 in

    dlogr (fun () -> (
		   sprintf "t0 = %f" t0));

    let nothing_to_do =
      (* For this test only non-weak resources count, so... *)
      Hashtbl.length strong_op = 0 in

    let pset_events, have_eintr = 
      try
	if nothing_to_do then (
	  dlogr (fun () -> "nothing_to_do");
	  ([], false)
	)
	else (
	  dlogr (fun () -> (
			 let ops = 
			   Hashtbl.fold (fun op _ l -> op::l) tmo_of_op [] in
			 let op_str = 
			   String.concat ";" (List.map string_of_op ops) in
			 sprintf "wait tmo=%f ops=<%s>" delta op_str));
	  (* Reset the cancel bit immediately before calling [wait]. Any
             event added up to now is considered anyway by [wait] because
             our lock is still held. Any new event added after we unlock will
             set the cancel_wait flag, and cause the [wait] to exit (if it is
             still running).
	   *)
	  pset # cancel_wait false;
	  waiting <- true;
	  mutex # unlock();
	  locked := false;
	  (pset # wait delta, false)
	)
      with
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    dlogr (fun () -> "wait signals EINTR");
	    ([], true) 
	| e ->   
	    (* Usually from [wait], but one never knows... *)
	    if !locked then mutex#unlock();
	    waiting <- false;
	    raise e
    in

    waiting <- false;
    if not !locked then mutex # lock();
    locked := true;
    try  
      (* Catch exceptions and unlock *)

      dlogr (fun () -> (
		     sprintf "wait returns <%d pset events>" 
		       (List.length pset_events)));
		   
      let t1 = Unix.gettimeofday() in
      dlogr (fun () -> (sprintf "t1 = %f" t1));
      (* t1 is the reference for determining the timeouts *)

      (* while waiting somebody might have removed resouces, so ... *)
      if Hashtbl.length tmo_of_op = 0 then
	pset # dispose();

      let operations = 
	(* The possible operations *)
	List.flatten
	  (List.map
	     (fun (fd,ev_in,ev_out) ->
		(* Note that POLLHUP and POLLERR can also mean that we
                   have data to read/write!
		 *)
		let (in_rd,in_wr,in_pri) = 
		  Netsys_posix.poll_req_triple ev_in in
		let out_rd = 
		  Netsys_posix.poll_rd_result ev_out in 
		let out_wr = 
		  Netsys_posix.poll_wr_result ev_out in 
		let out_pri = 
		  Netsys_posix.poll_pri_result ev_out in 
		let out_hup = 
		  Netsys_posix.poll_hup_result ev_out in 
		let out_err = 
		  Netsys_posix.poll_err_result ev_out in 
		let have_input  = 
		  in_rd && (out_rd || out_hup || out_err) in
		let have_output =
		  in_wr && (out_wr || out_hup || out_err) in
		let have_pri =
		  in_pri && (out_pri || out_hup || out_err) in
		if have_input || have_output || have_pri then (
		  let e1 = if have_pri then [Unixqueue.Wait_oob fd] else [] in
		  let e2 = if have_input then [Unixqueue.Wait_in fd] else [] in
		  let e3 = if have_output then [Unixqueue.Wait_out fd] else [] in
		  e1 @ e2 @ e3
		)
		else
		  []
	     )
	     pset_events) in
      let events =
	(* The events corresponding to [operations] *)
	List.flatten
	  (List.map
	     (fun op -> self#events_of_op_wl op)
	     operations) in

      let ops_timed_out =
	ops_until t1 ops_of_tmo in
      (* Note: this _must_ include operations until <= t1 (not <t1), otherwise
         a timeout value of 0.0 won't work
       *)

      let timeout_events = 
	(* Generate timeout events for all ops in [tmo_of_op] that have timed
           out and that are not in [events]
	 *)
	List.flatten
	  (List.map
	     (fun (_, ops) ->
		let ops' =
		  List.filter (fun op -> not(List.mem op operations)) ops in
		List.flatten
		  (List.map 
		     (fun op -> self#events_of_op_wl op)
		     ops')
	     )
	     ops_timed_out) in
      
      dlogr(fun() -> (sprintf "delivering <%s>"
			(String.concat ";" 
			   (List.map 
			      string_of_event
			      (events @ timeout_events)))));

      (* deliver events *)
      List.iter (Equeue.add_event _sys) events;
      List.iter (Equeue.add_event _sys) timeout_events;
      if have_eintr then (
	dlogr (fun () -> "delivering Signal");
	Equeue.add_event _sys Unixqueue.Signal
      )
      else
	if events = [] && timeout_events = [] && not nothing_to_do then (
          (* Ensure we always add an event to keep the event loop running: *)
	  dlogr (fun () -> "delivering Keep_alive");
	  Equeue.add_event _sys (Unixqueue.Extra Keep_alive)
	);
    
      (* Update ops_of_tmo: *)
      List.iter
	(fun (t,_) ->
	   ops_of_tmo <- FloatMap.remove t ops_of_tmo
	)
	ops_timed_out;

      (* Set a new timeout for all delivered events:
         (Note that [pset] remains unchanged, because the set of watched
         resources remains unchanged.)
       *)
      List.iter
	(fun evlist ->
	   List.iter
	     (fun ev ->
		try
		  let op = op_of_event ev in
		  let (tmo,_,g) = Hashtbl.find tmo_of_op op in (* or Not_found *)
		  let is_strong = 
		    try Hashtbl.find strong_op op; true
		    with Not_found -> false in
		  self#sched_remove_wl op;
		  let t2 = if tmo < 0.0 then tmo else t1 +. tmo in
		  self#sched_add_wl g op tmo t2 is_strong
		with
		  | Not_found -> assert false
	     )
	     evlist)
	[ events; timeout_events ];

      mutex # unlock();
      locked := false

    with
      | e ->
	  (* exceptions are unexpected, but we want to make sure not to mess
             with locks
	   *)
	  if !locked then mutex # unlock();
	  raise e


  (* Note: suffix _wl = while locked *)

  method private events_of_op_wl op =
    try 
      let (_,_,g) = Hashtbl.find tmo_of_op op in (* or Not_found *)
      match op with
	| Unixqueue.Wait_in fd ->
	    [Unixqueue.Input_arrived(g,fd)]
	| Unixqueue.Wait_out fd ->
	    [Unixqueue.Output_readiness(g,fd)]
	| Unixqueue.Wait_oob fd ->
	    [Unixqueue.Out_of_band(g,fd)]
	| Unixqueue.Wait _ ->
	    [Unixqueue.Timeout(g,op)]
    with
      | Not_found ->
	  (* A "ghost event", i.e. there is no handler anymore
             for it, but wasn't deleted quickly enough from 
             pset
           *)
	  []


  method private sched_remove_wl op =
    try
      let tmo, t1, g = Hashtbl.find tmo_of_op op in  (* or Not_found *)
      dlogr(fun () -> (sprintf "sched_remove %s" (string_of_op op)));
      Hashtbl.remove tmo_of_op op;
      Hashtbl.remove strong_op op;
      let l_ops =
	if tmo >= 0.0 then
	  try FloatMap.find t1 ops_of_tmo with Not_found -> [] 
	else [] in
      let l_ops' =
	List.filter (fun op' -> op <> op') l_ops in
      if l_ops' = [] then
	ops_of_tmo <- FloatMap.remove t1 ops_of_tmo
      else
	ops_of_tmo <- FloatMap.add t1 l_ops' ops_of_tmo
    with
      | Not_found -> ()


  method private pset_remove_wl op =
    match op with
      | Wait_in fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (false,o,p)
      | Wait_out fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (i,false,p)
      | Wait_oob fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (i,o,false)
      | Wait _ ->
	  ()
	    

  method private sched_add_wl g op tmo t1 is_strong =
    dlogr(fun () -> (sprintf "sched_add %s tmo=%f t1=%f is_strong=%b"
		       (string_of_op op) tmo t1 is_strong));
    Hashtbl.add tmo_of_op op (tmo, t1, g);
    if is_strong then
      Hashtbl.add strong_op op ();
    let l_ops =
      try FloatMap.find t1 ops_of_tmo with Not_found -> [] in
    if tmo >= 0.0 then
      ops_of_tmo <- FloatMap.add t1 (op :: l_ops) ops_of_tmo


  method private pset_add_wl op =
    match op with
      | Wait_in fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (true,o,p)
      | Wait_out fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (i,true,p)
      | Wait_oob fd ->
	  let (i,o,p) = pset_find pset fd in
	  pset_set pset fd (i,o,true)
      | Wait _ ->
	  ()
		
  method exists_resource op =
    while_locked mutex
      (fun () -> self # exists_resource_wl op)

  method private exists_resource_wl op =
    Hashtbl.mem tmo_of_op op


  method private exists_descriptor_wl fd =
    self#exists_resource_wl (Unixqueue.Wait_in fd) ||
    self#exists_resource_wl (Unixqueue.Wait_out fd) ||
    self#exists_resource_wl (Unixqueue.Wait_oob fd)


  method add_resource g (op, tmo) =
    while_locked mutex
      (fun () ->
	 self # add_resource_wl g (op, tmo) true
      )

  method add_weak_resource g (op, tmo) =
    while_locked mutex
      (fun () ->
	 self # add_resource_wl g (op, tmo) false
      )


  method private add_resource_wl g (op, tmo) is_strong =
    if g # is_terminating then
      invalid_arg "Unixqueue.add_resource: the group is terminated";
    if not (Hashtbl.mem tmo_of_op op) then (
      self#pset_add_wl op;
      let t1 = if tmo < 0.0 then tmo else Unix.gettimeofday() +. tmo in
      self#sched_add_wl g op tmo t1 is_strong;
      (* Multi-threading: interrupt [wait] *)
      pset # cancel_wait true;
      (* CHECK: In the select-based impl we add Keep_alive to equeue.
              This idea (probably): If [wait] is about to return, and the
              queue becomes empty, the whole esys terminates. The Keep_alive
              prevents that. 
              My current thinking is that this delays the race condition
              a bit, but does not prevent it from happening
       *)
    )
      (* Note: The second addition of a resource is silently ignored...
              Maybe this should be fixed, so that the timeout can be lowered
       *)


  method remove_resource g op =
    while_locked mutex
      (fun () ->
	 if g # is_terminating then
	   invalid_arg "remove_resource: the group is terminated";
	 let _, t1, g_found = Hashtbl.find tmo_of_op op in
	 if g <> g_found then
	   failwith "remove_resource: descriptor belongs to different group";
	 self#sched_remove_wl op;
	 self#pset_remove_wl op;

	 if not waiting && Hashtbl.length tmo_of_op = 0 then
	   pset # dispose();

	 pset # cancel_wait true;    (* interrupt [wait] *)
	 (* is there a close action ? *)
	 let fd_opt =
	   match op with
             | Wait_in  d -> Some d
             | Wait_out d -> Some d
             | Wait_oob d -> Some d
             | Wait _      -> None in
	 match fd_opt with
	   | Some fd ->
               if not aborting then (
		 let action = 
		   try Some (snd(Hashtbl.find close_tab fd)) 
		   with Not_found -> None in
		 match action with
		   | Some a ->
                       (* any open resource? *)
		       (* FIXME MT: We don't know yet whether fd can be closed.
                          This shouldn't be done before [wait] returns.
                        *)
                       if not (self#exists_descriptor_wl fd) then (
			 dlogr (fun () -> (sprintf "remove_resource <running close action for fd %s>"
                                              (string_of_fd fd)));
			 Hashtbl.remove close_tab fd;
			 escape_lock mutex (fun () -> a fd);
                       )
		   | None ->
                       ()
               )
	   | None -> ()
      )


  method new_group () =
    new group_object

  method new_wait_id () =
    new wait_object


  method add_close_action g (d,a) =
    while_locked mutex
      (fun () ->
	 if g # is_terminating then
	   invalid_arg "add_close_action: the group is terminated";
	 (* CHECK: Maybe we should fail if g is a different group than
          * the existing group
	  *)
	 if self#exists_descriptor_wl d then
	   Hashtbl.replace close_tab d (g,a)
             (* There can be only one close action.
              * TODO: Rename to set_close_action
              *)
	 else
	   failwith "add_close_action"
      )


  method add_abort_action g a =
    while_locked mutex
      (fun () ->
	 if g # is_terminating then
	   invalid_arg "add_abort_action: the group is terminated";
	 abort_tab <- (g,a) :: abort_tab
      )

  method add_event e =
    while_locked mutex
      (fun () -> self # add_event_wl e)


  method private add_event_wl e =
    Equeue.add_event (Lazy.force sys) e;
    pset # cancel_wait true
      (* Set the cancel bit, so that any pending [wait] is interrupted *)


  method private uq_handler (esys : event Equeue.t) ev =
    (* locking: it is assumed that we do not have the lock when uq_handler
       is called. It is a callback from Equeue
     *)
    (* The single Unixqueue handler. For all added (sub) handlers, uq_handler
     * gets the events, and delivers them
     *)

    let terminate_handler_wl g h =
      dlogr
	(fun() -> 
	   (sprintf "uq_handler <terminating handler group %d, handler %d>"
              (Oo.id g) (Oo.id h)));
      let hlist =
        try Hashtbl.find handlers g with Not_found -> [] in
      let hlist' =
        List.filter (fun h' -> h' <> h) hlist in
      if hlist' = [] then (
        Hashtbl.remove handlers g;
        handled_groups <- handled_groups - 1;
        if handled_groups = 0 then (
	  dlogr (fun () -> "uq_handler <self-terminating>");
          raise Equeue.Terminate  (* delete uq_handler from esys *)
	)
      ) else (
        Hashtbl.replace handlers g hlist'
      )
    in

    let rec forward_event_to g (hlist : ohandler list) =
      (* The caller does not have the lock when this fn is called! *)
      match hlist with
          [] ->
	    dlogr (fun () -> "uq_handler <empty list>");
            raise Equeue.Reject
        | h :: hlist' ->
            ( try
                (* Note: ues _must not_ be locked now *)
		dlogr
		  (fun () -> 
		     (sprintf 
			"uq_handler <invoke handler group %d, handler %d>"
			(Oo.id g) (Oo.id h)));
                h#run (self :> event_system) esys ev;
		dlogr
		  (fun () -> 
		     (sprintf 
			"uq_handler <invoke_success handler group %d, handler %d>"
			(Oo.id g) (Oo.id h)));
              with
                  Equeue.Reject ->
                    forward_event_to g hlist'
                | Equeue.Terminate ->
                    (* Terminate only this handler. *)
                    while_locked mutex
		      (fun () -> terminate_handler_wl g h)
                    (* Any error exceptions simply fall through. Equeue will
                     * catch them, and will add the event to the error queue
                     *)
            )
    in

    let forward_event g =
      (* The caller does not have the lock when this fn is called! *)
      dlogr
	(fun () ->
	   (sprintf "uq_handler <forward_event group %d>" (Oo.id g)));
      let hlist =
        while_locked mutex
	  (fun () -> 
             try Hashtbl.find handlers g with Not_found -> []) in
      forward_event_to g hlist
    in

    let forward_event_to_all() =
      (* The caller does not have the lock when this fn is called! *)
      let hlist_all =
	while_locked mutex
	  (fun () -> 
             Hashtbl.fold (fun g hlist l -> (g,hlist) :: l) handlers []) in
      try
        List.iter
          (fun (g,hlist) ->
             try
               forward_event_to g hlist;
               raise Exit_loop   (* event is delivered, so exit iteration *)
             with
                 (* event is rejected: try next group *)
                 Equeue.Reject -> ()
          )
          hlist_all;
        raise Equeue.Reject (* no handler has accepted the event, so reject *)
      with
          Exit_loop -> ()
    in

    dlogr
      (fun () ->
	 (sprintf "uq_handler <event %s>"
	    (string_of_event ev)));

    match ev with
      | Extra (Term g) ->
          (* Terminate all handlers of group g *)
	  while_locked mutex
	    (fun () ->
               if Hashtbl.mem handlers g then (
		 dlogr
		   (fun () ->
		      (sprintf "uq_handler <terminating group %d>" (Oo.id g)));
		 Hashtbl.remove handlers g;
		 handled_groups <- handled_groups - 1;
		 if handled_groups = 0 then (
		   dlogr (fun () -> "uq_handler <self-terminating>");
		   raise Equeue.Terminate  (* delete uq_handler from esys *)
		 )
               )
               else raise Equeue.Reject (* strange, should not happen *)
	    )
      | Extra Keep_alive ->
          raise Equeue.Reject
      | Input_arrived(g,_) ->
          if g # is_terminating then raise Equeue.Reject;
          forward_event g;
      | Output_readiness(g,_) ->
          if g # is_terminating then raise Equeue.Reject;
          forward_event g;
      | Out_of_band(g,_) ->
          if g # is_terminating then raise Equeue.Reject;
          forward_event g;
      | Timeout(g,_) ->
          if g # is_terminating then raise Equeue.Reject;
          forward_event g;
      | Signal ->
          forward_event_to_all();
      | Extra x ->
          forward_event_to_all();

  method private equeue_add_handler () =
    Equeue.add_handler (Lazy.force sys) self#uq_handler

  (* CHECK: There is a small difference between Equeue.add_handler and
   * this add_handler: Here, the handler is immediately active (if
   * uq_handler is already active). Can this lead to problems?
   *)

  method add_handler g h =
    while_locked mutex
      (fun () ->
	 let oh = new ohandler h in
	 dlogr
	   (fun () ->
	      (sprintf
		 "add_handler <group %d, handler %d>" (Oo.id g) (Oo.id oh)));
	 
	 if g # is_terminating then
	   invalid_arg "Unixqueue.add_handler: the group is terminated";
	 
	 ( try
             let old_handlers = Hashtbl.find handlers g in
             Hashtbl.replace handlers g (oh :: old_handlers)
	   with
             | Not_found ->
		 (* The group g is new *)
		 Hashtbl.add handlers g [oh];
		 handled_groups <- handled_groups + 1;
		 if handled_groups = 1 then
		   self#equeue_add_handler ()
	 )
      )

  method clear g =
    while_locked mutex
      (fun () -> self # clear_wl g)


  method private clear_wl g =
    dlogr (fun () -> (sprintf "clear <group %d>" (Oo.id g)));
    
    (* Set that g is terminating now: *)
    g # terminate();
    
    (* (i) delete all resources of g: *)
    let ops =
      Hashtbl.fold
	(fun op (_,_,g') l -> if g=g' then op::l else l)
	tmo_of_op
	[] in
    List.iter
      self#sched_remove_wl
      ops;
    List.iter
      self#pset_remove_wl
      ops;

    (* (ii) delete all handlers of g: *)
    self#add_event_wl (Extra (Term g));
    (* side effect: we also interrupt [wait] *)

    (* (iii) delete special actions of g: *)
    let to_remove =   (* remove from close_tab *)
      Hashtbl.fold
        (fun d (g',_) l -> if g = g' then d :: l else l) close_tab [] in
    List.iter
      (Hashtbl.remove close_tab) to_remove;
    
    abort_tab <- List.filter (fun (g',_) -> g<>g') abort_tab;

    (* Note: the Term event isn't caught after all handlers have been
     * deleted. The Equeue module simply discards events that are not
     * handled.
     *)

    if not waiting && Hashtbl.length tmo_of_op = 0 then
      pset # dispose();


  method private abort g ex =
    (* caller doesn't have the lock *)
    (* Note: If g has been terminated, the abort action is removed. So
     * we will never find here one.
     *)
    dlogr (fun () -> (sprintf "abort <group %d, exception %s>"
                         (Oo.id g) (Netexn.to_string ex)));
    let action =
      while_locked mutex
	(fun () ->
	   try Some (List.assoc g abort_tab) with Not_found -> None) in
    match action with
      | Some a ->
          begin
            dlogr (fun () -> "abort <running abort action>");
            let mistake = ref None in
	    while_locked mutex 
	      (fun () -> aborting <- true);
            begin try
              a g ex;
            with
              | any ->
                  mistake := Some any (* Wow *)
            end;
	    while_locked mutex 
	      (fun () ->
		 self#clear_wl g;
		 aborting <- false
	      );
            match !mistake with
              | None -> ()
              | Some m ->
                  dlogr (fun () -> (sprintf "abort <propagating exception %s>"
                                       (Netexn.to_string m)));
                  raise m
          end
      | None ->
          ()

  method run () =
    (* caller doesn't have the lock *)
    let continue = ref true in
    try
      while !continue do
        continue := false;
        try
          Equeue.run (Lazy.force sys);
        with
          | Abort (g,an_exception) ->
              begin
                match an_exception with
                  | (Equeue.Reject|Equeue.Terminate) ->
                      (* A serious programming error: *)
                      failwith "Caught 'Abort' exception with Reject or Terminate exception as argument; this is a programming error"
                  | Abort(_,_) ->
                      failwith "Caught 'Abort' exception with an 'Abort' exception as argument; this is a programming error"
                  | _ -> ()
              end;
              self#abort g an_exception;
              continue := true
      done;
    with
      | error ->
          raise error

  method is_running =
    Equeue.is_running (Lazy.force sys)

end


let pollset_event_system pset = 
  (new pollset_event_system pset :> Unixqueue_util.event_system)

