(* $Id$ *)

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




let pset_set (pset:Netsys_pollset.pollset) fd (i,o,p) =
  if not i && not o && not p then
    pset # remove fd
  else
    pset # add fd (Netsys.poll_in_events i o p)


let pset_find (pset:Netsys_pollset.pollset) fd =
  try Netsys.poll_in_triple(pset#find fd)
  with
    | Not_found -> (false,false,false)


class pollset_event_system (pset : Netsys_pollset.pollset) =
object(self)
  val mutable sys = 
    lazy (assert false)   (* initialized below *)
  val mutable handlers = 
    (Hashtbl.create 10 : (group, handler list) Hashtbl.t)
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

  val mutable aborting = false
  val mutable close_tab = (Hashtbl.create 10 : (Unix.file_descr, (group * (Unix.file_descr -> unit))) Hashtbl.t)
  val mutable abort_tab = ([] : (group * (group -> exn -> unit)) list)
  val mutable handlers = (Hashtbl.create 10 : (group, handler list) Hashtbl.t)
  val mutable handled_groups = 0
            (* the number of keys in [handlers] *)


  initializer (
    let equeue_sys = Equeue.create ~string_of_event self#source in
    sys <- lazy equeue_sys;
    ignore(Lazy.force sys)
  )


  method private source _sys =
    assert(Lazy.force sys == _sys);
    
    let t0 = Unix.gettimeofday() in
    let tmin = try min_key ops_of_tmo with Not_found -> (-1.0) in
    let delta = if tmin < 0.0 then (-1.0) else max (tmin -. t0) 0.0 in

    debug_print (lazy (
		   sprintf "t0 = %f" t0));

    let nothing_to_do =
      Hashtbl.length tmo_of_op = 0 in

    let pset_events, have_eintr = 
      try
	if nothing_to_do then (
	  debug_print (lazy "nothing_to_do");
	  ([], false)
	)
	else (
	  debug_print (lazy (sprintf "wait tmo=%f" delta));
	  (pset # wait delta, false)
	)
      with
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    debug_print (lazy "wait signals EINTR");
	    ([], true) 
    in

    debug_print (lazy (
		   sprintf "wait returns <%d pset events>" 
		     (List.length pset_events)));
		   
    let t1 = Unix.gettimeofday() in
    debug_print (lazy (sprintf "t1 = %f" t1));
    (* t1 is the reference for determining the timeouts *)

    let operations = 
      (* The possible operations *)
      List.flatten
	(List.map
	   (fun (fd,ev_in,ev_out) ->
	      let have_input  = Netsys.poll_input_result ev_out in 
	      let have_output = Netsys.poll_output_result ev_out in 
	      let have_pri    = Netsys.poll_priority_result ev_out in 
	      if have_input || have_output || have_pri then (
		let e1 =
		  if have_pri then
		    [Unixqueue.Wait_oob fd]
		  else if have_input then
		    [Unixqueue.Wait_in fd]
		  else [] in
		let e2 =
		  if have_output then
		    [Unixqueue.Wait_out fd]
		  else [] in
		e1 @ e2
	      )
	      else
		[]
	   )
	   pset_events) in
    let events =
      (* The events corresponding to [operations] *)
      List.flatten
	(List.map
	   (fun op -> self#events_of_op op)
	   operations) in

    let ops_timed_out =
      ops_until t1 ops_of_tmo in

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
		   (fun op -> self#events_of_op op)
		   ops')
	   )
	ops_timed_out) in

    debug_print(lazy (sprintf "delivering <%s>"
			(String.concat ";" 
			   (List.map 
			      string_of_event
			      (events @ timeout_events)))));

    (* deliver events *)
    List.iter (Equeue.add_event _sys) events;
    List.iter (Equeue.add_event _sys) timeout_events;
    if have_eintr then (
      debug_print (lazy "delivering Signal");
      Equeue.add_event _sys Unixqueue.Signal
    )
    else
      if events = [] && timeout_events = [] && not nothing_to_do then (
        (* Ensure we always add an event to keep the event loop running: *)
	debug_print (lazy "delivering Keep_alive");
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
		let op = self#op_of_event ev in
		let (tmo,_,g) = Hashtbl.find tmo_of_op op in (* or Not_found *)
		self#sched_remove op;
		let t2 = if tmo < 0.0 then tmo else t1 +. tmo in
		self#sched_add g op tmo t2
	      with
		| Not_found -> assert false
	   )
	   evlist)
      [ events; timeout_events ];

    ()


  method private events_of_op op =
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
	    assert false
    with
      | Not_found ->
	  (* A "ghost event", i.e. there is no handler anymore
             for it, but wasn't deleted quickly enough from 
             pset
           *)
	  []


  method private op_of_event ev =
    match ev with
      | Unixqueue.Input_arrived(_,fd)    -> Unixqueue.Wait_in fd
      | Unixqueue.Output_readiness(_,fd) -> Unixqueue.Wait_out fd
      | Unixqueue.Out_of_band(_,fd)      -> Unixqueue.Wait_oob fd
      | _ -> assert false


  method private sched_remove op =
    try
      let tmo, t1, g = Hashtbl.find tmo_of_op op in  (* or Not_found *)
      debug_print(lazy (sprintf "sched_remove %s" (string_of_op op)));
      Hashtbl.remove tmo_of_op op;
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


  method private pset_remove op =
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
	    

  method private sched_add g op tmo t1 =
    debug_print(lazy (sprintf "sched_add %s tmo=%f t1=%f"
			(string_of_op op) tmo t1));
    Hashtbl.add tmo_of_op op (tmo, t1, g);
    let l_ops =
      try FloatMap.find t1 ops_of_tmo with Not_found -> [] in
    if tmo >= 0.0 then
      ops_of_tmo <- FloatMap.add t1 (op :: l_ops) ops_of_tmo


  method private pset_add op =
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
    Hashtbl.mem tmo_of_op op


  method private exists_descriptor fd =
    self#exists_resource (Unixqueue.Wait_in fd) ||
    self#exists_resource (Unixqueue.Wait_out fd) ||
    self#exists_resource (Unixqueue.Wait_oob fd)


  method add_resource g (op, tmo) =
    if g # is_terminating then
      invalid_arg "Unixqueue.add_resource: the group is terminated";
    if not (Hashtbl.mem tmo_of_op op) then (
      self#pset_add op;
      let t1 = if tmo < 0.0 then tmo else Unix.gettimeofday() +. tmo in
      self#sched_add g op tmo t1
    )
    (* Note: The second addition of a resource is silently ignored...
       Maybe this should be fixed, so that the timeout can be lowered
     *)


  method remove_resource g op =
    if g # is_terminating then
      invalid_arg "remove_resource: the group is terminated";
    let _, t1, g_found = Hashtbl.find tmo_of_op op in
    if g <> g_found then
      failwith "remove_resource: descriptor belongs to different group";
    self#sched_remove op;
    self#pset_remove op;
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
                  if not (self#exists_descriptor fd) then (
                    debug_print (lazy (sprintf "remove_resource <running close action for fd %s>"
                                         (string_of_fd fd)));
                    a fd;
                    Hashtbl.remove close_tab fd
                  )
              | None ->
                  ()
          )
      | None -> ()


  method debug_log ?label msg =
    if !debug_mode then
      prerr_endline("Unixqueue debug log: " ^
                    ( match label with
                          Some l -> l
                        | None -> "anonymous" ) ^
                    " <" ^ msg ^ ">")

  method exn_log ?(suppressed = false) ?(to_string = Printexc.to_string)
                 ?label e =
    if !debug_mode then
      let msg =
        if suppressed then
          "Suppressed exn " ^ to_string e
        else
          "Exn " ^ to_string e in
      self # debug_log ?label msg

  method new_group () =
    new group_object

  method new_wait_id () =
    new wait_object


  method add_close_action g (d,a) =
    if g # is_terminating then
      invalid_arg "add_close_action: the group is terminated";
    (* CHECK: Maybe we should fail if g is a different group than
     * the existing group
     *)
    if self#exists_descriptor d then
      Hashtbl.replace close_tab d (g,a)
        (* There can be only one close action.
         * TODO: Rename to set_close_action
         *)
    else
      failwith "add_close_action"


  method add_abort_action g a =
    if g # is_terminating then
      invalid_arg "add_abort_action: the group is terminated";
    abort_tab <- (g,a) :: abort_tab
      

  method add_event e =
    Equeue.add_event (Lazy.force sys) e
      (* self#wake_up false *)

  method private uq_handler (esys : event Equeue.t) ev =
    (* The single Unixqueue handler. For all added (sub) handlers, uq_handler
     * gets the events, and delivers them
     *)

    let terminate_handler_nolock g h =
      debug_print (lazy (sprintf "uq_handler <terminating handler group %d>"
                           (Oo.id g)));
      let hlist =
        try Hashtbl.find handlers g with Not_found -> [] in
      let hlist' =
        List.filter (fun h' -> h' != h) hlist in
      if hlist' = [] then (
        Hashtbl.remove handlers g;
        handled_groups <- handled_groups - 1;
        if handled_groups = 0 then
          raise Equeue.Terminate  (* delete uq_handler from esys *)
      ) else (
        Hashtbl.replace handlers g hlist'
      )
    in

    let rec forward_event_to g (hlist : handler list) =
      match hlist with
          [] ->
            raise Equeue.Reject
        | h :: hlist' ->
            ( try
                (* Note: ues _must not_ be locked now *)
                h (self :> event_system) esys ev
              with
                  Equeue.Reject ->
                    forward_event_to g hlist'
                | Equeue.Terminate ->
                    (* Terminate only this handler. *)
                    (* self#protect! *) terminate_handler_nolock g h
                    (* Any error exceptions simply fall through. Equeue will
                     * catch them, and will add the event to the error queue
                     *)
            )
    in

    let forward_event g =
      let hlist =
        (* self#protect! *)
          (try Hashtbl.find handlers g with Not_found -> []) in
      forward_event_to g hlist
    in

    let forward_event_to_all() =
      let hlist_all =
        (* self#protect! *)
        Hashtbl.fold (fun g hlist l -> (g,hlist) :: l) handlers []
      in
      try
        List.iter
          (fun (g,hlist) ->
             try
               forward_event_to g hlist;
               raise Exit   (* event is delivered, so exit iteration *)
             with
                 (* event is rejected: try next group *)
                 Equeue.Reject -> ()
          )
          hlist_all;
        raise Equeue.Reject (* no handler has accepted the event, so reject *)
      with
          Exit -> ()
    in

    match ev with
      | Extra (Term g) ->
          (* Terminate all handlers of group g *)
          (* self#protect! *)
          if Hashtbl.mem handlers g then (
            Hashtbl.remove handlers g;
            handled_groups <- handled_groups - 1;
            if handled_groups = 0 then
              raise Equeue.Terminate  (* delete uq_handler from esys *)
          )
          else raise Equeue.Reject (* strange, should not happen *)
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
      (* It is not necessary to call wake_up *)

  (* CHECK: There is a small difference between Equeue.add_handler and
   * this add_handler: Here, the handler is immediately active (if
   * uq_handler is already active). Can this lead to problems?
   *)

  method add_handler g h =
    debug_print (lazy (sprintf "add_handler <group %d>" (Oo.id g)));

    if g # is_terminating then
      invalid_arg "Unixqueue.add_handler: the group is terminated";

    ( try
        let old_handlers = Hashtbl.find handlers g in
        Hashtbl.replace handlers g (h :: old_handlers)
      with
        | Not_found ->
            (* The group g is new *)
            Hashtbl.add handlers g [h];
            handled_groups <- handled_groups + 1;
            if handled_groups = 1 then
              self#equeue_add_handler ()
    )

  method clear g =
    debug_print (lazy (sprintf "clear <group %d>" (Oo.id g)));

    (* Set that g is terminating now: *)
    g # terminate();

    (* (i) delete all resources of g: *)
    let ops =
      Hashtbl.fold
	(fun op (_,_,g') l -> if g=g' then op::l else l)
	tmo_of_op
	[] in
    List.iter
      self#sched_remove
      ops;
    List.iter
      self#pset_remove
      ops;

    (* (ii) delete all handlers of g: *)
    self#add_event (Extra (Term g));

    (* (iii) delete special actions of g: *)
    let to_remove =   (* remove from close_tab *)
      Hashtbl.fold
        (fun d (g',_) l -> if g = g' then d :: l else l) close_tab [] in
    List.iter
      (Hashtbl.remove close_tab) to_remove;

    abort_tab <- List.filter (fun (g',_) -> g<>g') abort_tab;

    (* self#wake_up false; *)

    (* Note: the Term event isn't caught after all handlers have been
     * deleted. The Equeue module simply discards events that are not
     * handled.
     *)
    ()



  method abort g ex =
    (* is there an abort action ? *)
    (* Note: If g has been terminated, the abort action is removed. So
     * we will never find here one.
     *)
    debug_print (lazy (sprintf "abort <group %d, exception %s>"
                         (Oo.id g) (Printexc.to_string ex)));
    let action = try Some (List.assoc g abort_tab) with Not_found -> None in
    match action with
      | Some a ->
          begin
            debug_print (lazy "abort <running abort action>");
            aborting <- true;
            let mistake = ref None in
            begin try
              a g ex;
            with
              | any ->
                  mistake := Some any (* Wow *)
            end;
            self#clear g;
            aborting <- false;
            match !mistake with
              | None -> ()
              | Some m ->
                  debug_print (lazy (sprintf "abort <propagating exception %s>"
                                       (Printexc.to_string m)));
                  raise m
          end
      | None ->
          ()

  method run () =
    let continue = ref true in
    (* self # add_ctrl_pipe(); *)
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
      (* self # close_ctrl_pipe(); *)
    with
      | error ->
          (* self # close_ctrl_pipe(); *)
          raise error

  method is_running =
    Equeue.is_running (Lazy.force sys)

  method once g duration f =
    let id = self#new_wait_id () in
    let op = Wait id in
    let called_back = ref false in

    let handler ues ev e =
      if !called_back then
        raise Equeue.Terminate
      else
        if e = Timeout(g,op) then begin
          self#remove_resource g op;  (* delete the resource *)
          called_back := true;
          f();                        (* invoke f (callback) *)
          raise Equeue.Terminate      (* delete the handler *)
        end
        else
          raise Equeue.Reject
    in

    if duration >= 0.0 then begin
      self#add_resource g (op, duration);
      self#add_handler g handler
    end;
    ()

end


let pollset_event_system pset = 
  (new pollset_event_system pset :> Unixqueue_util.event_system)

