(* 
 * $Id$
 *)

(**********************************************************************)
(***                                                                ***)
(***                Extensions to the UNIX library                  ***)
(***                                                                ***)
(**********************************************************************)

open Unix;;
open Sys;;
open Printf;;

(**********************************************************************)
(* Helpers for multi-threading:                                       *)
(**********************************************************************)

let create_lock_unlock_pair =
  ref (fun () -> ( (fun () -> ()), (fun ()-> ()) ) ) ;;

let mt_mode = ref false

let init_mt create =
  create_lock_unlock_pair := create;
  mt_mode := true
;;

(* Idea: 
 *   let lock, unlock = !create_lock_unlock_pair()
 * which creates a mutex, and two functions:
 *   lock()
 *   unlock()
 *
 * In multi-threading mode, the variable create_lock_unlock_pair is
 * initialized right at startup with a reasonable mutex creator.
 * In non-mt mode, the default value of this variable contains
 * a dummy creator that does not lock anything.
 *)


(**********************************************************************)
(* A set of file descriptors:                                         *)
(**********************************************************************)

module Fdescr = struct
  type t = Unix.file_descr

  let compare = Pervasives.compare

end;;

module Fd_Set = Set.Make(Fdescr);;

let set_of_list =
  List.fold_left (fun set x -> Fd_Set.add x set) Fd_Set.empty 
;;


(**********************************************************************)
(* Basic type definitions                                             *)
(**********************************************************************)


type group = Unixqueue_util.group

type wait_id = Unixqueue_util.wait_object

type operation = Unixqueue_util.operation = 
    Wait_in  of file_descr
  | Wait_out of file_descr
  | Wait_oob of file_descr
  | Wait of wait_id

type event = Unixqueue_util.event =
    Input_arrived of (group * file_descr)
  | Output_readiness of (group * file_descr)
  | Out_of_band of (group * file_descr)
  | Timeout of (group * operation)
  | Signal
  | Extra of exn

exception Term of group
  (* Extra (Term g) is now the group termination event for g *)

exception Keep_alive
  (* Sometimes used to keep the event system alive *)


module RID = struct
  (* RID = resource identifier, i.e. the operation *)
  type t = operation
  let compare (rid1:t) (rid2:t) =
    Pervasives.compare rid1 rid2
end


module RID_Map = Map.Make(RID)

exception Exists;;


let rid_map_exists f map =
  try
    RID_Map.iter
      (fun k v -> if f k v then raise Exists)
      map;
    false
  with
      Exists -> true
;;

class type event_system =
object
  (* Public interface *)
  method new_group : unit -> group
  method new_wait_id : unit -> wait_id
  method exists_resource : operation -> bool
  method add_resource : group -> (operation * float) -> unit
  method add_close_action : group -> (file_descr * (file_descr -> unit)) -> unit  method add_abort_action : group -> (group -> exn -> unit) -> unit
  method remove_resource : group -> operation -> unit
  method add_handler : group -> (event_system -> event Equeue.t -> event -> unit) -> unit
  method add_event : event -> unit
  method clear : group -> unit
  method run : unit -> unit
  method is_running : bool
  method once : group -> float -> (unit -> unit) -> unit
  method exn_log : ?suppressed:bool -> ?to_string:(exn -> string) -> ?label:string -> exn -> unit
  method debug_log : ?label:string -> string -> unit
  (* Protected interface *)
  method private setup : unit -> (file_descr list * file_descr list * file_descr list * float)
  method private queue_events : (file_descr list * file_descr list * file_descr
list) -> bool
  method private source : event Equeue.t -> unit
end
  (* Note: This has to match the object type Unixqueue_util.event_system *)


type resource_prop = Unixqueue_util.resource_prop

type handler = Unixqueue_util.handler



(* The control pipe:
 * 
 * The control pipe is only used in multi-threaded programs: if thread
 * A adds an event/resource to the event system while thread B runs the
 * system (and blocks). In this case, A must wake up B such that B can
 * react on the changed conditions of the system.
 *
 * The control pipe has a read end and a write end. The read end is added
 * to the watched resources such that when bytes arrive thread B wakes
 * up. Thread A signals changed conditions by writing a single byte into
 * the write end of the control pipe.
 *
 * The control pipe is now created every time [run] is called, and it is
 * closed before this method is left. This avoids that we run out of 
 * file descriptors when lots of queues are created. 
 *)

exception Abort = Unixqueue_util.Abort

exception Exit;;   (* used locally in uq_handler *)


let debug_print = Unixqueue_util.debug_print
let debug_mode = Unixqueue_util.debug_mode
let set_debug_mode = Unixqueue_util.set_debug_mode

let string_of_op = Unixqueue_util.string_of_op
let string_of_fd = Unixqueue_util.string_of_fd

(**********************************************************************)
(* The class unix_event_system                                        *)
(**********************************************************************)


class select_event_system () : event_system =
  let es_lock, es_unlock = !create_lock_unlock_pair() in
object(self)

  val mutable sys = lazy (assert false)   (* initialized below *)
  val mutable res = (RID_Map.empty : resource_prop RID_Map.t)
            (* current resources *)
  val mutable new_res = (RID_Map.empty : resource_prop RID_Map.t)
            (* added resources *)
  val mutable close_tab = (Hashtbl.create 10 : (file_descr, (group * (file_descr -> unit))) Hashtbl.t)
  val mutable abort_tab = ([] : (group * (group -> exn -> unit)) list)
  val mutable aborting = false
  val mutable handlers = (Hashtbl.create 10 : (group, handler list) Hashtbl.t)
  val mutable handled_groups = 0
            (* the number of keys in [handlers] *)
  val mutable lock = es_lock
  val mutable unlock = es_unlock
  val mutable blocking = false
  val mutable ctrl_pipe_rd = None
  val mutable ctrl_pipe_wr = None
  val mutable ctrl_pipe_group = new Unixqueue_util.group_object

  (**********************************************************************)
  (* Implementation of the queue                                        *)
  (**********************************************************************)

  initializer
    let equeue_sys = 
      Equeue.create 
	~string_of_event:Unixqueue_util.string_of_event self#source in
    sys <- lazy equeue_sys;
    ignore(Lazy.force sys);

  method private setup () =
    (* Find out which resources should be watched for interesting events *)

    (* CHECK: Can we do this faster? Only new_res? *)
    let tref = gettimeofday () in

    if new_res <> RID_Map.empty then begin
      (* Append new_res to res, and change negative tlast to tref.
       * A negative event time tlast indicates that the
       * resource has just been added; such a resource is handled as if 
       * the last event has just been seen.
       *)    
      res <- RID_Map.fold 
                  (fun op (g, tout, tlast) res ->
		     RID_Map.add op (g, tout, tref) res)
                  new_res
                  res;
      new_res <- RID_Map.empty;
    end;

    debug_print (lazy (
		   let reslst =
		     RID_Map.fold
		       (fun op (g, tout, tlast) acc ->
			  (sprintf "%s => group %d, timeout %f, lastevent %f"
			     (string_of_op op) (Oo.id g) tout tlast) :: acc)
		       res
		       [] in
		   sprintf "setup <resources: %s>"
		     (String.concat "; " reslst)));

    if res <> RID_Map.empty then begin
      let infiles, outfiles, oobfiles, time =
	(* (infiles, outfiles, oobfiles, time): Lists of file descriptors
	 * that must be observed and the maximum period of time until
	 * something must have been happened (-1 means infinite period)
	 *)
	RID_Map.fold
	  (fun op (g, tout,tlast) (inf, outf, oobf, t) ->
	     (* (inf, outf, oobf, t): Current intermediate result. The descrip-
	      *      tors inf, outf, oobf are already known to be observed, and
	      *      t is the smallest timeout value of these descriptors
	      * rid = (g,op):
	      *      The resource being examined. g is the group; op is
	      *      the operation
	      * tout: The timeout value of the operation
	      * tlast: The point in time when the resource caused last an
	      *      event
	      *)
	     (* t': Compute the smallest timeout value if r is considered to
	      *     happen
	      *)
	     let t' =
	       if tout < 0.0 then 
		 (* Infinite timeout was specified for r; so t' = t *)
		 t 
	       else 
		 (* tref -. tlast: time since the last event from resource r
		  * tout -. (tref -. tlast): how much time may elapse until
		  *     the timeout happens. If this number is negative
		  *     the timeout should already have been happened.
		  * tdelta: how much time may elapse until the timeout
		  *     happens in the future. No negative values.
		  *)
		 let tdelta = max 0.0 (tout -. (tref -. tlast)) in
		 if t < 0.0 then 
		   (* t: was infinite timeout, and t' is now tdelta *)
		   tdelta 
		 else 
		   (* t: was finite timeout, and t' is the minimum *)
		   min t tdelta
	     in
	     (* Add the descriptor contained in op to one of the descriptor
	      * lists
	      *)
	     match op with
		 Wait_in  d -> (d :: inf, outf,      oobf,      t')
	       | Wait_out d -> (inf,      d :: outf, oobf,      t')
	       | Wait_oob d -> (inf,      outf,      d :: oobf, t')
	       | Wait _     -> (inf,      outf,      oobf,      t')
		   (* NOTE: In previous version of this library, we stored the
		    * group into the three lists intf, outf, oobf. The group can
		    * be retrieved by looking at res at any time, and it
		    * cannot be changed. So we do not do this any longer.
		    *)
	  )
	  res
	  ([],[],[],(-1.0))
      in
      (infiles, outfiles, oobfiles, time)
    end
    else ([],[],[],(-1.0))


  method private queue_events (infiles', outfiles', oobfiles') =
    let dummy_buf = String.create 1 in
    let deferred_exn = ref None in
    let have_event = ref false in
  
    (* Compare the watched resources infiles/outfiles/oobfiles with the
     * actually happened system events infiles'/outfiles'/oobfiles',
     * and add the events to the queue resulting from that.
     *)

    let read_ctrl_pipe() =
      (* Read bytes from the control pipe and ignore them. The control pipe
       * is in non-blocking mode.
       * Problem: Compiled with -vmthread, there is a bug in Unix.read
       * that EAGAIN is not returned. Instead, reading is restarted. To
       * avoid this, we first test with Unix.select, which seems to work
       * correctly.
       * Update: Does not work. Don't know how to fix this problem.
       *)
      match ctrl_pipe_rd with
	| None ->
	    ()   (* this should not happen *)
	| Some p ->
	    ( try
		while true do
		  let have_ctrl_data = Netsys.is_readable p in
		  if not have_ctrl_data then
		    raise(Unix_error(EAGAIN,"(artificial)",""));
		  ignore(read p dummy_buf 0 1)
		done;
		assert false
	      with
		  Unix_error(EAGAIN,_,_) ->
		    ()
		| Unix_error(EINTR,_,_) as err ->
		    (* It is unclear whether this can happen or not. *)
		    deferred_exn := Some err
	    )
    in

    (* first process file descriptors, and add events resulting from 
     * file descriptors to the event queue.
     *)
    let add tag ev_constructor res_constructor descr_list =
      List.iter 
	(fun d ->
	   try
	     let r = res_constructor d in
	     let g, _, _ = RID_Map.find r res in
	     if g = ctrl_pipe_group then
	       (* It's only the control pipe. Drop any bytes from the pipe. *)
	       read_ctrl_pipe()
	     else (
	       have_event := true;
	       Equeue.add_event (Lazy.force sys) (ev_constructor g d))
   	       (* Add the event *)
	   with
	       Not_found ->
		 prerr_endline ("Unixqueue: Got event without resource (implementation error?), for descriptor " ^ string_of_fd d ^ " (" ^ tag ^ ")")
	)
	descr_list in
    add "in"  (fun g d -> Input_arrived(g,d))    (fun d -> Wait_in d)  infiles';
    add "out" (fun g d -> Output_readiness(g,d)) (fun d -> Wait_out d) outfiles';
    add "oob" (fun g d -> Out_of_band(g,d))      (fun d -> Wait_oob d) oobfiles';

    (* determine which descriptors are timed out: *)
    let tref' = gettimeofday() in
    
    (* For faster lookups in these sets: *)
    let infiles'_set  = set_of_list infiles' in
    let outfiles'_set = set_of_list outfiles' in
    let oobfiles'_set = set_of_list oobfiles' in
    
    let res'' =        (* The updated resource list *)
      RID_Map.mapi
	(fun op (g, tout, tlast) ->
	   (* Note: In previous versions of this library, we compared the
	    * group of xxxfiles with g. Actually, the group is guaranteed
	    * to match, because it is not possible to change the groups
	    * in res between [setup] and [queue_events]. So this test
	    * is superflous.
	    *)
	   if match op with 
	       Wait_in d  -> Fd_Set.mem d infiles'_set
	     | Wait_out d -> Fd_Set.mem d outfiles'_set
	     | Wait_oob d -> Fd_Set.mem d oobfiles'_set
	     | Wait _     -> false
	   then
	     (* r denotes a file descriptor resource that must be updated 
	      * because an event happened
	      *)
	     (g, tout, tref')
	   else
	     (* r is some other resource. Find out if it is timed out
	      * and generate a Timeout event in this case.
	      *)
	     if tout >= 0.0 then begin
	       if tref' >= tout +. tlast then begin
		 have_event := true;
		 Equeue.add_event (Lazy.force sys) (Timeout(g,op));
		 (g, tout, tref')
	       end
	       else
		 (g, tout, tlast)      (* resource entry is unchanged *)
	     end
	     else
	       (* r is a resource without timeout value. It is always
		* unchanged.
		*)
	       (g, tout, tlast)
	)
	res in

    res <- res'';

    match !deferred_exn with
	None -> !have_event
      | Some exn -> raise exn


  method private source _sys =
    assert(Lazy.force sys == _sys);
    lock();
    try
      (* Find out which system events are interesting now: *)
      let (infiles, outfiles, oobfiles, time) = self#setup() in

      debug_print
	(lazy
	   ( sprintf "setup result <infiles=%s; outfiles=%s; oobfiles=%s; timeout=%f>"
	       (String.concat "," (List.map string_of_fd infiles))
	       (String.concat "," (List.map string_of_fd outfiles))
	       (String.concat "," (List.map string_of_fd oobfiles))
	       time
	   ));

      let interesting =
	rid_map_exists (fun _ (g,_,_) -> g <> ctrl_pipe_group) res in

      if interesting then begin
	(* There ARE interesting situations. *)
	
        (* Wait until these events happen: *)
	
	try
	  (* Maybe we get an EINTR *)

	  blocking <- true;
	  unlock();  (* Unlock because Unix.select may block *)

	  let (infiles', outfiles', oobfiles') as actual_tuple =
	      (* (infiles', outfiles', oobfiles'): Lists of file descriptors
	       * that can be handled
	       *)
	    Unix.select infiles outfiles oobfiles time in

	  lock();
	  blocking <- false;
	
          (* Now we have in infiles', outfiles', oobfiles' the actually
	   * happened file descriptor events.
	   * Furthermore, pure timeout events may have happened, but this
	   * is not indicated specially.
	   *)
    	  let have_event = self#queue_events actual_tuple in

	  (* Ensure we always add an event to keep the event loop running: *)
	  if not have_event then
	    Equeue.add_event (Lazy.force sys) (Extra Keep_alive)
	with
	    Unix.Unix_error(Unix.EINTR,_,_) ->
	      (* automatically generate a Signal event: *)
		lock();
		blocking <- false;
		Equeue.add_event (Lazy.force sys) Signal
      end;
      unlock()
    with
	any -> 
	  unlock(); 
	  raise any

  (**********************************************************************)
  (* External interface                                                 *)
  (**********************************************************************)

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

  method private protect : 's 't . ('s -> 't) -> 's -> 't =
    fun f arg ->
      lock();
      try
	let r = f arg in
	unlock();
	r
      with
	  any -> unlock(); raise any

  method new_group () =
    new Unixqueue_util.group_object

  method new_wait_id () =
    new Unixqueue_util.wait_object

  method private exists_resource_nolock op =
    try
      let _ = RID_Map.find op res in
      true
    with
	Not_found ->
	  try
	    let _ = RID_Map.find op new_res in
	    true
	  with
	      Not_found ->
		false

  method exists_resource op =
    self#protect self#exists_resource_nolock op

  method private wake_up keep_alive =
    (* wake_up requires the lock! *)
    if !mt_mode && blocking then begin
      try
	(* avoid that the queue becomes empty: *)
	if keep_alive then 
	  Equeue.add_event (Lazy.force sys) (Extra Keep_alive); 
	( match ctrl_pipe_wr with
	    | None -> ()
	    | Some p_wr ->
		let buf = String.make 1 'X' in
		ignore(single_write p_wr buf 0 1);
	)
      with
	  Unix_error(EINTR,_,_) ->
	    Equeue.add_event (Lazy.force sys) Signal;
	    self#wake_up keep_alive
    end

  method add_resource g (op,t) =
    debug_print (lazy (sprintf "add_resource <group %d, %s, timeout %f>"
			 (Oo.id g) (string_of_op op) t));
    if g # is_terminating then
      invalid_arg "Unixqueue.add_resource: the group is terminated";
    self#protect
      (fun () ->
	 if self#exists_resource_nolock op then (
	   (* CHECK: Maybe we should fail if g is a different group than
	    * the already existing group
	    *)
	   debug_print (lazy "add_resouce <resource exists already>");
	   ()
	 )
	 else begin
	   (* add the resource: *)
	   new_res <- RID_Map.add op (g,t,-1.0) new_res;
	   (* wake the thread up that runs the system (if in mt mode): *)
           self#wake_up true
	 end
      )
      ()

  method private exists_descriptor_nolock d =
    self#exists_resource_nolock (Wait_in d) ||
    self#exists_resource_nolock (Wait_out d) ||
    self#exists_resource_nolock (Wait_oob d)

  method private exists_descriptor d =
    self#protect self#exists_descriptor_nolock d

  method add_close_action g (d,a) =
    if g # is_terminating then
      invalid_arg "Unixqueue.add_close_action: the group is terminated";
    self#protect
      (fun () ->
	 (* CHECK: Maybe we should fail if g is a different group than
	  * the existing group
	  *)
	 if self#exists_descriptor_nolock d then begin
	   Hashtbl.replace close_tab d (g,a)
	     (* There can be only one close action.
	      * TODO: Rename to set_close_action
	      *)
	 end else
	   failwith "add_close_action"
      )
      ()

  method add_abort_action g a =
    if g # is_terminating then
      invalid_arg "Unixqueue.add_abort_action: the group is terminated";
    self#protect
      (fun () ->
	 abort_tab <- (g,a) :: abort_tab
      )
      ()

  method private remove_resource_nolock g op =
    debug_print (lazy (sprintf "remove_resource <group %d, %s>"
			 (Oo.id g) (string_of_op op)));
    if g # is_terminating then
      invalid_arg "Unixqueue.remove_resource: the group is terminated";
    (* If there is no resource (g,op) raise Not_found *)
    let g_found, _, _ = 
      try RID_Map.find op res 
      with Not_found -> RID_Map.find op new_res in
    if g <> g_found then
      failwith "remove_resource: descriptor belongs to different group";
    res     <- RID_Map.remove op res;
    new_res <- RID_Map.remove op new_res;
    (* is there a close action ? *)
    let sd =
      match op with
	  Wait_in  d' -> Some d'
	| Wait_out d' -> Some d'
	| Wait_oob d' -> Some d'
	| Wait _      -> None
    in
    match sd with
	Some d ->
	  if not aborting then begin
    	    let action = try Some (snd(Hashtbl.find close_tab d)) with Not_found -> None in
    	    match action with
		Some a -> 
		  (* any open resource? *)
      		  if not (self#exists_descriptor_nolock d) then begin
		    debug_print (lazy (sprintf "remove_resource <running close action for fd %s>"
					 (string_of_fd d)));
		    Hashtbl.remove close_tab d;
      		    a d;
      		  end
    	      | None ->
      		  ()
	  end
      | None -> ()


  method remove_resource =
    self#protect self#remove_resource_nolock

  method private add_event_nolock e =
    Equeue.add_event (Lazy.force sys) e; 
    self#wake_up false

  method add_event e =
    self#protect self#add_event_nolock e


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
		h (self : #event_system :> event_system) esys ev
	      with
		  Equeue.Reject ->
		    forward_event_to g hlist'
		| Equeue.Terminate ->
		    (* Terminate only this handler. *)
		    self#protect (terminate_handler_nolock g) h
		    (* Any error exceptions simply fall through. Equeue will
		     * catch them, and will add the event to the error queue
		     *)
	    )
    in
    
    let forward_event g =
      let hlist = 
	self#protect 
	  (fun () -> try Hashtbl.find handlers g with Not_found -> []) 
	  () in
      forward_event_to g hlist
    in
  
    let forward_event_to_all() =
      let hlist_all =
	self#protect
	  (fun () ->
	     Hashtbl.fold (fun g hlist l -> (g,hlist) :: l) handlers [])
	  ()
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
	Extra (Term g) ->
	  (* Terminate all handlers of group g *)
	  self#protect
	    (fun () ->
	       if Hashtbl.mem handlers g then (
		 Hashtbl.remove handlers g;
		 handled_groups <- handled_groups - 1;
		 if handled_groups = 0 then
		   raise Equeue.Terminate  (* delete uq_handler from esys *)
	       )
	       else raise Equeue.Reject (* strange, should not happen *)
	    )
	    ()
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

  method private add_handler_nolock g h =
    debug_print (lazy (sprintf "add_handler <group %d>" (Oo.id g)));
    
    if g # is_terminating then
      invalid_arg "Unixqueue.add_handler: the group is terminated";

    ( try
	let old_handlers = Hashtbl.find handlers g in
	Hashtbl.replace handlers g (h :: old_handlers)
      with
	  Not_found ->
	    (* The group g is new *)
	    Hashtbl.add handlers g [h];
	    handled_groups <- handled_groups + 1;
	    if handled_groups = 1 then
	      self#equeue_add_handler ()
    )

  method add_handler g =
    self#protect (self#add_handler_nolock g)

  method private clear_nolock g = 
    debug_print (lazy (sprintf "clear <group %d>" (Oo.id g)));
    
    (* Set that g is terminating now: *)
    g # terminate();

    (* (i) delete all resources of g: *)
    res <- RID_Map.fold
             (fun op (g',tout,tlast) res ->
		if g <> g' then RID_Map.add op (g',tout,tlast) res
		else res)
             res
             RID_Map.empty;
    new_res <- RID_Map.fold
                 (fun op (g',tout,tlast) res ->
		    if g <> g' then RID_Map.add op (g',tout,tlast) res
		    else res)
                 new_res
                 RID_Map.empty;

    (* (ii) delete all handlers of g: *)
    self#add_event_nolock (Extra (Term g));

    (* (iii) delete special actions of g: *)
    let to_remove =   (* remove from close_tab *)
      Hashtbl.fold 
	(fun d (g',_) l -> if g = g' then d :: l else l) close_tab [] in
    List.iter
      (Hashtbl.remove close_tab) to_remove;

    abort_tab <- List.filter (fun (g',_) -> g<>g') abort_tab;

    self#wake_up false;

    (* Note: the Term event isn't caught after all handlers have been
     * deleted. The Equeue module simply discards events that are not
     * handled.
     *)
    ()

  method  clear =
    self#protect self#clear_nolock

  method private abort_nolock g ex =
    (* is there an abort action ? *)
    (* Note: If g has been terminated, the abort action is removed. So
     * we will never find here one.
     *)
    debug_print (lazy (sprintf "abort <group %d, exception %s>"
			 (Oo.id g) (Printexc.to_string ex)));
    let action = try Some (List.assoc g abort_tab) with Not_found -> None in
    match action with
	Some a ->
	  begin
	    debug_print (lazy "abort <running abort action>");
            aborting <- true;
	    let mistake = ref None in
	    begin try
      	      a g ex;
	    with
		any ->
		  mistake := Some any (* Wow *)
	    end;
	    self#clear g;
	    aborting <- false;
	    match !mistake with
		None -> ()
	      | Some m -> 
		  debug_print (lazy (sprintf "abort <propagating exception %s>"
				       (Printexc.to_string m)));
		  raise m
	  end
      | None ->
	  ()

  method private abort g =
  self#protect self#abort_nolock g


  method private add_ctrl_pipe() =
    (* In multi-threaded mode: watch at least the control pipe *)
    if !mt_mode then (
      assert(ctrl_pipe_rd = None && ctrl_pipe_wr = None);
      let (p_rd, p_wr) = Unix.pipe() in
      ctrl_pipe_rd <- Some p_rd;
      ctrl_pipe_wr <- Some p_wr;
      new_res <- RID_Map.add (Wait_in p_rd) (ctrl_pipe_group,-1.0,-1.0) new_res
    );

  method private close_ctrl_pipe() =
    match ctrl_pipe_rd, ctrl_pipe_wr with
      | None, None -> ()
      | (Some p_rd), (Some p_wr) ->
	  let op = Wait_in p_rd in
	  res     <- RID_Map.remove op res;
	  new_res <- RID_Map.remove op new_res;
	  Unix.close p_wr;
	  Unix.close p_rd;
	  ctrl_pipe_rd <- None;
	  ctrl_pipe_wr <- None;
      | _ ->
	  assert false

  method run () =
    let continue = ref true in
    self # add_ctrl_pipe();
    try
      while !continue do
	continue := false;
	try
	  Equeue.run (Lazy.force sys);
	with
	    Abort (g,an_exception) ->
	      begin
		match an_exception with
		    (Equeue.Reject|Equeue.Terminate) ->
		      (* A serious programming error: *)
		      failwith "Caught 'Abort' exception with Reject or Terminate exception as argument; this is a programming error"
		  | Abort(_,_) ->
		      failwith "Caught 'Abort' exception with an 'Abort' exception as argument; this is a programming error"
		  | _ -> ()
	      end;
	      self#abort g an_exception;
	      continue := true
      done;
      self # close_ctrl_pipe();
    with
      | error ->
	  self # close_ctrl_pipe();
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
;;

class unix_event_system = select_event_system

(**********************************************************************)
(* Classic interface                                                  *)
(**********************************************************************)

let select_event_system() =
  new unix_event_system()

let new_group ues =
  ues # new_group()

let new_wait_id ues =
  ues # new_wait_id()

let exists_resource ues =
  ues # exists_resource

let add_resource ues =
  ues # add_resource

let add_close_action ues =
  ues # add_close_action

let add_abort_action ues =
  ues # add_abort_action

let remove_resource ues =
  ues # remove_resource

let add_handler ues =
  ues # add_handler

let add_event ues =
  ues # add_event

let clear ues =
  ues # clear

let run ues =
  ues # run ()

let is_running ues =
  ues # is_running

let once ues =
  ues # once 

let exn_log ues =
  ues # exn_log

let debug_log ues =
  ues # debug_log

let event_system_factory = ref select_event_system

let set_event_system_factory f =
  event_system_factory := f

let create_unix_event_system() =
  !event_system_factory()

