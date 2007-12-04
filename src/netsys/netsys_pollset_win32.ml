(* $Id$ *)

open Netsys_pollset
open Netsys_win32

exception Too_many_descriptors

let max_tmo =
  2147483    (* = max_int / 1000 -- on 32 bit systems, at least *)

let max_tmo_f =
  float max_tmo


let wait_for_multiple_events evobj tmo =
  if tmo >= 0.0 then (
    let twaited = ref 0.0 in
    let r = ref None in
    while !r = None && !twaited +. max_tmo_f < tmo do
      twaited := !twaited +. max_tmo_f;
      r := wsa_wait_for_multiple_events evobj (max_tmo * 1000)
    done;
    if !r = None then
      r := 
	wsa_wait_for_multiple_events 
	  evobj (truncate (max 0.0 (tmo -. !twaited)) * 1000);
    !r
  )
  else
     wsa_wait_for_multiple_events evobj (-1)


let list_of_sets ht in_set out_set pri_set =
  let in_arr  = Array.of_list in_set in
  let out_arr = Array.of_list out_set in
  let pri_arr = Array.of_list pri_set in
  Array.sort Pervasives.compare in_arr;
  Array.sort Pervasives.compare out_arr;
  Array.sort Pervasives.compare pri_arr;
  Hashtbl.fold
    (fun fd (ev,_) l ->
       let m_in = Netsys_util.mem_sorted_array fd in_arr in
       let m_out = Netsys_util.mem_sorted_array fd out_arr in
       let m_pri = Netsys_util.mem_sorted_array fd pri_arr in
       let x =
	 (if m_in then Netsys.pollin_const else 0) lor
	   (if m_out then Netsys.pollout_const else 0) lor
	   (if m_pri then Netsys.pollpri_const else 0) in
       if x <> 0 then
	 (fd, ev, x) :: l
       else
	 l
    )
    ht
    []


let socket_pollset () : pollset =
  let m = wsa_maximum_wait_events() in
  let evobj_cancel = wsa_create_event() in
object(self)
  val mutable ht = Hashtbl.create m

  method find fd =
    fst(Hashtbl.find ht fd)

  method add fd ev =
    if Hashtbl.length ht = m-1 then
      raise Too_many_descriptors;
    try
      let (_, evobj) = Hashtbl.find ht fd in
      Hashtbl.replace ht fd (ev, evobj)
    with
      | Not_found ->
	  let evobj = wsa_create_event() in
	  Hashtbl.replace ht fd (ev, evobj)


  method remove fd =
    Hashtbl.remove ht fd

  (* The only reason we use all the Win32 event stuff is that we can 
     support cancel_wait. We are very conservative, and use [select]
     to check the descriptors before waiting, and after waiting.
     We use the Win32 events, but we ignore what is exactly recorded
     in these event objects. The reason for the scepticism is that these
     events are mostly edge-triggered, but we need a level-triggered behavior
     (i.e. we need that events are reported as long as they last, and
     not only once when they start).
   *)

  method wait tmo =
    let (in_set, out_set, pri_set) =
      Hashtbl.fold
	(fun fd (ev,_) (in_set', out_set', pri_set') ->
	   let (e_in,e_out,e_pri) = Netsys.poll_in_triple ev in
	   let in_set'' = if e_in then fd :: in_set' else in_set' in
	   let out_set'' = if e_out then fd :: out_set' else out_set' in
	   let pri_set'' = if e_pri then fd :: pri_set' else pri_set' in
	   (in_set'', out_set'', pri_set'')
	)
	ht
	([], [], []) in
    (* First stop any pending event recording, and restart it with the
       events we are looking for.
     *)
    Hashtbl.iter
      (fun fd (ev, evobj) ->
	 (* ignore(wsa_enum_network_events fd evobj); *)
	 wsa_reset_event evobj;
	 wsa_event_select evobj fd ev
      )
      ht;
    let (in_set', out_set', pri_set') =
      Unix.select in_set out_set pri_set 0.0 in
    if in_set' <> [] || out_set' <> [] || pri_set'<> [] then (
      (* Ok, we are immediately ready. *)
      list_of_sets ht in_set' out_set' pri_set'
    )
    else (
      (* No pending events, so wait for the recorded events *)
      let evobjs = Array.make (Hashtbl.length ht + 1) evobj_cancel in
      let k = ref 1 in
      Hashtbl.iter
	(fun fd (_, evobj) ->
	   evobjs.( !k ) <- evobj;
	   incr k
	)
	ht;
      ( try 
	  let w_opt = wait_for_multiple_events evobjs tmo in
	  ()
(*
	  match w_opt with
	    | None -> prerr_endline "None"
	    | Some i -> prerr_endline ("Some " ^ string_of_int i)
 *)
	with
	  | error ->
	      wsa_reset_event evobj_cancel; (* always reset this guy *)
	      raise error
      );
      (* Check again for pending events *)
      let (in_set'', out_set'', pri_set'') =
	Unix.select in_set out_set pri_set 0.0 in
      list_of_sets ht in_set'' out_set'' pri_set''
    )

  method dispose () = 
    (* is automatic *)
    ()

  method cancel_wait cb =
    if cb then
      wsa_set_event evobj_cancel
    else
      wsa_reset_event evobj_cancel

end


type descriptor_type =
    [ `Socket ]


(* A pollset_helper starts a new thread and communicates with this thread *)

let pollset_helper pset (oothr : Netsys_oothr.mtprovider) =
object(self)
  val cmd_cond = oothr#create_condition()    (* Signals a changed [cmd] *)
  val cmd_mutex = oothr#create_mutex()
  val mutable cmd = None

  val result_cond = oothr#create_condition() (* Signals a changed [result] *)
  val result_mutex = oothr#create_mutex()
  val mutable result = None

  val mutable sep_thread = None

  initializer (
    let t = oothr#create_thread self#loop () in
    sep_thread <- Some t
  )
    

  method private loop() =
    cmd_mutex # lock();
    while cmd = None do
      cmd_cond # wait cmd_mutex
    done;
    let next_cmd =
      match cmd with
	| None -> 
	    assert false
	| Some c -> 
	    cmd <- None;
	    c in
    cmd_mutex # unlock();
    ( match next_cmd with
	| `Start_waiting(tmo,done_fun) ->
	    self # do_start_waiting tmo done_fun;
	    self # loop()
	| `Exit_thread ->
	    ()
    )

  method private do_start_waiting tmo done_fun =
    let r = pset # wait tmo in
    done_fun();
    result_mutex # lock();
    result <- Some r;
    result_cond # signal();
    result_mutex # unlock()


  (* The interface: 
     - We can issue a [start_waiting] command. The pset is then waited for
       in the separate thread. 
     - The [start_waiting] command has a [done_fun] function argument.
       It is called when a result exists. The call is done from the
       separate thread.
     - A [stop_waiting] cancels the wait (if any), and returns the
       result (if any)
     - A [join_thread] command causes the separate thread to terminate.
       It is an alternate command to [start_waiting].
   *)

  method start_waiting tmo done_fun =
    pset # cancel_wait false;
    cmd_mutex # lock();
    cmd <- Some(`Start_waiting(tmo,done_fun));
    cmd_cond # signal();
    cmd_mutex # unlock()

  method stop_waiting () =
    (* Now wait until the result is available: *)
    result_mutex # lock();
    while result = None do
      pset # cancel_wait true;   (* Can be invoked from a different thread *)
      result_cond # wait result_mutex
    done;
    let r =
      match result with
	| None -> assert false
	| Some r -> r in
    result <- None;
    result_mutex # unlock();
    r

  method join_thread() =
    cmd_mutex # lock();
    cmd <- Some `Exit_thread;
    cmd_cond # signal();
    cmd_mutex # unlock();
    match sep_thread with
      | None -> assert false
      | Some t -> t # join()

end


let threaded_pollset() =
  let oothr = !Netsys_oothr.provider in
object(self)
  val mutable pollset_list = []
    (* List of (descriptor_type, pollset, pollset_helper) *)

  val mutable ht = Hashtbl.create 10
    (* Maps descriptors to pollsets *)

  method find fd =
    let pset = Hashtbl.find ht fd in
    pset # find fd

  method add fd ev =
    try
      let pset = Hashtbl.find ht fd in
      pset # add fd ev
    with
      | Not_found ->
	  self # add_to pollset_list fd ev

  method private add_to l fd ev =
    match l with
      | [] ->
	  (* TODO: distinguish by descriptor type *)
	  let pset = socket_pollset() in
	  let pset_helper = pollset_helper pset oothr in
	  pollset_list <- (`Socket, pset, pset_helper) :: pollset_list;
	  pset # add fd ev; 
	  Hashtbl.replace ht fd pset

      | (dt, pset, pset_helper) :: l' ->  (* when dt is right... *)
	  ( try 
	      pset # add fd ev; 
	      Hashtbl.replace ht fd pset
	    with
	      | Too_many_descriptors ->
		  self # add_to l' fd ev
	  )

  method remove fd =
    try
      let pset = Hashtbl.find ht fd in
      pset # remove fd
	(* CHECK: terminate pollset_helpers for empty psets? *)
    with
      | Not_found -> ()

  val mutable d = false
  val d_mutex = oothr # create_mutex()
  val d_cond = oothr # create_condition()

  val mutable cancel_bit = false

  method wait tmo =
    d_mutex # lock();
    d <- cancel_bit;
    d_mutex # unlock();

    let when_done() =
      d_mutex # lock();
      d <- true;
      d_cond # signal();
      d_mutex # unlock()
    in

    List.iter
      (fun (_, _, pset_helper) ->
	 pset_helper # start_waiting tmo when_done
      )
      pollset_list;

    d_mutex # lock();
    while not d do
      d_cond # wait d_mutex
    done;
    d_mutex # unlock();

    let r = ref [] in
    List.iter
      (fun (_, _, pset_helper) ->
	 r := (pset_helper # stop_waiting()) @ !r
      )
      pollset_list;

    !r

  method cancel_wait cb =
    d_mutex # lock();
    d <- d || cb;
    cancel_bit <- cb;
    d_cond # signal();
    d_mutex # unlock();


  method dispose() =
    List.iter
      (fun (_, _, pset_helper) ->
	 pset_helper # join_thread()
      )
      pollset_list;
    pollset_list <- [];
    Hashtbl.clear ht
end
