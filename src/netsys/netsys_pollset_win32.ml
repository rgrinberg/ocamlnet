(* $Id$ *)

open Netsys_pollset
open Netsys_win32

exception Too_many_descriptors

let wait_for_multiple_events evobj tmo =
  Netsys_impl_util.slice_time_ms
    (wsa_wait_for_multiple_events evobj)
    tmo


let list_of_socket_sets ht in_set out_set pri_set =
  let in_arr  = Array.of_list in_set in
  let out_arr = Array.of_list out_set in
  let pri_arr = Array.of_list pri_set in
  Array.sort Pervasives.compare in_arr;
  Array.sort Pervasives.compare out_arr;
  Array.sort Pervasives.compare pri_arr;
  Hashtbl.fold
    (fun fd (ev,_) l ->
       let m_in = Netsys_impl_util.mem_sorted_array fd in_arr in
       let m_out = Netsys_impl_util.mem_sorted_array fd out_arr in
       let m_pri = Netsys_impl_util.mem_sorted_array fd pri_arr in
       let x =
	 (if m_in then Netsys_posix.const_rd_event else 0) lor
	   (if m_out then Netsys_posix.const_wr_event else 0) lor
	   (if m_pri then Netsys_posix.const_pri_event else 0) in
       if x <> 0 then
	 (fd, ev, Netsys_posix.act_events_of_int x) :: l
       else
	 l
    )
    ht
    []


let list_of_w32_objects ht =
  Hashtbl.fold
    (fun fd (ev, detail) l ->
       let (w_rd, w_wr, _) = Netsys_posix.poll_req_triple ev in
       let x =
	 match detail with
	   | Netsys_win32.W32_pipe_helper ph ->
	       let evobj_rd = Netsys_win32.pipe_rd_event ph in
	       let evobj_wr = Netsys_win32.pipe_wr_event ph in
	       let m_in = w_rd && test_event evobj_rd in
	       let m_out = w_wr && test_event evobj_wr in
	       (if m_in then Netsys_posix.const_rd_event else 0) lor
		 (if m_out then Netsys_posix.const_wr_event else 0)
	   | Netsys_win32.W32_event evobj ->
	       let m_in = w_rd && test_event evobj in
	       if m_in then Netsys_posix.const_rd_event else 0
       in
       if x <> 0 then
	 (fd, ev, Netsys_posix.act_events_of_int x) :: l
       else
	 l
    )
    ht
    []


let string_of_list l =
  String.concat ","
    (List.map
       (fun (fd, _, x) ->
	  let i = Netsys_posix.poll_rd_result x in
	  let o = Netsys_posix.poll_wr_result x in
	  let p = Netsys_posix.poll_pri_result x in
	  Int64.to_string
	    (Netsys.int64_of_file_descr fd) ^ ":" ^ 
	    (if i then "i" else "") ^
	    (if o then "o" else "") ^
	    (if p then "p" else "")
       )
       l)
       


let w32_count =
  function
    | Netsys_win32.W32_event _ -> 1
    | Netsys_win32.W32_pipe_helper _ -> 2


let pollset () : pollset =
  let m = wsa_maximum_wait_events() in
  let evobj_cancel = create_event() in
object(self)
  val mutable sockets = Hashtbl.create m
  val mutable w32_objects = Hashtbl.create m
  val mutable w32_size = 0

  method find fd =
    try
      fst(Hashtbl.find sockets fd)
    with
      | Not_found ->
	  fst(Hashtbl.find w32_objects fd)

  method add fd ev =
    let fd_detail =
      try Some(Netsys_win32.lookup fd) with Not_found -> None in
    (* Check whether the maximum number of event objects m is exceeded.
       A socket needs one event object, and a pipe needs 2 objects.
       We need one extra object for cancellation.
     *)
    let fd_count =
      match fd_detail with
	| None -> 1
	| Some w32_obj -> w32_count w32_obj in
    let count =
      Hashtbl.length sockets + w32_size + 1 in
    if count + fd_count > m then
      raise Too_many_descriptors;
    match fd_detail with
      | None -> (* socket *)
	  ( try
	      let (_, evobj) = Hashtbl.find sockets fd in
	      Hashtbl.replace sockets fd (ev, evobj)
	    with
	      | Not_found ->
		  let evobj = create_event() in
		  Hashtbl.replace sockets fd (ev, evobj)
	  )
      | Some w32_obj ->
	  ( try
	      let (_, old_detail) = Hashtbl.find w32_objects fd in
	      w32_size <- w32_size - w32_count old_detail
	    with Not_found ->
	      ()
	  );
	  Hashtbl.replace w32_objects fd (ev, w32_obj);
	  w32_size <- w32_size + w32_count w32_obj

    (* Check: this code assumes that an fd is never added as socket,
       and then as w32_object
     *)


  method remove fd =
    Hashtbl.remove sockets fd;
    if Hashtbl.mem w32_objects fd then (
      let (_, old_detail) = Hashtbl.find w32_objects fd in
      w32_size <- w32_size - w32_count old_detail;
      Hashtbl.remove w32_objects fd
    )

  (* The main reason we use all the Win32 event stuff is that we can 
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
	   let (e_in,e_out,e_pri) = Netsys_posix.poll_req_triple ev in
	   let in_set'' = if e_in then fd :: in_set' else in_set' in
	   let out_set'' = if e_out then fd :: out_set' else out_set' in
	   let pri_set'' = if e_pri then fd :: pri_set' else pri_set' in
	   (in_set'', out_set'', pri_set'')
	)
	sockets
	([], [], []) in
    (* First stop any pending event recording, and restart it with the
       events we are looking for.
     *)
    Hashtbl.iter
      (fun fd (ev, evobj) ->
	 (* ignore(wsa_enum_network_events fd evobj); *)
	 reset_event evobj;
	 wsa_event_select evobj fd ev
      )
      sockets;
    (* CHECK: it is unclear whether this test is needed *)
    let (in_set', out_set', pri_set') =
      Unix.select in_set out_set pri_set 0.0 in
    if in_set' <> [] || out_set' <> [] || pri_set'<> [] then (
      (* Ok, we are immediately ready. *)
      let l = 
	list_of_socket_sets sockets in_set' out_set' pri_set' 
	@ list_of_w32_objects w32_objects in
      prerr_endline ("WAIT CASE1 " ^ string_of_list l);
      l
    )
    else (
      (* No pending events, so wait for the recorded events *)
      let count =
	Hashtbl.length sockets + w32_size + 1 in
      let evobjs = Array.make count evobj_cancel in
      let fdobjs = Array.make count Unix.stdin in
      let k = ref 1 in
      Hashtbl.iter
	(fun fd (_, evobj) ->
	   evobjs.( !k ) <- evobj;
	   fdobjs.( !k ) <- fd;
	   incr k
	)
	sockets;
      Hashtbl.iter
	(fun fd (ev, detail) ->
	   let (w_rd, w_wr, _) = Netsys_posix.poll_req_triple ev in
	   match detail with
	     | Netsys_win32.W32_pipe_helper ph ->
		 let evobj_rd = Netsys_win32.pipe_rd_event ph in
		 let evobj_wr = Netsys_win32.pipe_wr_event ph in
		 if w_rd then (
		   evobjs.( !k ) <- evobj_rd;
		   fdobjs.( !k ) <- fd;
		 );
		 if w_wr then (
		   evobjs.( !k + 1 ) <- evobj_wr;
		   fdobjs.( !k + 1 ) <- fd;
		 );
		 k := !k + 2
	     | Netsys_win32.W32_event evobj ->
		 if w_rd then (
		   evobjs.( !k ) <- evobj;
		   fdobjs.( !k ) <- fd;
		 );
		 incr k
	)
	w32_objects;
      ( try 
	  let _w_opt = wait_for_multiple_events evobjs tmo in
	  ( match _w_opt with
	      | None ->
		  prerr_endline "WAIT TMO"
	      | Some n ->
		  prerr_endline ("WAIT SIGNAL " ^ 
				   Int64.to_string
				   (Netsys.int64_of_file_descr
				      (fdobjs.(n))));
	  );
	  ()
(*
	  match w_opt with
	    | None -> prerr_endline "None"
	    | Some i -> prerr_endline ("Some " ^ string_of_int i)
 *)
	with
	  | error ->
	      reset_event evobj_cancel; (* always reset this guy *)
	      raise error
      );
      (* Check again for pending events *)
      let (in_set'', out_set'', pri_set'') =
	Unix.select in_set out_set pri_set 0.0 in
      let l =
	list_of_socket_sets sockets in_set'' out_set'' pri_set'' 
        @ list_of_w32_objects w32_objects in
      prerr_endline ("WAIT CASE2 " ^ string_of_list l);
      l
    )

  method dispose () = 
    (* is automatic *)
    ()

  method cancel_wait cb =
    if cb then
      set_event evobj_cancel
    else
      reset_event evobj_cancel

end


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
    (* List of (pollset, pollset_helper) *)

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
	  let pset = pollset() in
	  let pset_helper = pollset_helper pset oothr in
	  pollset_list <- (pset, pset_helper) :: pollset_list;
	  pset # add fd ev; 
	  Hashtbl.replace ht fd pset

      | (pset, pset_helper) :: l' ->
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
      (fun (_, pset_helper) ->
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
      (fun (_, pset_helper) ->
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
      (fun (_, pset_helper) ->
	 pset_helper # join_thread()
      )
      pollset_list;
    pollset_list <- [];
    Hashtbl.clear ht
end