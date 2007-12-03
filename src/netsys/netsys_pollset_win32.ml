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
	  match w_opt with
	    | None -> prerr_endline "None"
	    | Some i -> prerr_endline ("Some " ^ string_of_int i)
	with
	  | error ->
	      wsa_reset_event evobj_cancel; (* always reset this guy *)
	      raise error
      );
      wsa_reset_event evobj_cancel;
      (* Check again for pending events *)
      let (in_set'', out_set'', pri_set'') =
	Unix.select in_set out_set pri_set 0.0 in
      list_of_sets ht in_set'' out_set'' pri_set''
    )

  method dispose () = 
    (* is automatic *)
    ()

  method cancel_wait() =
    wsa_set_event evobj_cancel

end

