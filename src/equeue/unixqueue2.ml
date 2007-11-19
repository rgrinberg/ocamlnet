(* $Id$ *)

open Unixqueue


module FloatMap = Map.Make(XXX)


let min_key m = XXX


let pset_add pset fd (i,o,p) =
  if not i && not o && not p then
    pset # remove fd
  else
    pset # add fd (Netsys.poll_in_events (i,o,p))


let pset_find pset fd =
  try Netsys.poll_in_triple(pset#find fd)
  with
    | Not_found -> (false,false,false)


class pollset_event_system (pset : Netsys_pollset.pollset) : event_system =
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

    let events = pset # wait delta in

    let timeout_events = XXX in

    (* ... deliver events ... *)
      
    (* ... update tmo_of_op for all delivered events ... *)

    ()

  method exists_resource op =
    Hashtbl.mem op tmo_of_op


  method add_resource g (op, t) =
    if g # is_terminating then
      invalid_arg "Unixqueue.add_resource: the group is terminated";
    if not (Hashtbl.mem tmo_of_op op) then (
      ( match op with
	  | Wait_in fd ->
	      let (i,o,p) = pset_find fd in
	      pset_add fd (true,o,p)
	  | Wait_out fd ->
	      let (i,o,p) = pset_find fd in
	      pset_add fd (i,true,p)
	  | Wait_oob fd ->
	      let (i,o,p) = pset_find fd in
	      pset_add fd (i,o,true)
	  | Wait _ ->
	      ()
      );
      let t1 = if t < 0 then t else Unix.gettimeofday() +. t in
      Hashtbl.add tmo_of_op op (t, t1, g);
      let l_ops =
	try FloatMap.find t1 ops_of_tmo with Not_found -> [] in
      if t1 >= 0.0 then
	ops_of_tmo <- FloatMap.add t1 (op :: l_ops) ops_of_tmo
    )


  method remove_resource g op =
    if g # is_terminating then
      invalid_arg "Unixqueue.remove_resource: the group is terminated";
    let _, t1, g_found = Hashtbl.find tmo_of_op op in
    if g <> g_found then
      failwith "remove_resource: descriptor belongs to different group";
    Hashtbl.remove tmo_of_op op;
    let l_ops =
      if t1 >= 0.0 then
	try FloatMap.find t1 ops_of_tmo with Not_found -> [] 
      else [] in
    let l_ops' =
      List.filter (fun op' -> op <> op') l_ops in
    if l_ops' = [] then
      ops_of_tmo <- FloatMap.remove t1 ops_of_tmo
    else
      ops_of_tmo <- FloatMap.add t1 l_ops' ops_of_tmo;
    ( match op with
	| Wait_in fd ->
	    let (i,o,p) = pset_find fd in
	    pset_add fd (false,o,p)
	| Wait_out fd ->
	    let (i,o,p) = pset_find fd in
	    pset_add fd (i,false,p)
	| Wait_oob fd ->
	    let (i,o,p) = pset_find fd in
	    pset_add fd (i,o,false)
	| Wait _ ->
	    ()
    )
    (* TODO: run close action *)

end
