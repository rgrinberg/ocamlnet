(* $Id$ *)

open Netsys_pollset


let poll_based_pollset minsize : pollset =
  let () =
    if minsize < 1 then invalid_arg "Netsys_pollset.poll_based_pollset";
    () in
object(self)
  val mutable pa = Netsys.create_poll_array minsize
  val mutable free = []
  val mutable ht = Hashtbl.create minsize

  initializer (
    for k = 0 to minsize - 1 do
      free <- k :: free
    done
  )

  method find fd =
    let k = Hashtbl.find ht fd in
    let c = Netsys.get_poll_cell pa k in
    assert(c.Netsys.poll_fd = fd);
    c.Netsys.poll_events

  method add fd ev =
    try
      let k = Hashtbl.find ht fd in    (* or Not_found *)
      let c = Netsys.get_poll_cell pa k in
      assert(c.Netsys.poll_fd = fd);
      c.Netsys.poll_events <- ev;
      Netsys.set_poll_cell pa k c
    with
	Not_found ->
	  let k =
	    match free with
	      | k :: free' ->
		  free <- free';
		  k
	      | [] ->
		  let l = Netsys.poll_array_length pa in
		  let pa' = Netsys.create_poll_array (2*l) in
		  Netsys.blit_poll_array pa 0 pa' 0 l;
		  pa <- pa';
		  for j = l+1 to 2*l-1 do
		    free <- j :: free
		  done;
		  l
	  in
	  Netsys.set_poll_cell pa k
	    { Netsys.poll_fd = fd;
	      poll_events = ev;
	      poll_revents = Netsys.poll_out_events()
	    };
	  Hashtbl.replace ht fd k

  method remove fd =
    try
      let k = Hashtbl.find ht fd in
      Netsys.set_poll_cell pa k
	{ Netsys.poll_fd = Unix.stdin;
	  poll_events = Netsys.poll_in_events false false false;
	  poll_revents = Netsys.poll_out_events()
	};
      free <- k :: free;
      Hashtbl.remove ht fd;
      let l = Netsys.poll_array_length pa in
      if l > minsize && 2 * (Hashtbl.length ht) < l then
	self # rebuild_array()
    with
	Not_found -> ()


  method private rebuild_array() =
    let n = Hashtbl.length ht in
    let l = max n minsize in
    let pa' = Netsys.create_poll_array l in
    let ht' = Hashtbl.create l in
    let j = ref 0 in
    Hashtbl.iter
      (fun fd k ->
	 let c = Netsys.get_poll_cell pa k in
	 Netsys.set_poll_cell pa' !j c;
	 Hashtbl.replace ht' fd !j;
	 incr j
      )
      ht;
    pa <- pa';
    ht <- ht';
    free <- [];
    for k = n to l-1 do
      free <- k :: free
    done


  method wait tmo =
    let l = Netsys.poll_array_length pa in
    let n = ref(Netsys.poll pa l tmo) in
    let r = ref [] in
    let k = ref 0 in
    while !n > 0 && !k < l do
      let c = Netsys.get_poll_cell pa !k in
      if Netsys.poll_result c.Netsys.poll_revents then (
	let fd = c.Netsys.poll_fd in
	let c_used =
	  try Hashtbl.find ht fd = !k with Not_found -> false in
	if c_used then
	  r := (fd, c.Netsys.poll_events, c.Netsys.poll_revents) :: !r;
	decr n
      );
      incr k
    done;
    !r
    

  method dispose() = ()


  method cancel_wait _ = assert false (* TODO *)
    

end
