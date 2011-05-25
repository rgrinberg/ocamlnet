(* $Id$ *)

type channel_binding_id = int
 
type conn_state = [ `Inactive of channel_binding_id | `Active of < > ]

class type connection_cache =
object
  method get_connection_state : Unix.file_descr -> conn_state
  method set_connection_state : Unix.file_descr -> conn_state -> unit
  method find_inactive_connection : Unix.sockaddr -> channel_binding_id -> Unix.file_descr
  method find_my_connections : < > -> Unix.file_descr list
  method close_connection : Unix.file_descr -> unit
  method close_all : unit -> unit
end



class restrictive_cache() : connection_cache =
object(self)
  val mutable active_conns = Hashtbl.create 10
  val mutable rev_active_conns = Hashtbl.create 10

  method get_connection_state fd =
    `Active(Hashtbl.find active_conns fd)

  method set_connection_state fd state =
    match state with
      | `Active owner ->
	  Hashtbl.replace active_conns fd owner;
	  let fd_list = 
	    try Hashtbl.find rev_active_conns owner with Not_found -> [] in
	  if not (List.mem fd fd_list) then
	    Hashtbl.replace rev_active_conns owner (fd :: fd_list);
	  
      | `Inactive _ ->
	  self # remove_connection fd;
	  raise Not_found

  method find_inactive_connection _ _ = raise Not_found

  method find_my_connections owner =
    try
      Hashtbl.find rev_active_conns owner
    with
	Not_found -> []

  method private remove_connection fd =
    ( try
	let owner = Hashtbl.find active_conns fd in
	let fd_list = 
	  try Hashtbl.find rev_active_conns owner with Not_found -> [] in
	let fd_list' =
	  List.filter (fun fd' -> fd' <> fd) fd_list in
	Hashtbl.replace rev_active_conns owner fd_list'
      with
	  Not_found -> ()
    );
    Hashtbl.remove active_conns fd;


  method close_connection fd =
    self # remove_connection fd;
    Netlog.Debug.release_fd fd;
    Unix.close fd

  method close_all () =
    Hashtbl.iter
      (fun fd _ ->
	 Netlog.Debug.release_fd fd;
	 Unix.close fd)
      active_conns;
    Hashtbl.clear active_conns;
    Hashtbl.clear rev_active_conns
	  
end


class aggressive_cache () : connection_cache =
object(self)
  val mutable active_conns = Hashtbl.create 10
    (* maps file_descr to owner *)
  val mutable rev_active_conns = Hashtbl.create 10
    (* maps owner to file_descr list *)
  val mutable inactive_conns = Hashtbl.create 10
    (* maps file_descr to (cb,sockaddr) *)
  val mutable rev_inactive_conns = Hashtbl.create 10
    (* maps (cb,sockaddr) to file_descr list *)

  method get_connection_state fd =
    try
      `Active(Hashtbl.find active_conns fd)
    with
	Not_found ->
	  let (cb,_) = Hashtbl.find inactive_conns fd in
	  `Inactive cb

  method set_connection_state fd state =
    match state with
      | `Active owner ->
	  self # forget_inactive_connection fd;
	  Hashtbl.replace active_conns fd owner;
	  let fd_list = 
	    try Hashtbl.find rev_active_conns owner with Not_found -> [] in
	  if not (List.mem fd fd_list) then
	    Hashtbl.replace rev_active_conns owner (fd :: fd_list);
      | `Inactive cb ->
	  ( try
	      let peer = Netsys.getpeername fd in
	      self # forget_active_connection fd;
	      Hashtbl.replace inactive_conns fd (cb,peer);
	      let fd_list =
		try Hashtbl.find rev_inactive_conns (cb,peer)
		with Not_found -> [] in
	      if not (List.mem fd fd_list) then
		Hashtbl.replace rev_inactive_conns (cb,peer) (fd :: fd_list)
	    with
	      | Unix.Unix_error(Unix.ENOTCONN,_,_) ->
		  self # close_connection fd
	  )

  method find_inactive_connection peer cb =
    match Hashtbl.find rev_inactive_conns (cb,peer) with
      | [] -> raise Not_found
      | fd :: _ -> fd

  method find_my_connections owner =
    try
      Hashtbl.find rev_active_conns owner
    with
	Not_found -> []

  method private forget_active_connection fd =
    ( try
	let owner = Hashtbl.find active_conns fd in
	let fd_list = 
	  try Hashtbl.find rev_active_conns owner with Not_found -> [] in
	let fd_list' =
	  List.filter (fun fd' -> fd' <> fd) fd_list in
	if fd_list' <> [] then 
	  Hashtbl.replace rev_active_conns owner fd_list'
	else
	  Hashtbl.remove rev_active_conns owner
      with
	  Not_found -> ()
    );
    Hashtbl.remove active_conns fd;
   

  method private forget_inactive_connection fd =
    try
      let cb, peer = Hashtbl.find inactive_conns fd in
      (* Do not use getpeername! fd might be disconnected in the meantime! *)
      let fd_list = 
	try Hashtbl.find rev_inactive_conns (cb,peer) with Not_found -> [] in
      let fd_list' =
	List.filter (fun fd' -> fd' <> fd) fd_list in
      if fd_list' <> [] then 
	Hashtbl.replace rev_inactive_conns (cb,peer) fd_list'
      else
	Hashtbl.remove rev_inactive_conns (cb,peer);
      Hashtbl.remove inactive_conns fd;
    with
      | Not_found ->
	  ()


  method close_connection fd =
    self # forget_active_connection fd;
    self # forget_inactive_connection fd;
    Netlog.Debug.release_fd fd;
    Unix.close fd


  method close_all () =
    Hashtbl.iter
      (fun fd _ ->
	 Netlog.Debug.release_fd fd;
	 Unix.close fd)
      active_conns;
    Hashtbl.clear active_conns;
    Hashtbl.clear rev_active_conns;
    Hashtbl.iter
      (fun fd _ ->
	 Netlog.Debug.release_fd fd;
	 Unix.close fd)
      inactive_conns;
    Hashtbl.clear inactive_conns;
    Hashtbl.clear rev_inactive_conns
end
