(* $Id$ *)

let is_win32 =
  Sys.os_type = "Win32"

external netsys_is_darwin : unit -> bool = "netsys_is_darwin"

let is_darwin =
  netsys_is_darwin()

let restart = Netsys_impl_util.restart
let restart_tmo = Netsys_impl_util.restart_tmo

let restarting_select fd_rd fd_wr fd_oob tmo =
  restart_tmo (Unix.select fd_rd fd_wr fd_oob) tmo

let sleep t =
  let _,_,_ =
    Unix.select [] [] [] t in
  ()

let restarting_sleep t =
  restart_tmo sleep t


let getpeername fd =
  try
    Unix.getpeername fd
  with
    | Unix.Unix_error(Unix.EINVAL,a1,a2) ->
	(* SUS defines EINVAL as "socket has been shut down". This is a bit
         * surprising for developers of Open Source OS where this is reported
         * as ENOTCONN. We map it here.
	 *)
	raise(Unix.Unix_error(Unix.ENOTCONN,a1,a2))


type fd_style =
    [ `Read_write
    | `Recv_send of Unix.sockaddr * Unix.sockaddr
    | `Recv_send_implied
    | `Recvfrom_sendto
    | `W32_pipe
    | `W32_event
    ]

let get_fd_style fd =
  let w32_obj_opt =
    try Some(Netsys_win32.lookup fd)
    with Not_found -> None in
  match w32_obj_opt with
    | Some (Netsys_win32.W32_pipe_helper _) ->
	`W32_pipe
    | Some (Netsys_win32.W32_event _) ->
	`W32_event
    | None ->
	(* Check whether we have a socket or not: *)
	try
	  let _socktype = Unix.getsockopt_int fd Unix.SO_TYPE in
	  (* Now check whether the socket is connected or not: *)
	  try
	    let sockaddr = Unix.getsockname fd in
	    let peeraddr = getpeername fd in
	    (* fd is a connected socket *)
	    `Recv_send(sockaddr,peeraddr)
	  with
	    | Unix.Unix_error(Unix.ENOTCONN,_,_) ->
		(* fd is an unconnected socket *)
		`Recvfrom_sendto
	    | Unix.Unix_error(Unix.ENOTSOCK,_,_) -> 
		failwith "Got unexpected ENOTSOCK" (* hopefully we never see this *)
	    | _ ->
		(* There are various error codes in use for socket types that
                   do not use addresses, e.g. socketpairs are considered
                   as not having addresses by some OS. Common are
                   EAFNOSUPPORT, EOPNOTSUPP, EINVAL. For simplicity we catch
                   here all, which is allowed as we already know that fd is a
                   socket.
		 *)
		`Recv_send_implied
	with
	  | Unix.Unix_error((Unix.ENOTSOCK|Unix.EINVAL),_,_) -> 
	      (* Note: EINVAL is used by some oldish OS in this case *)
	      (* fd is not a socket *)
	      `Read_write
	  | e ->
	      prerr_endline ("get_fd_style: saw strange error " ^ 
			       Netexn.to_string e);
	      assert false

let wait_until_readable fd_style fd tmo =
  if Netsys_posix.have_poll() then
    restart_tmo
      (Netsys_posix.poll_single fd true false false) tmo
  else
    match fd_style with
      | `Read_write when is_win32 ->  (* effectively not supported! *)
	  true
      | `W32_pipe ->
	  let ph = Netsys_win32.lookup_pipe_helper fd in
	  Netsys_win32.pipe_wait_rd ph tmo
      | `W32_event ->
	  let eo = Netsys_win32.lookup_event fd in
	  Netsys_win32.event_wait eo tmo
      | _ ->
	  let l,_,_ = restart_tmo (Unix.select [fd] [] []) tmo in
	  l <> []

let wait_until_writable fd_style fd tmo =
  if Netsys_posix.have_poll() then
    restart_tmo
      (Netsys_posix.poll_single fd false true false) tmo
  else
    match fd_style with
      | `Read_write when is_win32 ->  (* effectively not supported! *)
	  true
      | `W32_pipe ->
	  let ph = Netsys_win32.lookup_pipe_helper fd in
	  Netsys_win32.pipe_wait_wr ph tmo
      | `W32_event ->
	  let eo = Netsys_win32.lookup_event fd in
	  Netsys_win32.event_wait eo tmo
      | _ ->
	  let _,l,_ = restart_tmo (Unix.select [] [fd] []) tmo in
	  l <> []

let wait_until_prird fd_style fd tmo =
  if Netsys_posix.have_poll() then
    restart_tmo
      (Netsys_posix.poll_single fd false false true) tmo
  else
    match fd_style with
      | `Read_write when is_win32 ->  (* effectively not supported! *)
	  true
      | `W32_pipe ->
	  false
      | `W32_event ->
	  let eo = Netsys_win32.lookup_event fd in
	  Netsys_win32.event_wait eo tmo
      | _ ->
	  let _,_,l = restart_tmo (Unix.select [] [] [fd]) tmo in
	  l <> []


let is_readable fd_style fd = wait_until_readable fd_style fd 0.0
let is_writable fd_style fd = wait_until_writable fd_style fd 0.0
let is_prird fd_style fd = wait_until_prird fd_style fd 0.0


let gwrite fd_style fd s pos len =
  match fd_style with
    | `Read_write ->
	Unix.single_write fd s pos len
    | `Recv_send _ 
    | `Recv_send_implied ->
	Unix.send fd s pos len []
    | `Recvfrom_sendto ->
	failwith "Netsys.gwrite: the socket is unconnected"
    | `W32_pipe ->
	let ph = Netsys_win32.lookup_pipe_helper fd in
	Netsys_win32.pipe_write ph s pos len
    | `W32_event ->
	failwith "Netsys.gwrite: cannot write to event descriptor"


let rec really_gwrite fd_style fd s pos len =
  try
    let n = gwrite fd_style fd s pos len in
    really_gwrite fd_style fd s (pos+n) (len-n)
  with
    | Unix.Unix_error(Unix.EINTR, _, _) ->
	really_gwrite fd_style fd s pos len
    | Unix.Unix_error( (Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
	ignore(wait_until_writable fd_style fd (-1.0));
	really_gwrite fd_style fd s pos len


let gread fd_style fd s pos len =
  match fd_style with
    | `Read_write ->
	Unix.read fd s pos len
    | `Recv_send _ 
    | `Recv_send_implied ->
	Unix.recv fd s pos len []
    | `Recvfrom_sendto ->
	failwith "Netsys.gread: the socket is unconnected"
    | `W32_pipe ->
	let ph = Netsys_win32.lookup_pipe_helper fd in
	Netsys_win32.pipe_read ph s pos len
    | `W32_event ->
	failwith "Netsys.gread: cannot read from event descriptor"


let blocking_gread fd_style fd s pos len =
  let rec loop pos len p =
    if len >= 0 then
      try
	let n = gread fd_style fd s pos len in
	if n=0 then
	  p
	else
	  loop (pos+n) (len-n) (p+n)
      with
	| Unix.Unix_error(Unix.EINTR, _, _) ->
	    loop pos len p
	| Unix.Unix_error( (Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
	    ignore(wait_until_readable fd_style fd (-1.0));
	    loop pos len p
    else
      p
  in
  loop pos len 0


let really_gread fd_style fd s pos len =
  let p = blocking_gread fd_style fd s pos len in
  if p < len then raise End_of_file;
  ()


let wait_until_connected fd tmo =
  if is_win32 then
    let l1,_,l2 = Unix.select [] [fd] [fd] tmo in
    l1 <> [] || l2 <> []
  else
    wait_until_writable `Recv_send fd tmo


external unix_error_of_code : int -> Unix.error = "netsys_unix_error_of_code"


let connect_check fd =
  let e_code = Unix.getsockopt_int fd Unix.SO_ERROR in
  try
    ignore(getpeername fd); 
    ()
  with
    | Unix.Unix_error(Unix.ENOTCONN,_,_) ->
	raise(Unix.Unix_error(unix_error_of_code e_code,
			      "connect_check", ""))

(* Misc *)

let domain_of_inet_addr addr =
  Unix.domain_of_sockaddr(Unix.ADDR_INET(addr,0))

external int64_of_file_descr : Unix.file_descr -> int64
  = "netsys_int64_of_file_descr"
  (* Also occurs in netsys_win32.ml! *)

external _exit : int -> unit = "netsys__exit";;


external mcast_set_loop : Unix.file_descr -> bool -> unit 
  = "netsys_mcast_set_loop"
external mcast_set_ttl : Unix.file_descr -> int -> unit 
  = "netsys_mcast_set_ttl"
external mcast_add_membership : 
  Unix.file_descr -> Unix.inet_addr -> Unix.inet_addr -> unit 
  = "netsys_mcast_add_membership"
external mcast_drop_membership : 
  Unix.file_descr -> Unix.inet_addr -> Unix.inet_addr -> unit 
  = "netsys_mcast_drop_membership"



(* Compatibility with older ocamlnet versions *)

let really_write = really_gwrite `Read_write
let blocking_read = blocking_gread `Read_write
let really_read = really_gread `Read_write

let int_of_file_descr = Netsys_posix.int_of_file_descr
let file_descr_of_int = Netsys_posix.file_descr_of_int

let have_posix_shm = Netsys_posix.have_posix_shm

type shm_open_flag = 
    Netsys_posix.shm_open_flag =
  | SHM_O_RDONLY
  | SHM_O_RDWR
  | SHM_O_CREAT
  | SHM_O_EXCL
  | SHM_O_TRUNC

let shm_open = Netsys_posix.shm_open
let shm_unlink = Netsys_posix.shm_unlink
