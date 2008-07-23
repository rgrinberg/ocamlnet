(* $Id$ *)

open Unix

let rec restart f arg =
  try 
    f arg
  with
    | Unix.Unix_error(Unix.EINTR,_,_) ->
	restart f arg



let restart_tmo f tmo =
  let t0 = Unix.gettimeofday() in
  let rec tryagain t_elapsed =
    let tmo' = tmo -. t_elapsed in
    try
      f (max tmo' 0.0)
    with
      | Unix.Unix_error(Unix.EINTR,_,_) ->
	  let t1 = Unix.gettimeofday() in
	  tryagain (t1 -. t0)
  in
  if tmo > 0.0 then
    tryagain 0.0
  else
    restart f tmo


let restarting_select fd_rd fd_wr fd_oob tmo =
  restart_tmo (Unix.select fd_rd fd_wr fd_oob) tmo


let rec really_write fd s pos len =
  if len > 0 then
    try
      let n = Unix.single_write fd s pos len in
      really_write fd s (pos+n) (len-n)
    with
      | Unix.Unix_error(Unix.EINTR, _, _) ->
	  really_write fd s pos len
      | Unix.Unix_error( (Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
	  ignore(restart (Unix.select [] [fd] []) (-1.0));
	  really_write fd s pos len
  else
    ()


let blocking_read fd s pos len =
  let rec loop pos len p =
    if len > 0 then
      try
	let n = Unix.read fd s pos len in
	if n=0 then
	  p
	else
	  loop (pos+n) (len-n) (p+n)
      with
	| Unix.Unix_error(Unix.EINTR, _, _) ->
	    loop pos len p
	| Unix.Unix_error( (Unix.EAGAIN | Unix.EWOULDBLOCK), _, _) ->
	    ignore(restart (Unix.select [fd] [] []) (-1.0));
	    loop pos len p
    else
      p
  in
  loop pos len 0


let really_read fd s pos len =
  if len > 0 then
    let p = blocking_read fd s pos len in
    if p < len then raise End_of_file;
    ()
  else
    ()


let domain_of_inet_addr addr =
  Unix.domain_of_sockaddr(Unix.ADDR_INET(addr,0))

external unix_error_of_code : int -> Unix.error = "netsys_unix_error_of_code"


(* Misc *)

let int_of_file_descr =
  match Sys.os_type with
    | "Unix" | "Cygwin" ->
	(fun fd -> (Obj.magic (fd:file_descr) : int))
    | _ ->
	(fun fd -> invalid_arg "Netsys.int_of_file_descr")

let file_descr_of_int =
  match Sys.os_type with
    | "Unix" | "Cygwin" ->
	(fun n -> (Obj.magic (n:int) : file_descr))
    | _ ->
	(fun n -> invalid_arg "Netsys.file_descr_of_int")


external _exit : int -> unit = "netsys__exit";;

(* Limits  & resources *)

external sysconf_open_max : unit -> int = "netsys_sysconf_open_max";;

(* Process groups, sessions, terminals *)

external getpgid : int -> int = "netsys_getpgid";;
let getpgrp() = getpgid 0;;
external setpgid : int -> int -> unit = "netsys_setpgid";;
let setpgrp() = setpgid 0 0;;

external tcgetpgrp : file_descr -> int = "netsys_tcgetpgrp";;
external tcsetpgrp : file_descr -> int -> unit = "netsys_tcsetpgrp";;

external ctermid : unit -> string = "netsys_ctermid";;
external ttyname : file_descr -> string = "netsys_ttyname";;

external getsid : int -> int = "netsys_getsid";;

(* Users and groups *)

external setreuid : int -> int -> unit = "netsys_setreuid";;
external setregid : int -> int -> unit = "netsys_setregid";;

(* poll *)

external pollfd_size : unit -> int = "netsys_pollfd_size"

let the_pollfd_size = pollfd_size()

let have_poll = the_pollfd_size > 0

type poll_in_events = int
type poll_out_events = int
type poll_cell =
    { mutable poll_fd : Unix.file_descr;
      mutable poll_events : poll_in_events;
      mutable poll_revents : poll_out_events;
    }
type poll_mem
type poll_array =
  | Poll_mem of poll_mem * int (*length*)
  | Poll_emu of poll_cell array

let null_poll_cell =
  { poll_fd = Unix.stdin;
    poll_events = 0;
    poll_revents = 0
  }

external mk_poll_mem : int -> poll_mem
  = "netsys_mk_poll_mem"

external set_poll_mem : poll_mem -> int -> Unix.file_descr -> int -> int -> unit
  = "netsys_set_poll_mem"

external get_poll_mem : poll_mem -> int -> (Unix.file_descr * int * int)
  = "netsys_get_poll_mem"

external blit_poll_mem : poll_mem -> int -> poll_mem -> int -> int -> unit
  = "netsys_blit_poll_mem"

external poll_constants : unit -> int array = "netsys_poll_constants"

let the_poll_constants = poll_constants()

let pollin_const = the_poll_constants.(0)
let pollpri_const = the_poll_constants.(1)
let pollout_const = the_poll_constants.(2)
let pollerr_const = the_poll_constants.(3)
let pollhup_const = the_poll_constants.(4)
let pollnval_const = the_poll_constants.(5)

let poll_in_events inp out pri =
  (if inp then pollin_const else 0) lor
  (if out then pollout_const else 0) lor
  (if pri then pollpri_const else 0)

let poll_in_triple p =
  (p land pollin_const <> 0,
   p land pollout_const <> 0,
   p land pollpri_const <> 0
  )

let poll_out_events() = 0

let poll_result p = p <> 0
let poll_input_result p = p land pollin_const <> 0
let poll_output_result p = p land pollout_const <> 0
let poll_priority_result p = p land pollpri_const <> 0
let poll_error_result p = p land pollerr_const <> 0
let poll_hangup_result p = p land pollhup_const <> 0
let poll_invalid_result p = p land pollnval_const <> 0

let poll_array_length =
  function
    | Poll_mem(_,n) -> n
    | Poll_emu e -> Array.length e

let set_poll_cell a k c =
  if k < 0 || k >= poll_array_length a then
    invalid_arg "Netsys.set_poll_cell";
  match a with
    | Poll_mem(s,_) ->
	set_poll_mem s k c.poll_fd c.poll_events (* c.poll_revents *) 0 
    | Poll_emu e ->
	e.(k) <- { c with poll_fd = c.poll_fd } (* copy *)

let get_poll_cell a k =
  if k < 0 || k >= poll_array_length a then
    invalid_arg "Netsys.get_poll_cell";
  match a with
    | Poll_mem(s,_) ->
	let (fd, ev, rev) = get_poll_mem s k in
	{ poll_fd = fd; poll_events = ev; poll_revents = rev }
    | Poll_emu e ->
	let c = e.(k) in
	{ c with poll_fd = c.poll_fd }   (* copy *)

let blit_poll_array a1 k1 a2 k2 len =
  let l1 = poll_array_length a1 in
  let l2 = poll_array_length a2 in
  if len < 0 || k1 < 0 || k1+len > l1 || k2 < 0 || k2+len > l2 then
    invalid_arg "Netsys.get_poll_cell";
  match (a1, a2) with
    | (Poll_mem(s1,_), Poll_mem(s2,_)) ->
	blit_poll_mem s1 k1 s2 k2 len
    | (Poll_emu e1, Poll_emu e2) ->
	Array.blit e1 k1 e2 k2 len
    | _ ->
	assert false

let create_poll_array n =
  if have_poll then (
    let s = mk_poll_mem n in
    Poll_mem(s,n)
  )
  else (
    let e = Array.create n null_poll_cell in
    Poll_emu e
  )

external netsys_poll : poll_mem -> int -> float -> int = "netsys_poll"


let do_poll a k tmo =
  match a with
    | Poll_mem(s,_) ->
	netsys_poll s k tmo
    | Poll_emu e ->
	(* Emulate poll using Unix.select. This is slow! *)
	let l_inp = ref [] in
	let l_out = ref [] in
	let l_pri = ref [] in
	for j = 0 to k-1 do
	  let c = e.(j) in
	  let (f_inp, f_out, f_pri) = poll_in_triple c.poll_events in
	  if f_inp then l_inp := c.poll_fd :: !l_inp;
	  if f_out then l_out := c.poll_fd :: !l_out;
	  if f_pri then l_pri := c.poll_fd :: !l_pri;
	done;
	let (o_inp, o_out, o_pri) = Unix.select !l_inp !l_out !l_pri tmo in
	let a_inp = Array.of_list o_inp in
	let a_out = Array.of_list o_out in
	let a_pri = Array.of_list o_pri in
	Array.sort Pervasives.compare a_inp;
	Array.sort Pervasives.compare a_out;
	Array.sort Pervasives.compare a_pri;
	let n = ref 0 in
	for j = 0 to k-1 do
	  let c = e.(j) in
	  let g_inp = Netsys_util.mem_sorted_array c.poll_fd a_inp in
	  let g_out = Netsys_util.mem_sorted_array c.poll_fd a_out in
	  let g_pri = Netsys_util.mem_sorted_array c.poll_fd a_pri in
	  let rev =
	    (if g_inp then pollin_const else 0) lor
	    (if g_out then pollout_const else 0) lor
	    (if g_pri then pollpri_const else 0) in
	  c.poll_revents <- rev;
	  if rev <> 0 then incr n
	done;
	!n


let max_tmo =
  2147483.0

let poll a k tmo =
  if k < 0 || k > poll_array_length a then
    invalid_arg "Netsys.poll";
  (* tmo is restricted to max_int/1000, which is not very much on 32 bit
     systems. So loop if necessary
   *)
  if tmo >= 0.0 then (
    let twaited = ref 0.0 in
    let n = ref 0 in
    while !n = 0 && !twaited +. max_tmo < tmo do
      twaited := !twaited +. max_tmo;
      n := do_poll a k max_tmo
    done;
    if !n = 0 then
      n := do_poll a k (max 0.0 (tmo -. !twaited));  
                           (* One sub is unavoidable *)
    !n
  )
  else
    do_poll a k (-1.0)
  
let restarting_poll a k tmo =
  restart_tmo (poll a k) tmo

let sleep t =
  let _,_,_ =
    Unix.select [] [] [] t in
  ()

let restarting_sleep t =
  restart_tmo sleep t

let poll_non_blocking fd r w pri tmo =
  let a = create_poll_array 1 in
  set_poll_cell a 0 { poll_fd = fd; 
		      poll_events = poll_in_events r w pri;
		      poll_revents = poll_out_events()
		    };
  poll a 1 tmo > 0


let wait_until_readable fd tmo =
  if have_poll then
    poll_non_blocking fd true false false tmo
  else
    let l,_,_ = Unix.select [fd] [] [] tmo in
    l <> []

let wait_until_writable fd tmo =
  if have_poll then
    poll_non_blocking fd false true false tmo
  else
    let _,l,_ = Unix.select [] [fd] [] tmo in
    l <> []

let wait_until_prireadable fd tmo =
  if have_poll then
    poll_non_blocking fd false false true tmo
  else
    let _,_,l = Unix.select [] [] [fd] tmo in
    l <> []

let is_readable fd = wait_until_readable fd 0.0
let is_writable fd = wait_until_writable fd 0.0
let is_prireadable fd = wait_until_prireadable fd 0.0

let wait_until_connected fd tmo =
  match Sys.os_type with
    | "Win32" ->
	let l1,_,l2 = Unix.select [] [fd] [fd] tmo in
	l1 <> [] || l2 <> []
    | _ ->
	wait_until_writable fd tmo

let connect_check fd =
  let e_code = Unix.getsockopt_int fd Unix.SO_ERROR in
  try
    ignore(Unix.getpeername fd); 
    ()
  with
    | Unix.Unix_error(Unix.ENOTCONN,_,_) ->
	raise(Unix.Unix_error(unix_error_of_code e_code,
			      "connect_check", ""))


(* Optional POSIX functions *)

external have_fadvise : unit -> bool = "netsys_have_posix_fadvise"
type advice =
  | FADV_NORMAL
  | FADV_SEQUENTIAL
  | FADV_RANDOM
  | FADV_NOREUSE
  | FADV_WILLNEED
  | FADV_DONTNEED
external fadvise : Unix.file_descr -> int64 -> int64 -> advice -> unit
                 = "netsys_fadvise"

external have_fallocate : unit -> bool = "netsys_have_posix_fallocate"
external fallocate : Unix.file_descr -> int64 -> int64 -> unit
                   = "netsys_fallocate"

(* POSIX shared memory *)

external have_posix_shm : unit -> bool = "netsys_have_posix_shm"
type shm_open_flag =
  | SHM_O_RDONLY
  | SHM_O_RDWR
  | SHM_O_CREAT
  | SHM_O_EXCL
  | SHM_O_TRUNC
external shm_open : string -> shm_open_flag list -> int -> file_descr
  = "netsys_shm_open"
external shm_unlink : string -> unit = "netsys_shm_unlink"

type ioprio_target =
  | Ioprio_process of int
  | Ioprio_pgrp of int
  | Ioprio_user of int

type ioprio =
  | Noprio
  | Real_time of int
  | Best_effort of int
  | Idle

external ioprio_get : ioprio_target -> ioprio = "netsys_ioprio_get"
external ioprio_set : ioprio_target -> ioprio -> unit = "netsys_ioprio_set"

let have_ioprio() =
  try let _ = ioprio_get(Ioprio_process(Unix.getpid())) in true
  with _ -> false
