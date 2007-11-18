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


(* Misc *)

let int_of_file_descr =
  match Sys.os_type with
    | "Unix" | "Cygwin" ->
	(fun fd -> (Obj.magic (fd:file_descr) : int))
    | _ ->
	invalid_arg "Netsys.int_of_file_descr"

let file_descr_of_int =
  match Sys.os_type with
    | "Unix" | "Cygwin" ->
	(fun n -> (Obj.magic (n:int) : file_descr))
    | _ ->
	invalid_arg "Netsys.file_descr_of_int"


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

type poll_array = string
type poll_in_events = int
type poll_out_events = int
type poll_cell =
    { mutable poll_fd : Unix.file_descr;
      mutable poll_events : poll_in_events;
      mutable poll_revents : poll_out_events;
    }

let null_poll_cell =
  { poll_fd = Unix.stdin;
    poll_events = 0;
    poll_revents = 0
  }

external mk_string_poll_cell : Unix.file_descr -> int -> int -> string
  = "netsys_mk_string_poll_cell"

external rd_string_poll_cell : string -> (Unix.file_descr * int * int)
  = "netsys_rd_string_poll_cell"

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

let poll_input_result p = p land pollin_const <> 0
let poll_output_result p = p land pollout_const <> 0
let poll_priority_result p = p land pollpri_const <> 0
let poll_error_result p = p land pollerr_const <> 0
let poll_hangup_result p = p land pollhup_const <> 0
let poll_invalid_result p = p land pollnval_const <> 0

let poll_array_length a =
  String.length a / the_pollfd_size

let set_poll_cell a k c =
  if k < 0 || k >= poll_array_length a then
    invalid_arg "Netsys.set_poll_cell";
  let u = mk_string_poll_cell c.poll_fd c.poll_events 0 in
  String.blit u 0 a (k*the_pollfd_size) the_pollfd_size

let get_poll_cell a k =
  if k < 0 || k >= poll_array_length a then
    invalid_arg "Netsys.get_poll_cell";
  let u = String.create the_pollfd_size in
  String.blit a (k*the_pollfd_size) u 0 the_pollfd_size;
  let (fd, ev, rev) = rd_string_poll_cell u in
  { poll_fd = fd; poll_events = ev; poll_revents = rev }

let blit_poll_array a1 k1 a2 k2 len =
  let l1 = poll_array_length a1 in
  let l2 = poll_array_length a2 in
  if len < 0 || k1 < 0 || k1+len > l1 || k2 < 0 || k2+len > l2 then
    invalid_arg "Netsys.get_poll_cell";
  String.blit 
    a1 (k1*the_pollfd_size) a2 (k2*the_pollfd_size) (len*the_pollfd_size)

let create_poll_array n =
  if not have_poll then
    invalid_arg "Netsys.create_poll_array: unsupported";
  let s = String.create (n*the_pollfd_size) in
  for k = 0 to n - 1 do
    set_poll_cell s k null_poll_cell
  done;
  s

external netsys_poll : poll_array -> int -> float -> int = "netsys_poll"

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
      n := netsys_poll a k max_tmo
    done;
    if !n = 0 then
      n := netsys_poll a k (max 0.0 (tmo -. !twaited));  
                           (* One sub is unavoidable *)
    !n
  )
  else
    netsys_poll a k (-1.0)
  
let restarting_poll a k tmo =
  restart_tmo (poll a k) tmo

let sleep t =
  let _,_,_ =
    Unix.select [] [] [] t in
  ()

let restarting_sleep t =
  restart_tmo sleep t

let poll_non_blocking fd r w pri =
  let a = create_poll_array 1 in
  set_poll_cell a 0 { poll_fd = fd; 
		      poll_events = poll_in_events r w pri;
		      poll_revents = poll_out_events()
		    };
  poll a 1 0.0 > 0


let is_readable fd =
  if have_poll then
    poll_non_blocking fd true false false
  else
    let l,_,_ = Unix.select [fd] [] [] 0.0 in
    l <> []

let is_writable fd =
  if have_poll then
    poll_non_blocking fd false true false
  else
    let _,l,_ = Unix.select [] [fd] [] 0.0 in
    l <> []

let is_prireadable fd =
  if have_poll then
    poll_non_blocking fd false false true
  else
    let _,_,l = Unix.select [] [] [fd] 0.0 in
    l <> []

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
