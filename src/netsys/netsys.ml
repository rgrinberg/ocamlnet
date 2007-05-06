(* $Id$ *)

open Unix

let rec restart f arg =
  try 
    f arg
  with
    | Unix.Unix_error(Unix.EINTR,_,_) ->
	restart f arg

let restarting_select fd_rd fd_wr fd_oob tmo =
  let t0 = Unix.gettimeofday() in
  
  let rec tryagain t_elapsed =
    let tmo' = tmo -. t_elapsed in
    if tmo' >= 0.0 then
      try
	Unix.select fd_rd fd_wr fd_oob tmo'
      with
	| Unix.Unix_error(Unix.EINTR,_,_) ->
	    let t1 = Unix.gettimeofday() in
	    tryagain (t1 -. t0)
    else
      ([], [], [])
  in

  if tmo > 0.0 then
    tryagain 0.0
  else
    restart (Unix.select fd_rd fd_wr fd_oob) tmo


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
