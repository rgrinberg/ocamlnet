(* $Id$ *)

open Unix

let rec restart f arg =
  try 
    f arg
  with
    | Unix.Unix_error(Unix.EINTR,_,_) ->
	restart f arg


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
