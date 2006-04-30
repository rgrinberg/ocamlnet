(* $Id: unix_exts.ml 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

open Unix;;

(* Misc *)

let int_of_file_descr fd = (Obj.magic (fd:file_descr) : int);;
let file_descr_of_int n  = (Obj.magic (n:int) : file_descr);;

external _exit : int -> unit = "unix__exit";;

(* Limits  & resources *)

external sysconf_open_max : unit -> int = "unix_sysconf_open_max";;

(* Process groups, sessions, terminals *)

external getpgid : int -> int = "unix_getpgid";;
let getpgrp() = getpgid 0;;
external setpgid : int -> int -> unit = "unix_setpgid";;
let setpgrp() = setpgid 0 0;;

external tcgetpgrp : file_descr -> int = "unix_tcgetpgrp";;
external tcsetpgrp : file_descr -> int -> unit = "unix_tcsetpgrp";;

external ctermid : unit -> string = "unix_ctermid";;
external ttyname : file_descr -> string = "unix_ttyname";;

external getsid : int -> int = "unix_getsid";;

(* Users and groups *)

external setreuid : int -> int -> unit = "unix_setreuid";;
external setregid : int -> int -> unit = "unix_setregid";;
