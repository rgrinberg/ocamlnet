(* $Id: unix_exts.mli 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Some POSIX system calls missing in the [Unix] module *)


open Unix;;

(* Misc *)

val int_of_file_descr : file_descr -> int;;
  (** Return the file descriptor as integer *)

val file_descr_of_int : int -> file_descr;;
  (** Make a file descriptor from an integer *)

external _exit : int -> unit = "unix__exit";;
  (** Exit the program immediately without running the atexit handlers.
   * The argument is the exit code, just as for [exit].
   *)

(* Limits  & resources *)

external sysconf_open_max : unit -> int = "unix_sysconf_open_max";;
  (** Return the maximum number of open file descriptor per process.
   * It is also ensured that for every file descriptor [fd]:
   * [fd < sysconf_open_max()]
   *)

(* Process groups, sessions, terminals *)

external getpgid : int -> int = "unix_getpgid";;
  (** Return the process group ID of the process with the passed PID.
   * For the number 0, the process group ID of the current process is
   * returned.
   *)

val getpgrp : unit -> int;;
  (** Same as [getpgid 0], i.e. returns the process group ID of the
   * current process.
   *)

external setpgid : int -> int -> unit = "unix_setpgid";;
  (** [setpgid pid pgid]: Set the process group ID of the process [pid]
   * to [pgid]. If [pid = 0], the process group ID of the current process
   * is changed. If [pgid = 0], as process group ID the process ID of the
   * process referenced by [pid] is used.
   *
   * It is only possible for a process to join a process group if both
   * belong to the same session.
   *)

val setpgrp : unit -> unit;;
  (** Same as [setpgid 0 0]: A new process group ID is created, and the
   * current process becomes its sole member.
   *)

external tcgetpgrp : file_descr -> int = "unix_tcgetpgrp";;
  (** Return the process group ID of the foreground process group of
   * the session associated with the file descriptor, which must be
   * a tty.
   *)

external tcsetpgrp : file_descr -> int -> unit = "unix_tcsetpgrp";;
  (** Sets the foreground process group ID of the session associated
   * with the file descriptor, which must be a tty.
   *)

external ctermid : unit -> string = "unix_ctermid";;
  (** Returns the name of the controlling tty of the current process 
   * as pathname to a device file
   *)

external ttyname : file_descr -> string = "unix_ttyname";;
  (** Returns the name of the controlling tty referred to by the
   * file descriptor.
   *)

external getsid : int -> int = "unix_getsid";;
  (** Returns the session ID of the process with the passed PID.
   * For the PID 0, the session ID of the current process is returned.
   *)

(* Users and groups *)

external setreuid : int -> int -> unit = "unix_setreuid";;
  (** Changes both the real and the effective user ID of the current
   * process.
   *)

external setregid : int -> int -> unit = "unix_setregid";;
  (** Changes both the real and the effective group ID of the current
   * process.
   *)

(* Still missing:
 * - improved signal handling
 * - async I/O
 * - poll syscall
 * - message catalogues
 * - syslog
 * - crypt
 * - fnmatch
 * - get/setitimer
 * - pseudo ttys (how to do it in a portable way?)
 *)
