(* $Id$ *)

(** Some POSIX system calls missing in the [Unix] module *)

open Unix


(** {1 Helper functions} *)

val restart : ('a -> 'b) -> 'a -> 'b
  (** [restart f arg] calls [f arg], and restarts this call if the
    * exception [Unix_error(EINTR,_,_)] is caught.
    *
    * Note that there are some cases where this handling of [EINTR] is
    * not sufficient:
    * - Functions that have a timeout argument like [Unix.select]: When
    *   [EINTR] is caught the timeout should be adjusted.
    * - [Unix.connect] with a blocking descriptor because this is not
    *   well-enough specified by POSIX
   *)

val restarting_select : 
      Unix.file_descr list -> Unix.file_descr list -> Unix.file_descr list ->
      float ->
        (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list)
  (** A wrapper around [Unix.select] that handles the [EINTR] condition *)

val really_write : Unix.file_descr -> string -> int -> int -> unit
  (** [really_write fd s pos len]: Writes exactly the [len] bytes from [s]
    * to [fd] starting at [pos]. The conditions [EINTR], [EAGAIN] and
    * [EWOULDBLOCK] are handled.
   *)

val blocking_read : Unix.file_descr -> string -> int -> int -> int
  (** [let p = blocking_read fd s pos len]: Reads exactly [p] bytes from [fd]
    * and stores them in [s] starting at [pos] where [p] is the minimum
    * of [len] and the number of bytes that are available on [fd] until
    * the end of the file. If the function is called with [len>0] but 
    * returns less than [len] this indicates end of file.
    * The conditions [EINTR], [EAGAIN] and [EWOULDBLOCK] are handled.
   *)

val really_read : Unix.file_descr -> string -> int -> int -> unit
  (** [really_read fd s pos len]: Reads exactly [len] bytes from [fd]
    * and stores them in [s] starting at [pos]. If the end of file condition
    * is seen before [len] bytes are read, the exception [End_of_file]
    * is raised, and it is unspecified how many bytes have been stored in
    * [s]. The conditions [EINTR], [EAGAIN] and [EWOULDBLOCK] are handled.
   *)

val domain_of_inet_addr : Unix.inet_addr -> Unix.socket_domain
  (** Returns the socket domain of Internet addresses, i.e. whether the
    * address is IPv4 or IPv6
   *)


(** {1 Standard POSIX functions} *)

(* Misc *)

val int_of_file_descr : file_descr -> int
  (** Return the file descriptor as integer *)

val file_descr_of_int : int -> file_descr
  (** Make a file descriptor from an integer *)

external _exit : int -> unit = "netsys__exit"
  (** Exit the program immediately without running the atexit handlers.
   * The argument is the exit code, just as for [exit].
   *)

(* Limits  & resources *)

external sysconf_open_max : unit -> int = "netsys_sysconf_open_max"
  (** Return the maximum number of open file descriptor per process.
   * It is also ensured that for every file descriptor [fd]:
   * [fd < sysconf_open_max()]
   *)

(* Process groups, sessions, terminals *)

external getpgid : int -> int = "netsys_getpgid"
  (** Return the process group ID of the process with the passed PID.
   * For the number 0, the process group ID of the current process is
   * returned.
   *)

val getpgrp : unit -> int
  (** Same as [getpgid 0], i.e. returns the process group ID of the
   * current process.
   *)

external setpgid : int -> int -> unit = "netsys_setpgid"
  (** [setpgid pid pgid]: Set the process group ID of the process [pid]
   * to [pgid]. If [pid = 0], the process group ID of the current process
   * is changed. If [pgid = 0], as process group ID the process ID of the
   * process referenced by [pid] is used.
   *
   * It is only possible for a process to join a process group if both
   * belong to the same session.
   *)

val setpgrp : unit -> unit
  (** Same as [setpgid 0 0]: A new process group ID is created, and the
   * current process becomes its sole member.
   *)

external tcgetpgrp : file_descr -> int = "netsys_tcgetpgrp"
  (** Return the process group ID of the foreground process group of
   * the session associated with the file descriptor, which must be
   * a tty.
   *)

external tcsetpgrp : file_descr -> int -> unit = "netsys_tcsetpgrp"
  (** Sets the foreground process group ID of the session associated
   * with the file descriptor, which must be a tty.
   *)

external ctermid : unit -> string = "netsys_ctermid"
  (** Returns the name of the controlling tty of the current process 
   * as pathname to a device file
   *)

external ttyname : file_descr -> string = "netsys_ttyname"
  (** Returns the name of the controlling tty referred to by the
   * file descriptor.
   *)

external getsid : int -> int = "netsys_getsid"
  (** Returns the session ID of the process with the passed PID.
   * For the PID 0, the session ID of the current process is returned.
   *)

(* Users and groups *)

external setreuid : int -> int -> unit = "netsys_setreuid"
  (** Changes both the real and the effective user ID of the current
   * process.
   *)

external setregid : int -> int -> unit = "netsys_setregid"
  (** Changes both the real and the effective group ID of the current
   * process.
   *)



(** {1 POSIX Shared Memory} *)

external have_posix_shm : unit -> bool = "netsys_have_posix_shm"
  (** Returns whether the OS supports POSIX shared memory *)

type shm_open_flag =
  | SHM_O_RDONLY
  | SHM_O_RDWR
  | SHM_O_CREAT
  | SHM_O_EXCL
  | SHM_O_TRUNC

external shm_open : string -> shm_open_flag list -> int -> file_descr
  = "netsys_shm_open"
  (** Opens a shared memory object. The first arg is the name of the
    * object. The name must begin with a slash, but there must be no
    * further slash in it (e.g. "/sample"). The second arg are the
    * open flags. The third arg are the permission bits.
    *
    * The open flags are interpreted as follows:
    * - [SHM_O_RDONLY]: Open the object for read access
    * - [SHM_O_RDWR]: Open the object for read-write access
    * - [SHM_O_CREAT]: Create the object if it does not exist
    * - [SHM_O_EXCL]: If [SHM_O_CREAT] was also specified, and a an object
    *   with the given name already exists, return an error
    *   ([Unix.EEXIST]).
    * - [SHM_O_TRUNC]: If the object already exists, truncate it to 
    *   zero bytes
    *
    * One of [SHM_O_RDONLY] or [SHM_O_RDWR] must be given.
    *
    * On success, the function returns a file descriptor representing the
    * object. To access the object, one has to memory-map this file
    * (in O'Caml use one of the [map_file] functions in the [Bigarray]
    * module). Use [Unix.ftruncate] to resize the object.
    *
    * Note that it is unspecified whether this file pops up somewhere
    * in the file system, and if so, where.
    *
    * If a system error occurs, the function raises a [Unix.Unix_error]
    * exception.
   *)

external shm_unlink : string -> unit = "netsys_shm_unlink"
  (** Unlinks the name for a shared memory object *)
