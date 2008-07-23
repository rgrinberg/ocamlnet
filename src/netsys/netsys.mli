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

val restart_tmo : (float -> 'b) -> float -> 'b
  (** [restart_tmo f tmo] calls [f] with a timeout argument [tmo], and
    * restarted the call if the exception [Unix_error(EINTR,_,_)] is caught.
    * In the restart case, the timeout argument is reduced by the
    * already elapsed time.
    *
    * Negative timeout arguments are interpreted as "no timeout".
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

val is_readable : Unix.file_descr -> bool
val is_writable : Unix.file_descr -> bool
val is_prireadable : Unix.file_descr -> bool
  (** Test whether the descriptor would not block if one of the input,
      output, or priority input operations were done.

      If [poll] is supported, these tests are based on it, and file descriptors
      beyond the numeric limit for [select] can be tested. Otherwise, these tests
      are done by calling [Unix.select], and the restrictions of this syscall
      apply.
   *)

val wait_until_readable : Unix.file_descr -> float -> bool
val wait_until_writable : Unix.file_descr -> float -> bool
val wait_until_prireadable : Unix.file_descr -> float -> bool
  (** Wait until an operation for a single descriptor becomes possible.
      The float argument is the timeout (negative value means no timeout).
      Returns whether the operation is possible ([true]). Otherwise,
      there was a timeout ([false]).

      Like the [is_*] functions, these tests are based on [poll] if
      available, and on [select] otherwise.

      Additional Unix conditions like [EINTR] are not handled (use 
      [restart_tmo] for that).
   *)

val wait_until_connected : Unix.file_descr -> float -> bool
  (** After a non-blocking connect has been initiated, this function can be
      used to wait until (1) the connect is successful, or (2) the connect
      fails, or (3) the operation times out. The [float] argument is the
      timeout value (negative value means no timeout).
      The function returns [true] for the cases (1) and (2), and [false]
      for case (3). The cases (1) and (2) can be further analyzed by
      calling [connect_check] (see below).

      On POSIX, this function is identical to [wait_until_writable]. On
      Win32 it is different.

      Additional Unix conditions like [EINTR] are not handled (use 
      [restart_tmo] for that).
   *)

val connect_check : Unix.file_descr -> unit
  (** Tests whether the socket is connected with the peer after calling
      Unix.connect. If the socket is connected, the function returns normally.
      Otherwise, the current socket error is raised as a [Unix.Unix_error]
      exception. This function is intended to be called after a 
      non-blocking connect has been initiated, and the success or error
      is indicated (e.g. after [wait_until_connected] returns).

      Side effect: The per-socket error code may be reset.
   *)

val sleep : float -> unit
val restarting_sleep : float -> unit
  (** Sleep for the passed time. [restarting_sleep] additionally handles
      [EINTR].
   *)

val domain_of_inet_addr : Unix.inet_addr -> Unix.socket_domain
  (** Returns the socket domain of Internet addresses, i.e. whether the
    * address is IPv4 or IPv6
   *)

val unix_error_of_code : int -> Unix.error
  (** Converts an integer error into the corresponding variant *)

(** {1 File descriptor polling} *)

type poll_array
  (** The array of [poll_cell] entries *)

type poll_in_events = int
type poll_out_events = int
  (** Poll events *)

type poll_cell =
    { mutable poll_fd : Unix.file_descr;
      mutable poll_events : poll_in_events;
      mutable poll_revents : poll_out_events;
    }
  (** The poll cell refers to the descriptor [poll_fd]. The [poll_events]
      are the events the descriptor is polled for. The [poll_revents]
      are the output events.
   *)

val poll_in_events : bool -> bool -> bool -> poll_in_events
  (** [poll_in_events inp out pri]: Create a set of in events consisting
      of the bits [inp], [out], and [pri]. [inp] means to poll for 
      input data, [out] to poll for output data, and [pri] to poll for urgent
      input data.
   *)

val poll_in_triple : poll_in_events -> bool * bool * bool
  (** Looks into a [poll_in_events] value, and returns the triple
      [(inp,out,pri)].
   *)

val poll_out_events : unit -> poll_out_events
  (** Create an empty set of [poll_out_events], for initilization
      of poll cells.
   *)

val poll_result : poll_out_events -> bool
  (** Look whether there is any event in [poll_out_events] *)

val poll_input_result : poll_out_events -> bool
val poll_output_result : poll_out_events -> bool
val poll_priority_result : poll_out_events -> bool
val poll_error_result : poll_out_events -> bool
val poll_hangup_result : poll_out_events -> bool
val poll_invalid_result : poll_out_events -> bool
  (** Look for the bit in [poll_out_events] and return the status *)

val create_poll_array : int -> poll_array
  (** Create a poll array with the given size. The [poll_fd] member is
      initialized with [Unix.stdin], and the two event members are empty.
   *)

val set_poll_cell : poll_array -> int -> poll_cell -> unit
  (** [set_poll_cell a k c]: Sets the poll cell [k] to [c].
      The index [k] must be in the range from [0] to [N-1] when [N] is the
      length of the poll array.
   *)

val get_poll_cell : poll_array -> int -> poll_cell
  (** [get_poll_cell a k]: Returns the poll cell [k].
      The index [k] must be in the range from [0] to [N-1] when [N] is the
      length of the poll array.
   *)

val blit_poll_array : poll_array -> int -> poll_array -> int -> int -> unit
  (** [blit_poll_array a1 p1 a2 p2 len]: Copies the [len] cells at index [p1]
      from [a1] to [a2] at index [p2].
   *)

val poll_array_length : poll_array -> int
  (** Return the number of cells in the poll array *)

val poll : poll_array -> int -> float -> int
  (** [poll a n tmo]: Poll for the events of the cells 0 to [n-1] of 
      poll array [a], and set the [poll_revents] member of all cells.
      Wait for at most [tmo] seconds (a negative value means there is
      no timeout). Returns the number of ready file descriptors.

      On platforms without native support for [poll] the function is
      emulated using [Unix.select]. Note, however, that there is a
      performance penalty for the emulation, and that the output
      flags [poll_error_result], [poll_hangup_result], and
      [poll_invalid_result] are not emulated.
   *)

val restarting_poll :
      poll_array -> int -> float -> int
  (** A wrapper around [poll] (see below) that handles the [EINTR] condition *)


(** {1 Standard POSIX functions} *)

(** These are not available on Win32 *)

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


(** {1 Optional POSIX functions} *)

external have_fadvise : unit -> bool = "netsys_have_posix_fadvise"
  (** Returns whether the OS supports the fadvise POSIX option *)

type advice =
  | FADV_NORMAL
  | FADV_SEQUENTIAL
  | FADV_RANDOM
  | FADV_NOREUSE
  | FADV_WILLNEED
  | FADV_DONTNEED


external fadvise : Unix.file_descr -> int64 -> int64 -> advice -> unit
                 = "netsys_fadvise"
  (** Advises to load pages into the page table from the file, or to remove
      such pages.
   *)

external have_fallocate : unit -> bool = "netsys_have_posix_fallocate"
  (** Returns whether the OS supports the fallocate POSIX option *)

external fallocate : Unix.file_descr -> int64 -> int64 -> unit
                   = "netsys_fallocate"
  (** Allocate space for the file and the specified file region *)


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


(** {1 Linux I/O Priorities} *)

(** These system calls are only available on Linux since kernel 2.6.13,
    and not even on every architecture. i386, x86_64, ia64, and PPC are
    known to work. 

    Per-process I/O priorities are currently only supported by the
    CFQ I/O scheduler.
 *)

val have_ioprio : unit -> bool
  (** Returns [true] if the system call [ioprio_get] is supported *)

type ioprio_target =
  | Ioprio_process of int   (** A single process *)
  | Ioprio_pgrp of int      (** A process group *)
  | Ioprio_user of int      (** All processes owned by this user *)

type ioprio =
  | Noprio                  (** I/O prioritization is unsupported by block layer *)
  | Real_time of int        (** 0..7 (higest..lowest prio) *)
  | Best_effort of int      (** 0..7 (higest..lowest prio) *)
  | Idle


external ioprio_get : ioprio_target -> ioprio = "netsys_ioprio_get"
    (** Retrieve the priority of the target. If several processes match the
        target, the highest priority is returned. If no process matches,
        the unix error [ESRCH] will be raised.
     *)

external ioprio_set : ioprio_target -> ioprio -> unit = "netsys_ioprio_set"
    (** Sets the priority of the target processes. *)

(**/**)

val pollin_const : int
val pollpri_const : int
val pollout_const : int
val pollerr_const : int
val pollhup_const : int
val pollnval_const : int
