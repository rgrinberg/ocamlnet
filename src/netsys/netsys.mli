(* $Id$ *)

(** System calls missing in the [Unix] module *)

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

val sleep : float -> unit
val restarting_sleep : float -> unit
  (** Sleep for the passed time. [restarting_sleep] additionally handles
      [EINTR].
   *)

val unix_error_of_code : int -> Unix.error
  (** Converts an integer error into the corresponding variant *)

val int64_of_file_descr : Unix.file_descr -> int64
  (** Returns the file descriptor as int64 number. Works for all OS. *)

external _exit : int -> unit = "netsys__exit"
  (** Exit the program immediately without running the atexit handlers.
   * The argument is the exit code, just as for [exit].
   *)


(** {1 Generic file descriptors} *)

(** Not all OS provide generic read/write functions, or some emulation
    layer does not allow to use a descriptor with read/write. In the
    following functions, the style of the descriptor can be passed along
    with the descriptor to select the right I/O method.
 *)

type fd_style =
    [ `Read_write
    | `Recv_send of Unix.sockaddr * Unix.sockaddr
    | `Recv_send_implied
    | `Recvfrom_sendto
    | `W32_pipe
    | `W32_event
    ]
  (** Some information what kind of operations are reasonable for descriptors:
      - [`Read_write]: The descriptor is neither a socket not one of the
        other special cases, so only read/write is possible if read/write
        is possible at all.
      - [`Recv_send(sockaddr,peeraddr)]: The descriptor is a connected socket.
        recv/send are the preferred operations.
      - [`Recvfrom_sendto]: The descriptor is an unconnected socket, and
        it is possible to ask for addresses when exchanging data, so 
        recvfrom/sendto are the preferred operations.
      - [`Recv_send_implied]: The descriptor is a socket with implied 
        connection. There are no socket addresses.
        recv/send are the preferred operations. It is not possible to call
        [getsockname] or [getpeername].
      - [`W32_pipe]: The descriptor is a Win32 named pipe as returned by
        {!Netsys_win32.pipe_descr}. 
      - [`W32_event]: The descriptor is a Win32 event as returned by
        {!Netsys_win32.create_event}. It is not possible to read/write
        with these descriptors.
   *)

val get_fd_style : Unix.file_descr -> fd_style
  (** Get the file descriptor style *)

val gread : fd_style -> Unix.file_descr -> string -> int -> int -> int
  (** [gread fd_style fd s pos len]: Reads up to [len] bytes from 
      descriptor [fd] which is supposed to support the I/O style 
      [fd_style], i.e. the right system call ([read], [recv],
      [recvfrom]) is chosen to read from the descriptor.
       After [n <= len] bytes have been read these are put into
      string [s] at positions [pos] to [pos+n-1], and [n] is returned.
      The function can fail with any I/O exception defined for the
      actually performed I/O operation. Whether the operation is blocking
      or non-blocking depends on the descriptor.

      If [len>0] but [n=0] the end of the input data is reached.
   *)

val blocking_gread : fd_style -> Unix.file_descr -> string -> int -> int -> int
  (** [let p = blocking_gread fd_style fd s pos len]: 
      Like [gread] up to [len] bytes are read from [fd] and stored in [s].
      If the I/O operation is blocking but the descriptor is in 
      non-blocking mode, this function blocks until the operation can
      be performed. If the operation is interrupted by a signal it is
      automatically restarted.

      If [n < len] the end of the input data is reached (where [n] is the
      returned number).

      See [wait_until_readable] below for further information which
      types of descriptors can be handled in non-blocking mode.
   *)

val really_gread : fd_style -> Unix.file_descr -> string -> int -> int -> unit
  (** [really_read fd_style fd s pos len]: Reads exactly [len] bytes from [fd]
      and stores them in [s] starting at [pos]. If the end of file condition
      is seen before [len] bytes are read, the exception [End_of_file]
      is raised, and it is unspecified how many bytes have been stored in
      [s]. Like [blocking_gread], non-blocking descriptors are forced
      to block until the operation becomes possible, and interruptions by
      signals are handled.

      See [wait_until_readable] below for further information which
      types of descriptors can be handled in non-blocking mode.
   *)

val gwrite : fd_style -> Unix.file_descr -> string -> int -> int -> int
  (** [gwrite fd_style fd s pos len]: Writes up to [len] bytes to
      descriptor [fd] which is supposed to support the I/O style 
      [fd_style], i.e. the right system call ([write], [send],
      [sendto]) is chosen to write to the descriptor.
    . The [n <= len] written bytes are taken from string [s],
      starting at position [pos] until [pos+n-1]. The number [n] is
      returned. The function can fail with any I/O exception defined for the
      actually performed I/O operation. Whether the operation is blocking
      or non-blocking depends on the descriptor.
   *)

val really_gwrite : fd_style -> Unix.file_descr -> string -> int -> int -> unit
  (** [really_write fd_style fd s pos len]: Writes exactly the [len] bytes
      from [s] to [fd] starting at [pos]. 
      If the I/O operation is blocking but the descriptor is in 
      non-blocking mode, this function blocks until the operation can
      be performed. If the operation is interrupted by a signal it is
      automatically restarted.

      See [wait_until_writable] below for further information which
      types of descriptors can be handled in non-blocking mode.
   *)

val is_readable : fd_style -> Unix.file_descr -> bool
val is_writable : fd_style -> Unix.file_descr -> bool
val is_prird : fd_style -> Unix.file_descr -> bool
  (** Test whether the descriptor would not block if one of the input,
      output, or priority input operations were done.

      On POSIX systems the tests work for a wide variety of descriptor 
      types (but not for regular files which are assumed to be always
      readable and writable).
      If the [poll] interface is available it is preferred over the
      [select] interface to conduct the test.

      On Win32, the tests are limited to sockets, named pipes and
      event objects. (The latter two only in the form provided by
      {!Netsys_win32}, see there.)

      Generally, if the blocking status cannot be determined for
      a class of I/O operations, the functions return [true], in
      the hope that it is better to block than to never conduct
      the operation.
   *)

val wait_until_readable : fd_style -> Unix.file_descr -> float -> bool
val wait_until_writable : fd_style -> Unix.file_descr -> float -> bool
val wait_until_prird : fd_style -> Unix.file_descr -> float -> bool
  (** Wait until an operation for a single descriptor becomes possible.
      The float argument is the timeout (negative value means no timeout).
      Returns whether the operation is possible ([true]). Otherwise,
      there was a timeout ([false]).

      On POSIX systems this works for a wide variety of descriptor 
      types (but not for regular files which are assumed to be always
      readable and writable).
      If the [poll] interface is available it is preferred over the
      [select] interface to wait for I/O. The functions also catch
      interruptions by signals.

      On Win32, waiting is limited to sockets, named pipes and
      event objects. (The latter two only in the form provided by
      {!Netsys_win32}, see there.)

      Generally, if waiting is not supported for
      a class of I/O operations, the functions return immediately [true], in
      the hope that it is better to block than to never conduct
      the operation.
   *)


(** {1 Functions for sockets} *)

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
   *)

val connect_check : Unix.file_descr -> unit
  (** Tests whether the socket is connected with the peer after calling
      [Unix.connect]. If the socket is connected, the function returns normally.
      Otherwise, the current socket error is raised as a [Unix.Unix_error]
      exception. This function is intended to be called after a 
      non-blocking connect has been initiated, and the success or error
      is indicated (e.g. after [wait_until_connected] returns).

      Side effect: The per-socket error code may be reset.
   *)

val domain_of_inet_addr : Unix.inet_addr -> Unix.socket_domain
  (** Returns the socket domain of Internet addresses, i.e. whether the
    * address is IPv4 or IPv6
   *)

val getpeername : Unix.file_descr -> Unix.sockaddr
  (** like [Unix.getpeername], but errors are fixed up. [ENOTCONN] is
      ensured when the socked is unconnected or shut down.
   *)

(** {1 Multicast Functions} *)

val mcast_set_loop : Unix.file_descr -> bool -> unit
  (** Whether sent multicast messages are received by the sending host *)

val mcast_set_ttl : Unix.file_descr -> int -> unit
  (** Set TTL/hops value *)

val mcast_add_membership : Unix.file_descr -> 
                           Unix.inet_addr -> Unix.inet_addr -> unit
  (** Join a multicast group.

      First inet addr is the group to join. Second inet addr selects the
      network interface (or [Unix.inet_addr_any]).
   *)

val mcast_drop_membership : Unix.file_descr -> 
                            Unix.inet_addr -> Unix.inet_addr -> unit
  (** Leave a multicast group.
   
     First inet addr is the group to leave. Second inet addr selects the
     network interface (or [Unix.inet_addr_any]).
   *)



(** {1 Deprecated} *)

(** The following interfaces have been replaced by more generic implementations
    that work on more platforms.
 *)

val blocking_read : Unix.file_descr -> string -> int -> int -> int
  (** Same as [blocking_gread `Read_write] *)

val really_read : Unix.file_descr -> string -> int -> int -> unit
  (** Same as [really_gread `Read_write] *)

val really_write : Unix.file_descr -> string -> int -> int -> unit
  (** Same as [really_gwrite `Read_write] *)

(** The following interfaces have been moved to {!Netsys_posix}. *)

type shm_open_flag =
    Netsys_posix.shm_open_flag =
  | SHM_O_RDONLY
  | SHM_O_RDWR
  | SHM_O_CREAT
  | SHM_O_EXCL
  | SHM_O_TRUNC

val have_posix_shm : unit -> bool
val shm_open : string -> shm_open_flag list -> int -> Unix.file_descr
val shm_unlink : string -> unit
