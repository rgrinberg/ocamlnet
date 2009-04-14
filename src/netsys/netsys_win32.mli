(* $Id$ *)

(** Primitives for Win32 *)

val fill_random : string -> unit
  (** Fills the string with random bytes. A cryptographically secure RNG
      is used
   *)

(** {1 Event objects} *)

type w32_event

val create_event : unit -> w32_event
  (** Create an event object *)

val set_event : w32_event -> unit
  (** Set the object to "signaled" state *)

val reset_event : w32_event -> unit
  (** Set the object to "non-signaled" state *)

val test_event : w32_event -> bool
  (** Test whether the object is in signaled state *)

val event_wait : w32_event -> float -> bool
  (** Wait until the event is set to signaled state. The float argument
      is the timeout in seconds. The function returns whether the object
      is in signaled state.
   *)

val event_descr : w32_event -> Unix.file_descr
  (** Creates a proxy descriptor for the event. See [lookup] below for
      more on proxy descriptors.
   *)


(** {1 Primitives for sockets} *)

val wsa_event_select : 
      w32_event -> Unix.file_descr -> Netsys_posix.poll_req_events -> unit
  (** associate event objects with socket conditions *)

val wsa_maximum_wait_events : 
      unit -> int
  (** max size of the array passed to [wsa_wait_for_multiple_events] *)


val wsa_wait_for_multiple_events : 
      w32_event array -> int -> int option
    (** Waits until one of the events in the array is in signaled state,
        or until a timeout happens. The int is the timeout in milliseconds.
        A negative timeout means infinity.

        The function returns the first index in the array that is signaled.

        On timeout, [None] is returned.

        The return value [WSA_WAIT_IO_COMPLETION] is mapped to the
        Unix error [EINTR].
     *)


val wsa_enum_network_events : 
      Unix.file_descr -> w32_event -> Netsys_posix.poll_act_events
    (** Checks whether an event has been recorded *)


(** {1 Support for named pipes} *)

(** Win32 named pipes do not allow to check whether an operation would block
    before starting the operation. There is so-called overlapped I/O,
    but it works differently than Unix-style multiplexing.

    Note that anonymous pipes (as returned by Unix.pipe) do not support
    overlapped I/O. The following is restricted to named pipes!
    Also note that Win32 named pipes work a lot like Unix domain sockets,
    i.e. there can be several independent connections to a single server.
    We only support byte stream pipes.

    The following functions add a layer to the Win32 primitives that
    helps using pipes in a way similar to multiplexing. We allocate
    buffers for input and output, and the functions [pipe_read] and
    [pipe_write] access these buffers in the first place. When reading,
    but the read buffer is empty, we start an  overlapped read operation  
    from the pipe handle. The arriving data refills the read buffer, and
    a w32_event is signaled to wake up any pending event loop.
    During the pending read from the pipe handle, the read buffer is
    locked, and [pipe_read] will return EWOULDBLOCK.

    Writing is slightly more difficult. The first [pipe_write] puts
    the data into the write buffer, and immediately starts an overlapped
    I/O operation to write the data to the pipe handle. During this
    operation the write buffer is locked, and cannot be further used
    to accumulate data, even if there is space. So [pipe_write] will
    return EWOULDBLOCK while the operation takes place. A w32_event is
    signaled when the write operation is over.
 *)

type w32_pipe_helper

type pipe_mode = Pipe_in | Pipe_out | Pipe_duplex

type pipe_conn_state = Pipe_deaf | Pipe_listening | Pipe_connected | Pipe_down

val create_local_named_pipe : string -> pipe_mode -> int -> w32_pipe_helper
  (** [create_local_named_pipe name mode n]: Create an instance of a 
      named pipe. The [name] must have the format "\\.\pipe\<name>".
      In [n] the maximum number of instances is passed. The pipe is
      set up with a security descriptor so only clients on the same system
      can connect. The pipe handles are not inheritable to child processes.

      This function is called by the pipe server. It can be called up to
      [n] times with the same name.

      The returned pipe is in the connection state [Pipe_deaf].
   *)

(** In the following, a terminology has been chosen that is similar to
    those of the socket API. The terms are different from those Microsoft
    prefers, however.
 *)

val pipe_listen : w32_pipe_helper -> unit
  (** Triggers that new connections are accepted by a pipe server. When
      the connection is established the pipe becomes writable (even
      for read-only pipes). In this case:
       - [pipe_write] with a length of 0 will simply check for the 
         error code
       - [pipe_write] with a length > 0 is also possible. If a connect
         error occurred the error code is immediately returned.
       - [pipe_read] is also possible (length of 0 only checks the error
         code, and length > 0 tries to read). Note, however, that the
         pipe never becomes readable after the connection is 
         established - even if there is data to read. The reason is that the 
         Win32 API permits to signal the completion of an I/O operation
         by one event object only, and not by two objects. After the
         first [pipe_read] or [pipe_write] it is then correctly signaled
         which kind of I/O is possible.

      This function causes that the connection state transitions from
      [Pipe_deaf] to [Pipe_listening]. When the connection is established,
      the state transitions further to [Pipe_connected].
   *)

val pipe_deafen : w32_pipe_helper -> unit
  (** The pipe server forces the client to disconnect. The pipe handle can
      be reused for further [pipe_listen] calls.

      See the comments on shutting down pipes below.

      This function causes that the connection state transitions from
      [Pipe_listening] or [Pipe_connected] to [Pipe_deaf].
   *)

val pipe_connect : string -> pipe_mode -> w32_pipe_helper
  (** [pipe_connect name mode]: Creates a client pipe handle, and tries
      to connect to the pipe server [name]. The function fails with the
      Unix error [EAGAIN] if there are currently no listening instances of the
      pipe at the server.

      Note that there is no other way to wait asynchronously for the
      availability of the pipe than busy waiting.

      The pipe is in state [Pipe_connected].
   *)

val pipe_pair : pipe_mode -> (w32_pipe_helper * w32_pipe_helper)
  (** Returns a pair of connected pipes (using automatically generated
      names). The left pipe is in the passed [pipe_mode], and the
      right pipe is in the matching complementaty mode.
   *)


val pipe_read : w32_pipe_helper -> string -> int -> int -> int
  (** [pipe_read p s pos len]: Tries to read data from the pipe. If data
      is available, it is put into the [len] bytes at position [pos] of
      the string [s], and the actual number of read bytes is returned.

      If no data is available, the function fails with a Unix error of
      [EAGAIN].

      If the end of the pipe is reached, the function returns 0.
   *)

val pipe_write : w32_pipe_helper -> string -> int -> int -> int
  (** [pipe_write p s pos len]: Tries to write data to the pipe. If space
      is available, the data is taken from the [len] bytes at position [pos] of
      the string [s], and the actual number of written bytes is returned.

      If no space is available, the function fails with a Unix error of
      [EAGAIN].
   *)

val pipe_shutdown : w32_pipe_helper -> unit
  (** Cancels all pending I/O operations and closes the pipe handle.

      Note that there is no way to close only one direction of bidirectional
      pipes.

      See the comments on closing pipes below.

      It is an error to call this function after [pipe_deaf]. Just forget
      about the pipe helper in this case. This function causes that the
      connection state transitions from [Pipe_connected] to [Pipe_down].
   *)

val pipe_conn_state : w32_pipe_helper -> pipe_conn_state
  (** Return the connection state *)

val pipe_rd_event : w32_pipe_helper -> w32_event
val pipe_wr_event : w32_pipe_helper -> w32_event
  (** The event objects signaling that read and write operations are possible.
      The read event is in signaled state when the read buffer is non-empty
      (even for write-only pipes). The write event is in signaled state when
      the pipe is connected and the write buffer is empty (even for 
      read-only pipes).
   *)

val pipe_wait_rd : w32_pipe_helper -> float -> bool
val pipe_wait_wr : w32_pipe_helper -> float -> bool
  (** Wait until the pipe becomes readable or writable. The float argument
      is the timeout in seconds. The function returns whether there is
      data to read or write. If not, a timeout has occurred.
   *)

val pipe_signal : w32_pipe_helper -> w32_event -> unit
  (** Associates the pipe with an event object. The event is signaled
      when the pipe changes its connection state to Pipe_deaf or
      Pipe_down. The event must be manually reset by the caller.
   *)


val pipe_descr : w32_pipe_helper -> Unix.file_descr
  (** [pipe_descr] returns a file descriptor that can be used as proxy in
      all interfaces that use file descriptors to identify system objects.
      Subsequent calls of [pipe_descr] return the same descriptor.
      See the docs on [lookup] below.
   *)


(** {b Shutting down pipes.} The suggested model is that the client shuts
    down the pipe first. A pipe client ensures that all data are transmitted
    by waiting until the pipe becomes writable again, and then calling
    [pipe_shutdown]. The server then sees EOF when reading from the pipe,
    or gets an [EPIPE] error when writing to the pipe. The server can
    react on this by reusing the pipe instance for another client
    ([pipe_deaf] followed by [pipe_listen]), or by getting rid of the instance
    ([pipe_shutdown]).

    When servers start the closure of connections, there is no clean way
    of ensuring that all written data are transmitted. There is the
    [FlushFileBuffers] Win32 function, but it is blocking.
 *)


(** {1 Proxy Descriptors} *)

(** For a number of objects ([w32_event], [w32_pipe_helper]) it is possible
    to obtain proxy descriptors. These have type [Unix.file_descr] and they
    contain a real file handle. The purpose of these descriptors is to
    be used as proxy objects that can be passed to functions expecting
    file descriptors as input. However, you cannot do anything with the
    proxies except looking the corresponding real objects up. This feature
    is used by Ocamlnet to emulate POSIX behavior for Win32 kernel objects
    lacking it.

    Note that you can call functions like [Unix.close] or [Unix.dup] on
    the proxies, but they are meaningless, and can even cause malfunction
    (especially [close]).

    Proxy descriptors are automatically closed.
 *)

type w32_object =
    | W32_event of w32_event
    | W32_pipe_helper of w32_pipe_helper

val lookup : Unix.file_descr -> w32_object
  (** Returns the real object behind a proxy descriptor, or raises
      [Not_found]
   *)

val lookup_event : Unix.file_descr -> w32_event
val lookup_pipe_helper : Unix.file_descr -> w32_pipe_helper
  (** Returns the real object. If not found, or if the object is of unexpected
      type, [Failure] is raised.
   *)

