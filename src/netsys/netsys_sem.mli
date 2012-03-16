(* $Id$ *)

(** {1 Generic anonymous semaphores} *)

(** This module purely exists to also support "kind of anonymous"
    sempahores on MacOS X (which only has named semaphores). On
    other OS, it is just a wrapper for the functions in
    {!Netsys_posix}.
 *)

val have_anon_semaphores : unit -> bool
  (** Returns [true] if anonymous semaphores are supported on this 
      system *)

(** {b Constants.} *)

val sem_value_max : int
  (** The maximum value of a semaphore, but at most [max_int] *)

val sem_size : int
  (** The size of an anonymous semaphore in bytes ([sizeof(sem_t)]) *)

(** {b Types.} *)

type container
  (** The container of the semaphore is the shared memory object *)

type anon_semaphore

type sem_open_flag = Netsys_posix.sem_open_flag =
  | SEM_O_CREAT
  | SEM_O_EXCL

(** {b Container functions.} *)

val container : string -> container
  (** [container prefix]: The prefix shall identify the container uniquely.
      Once can e.g. use the path of the shared memory object. The prefix
      is used to construct names for persistent objects.
   *)

val drop : container -> unit
  (** Drop the semaphores in this container.

      This function is a no-op if the OS supports anonymous semaphores
      directly (because in this case the deletion of the container will
      automatically destroy the semaphores).
   *)

(** {b Semaphore functions.} *)

val sem_init : container -> Netsys_types.memory -> int -> bool -> int -> 
                 anon_semaphore
  (** [sem_init cont mem pos pshared init_value]: Initializes the memory
      at position [pos] to [pos + sem_size() - 1] as anonymous semaphore.
      If [pshared] the semaphore is shared between processes. 
      [init_value] is the initial non-negative value (max is 
      [sem_value_max].
   *)

val sem_destroy : container -> anon_semaphore -> unit
  (** Destroys the anonymous semaphore *)

val as_sem : container -> Netsys_types.memory -> int -> anon_semaphore
  (** [as_sem mem pos]: Interprets the memory at position [pos]
      to [pos + sem_size() - 1] as anonymous semaphore.
      The memory region must already have been initialized.
   *)

val sem_getvalue : anon_semaphore -> int
  (** Returns the value of the semaphore. If the value is bigger than
      what can be represented as [int], an [EINVAL] error is returned.

      The returned value is non-negative - if the underlying POSIX
      function reports a negative value zero is returned instead.

      {b Unavailable on MacOS.}
   *)

val sem_post : anon_semaphore -> unit
  (** Unlocks the semaphore (increases the value by 1) *)

type sem_wait_behavior = Netsys_posix.sem_wait_behavior =
  | SEM_WAIT_BLOCK
  | SEM_WAIT_NONBLOCK

val sem_wait : anon_semaphore -> sem_wait_behavior -> unit
  (** Locks the semaphore (decreases the value by 1). If the semaphore
      value is already zero, and [SEM_WAIT_BLOCK] is given, the function
      waits until another process or thread posts. If [SEM_WAIT_NONBLOCK]
      the error [EAGAIN] is returned.

      [sem_wait] may be interrupted by signals.
   *)

(**/**)

val force_emulation : unit -> unit
