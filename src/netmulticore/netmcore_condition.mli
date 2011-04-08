(* $Id$ *)

(** Condition variables *)

(** Condition variables are here defined as values that reside in shared heaps
    ({!Netmcore_heap}), for example in the header field of 
    {!Netmcore_array} or somewhere else in heap-allocated
    data structures.

    In order to ensure that the condition variable is in the heap, the
    special function [create_condition] must be used to initialize it
    there. As [create_condition] requires a mutator as argument, this is
    only possible by calling [create_condition] from the callback of
    {!Netmcore_heap.modify}.

    Condition variables are special values, and cannot be copied or moved.

    Condition variables are implemented on top of semaphores. Compared to
    the [pthreads] version of condition variables, the user needs here to
    allocate special [wait_entry] slots, one for each process. An entry
    can be used for all condition variables a process needs to wait for.
    (Actually, such entries also exist in typical [pthreads] implementations,
    but are hidden from the user in the thread control block. We just
    don't have here a place where we could allocate process-specific
    shared memory.)
 *)

type condition
  (** The condition variable *)

type wait_entry
  (** Each process that wants to [wait] needs a [wait_entry]. These entries
      can be used for several condition variables, so typically each process
      has only one entry for each heap.
   *)

type wait_set
  (** A set of [wait_entry], for easier management. This set can e.g. be
      stored side by side with the condition variable(s). It is important
      that the [wait_set] resides in the same shared heap as the
      condition variable.
   *)

val dummy_condition : unit -> condition
  (** A dummy condition is non-functional, but can be used to put something
      into [condition]-typed variables
   *)

val dummy_wait_set : unit -> wait_set
  (** A dummy [wait_set] is non-functional, but can be used to put something
      into [wait_set]-typed variables
   *)


val create_condition : Netmcore_heap.mutator -> condition
  (** [create m]: Creates a condition variable, and
      pushes it to the heap, using the mutator [m].

      After being pushed to the heap, the variable can be used. It is
      nonsense to copy it outside the heap.
   *)

val create_wait_set :  Netmcore_heap.mutator -> wait_set
  (** Creates a [wait_set] in the heap designated by the mutator *)

val alloc_wait_entry : Netmcore_heap.mutator -> wait_set -> wait_entry
  (** Allocates a [wait_entry] *)

val free_wait_entry : Netmcore_heap.mutator -> wait_set -> wait_entry -> unit
  (** Frees a [wait_entry] *)

val wait : wait_entry -> condition -> Netmcore_mutex.mutex -> unit
  (** [wait we c m] atomically unlocks the mutex [m] and suspends the
      calling process on the condition variable [c]. The process will
      restart after the condition variable [c] has been signalled.
      The mutex [m] is locked again before [wait] returns.

      At the time of calling, the [wait_entry] [we] must not be used to
      manage another [wait].  When allocating a separate [wait_entry]
      per process this problem does not occur.
   *)

val signal : condition -> unit
  (** [signal c] restarts one of the processes waiting on the
      condition variable [c].
   *)

val broadcast : condition -> unit
  (** [broadcast c] restarts all processes waiting on the
      condition variable [c].
   *)

val destroy_condition : condition -> unit
val destroy_wait_set : wait_set -> unit
  (** Destroys these objects *)
