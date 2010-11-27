(* $Id$ *)

(** Camlboxes are a fast IPC mechanism to send Ocaml values from one
   process to another. Source and destination processes must run on
   the same machine (no network). The Ocaml value is copied to a
   shared memory object where it can be directly accessed by the
   receiver without unmarshalling step. This means the sender writes
   the value into the shared memory in a format that can immediately
   interpreted by the receiver.

   A camlbox is owned by the single receiving process. Only this process
   (or a fork) can look for new messages and can read them. There can be
   any number of sending processes, i.e. we have a n:1 message passing
   scenario.

   The receiver process creates the camlbox, and is seen as the owner.
   The receiver is accountible for deleting the camlbox when it is no
   longer needed.

   The sender(s) can send messages to any existing camlbox. There is
   no notification whether the messages are actually read. The sender,
   however, blocks when the destination camlbox is full, and will only
   proceed when the receiver makes room for new messages. If there is
   space in the camlbox the sender does not need to synchronize with the
   receiver, i.e. it is possible to put a message into the box when
   the receiver is busy with something else (asynchronous send operation).

   Camlboxes have a fixed capacity of messages, and the message slots
   have a fixed maximum length. The messages can have any type with only
   a few restrictions (e.g. no functions and no custom blocks). There is
   no check whether the sender and the receiver assume the same type
   of the messages. This is left to the user. Breaking this assumption
   will lead to unpredictable effects, including program crashes.
   It is strongly advised to only communicate between processes that
   run the same executable.

   The user is also responsible for keeping only references to 
   existing messages. It is possible to get a value pointer 
   for a certain message via [camlbox_get] and then to delete the message. 
   The user must no longer access the value - once the value is deleted
   it may be overwritten, and the program may crash. Another danger
   is that message values are modified so that pointers to heap
   values are put into the message. This may lead to delayed crashes
   when the heap value is moved to a different location or is even
   deleted by the garbage collector. There is nothing the camlbox
   implementation can do about that. If this is a problem, it is
   advised to use [camlbox_get_copy] instead which is not dangerous
   in this respect.

   On the system level, camlboxes are stored in POSIX shared memory
   objects. These objects have kernel persistence and continue to
   live after the process creating the camlbox has terminated without
   unlinking the box.

   This module requires Ocaml 3.11 or newer. The system must support
   POSIX shared memory and POSIX semaphores. Camlboxes may be used
   in multi-threaded programs as long as the values [camlbox] and
   [camlbox_sender] are not used by several threads at the same time.

   {b Examples.} There a few examples in the distribution tarball
   (examples/camlbox).

   {b Multi-core:}
   Camlboxes can be used to gain speed-ups on multi-cores. See
   examples/camlbox/README in the distribution tarball for an example
   how to accomplish this.

   {b Integration into event-based programs:} See the section
   below, {!Netcamlbox.events}.
 *)


type camlbox_address = string
    (** The address of a camlbox is a string that does not contain
        slashes. Addresses are system-global.
     *)

type 'a camlbox
  (** A [camlbox] may receive messages of type ['a] *)

type 'a camlbox_sender
  (** An endpoint that may send messages of type ['a] to a camlbox *)

exception Empty
exception Message_too_big

val create_camlbox : camlbox_address -> int -> int -> 'a camlbox
  (** [create_camlbox addr n size]: Creates a new camlbox for up to
      [n] messages of [size] bytes. The messages are numbered from
      0 to [n-1]. The camlbox is only meaningful for the creating
      process, and must not be directly accessed by other processes.
      Other processes can only send using a [camlbox_sender].

      It is an error if the camlbox already exists.

      It is suggested that the result of [create_camlbox] is immediately
      coerced to the right type [t], e.g.
      {[
        let box = (create_camlbox addr n size : t camlbox)
      ]}
      as this ensures type safety for all following operations.
   *)

val unlink_camlbox : camlbox_address -> unit
  (** Removes the global name of the camlbox. All functions requiring 
      a [camlbox_address] as input will not find the box anymore. The
      box, however, continues to exist until the receiver and the senders
      are done with it.
   *)

val format_camlbox : Unix.file_descr -> int -> int -> 'a camlbox
  (** [format_camlbox fd n size]: The file [fd] is mapped into memory,
      and formatted as camlbox.
   *)

val camlbox_fd : camlbox_address -> Unix.file_descr
  (** Opens a new file descriptor to this address *)

val camlbox_capacity : camlbox_address -> int
  (** Returns the maximum number of messages [n] *)

val camlbox_bcapacity : 'a camlbox -> int
  (** same for an already opened box *)

val camlbox_scapacity : 'a camlbox_sender -> int
  (** same for a box already opened for sending *)

val camlbox_msg_size : camlbox_address -> int
  (** Returns the max size of a message in bytes *)

val camlbox_bmsg_size : 'a camlbox -> int
  (** same for an already opened box *)

val camlbox_smsg_size : 'a camlbox_sender -> int
  (** same for a box already opened for sending *)

val camlbox_messages : camlbox_address -> int
  (** Returns the number of messages at the moment *)

val camlbox_bmessages : 'a camlbox -> int
  (** same for an already opened box *)

val camlbox_smessages : 'a camlbox_sender -> int
  (** same for a box already opened for sending *)

val camlbox_get : 'a camlbox -> int -> 'a
  (** [camlbox_get box k]: Returns message number [k] from [box].
      The returned value lives in the camlbox, and using it is only
      safe as long as the camlbox exists and the message is not
      deleted.

      If there is no message at [k] the exception [Empty] will be
      raised.

      The result value must have the same type as the sent value.
      This is not checked, however. Violating this rule is likely
      to crash the program.
   *)

val camlbox_get_copy : 'a camlbox -> int -> 'a
  (** [camlbox_get box k]: Returns a deep copy of message number [k] from [box].
      This is safer than [camlbox_get], because the returned value remains
      valid when the message is deleted from the box.

      If there is no message at [k] the exception [Empty] will be
      raised.

      The result value must have the same type as the sent value.
      This is not checked, however. Violating this rule is likely
      to crash the program.
   *)

val camlbox_delete : 'a camlbox -> int -> unit
  (** [camlbox_delete box k]: Deletes the message number [k] from [box].
      Any value obtained via [camlbox_get] for a message or a part
      of a message becomes invalid and must not be used anymore.
      There is no way to check this - violating this rule is likely
      to crash the program. (In doubt use [camlbox_get_copy] instead
      which cannot interfer with [camlbox_delete].)

      If there is no message at [k] the exception [Empty] will be
      raised.
   *)

val camlbox_wait : 'a camlbox -> int list
  (** Waits until new messages arrive, and return the message numbers.
      A new message is only reported once by [camlbox_wait]. The
      order of the messages is not specified.

      Only one thread at a time must wait for new messages.

      It is allowed that this function returns the empty list.
   *)

val camlbox_cancel_wait : 'a camlbox -> unit
  (** Cancels a [camlbox_wait] operation called by a different thread *)

val camlbox_sender : camlbox_address -> 'a camlbox_sender
  (** Prepares for sending.

      It is suggested that the result of [camlbox_sender] is immediately
      coerced to the right type [t], e.g.
      {[
        let box = (camlbox_sender addr : t camlbox_sender)
      ]}
      as this ensures type safety for all following operations.
 *)

val camlbox_sender_of_fd : Unix.file_descr -> 'a camlbox_sender
  (** Gets a sender for a file descriptor from [camlbox_fd]. *)

val camlbox_send : 'a camlbox_sender -> 'a -> unit
  (** Sends a message to a camlbox. The value must be boxed (neither [char],
      [bool], [int], nor a variant type), and a number of restrictions apply:
       - The size of the representation must not exceed the maximum
         message size of the camlbox, or the exception [Message_too_big]
         is raised.
       - Objects, closures, and lazy values are not supported
       - Abstract and custom block values are not supported except
         bigarrays, [int32], [int64], and [nativeint].
       - Atoms (like empty arrays) may cause problems when the message
         is extracted by [camlbox_get] because atoms are duplicated,
         and no longer unique. For example, a test [if array=[||] then...]
         is likely not to work. Use [if Array.length array = 0 then...],
         or use [camlbox_get_copy] for extraction.

      The value is copied to the receiving camlbox.

      This function blocks until the receiving camlbox has free space.

      Several threads may try to send messages at the same time.
   *)

val camlbox_wake : 'a camlbox_sender -> unit
  (** Sends an "empty message" - this only means that if the receiving
      thread is waiting for new messages it is interrupted and 
      [camlbox_wait] will return the empty list.

      This function is non-blocking.
   *)

(**
   {2:events Integration into event-based programs}

   The functions [camlbox_wait] and [camlbox_send] may both block the
   execution of the program when no message has arrived, and no space
   is available, respectively. This is a challenge for event-based
   programs where all waiting is bound to events on file descriptors.
   
   Generally, Camlboxes use semaphores for speed. The results are good,
   often only 4 microseconds for sending and receiving a short
   message.  This is only possible because semaphores implement a fast
   path where the help of the kernel is not needed, i.e. no context
   switch happens. This is basically incompatible with the style of
   waiting implemented for file descriptors, because this kind of
   waiting for an event must always go through the kernel, and is
   thus slower by design.

   But anyway, what to do if Camlboxes need to be integrated into
   a program that bases already on file descriptor polling? Of course,
   speed will decrease, but maybe not dramatically. We assume here
   that the program uses {!Unixqueue}s as the basic data structure
   for organizing polling.

   {b Option 1: Use Threads.} A program waiting for incoming Camlbox
   messages could be structured as follows:
    - There is a fast path and a slow path. The fast path: After
      processing the previous message, the program looks at 
      the number of messages in the box, and if this is > 0, the next
      message is immediately taken and processed (no waiting).
    - The slow path: If the number is zero, a worker thread is created
      that calls [camlbox_wait] and thus blocks until a message arrives.
      If this happens, an artifical [Unixqueue] event is created and
      added to [esys]. While the spawned thread is waiting for the message,
      the main thread just performs its normal operations on the [esys]
      (e.g. an RPC server processes incoming calls). The main thread is
      set up to watch for the special event the worker thread adds, and
      to run the piece of code processing the incoming message.

   As usual, multi-threaded programming is not very nice to control.

   {b Option 2: Use a named pipe.} A program waiting for incoming Camlbox
   messages could be structured as follows. In this suggestion, we need
   help from the message sender to get notified in a poll-compatible
   way when a message is sent.
    - The message receiver also creates a named pipe. The receiver opens
      one end of the pipe for reading. Every sender opens the other end
      of the pipe for writing.
    - When a message is sent, the sender not only sends the message via
      the camlbox, but also sends a single byte over the named pipe.
    - The receiver waits for events on the named pipe. If bytes arrive,
      they are read, and the fast path loop is entered.
    - In the fast path loop, the program looks repeatedly at 
      the number of messages in the box, and while this is > 0, the next
      message is immediately taken and processed (no waiting).
    - When the fast path loop is left, control is passed back to the
      normal file descriptor polling.

   Instead of a named pipe, any other notification means can also be used,
   so far it is compatible with file descriptor polling. (E.g.
   Linux added recently the [eventfd] system call as a cheaper
   way for pure notification via file descriptors.)

   The two options only handle the case of [camlbox_wait]. But also
   [camlbox_send] can block. This is usually a bit simpler to solve,
   especially in the case where two processes exchange messages, and
   both are sender and receiver. In this frequent scenario, one can
   track the number of messages in the Camlbox, and one can arrange that
   the sender is notified when the number of messages decreases, i.e.
   more space becomes available in the Camlbox. (In some sense, the
   problem of handling [camlbox_send] is reduced to [camlbox_wait].)

   In the general case, though, when there are n>1 senders, the
   problems of handling [camlbox_send] is really difficult. One would
   need a way to broadcast events from the receiver to all senders.
   The only way I'm aware of (avoiding multi-threading) is the following:

   - Again, a (single) named pipe is used for notification. The
     receiver creates it, and opens both ends of the pipe. It writes
     bytes into the writing end until the internal pipe buffer is full.
     The write side is then closed, and only the read side is kept open.
     One byte is read, so initially the pipe buffer is almost full.
     We are going to use a full pipe buffer as representation that
     the Camlbox has no space, and an almost full pipe buffer (one free byte) as
     representation that the box has space.
   - The senders open the write end of the pipe. They poll until there
     is space in the pipe buffer. If so, they all try to write a single
     byte into the buffer, but in non-blocking mode. Usually only one
     sender will succeed doing so, and this sender is allowed to send
     the message via the Camlbox.
   - The receiver takes care that the notification via the pipe buffer
     works. After a message is processed, a byte is read from the 
     pipe buffer, allowing another message to be sent.

   One can improve this by encoding the number of free slots in the
   Camlbox via the number of free bytes in the pipe buffer (i.e. n
   free slots = n missing bytes in the buffer until it is full).
   
*)
