(* $Id$ *)

(** Camlboxes are a fast IPC mechanism to send Ocaml values from one
   process to another. (It is also a low-level mechanism that makes it
   easy to crash the program. So be careful.)

   A camlbox is owned by the single receiving process. Only this process
   (or a fork) can look for new messages and can read them. There can be
   any number
   of sending processes. The senders are not notified about whether
   messages are received.

   Camlboxes have a fixed capacity of messages, and the message slots
   have a fixed maximum length.

   On the system level, camlboxes are stored in POSIX shared memory
   objects. These objects have kernel persistence and continue to
   live after the process creating the camlbox has terminated without
   unlinking the box.
 *)


type camlbox_address = string
    (** The address of a camlbox is a string that does not contain
        slashes. Addresses are system-global.
     *)

type camlbox
  (** A [camlbox] may receive messages *)

type camlbox_sender
  (** An endpoint that may send messages to a camlbox *)

exception Empty
exception Message_too_big

val create_camlbox : camlbox_address -> int -> int -> camlbox
  (** [create_camlbox addr n size]: Creates a new camlbox for up to
      [n] messages of [size] bytes. The messages are numbered from
      0 to [n-1]. The camlbox is only meaningful for the creating
      process, and must not be directly accessed by other processes.
      Other processes can only send using a [camlbox_sender].

      It is an error if the camlbox already exists.
   *)

val unlink_camlbox : camlbox_address -> unit
  (** Removes the global name of the camlbox. All functions requiring 
      a [camlbox_address] as input will not find the box anymore. The
      box, however, continues to exist until the receiver and the senders
      are done with it.
   *)

val camlbox_fd : camlbox_address -> Unix.file_descr
  (** Opens a new file descriptor to this address *)

val camlbox_capacity : camlbox_address -> int
  (** Returns the maximum number of messages [n] *)

val camlbox_msg_size : camlbox_address -> int
  (** Returns the max size of a message in bytes *)

val camlbox_messages : camlbox_address -> int
  (** Returns the number of messages at the moment *)

val camlbox_get : camlbox -> int -> 'a
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

(* TODO: camlbox_get_copy: Safer but slower *)

val camlbox_delete : camlbox -> int -> unit
  (** [camlbox_delete box k]: Deletes the message number [k] from [box].
      Any value obtained via [camlbox_get] for a message or a part
      of a message becomes invalid and must not be used anymore.
      There is no way to check this - violating this rule is likely
      to crash the program.

      If there is no message at [k] the exception [Empty] will be
      raised.
   *)


val camlbox_wait : camlbox -> int list
  (** Waits until new messages arrive, and return the message numbers.
      A new message is only reported once by [camlbox_wait].

      Only one thread at a time must wait for new messages.

      It is allowed that this function returns the empty list.
   *)

val camlbox_cancel_wait : camlbox -> unit
  (** Cancels a [camlbox_wait] operation called by a different thread *)

val camlbox_sender : camlbox_address -> camlbox_sender
  (** Prepares for sending *)

val camlbox_sender_of_fd : Unix.file_descr -> camlbox_sender
  (** Gets a sender for a file descriptor from [camlbox_fd]. *)

val camlbox_send : camlbox_sender -> 'a -> unit
  (** Sends a message to a camlbox. The value must be boxed (neither [char],
      [bool], [int], nor a variant type), and a number of restrictions apply:
       - The size of the representation must not exceed the maximum
         message size of the camlbox, or the exception [Message_too_big]
         is raised.
       - Objects, closures, and lazy values are not supported
       - Abstract and custom block values are not supported. This
         also holds for bigarrays, [int32], [int64], and [nativeint].
       - (CHECK: atoms like empty arrays)
       - Values returned by C wrappers that do not use abstract or custom
         blocks for wrapping data may break this function.

      The value is copied to the receiving camlbox.

      This function blocks until the receiving camlbox has free space.

      Several threads may try to send messages at the same time.
   *)

val camlbox_wake : camlbox_sender -> unit
  (** Sends an "empty message" - this only means that if the receiving
      thread is waiting for new messages it is interrupted and 
      [camlbox_wait] will return the empty list.

      This function is non-blocking.
   *)
