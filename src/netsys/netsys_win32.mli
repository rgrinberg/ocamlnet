(* $Id$ *)

(** Primitives for Win32 *)

(** {1 Primitives for sockets} *)

type wsa_event

val wsa_create_event : unit -> wsa_event

val wsa_close_event : wsa_event -> unit

val wsa_event_select : 
      wsa_event -> Unix.file_descr -> Netsys.poll_in_events -> unit

val wsa_maximum_wait_events : 
      unit -> int

val wsa_wait_for_multiple_events : 
      wsa_event array -> int -> int option
    (** Waits until one of the events in the array is in signaled state,
        or until a timeout happens. The int is the timeout in milliseconds.
        A negative timeout means infinity.

        The function returns the first index in the array that is signaled.

        On timeout, [None] is returned.

        The return value [WSA_WAIT_IO_COMPLETION] is mapped to the
        Unix error [EINTR].
     *)


val wsa_enum_network_events : 
      Unix.file_descr -> wsa_event -> Netsys.poll_out_events
    (** Checks whether an event has been recorded *)

(*

(** {1 Primitives for pipes} *)

val is_pipe_readable : Unix.file_descr -> bool
  (** Returns whether there is something to read from a pipe *)

(** {1 Primitives for event objects} *)

val create_event : unit -> Unix.file_descr
  (** Creates an event object with the properties:
      - default security descriptor
      - manual reset
      - initially the event is in nonsignaled state
      - no name is associated with the event
   *)


val reset_event : Unix.file_descr -> unit

val set_event : Unix.file_descr -> unit

val wait_for_multiple_objects : 
      Unix.file_descr array -> int -> int option
    (** Waits until one of the events in the array is in signaled state,
        or until a timeout happens. The int is the timeout in milliseconds.
        A negative timeout means infinity.

        The function returns the first index in the array that is signaled.

        On timeout, [None] is returned.
     *)
 *)
