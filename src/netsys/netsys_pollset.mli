(* $Id$ *)

(** Polling sets of file descriptors *)

class type pollset =
object
  method find : Unix.file_descr -> Netsys.poll_in_events
    (** Checks whether a descriptor is member of the set, and returns
        its event mask. Raises [Not_found] if the descriptor is not in the set.
     *)

  method add : Unix.file_descr -> Netsys.poll_in_events -> unit
    (** Add a descriptor or modify an existing descriptor *)

  method remove : Unix.file_descr -> unit
    (** Remove a descriptor from the set (if it is member) *)

  method wait : float -> 
                ( Unix.file_descr * 
		  Netsys.poll_in_events * 
		  Netsys.poll_out_events ) list
    (** Wait for events, and return the output events matching the event
        mask. This is level-triggered polling, i.e. if a descriptor continues
        to match an event mask, it is again reported the next time [wait]
        is invoked.

        There is no guarantee that the list is complete.

        It is unspecified how the set reacts on descriptors that became
        invalid (i.e. are closed) while being member of the set. The set
        implementation is free to silently disregard such descriptors,
        or to report them as invalid. It is strongly recommended to
        remove descriptors before closing them.
     *)

  method dispose : unit -> unit
    (** Release any OS resources associated with the poll set. *)
end


val poll_based_pollset : int -> pollset
  (** Returns a poll set whose implementation is based on the [poll] system
      call. The passed integer is the minimum size of the poll array. The
      array will grow beyond that if necessary.
   *)
