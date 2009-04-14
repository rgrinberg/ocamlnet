(* $Id$ *)

(** Unixqueue implementation on top of {!Netsys_pollset} *)

class pollset_event_system : 
      Netsys_pollset.pollset -> Unixqueue_util.event_system

val pollset_event_system : 
      Netsys_pollset.pollset -> Unixqueue_util.event_system_t
  (** Implements a unixqueue on top of a pollset.
   *)
