(* $Id$ *)

(** Experimental alternate Unixqueue implementations *)

val pollset_event_system : 
      Netsys_pollset.pollset -> Unixqueue.event_system
  (** Implements a unixqueue on top of a pollset.

      Currently not multi-threading aware!
   *)
