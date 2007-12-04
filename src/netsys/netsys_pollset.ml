(* $Id$ *)

class type pollset =
object
  method find : Unix.file_descr -> Netsys.poll_in_events
  method add : Unix.file_descr -> Netsys.poll_in_events -> unit
  method remove : Unix.file_descr -> unit
  method wait : float -> 
                ( Unix.file_descr * 
		  Netsys.poll_in_events * 
		  Netsys.poll_out_events ) list
  method dispose : unit -> unit
  method cancel_wait : bool -> unit
end
