(* $Id$ *)

class type pollset =
object
  method find : Unix.file_descr -> Netsys_posix.poll_req_events
  method add : Unix.file_descr -> Netsys_posix.poll_req_events -> unit
  method remove : Unix.file_descr -> unit
  method wait : float -> 
                ( Unix.file_descr * 
		  Netsys_posix.poll_req_events * 
		  Netsys_posix.poll_act_events ) list
  method dispose : unit -> unit
  method cancel_wait : bool -> unit
end
