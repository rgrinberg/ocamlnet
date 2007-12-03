(* $Id$ *)

type wsa_event

external wsa_create_event : unit -> wsa_event 
  = "netsys_wsa_create_event"

external wsa_close_event : wsa_event -> unit
  = "netsys_wsa_close_event"

external wsa_event_select :  
  wsa_event -> Unix.file_descr -> Netsys.poll_in_events -> unit
  = "netsys_wsa_event_select"

external wsa_maximum_wait_events : unit -> int
  = "netsys_wsa_maximum_wait_events"

external wsa_wait_for_multiple_events : 
  wsa_event array -> int -> int option
  = "netsys_wsa_wait_for_multiple_events"

external wsa_enum_network_events : 
  Unix.file_descr -> wsa_event -> Netsys.poll_out_events
  = "netsys_wsa_enum_network_events"
