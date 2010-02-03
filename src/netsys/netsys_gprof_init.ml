(* $Id$ *)

external netsys_moncontrol : bool -> unit = "netsys_moncontrol"

let () =
  Netsys.set_moncontrol netsys_moncontrol

let init() =
  ()
