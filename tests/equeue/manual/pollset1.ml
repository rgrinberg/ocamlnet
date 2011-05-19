open Uq_engines;;
Unixqueue_util.set_debug_mode true;;
let pset = Netsys_pollset.poll_based_pollset 10;;
let x = Unix.openfile "x" [Unix.O_RDONLY] 0;;
let y = Unix.openfile "y" [Unix.O_WRONLY; Unix.O_CREAT] 0o666;;
let ues = Unixqueue2.pollset_event_system pset;;
let e = new copier (`Unidirectional(x,y)) ues;;
Unixqueue.run ues;;
e # state;;
