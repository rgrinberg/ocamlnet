open Uq_engines;;
(* Unixqueue.set_debug_mode true;; *)
let x = Unix.openfile "x" [Unix.O_RDONLY] 0;;
let y = Unix.openfile "y" [Unix.O_WRONLY; Unix.O_CREAT] 0o666;;
let ues = Unixqueue.create_unix_event_system();;
let e = new copier (`Unidirectional(x,y)) ues;;
Unixqueue.run ues;;
e # state;;
