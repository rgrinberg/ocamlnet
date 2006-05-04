(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* Initialize multi-threading mode: *)

(* let str_mutex = Mutex.create();; *)
let mappings_mutex = Mutex.create();;

(*
Netstring_str.init_mt
  (fun () -> Mutex.lock str_mutex)
  (fun () -> Mutex.unlock str_mutex);
*)
Netmappings.init_mt
  (fun () -> Mutex.lock mappings_mutex)
  (fun () -> Mutex.unlock mappings_mutex)
;;

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 2.2  2004/06/30 22:27:42  stolpmann
 * 	Update: Cgi is optional, -enable-compatcgi still possible.
 *
 * Revision 2.1  2003/11/08 13:15:09  stolpmann
 * 	Fix: ocamlopt build works again if only vmthreads are available
 *
 * Revision 2.2  2001/11/07 00:32:39  stolpmann
 * 	Removed the initialization of Netstring_str.
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.2  2000/08/29 00:45:42  gerd
 * 	Initializing Netmappings, too
 *
 * Revision 1.1  2000/06/25 21:15:27  gerd
 * 	Initial revision
 *
 * 
 *)
