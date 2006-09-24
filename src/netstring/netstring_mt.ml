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
