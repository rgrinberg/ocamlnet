(* $Id: shell_mt.ml 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

let mk_mutex_pair() =
  let m = Mutex.create() in
  (fun () -> Mutex.lock m), (fun () -> Mutex.unlock m)
in
Shell_sys.init_mt mk_mutex_pair;;
