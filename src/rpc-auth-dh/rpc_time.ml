(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

exception Time_not_available

let remote_time ?(timeout = 5) peer =
  let cleanup = ref [] in
  let add_action f = cleanup := f :: !cleanup in
  let do_cleanup() = List.iter (fun f -> f()) !cleanup; cleanup := [] in
  try
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    add_action (fun () -> Unix.close s);
    Unix.set_nonblock s;               (* so that Unix.connect does not block *)
    ( try
	Unix.connect s (Unix.ADDR_INET(peer, 37))
      with
	  Unix.Unix_error(Unix.EINPROGRESS,_,_) -> ()
    );
    Unix.clear_nonblock s;
    let buf = String.create 4 in
    let pos = ref 0 in
    while !pos < 4 do
      let r, _, _ = Unix.select [ s ] [] [] (float timeout) in
      if r = [] then raise Time_not_available;
      let n = Unix.read s buf !pos (String.length buf - !pos) in
      pos := !pos + n;
      if !pos < 4 && n=0 then raise Time_not_available;
    done;
    do_cleanup();
    let x3 = float (Char.code buf.[0]) in
    let x2 = float (Char.code buf.[1]) in
    let x1 = float (Char.code buf.[2]) in
    let x0 = float (Char.code buf.[3]) in
    x3 *. 16777216.0 +. x2 *. 65536.0 +. x1 *. 256.0 +. x0 -. 2208988800.0
      (* 2208988800 = 1 Jan 1970 00:00:00 *)
  with
      err ->
	do_cleanup();
	raise Time_not_available
;;
