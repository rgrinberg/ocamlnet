(* $Id$ *)

open Netsys_pollset

let standard_pollset() =
  match Sys.os_type with
    | "Win32" ->
	if ( !Netsys_oothr.provider ) # single_threaded then
	  Netsys_pollset_win32.pollset()
	else
	  Netsys_pollset_win32.threaded_pollset()
    | _ ->
	Netsys_pollset_posix.poll_based_pollset()


let performance_pollset = standard_pollset
