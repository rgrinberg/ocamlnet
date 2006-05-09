(* $Id$ *)

(** Multi-processing provider *)

let rec restart f arg =
  try
    f arg
  with
    | Unix.Unix_error(Unix.EINTR,_,_) ->
	restart f arg
;;


class mp () : Netplex_types.parallelizer =
object
  method ptype = `Multi_processing

  method init() =
    (* We do not wait for forked processes, so ignore SIGCHLD: *)
    Sys.set_signal Sys.sigchld Sys.Signal_ignore

  method start_thread : 't . ('t -> unit) -> 't -> 'x -> unit =
    fun f arg l ->
      let (fd_rd, fd_wr) = Unix.pipe() in
      match Unix.fork() with
	| 0 ->
	    Sys.set_signal Sys.sigchld Sys.Signal_default;
	    Unix.close fd_rd;
	    (* We close all file descriptors except those in [l]. Note that
             * this is important for proper function of the main process
             * (e.g. to detect EOF of file descriptors).
             *
             * Make sure we close [fd_wr] last! This tells the main process
             * that the critical section is over.
             *)
	    let l' = List.map Unix_exts.int_of_file_descr (fd_wr :: l) in
	    let fd_max = Unix_exts.sysconf_open_max() in
	    for k = 3 to fd_max - 1 do  (* Note: Keep 0, 1, 2 open *)
	      if not(List.mem k l') then
		( try
		    Unix.close (Unix_exts.file_descr_of_int k)
		  with
		    | _ -> ()
		)
	    done;
	    Unix.close fd_wr;
	    ( try
		f arg
	      with
		| _ -> ()
	    );
	    exit 0
	      (* CHECK: Not sure whether we want to run onexit handlers *)

      | pid ->
	  (* Wait until the child completes the critical section: *)
	  Unix.close fd_wr;
	  ignore (restart (Unix.select [ fd_rd ] [] []) (-1.0));
	  Unix.close fd_rd
	  

end
