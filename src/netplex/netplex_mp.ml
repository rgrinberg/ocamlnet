(* $Id$ *)

(** Multi-processing provider *)

open Netplex_types
open Printf

class mp () : Netplex_types.parallelizer =
object(self)
  val mutable pid_list = []

  method ptype = `Multi_processing

  method current_sys_id =
    `Process (Unix.getpid())

  method create_mem_mutex() =
    (fun () -> ()), (fun () -> ())

  method init() =
(*
    (* We do not wait for forked processes, so ignore SIGCHLD: *)
    Sys.set_signal Sys.sigchld Sys.Signal_ignore
 *)

    (* SIGTERM is forwarded to all children: *)
    Sys.set_signal 
      Sys.sigterm
      (Sys.Signal_handle
	 (fun _ ->
	    List.iter
	      (fun pid ->
		 try Unix.kill pid Sys.sigterm
		 with _ -> ()
	      )
	      pid_list;
	    exit 6
	 )
      );

    ()

  method start_thread : 
           (par_thread -> unit) -> 'x -> string -> logger -> par_thread =
    fun f l srv_name logger ->
      let (fd_rd, fd_wr) = Unix.pipe() in
      let r_fork = 
	try Unix.fork()
	with err ->
	  Unix.close fd_rd; Unix.close fd_wr; raise err in
      match r_fork with
	| 0 ->
(*
	    Sys.set_signal Sys.sigchld Sys.Signal_default;
 *)
	    Sys.set_signal Sys.sigterm Sys.Signal_default;

	    Unix.close fd_rd;
	    (* We close all file descriptors except those in [l]. Note that
             * this is important for proper function of the main process
             * (e.g. to detect EOF of file descriptors).
             *
             * Make sure we close [fd_wr] last! This tells the main process
             * that the critical section is over.
             *)
	    let l' = List.map Netsys.int_of_file_descr (fd_wr :: l) in
	    let fd_max = Netsys.sysconf_open_max() in
	    for k = 3 to fd_max - 1 do  (* Note: Keep 0, 1, 2 open *)
	      if not(List.mem k l') then
		( try
		    Unix.close (Netsys.file_descr_of_int k)
		  with
		    | _ -> ()
		)
	    done;
	    Unix.close fd_wr;
	    let pid = Unix.getpid() in
	    let arg =
	      ( object
		  method ptype = `Multi_processing
		  method sys_id = `Process pid
		  method info_string = "Process " ^ string_of_int pid
		  method watch_shutdown _ = assert false
		  method parallelizer = (self : #parallelizer :> parallelizer)
		end
	      ) in
	    ( try
		f arg
	      with
		| error ->
		    prerr_endline
		      ("Netplex Catastrophic Error: Uncaught exception in child process " ^ string_of_int pid ^ ": " ^ Printexc.to_string error);
		    exit 2
	    );
	    exit 0
	      (* CHECK: Not sure whether we want to run onexit handlers *)

      | pid ->
	  pid_list <- pid :: pid_list;
	  (* Wait until the child completes the critical section: *)
	  Unix.close fd_wr;
	  ignore (Netsys.restart (Netsys.wait_until_readable fd_rd) (-1.0));
	  Unix.close fd_rd;

	  ( object
	      val mutable watching = false

	      method ptype = `Multi_processing
	      method sys_id = `Process pid
	      method info_string = "Process " ^ string_of_int pid
	      method parallelizer = (self : #parallelizer :> parallelizer)
	      method watch_shutdown esys =
		let g = Unixqueue.new_group esys in
		let cnt = ref 0 in

		let remove() =
		  pid_list <- 
		    List.filter (fun p -> p <> pid) pid_list in

		let watch() =
		  incr cnt;
		  if !cnt = 60 then (
		    logger # log 
		      ~component:"netplex.controller"
		      ~level:`Alert
		      ~message:(sprintf
				  "Process %d for service %s seems to be non-responsive, killing it now"
				  pid srv_name);
		    Unix.kill pid Sys.sigterm
		  );
		  try
		    let p, s = Unix.waitpid [ Unix.WNOHANG ] pid in
		    if p = 0 then  (* p=0: not yet terminated *)
		      true
		    else
		      ( remove();
			match s with
			  | Unix.WEXITED 0 ->
			      false
			  | Unix.WEXITED n ->
			      logger # log 
				~component:"netplex.controller"
				~level:`Alert
				~message:(sprintf
					    "Process %d for service %s terminated with exit code %d"
					    pid srv_name n);
			      false
			  | Unix.WSIGNALED n ->
			      logger # log 
				~component:"netplex.controller"
				~level:`Alert
				~message:(sprintf
					    "Process %d for service %s terminated with signal %d"
					    pid srv_name n);
			      false
			  | _ ->
			      assert false
		      )
		  with
		    | Unix.Unix_error(Unix.EINTR,_,_) ->
			true
		    | Unix.Unix_error(Unix.EAGAIN,_,_) ->
			true
		in

		let rec watch_loop() =
		  Unixqueue.once esys g 1.0
		    (fun () ->
		       if watch() then watch_loop()
		    )
		in
		if not watching then (
		  watching <- true;
		  if watch() then  watch_loop()
		)
	    end
	  )
	  

end

let the_mp = lazy(
  let par = new mp() in
  Netplex_cenv.register_par par;
  par
)

let mp () = Lazy.force the_mp
