(*
#use "topfind";;
#require "shell";;
*)

let run() =
  let esys = Unixqueue.create_unix_event_system() in
  let cmd =
    Shell.cmd "/bin/true" [] in
  let b = Buffer.create 80 in
  let e = new Shell_uq.call_engine
    ~stdout:(Shell.to_buffer b)
    [cmd] esys in
  Unixqueue.run esys;
  match e#state with
    | `Done st ->
	( match st with
	    | Shell_sys.Job_running ->
		prerr_endline "Job_running"
	    | Shell_sys.Job_partially_running ->
		prerr_endline "Job_partially_running"
	    | Shell_sys.Job_error ->
		prerr_endline "Job_error"
	    | Shell_sys.Job_abandoned ->
		prerr_endline "Job_abandoned"
	    | Shell_sys.Job_ok ->
		List.iter
		  (fun p ->
		     let pid = Shell_sys.process_id p in
		     ( try 
			 ignore(Unix.kill pid 0);
			 prerr_endline ("Process exists! pid=" ^ string_of_int pid)
		       with _ -> ()
		     )
		  )
		  (Shell_sys.processes e#job_instance)
	)
    | `Error e ->
	prerr_endline("Error: " ^ Netexn.to_string e)
    | `Aborted ->
	prerr_endline "Aborted"
    | `Working _ ->
	prerr_endline "Working"


let test() = 
  for n = 1 to 1000 do
    run()
  done


let () =
  test()
