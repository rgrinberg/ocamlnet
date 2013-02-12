(* Example: Create a number of processes and join them *)

open Printf

let square k =
  (* a process body squaring an int *)
  let r = k * k in
  printf "process: square(%d)=%d\n%!" k r;
  r

let fork_square, join_square = Netmcore_process.def_process square



let compute _ =
  (* a process body that is kind of a main program for our computation *)

  (* start a few processes... *)
  let processes =
    List.map
      (fun k ->
	 let `Process pid = Netmcore_process.start fork_square k in
	 printf "start(%d): pid=%d\n%!" k pid;
	 `Process pid
      )
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in

  (* and join them, adding up the results *)
  let r =
    List.fold_left
      (fun acc pid ->
	 match Netmcore_process.join join_square pid with
	   | None ->
	       failwith "no result from process"
	   | Some sq ->
	       acc + sq
      )
      0
      processes in

  r

let fork_compute, join_compute = Netmcore_process.def_process compute



let () =
  (* Netmcore.Debug.enable := true; *)
  (* Netplex_controller.Debug.enable := true; *)
  (* Netlog.Debug.enable_module "Netplex_controller"; *)
  (* Netplex_container.Debug.enable := true; *)
  let r =
    Netmcore.run
      ~socket_directory:"run_create_join"
      ~first_process:(fun () -> Netmcore_process.start fork_compute ())
      ~extract_result:(fun ctrl pid ->
                       (* NB. We cannot use {!Netmcore_process.join} here,
                          because we are running in the master process. However,
                          we know that the first process is already finished,
                          and so [join_nowait] is sufficient.
                        *)
                       match Netmcore_process.join_nowait join_compute pid with
                         | None ->
                              failwith "no result"
                         | Some r ->
                              r
                      )
      () in
  printf "Final result: %d\n%!" r;
  ()

