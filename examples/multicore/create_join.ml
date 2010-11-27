(* Example: Create a number of processes and join them *)

open Printf

module Int_encap = Netplex_encap.Make_encap(struct type t=int end)
module Unit_encap = Netplex_encap.Make_encap(struct type t=unit end)

let square k_enc =
  (* a process body squaring an int *)
  let k = Int_encap.unwrap k_enc in
  let r = k * k in
  let r_enc = Int_encap.wrap r in
  printf "process: square(%d)=%d\n%!" k r;
  r_enc

let fork_square, join_square = Netmcore.def_process square



let compute _ =
  (* a process body that is kind of a main program for our computation *)

  (* start a few processes... *)
  let processes =
    List.map
      (fun k ->
	 let k_enc = Int_encap.wrap k in
	 let `Process pid = Netmcore.start fork_square k_enc in
	 printf "start(%d): pid=%d\n%!" k pid;
	 `Process pid
      )
      [ 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 ] in

  (* and join them, adding up the results *)
  let r =
    List.fold_left
      (fun acc pid ->
	 match Netmcore.join join_square pid with
	   | None ->
	       failwith "no result from process"
	   | Some sq_enc ->
	       acc + Int_encap.unwrap sq_enc
      )
      0
      processes in

  printf "Final result: %d\n%!" r;
  
  (* we have to return a final result *)
  Unit_encap.wrap ()


let fork_compute, join_compute = Netmcore.def_process compute



let () =
  Netmcore.Debug.enable := true;
  (* Netplex_controller.Debug.enable := true; *)
  Netlog.Debug.enable_module "Netplex_controller";
  Netplex_container.Debug.enable := true;
  Netmcore.startup
    ~socket_directory:"run_create_join"
    ~first_process:(fork_compute, Unit_encap.wrap ())
    ()
