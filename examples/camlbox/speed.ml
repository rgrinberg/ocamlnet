(* Send small messages to a forked subprocess *)

let name = "camlbox_" ^ string_of_int(Unix.getpid())

let create() =
  let box = Netcamlbox.create_camlbox name 10 512 in
  let fd = Netcamlbox.camlbox_fd name in
  Netcamlbox.unlink_camlbox name;
  (box, fd)

let receiver box =
  let cont = ref true in
  let sum = ref 0 in
  while !cont do
    let new_list =
      Netcamlbox.camlbox_wait box in
    List.iter
      (fun k ->
	 let (msg : int list ref) =
	   Netcamlbox.camlbox_get box k in
	 sum := List.fold_left ( + ) !sum !msg;
	 if !msg = [] then
	   cont := false;
	 Netcamlbox.camlbox_delete box k
      )
      new_list
  done;
  print_endline ("Sum: " ^ string_of_int !sum);
  exit 0

let sender fd n =
  let bs = Netcamlbox.camlbox_sender_of_fd fd in
  for k = 1 to n do
    let (msg : int list ref) = 
      ref [ 1; 2; 3 ] in
    Netcamlbox.camlbox_send bs msg
  done;
  Netcamlbox.camlbox_send bs (ref [])
    (* that's the reason we use a ref: Otherwise the empty list is not
       boxed, and will cause an error
     *)

let main() =
  let n = int_of_string (Sys.argv.(1)) in
  let (box, fd) = create() in
  match Unix.fork() with
    | 0 ->
	receiver box
    | pid ->
	sender fd n;
	ignore(Unix.waitpid [] pid)

let () =
  main()

