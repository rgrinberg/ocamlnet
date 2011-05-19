open Netshm

let name = 
  let sd = create_unique_shm (`File "file_XXXX") 0o666  in
  let n = name_of_shm sd in
  close_shm sd;
  n

let fill() =
  let sd = open_shm name [Unix.O_RDWR] 0 in
  let t = manage `Record_locking sd in
  for k = 0 to 99 do
    add t (Int32.of_int k) (Netshm.bigarray [| 1 |])
  done;
  close_shm sd

let rec restart f arg =
  try
    f arg
  with
    | Deadlock -> 
	Unix.sleep 1;
	restart f arg

let wild_thing() =
  let sd = open_shm name [Unix.O_RDWR] 0 in
  let t = manage `Record_locking sd in
  for n = 1 to 10000 do
    if n mod 100 = 0 then
      Printf.printf "Starting round %d/100\n%!" (n/100);
    let k = Random.int 100 in
    restart
      (group t
	 (fun () ->
	    let x = find t (Int32.of_int k) in
	    let l = Bigarray.Array1.dim x in
	    let x' = Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout (l+1) in
	    Bigarray.Array1.blit
	      x
	      (Bigarray.Array1.sub x' 0 l);
	    x'.{ l } <- Int32.of_int (l+1);
	    replace t (Int32.of_int k) x'
	 )
      )
      ()
  done;
  close_shm sd

let fork'n'go() =
  let l = ref [] in
  for n = 1 to 10 do
    match Unix.fork() with
      | 0 ->
	  Random.init 1;
	  wild_thing();
	  exit 0
      | pid ->
	  l := pid :: !l
  done;
  List.iter
    (fun pid ->
       let _ = Unix.waitpid [] pid in
       ()
    )
    !l

let main() =
  fill();
  fork'n'go();
  let sd = open_shm name [Unix.O_RDWR] 0 in
  let t = manage `Record_locking sd in
  dump t;
  close_shm sd


let () =
  main()
