(* Example: pipeline between processes

   Here a Netmcore_buffer is used as a pipe buffer. A producer
   pumps data into the pipeline, and the consumer reads it.

   A Netmcore_buffer can grow to arbitrary sizes. In order to 
   prevent this, we put a few synchronization variables into
   the header of the buffer.
 *)

open Printf

type header =
    { mutable length : int;
      mutable eof : bool;
      mutable lock : Netmcore_mutex.mutex;
      mutable have_space : Netmcore_condition.condition;
      mutable have_data : Netmcore_condition.condition;
      mutable wait_set : Netmcore_condition.wait_set
    }

type buffer = header Netmcore_buffer.buffer

type buffer_descr = header Netmcore_buffer.buffer_descr

let pool_size = 1024 * 1024                   (* 1 M *)
  (* Size of the shm block *)

let max_buffer = 65536                        (* 64 K *)
  (* Size of the pipe buffer *)

let buf_increment = 16384
  (* Pipe buffer space is allocated in units of this *)

let send_size = 16 * 1024 * 1024 * 1024       (* 16 G *)
  (* This much data is sent over the pipe in total *)

let variant = `Nocopy
  (* see consumer *)

(* Define the pool. This is allowed to do in the master process.
   Note that the process calling create_mempool generally cannot use
   the pool!
 *)

let pool = Netmcore_mempool.create_mempool pool_size

let producer (bd:buffer_descr) =
  (* We send (almost) endlessly the string 0123456789... *)
  let b = Netmcore_buffer.buffer_of_descr pool bd in
  let p_len = max_buffer - (max_buffer mod 10) + 10 in
  let p = String.make p_len ' ' in
  for k = 0 to p_len-1 do
    p.[k] <- Char.chr(48 + (k mod 10))
  done;

  let h = Netmcore_buffer.header b in
  let w = 
    (* Get a wait entry for the condition variables *)
    Netmcore_heap.modify
      (Netmcore_buffer.heap b)
      (fun mut ->
	 Netmcore_condition.alloc_wait_entry mut h.wait_set
      ) in

  let n = ref 0 in
  let q = ref 0 in

  while !n < send_size do
    let l = (
      let l_ref = ref 0 in
      Netmcore_mutex.lock h.lock;
      while h.length >= max_buffer do
	Netmcore_condition.wait w h.have_space h.lock
      done;
      l_ref := h.length;
      Netmcore_mutex.unlock h.lock;
      !l_ref
    ) in

    let space = max_buffer - l in

    let to_send0 = min (send_size - !n) space in
    let to_send = ref to_send0 in
(* printf "after %d bytes, sending %d bytes\n%!" !n to_send0; *)
    while !to_send > 0 do
      let m = min !to_send (p_len - !q) in
      Netmcore_buffer.add_sub_string b p !q m;
      to_send := !to_send - m;
      q := (!q + m) mod 10;
      n := !n + m;
    done;

    Netmcore_mutex.lock h.lock;
    h.length <- h.length + to_send0;
    h.eof <- !n = send_size;
    Netmcore_mutex.unlock h.lock;
    
    Netmcore_condition.signal h.have_data
  done


let producer_fork, producer_join =
  Netmcore_process.def_process producer


let one_meg = float (1024 * 1024)

let consumer (bd:buffer_descr) =
  (* We compute a checksum, just to do something with the data *)
  let b = Netmcore_buffer.buffer_of_descr pool bd in
  let cksum = ref 0 in
  let t0 = Unix.gettimeofday() in
  let t1 = ref t0 in
  let n1 = ref 0 in

  let h = Netmcore_buffer.header b in
  let w = 
    Netmcore_heap.modify
      (Netmcore_buffer.heap b)
      (fun mut ->
	 Netmcore_condition.alloc_wait_entry mut h.wait_set
      ) in

  let continue = ref true in
  let i = ref 0 in

  while !continue do
    let l = (
      let l_ref = ref 0 in
      Netmcore_mutex.lock h.lock;
      while h.length = 0 do
	Netmcore_condition.wait w h.have_data h.lock
      done;
      l_ref := h.length;
      continue := not h.eof;
      Netmcore_mutex.unlock h.lock;
      !l_ref
    ) in

    (* We implement here two variants:
       (1) Copy data immediately, signal, then evaluate data
           (good if evaluation is expensive)
       (2) Evaluate data on the original string, then signal
           (good if evaluation is cheap)
     *)
    let process s pos len =
      for k = 0 to len-1 do
	let c = Char.code (String.unsafe_get s (pos+k)) in
	cksum := (!cksum lsl 3) + c
      done;
      i := !i + len;
      n1 := !n1 + len in

    let postprocess () =
      let t = Unix.gettimeofday() in
      if t -. !t1 >= 1.0 then (
	printf "received %d bytes, %.1f M/s\n%!"
	  !n1 ((float !n1 /. (t -. !t1)) /. one_meg);
	n1 := 0;
	t1 := t;
      ) in

    let signal len =
      Netmcore_mutex.lock h.lock;
      h.length <- h.length - len;
      Netmcore_mutex.unlock h.lock;
      Netmcore_condition.signal h.have_space in

    match variant with
      | `Copy ->
	  let s = Netmcore_buffer.sub b !i l in
	  Netmcore_buffer.delete_hd b l;
	  signal l;
	  process s 0 l;
	  postprocess()
      | `Nocopy ->
	  let r = ref 0 in
	  while !r < l do
	    Netmcore_buffer.access b !i
	      (fun s pos len ->
		 let len' = min len (l - !r) in
		 process s pos len';
		 r := !r + len';
	      )
	  done;
	  Netmcore_buffer.delete_hd b l;
	  signal l;
	  postprocess()
  done;

  let t = Unix.gettimeofday() in
  printf "Total: received %d bytes, %.1f M/s\n%!"
    !i ((float !i /. (t -. t0)) /. one_meg);
  printf "Checksum: %d\n%!" (!cksum land 0x3fff_ffff)


let consumer_fork, consumer_join =
  Netmcore_process.def_process consumer


let control() =
  (* Create the buffer. Note that synchronization variables must be
     directly created in shared memory. To do so, we first use
     dummy values (h_orig is in normal memory), and after the header
     has been copied to shm, we do the real initialization.
   *)
  let h_orig =
    { length = 0;
      eof = false;
      lock = Netmcore_mutex.dummy();
      have_space = Netmcore_condition.dummy_condition();
      have_data = Netmcore_condition.dummy_condition();
      wait_set = Netmcore_condition.dummy_wait_set()
    } in
  let b = Netmcore_buffer.create pool buf_increment h_orig in
  Netmcore_heap.modify
    (Netmcore_buffer.heap b)
    (fun mut ->
       (* N.B. h is a copy of h_orig residing in shm *)
       let h = Netmcore_buffer.header b in
       h.lock <- Netmcore_mutex.create mut `Normal;
       h.have_space <- Netmcore_condition.create_condition mut;
       h.have_data <- Netmcore_condition.create_condition mut;
       h.wait_set <- Netmcore_condition.create_wait_set mut;
    );

  (* Now start the workers for producing and consuming data *)
  let producer_pid =
    Netmcore_process.start
      ~inherit_resources:`All
      producer_fork (Netmcore_buffer.descr_of_buffer b) in
  let consumer_pid =
    Netmcore_process.start
      ~inherit_resources:`All
      consumer_fork (Netmcore_buffer.descr_of_buffer b) in
  
  (* Now wait until these processes are done *)
  ( match Netmcore_process.join producer_join producer_pid with
      | None ->
	  failwith "Error in the producer"
      | Some _ ->
	  ()
  );
  ( match Netmcore_process.join consumer_join consumer_pid with
      | None ->
	  failwith "Error in the consumer"
      | Some _ ->
	  ()
  );

  (* Cleanup: *)
  let h = Netmcore_buffer.header b in
  Netmcore_mutex.destroy h.lock;
  Netmcore_condition.destroy_condition h.have_space;
  Netmcore_condition.destroy_condition h.have_data;
  Netmcore_condition.destroy_wait_set h.wait_set;
  Netmcore_buffer.destroy b


let control_fork, control_join =
  Netmcore_process.def_process control


let () =
  (* Netmcore_mempool.Debug.enable_alloc := true; *)
  Netmcore.startup
    ~socket_directory:"run_pipeline"
    ~first_process:(fun () -> 
		      Netmcore_process.start 
			~inherit_resources:`All control_fork ())
    ()
