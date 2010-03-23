(* $Id$ *)

(* Format of the shm object:

   - Const: CAPACITY

     Number of message slots

   - Const: MSG_SIZE

     Max size of messages

   - Sem: NOTIFICATIONS

     This semaphore is increased by the sender to wake up the receiver,
     and decreased by the receiver.

   - Sem: FREE_SLOTS

     Number of free slots. Increased by the receiver on [delete].
     Decreased by the sender when waiting for space.

   - For each slot: Sem SLOT_IGNORE.

     The number 0 if the slot is filled and has a message,
     or 1 if it is free or should be ignored by the receiver.

   - Array SLOT_LIST and number SPLIT_IDX:

     The array positions 0 .. SPLIT_IDX-1 contain the numbers of the
     filled slots. The array positions SPLIT_IDX .. CAPACITY-1 contain
     the number of free slots.

     Sem SLOT_LOCK: mutex for SLOT_LIST and SPLIT_IDX.


  Sending:
   - sem_wait(FREE_SLOTS)
   - lock SLOT_LOCK
   - find a free slot, and move it to the part of the slot list with the
     filled slots
   - unlock SLOT_LOCK
   - copy the value to the chosen slot
   - sem_wait(SLOT_IGNORE) for the chosen slot to lower the value to 0
   - sem_post(NOTIFICATIONS)

  Receiving:
  1. Wait for message:
      - sem_wait(NOTIFICATIONS)
      - lock SLOT_LOCK
      - get the part of the slot list with the filled slots
      - unlock SLOT_LOCK
      - for each filled slot:
          - get the value of SLOT_IGNORE. If it is 1 skip the slot.
          - if the slot was already reported to the user, skip the slot
          - otherwise include the slot in the result list

  2. Get message:
      - test SLOT_IGNORE. If it is 0 read the message

  3. Delete the message:
      - Remove the message from the messages that "wait" already reported
      - sem_post(SLOT_IGNORE)
      - lock SLOT_LOCK
      - move the slot to the part of the slot list with the empty slots
      - unlock SLOT_LOCK
      - sem_post(FREE_SLOTS)


   FORMAT OF THE SHARED MEMORY OBJECT:

   byte 0: <header>
   byte <sem_offset>: <array of semaphores>
   byte <sl_offset>:  <slot list>
   byte <offset>:     first slot. Slots come one after the other, and
                      are aligned at 128 byte boundaries

   <header>: the ocaml representation of type [header]
   <array of semaphores>: the semaphores one after the other
   <slot list>: the ocaml representation of type [int array]

 *)

open Netsys_mem

type camlbox_address = string

(* The header must not contain pointers, so it is relocatable *)
type header =
    { mutable address : int;    (* address lsr 1 *)
      mutable sem_offset : int; (* offset to semaphore area *)
      mutable sl_offset : int;  (* offset to slot_list *)
      mutable offset : int;     (* offset to first slot *)
      capacity : int;
      msg_size : int;
      mutable split_idx : int;
    }

type semaphores =
    { s_notifications : Netsys_posix.anon_sempahore;
      s_free_slots : Netsys_posix.anon_sempahore;
      s_slot_ignore_a : Netsys_posix.anon_sempahore array;
      s_slot_lock : Netsys_posix.anon_sempahore ;
    }

type camlbox =
    { mem : memory;
      hdr : header;            (* pointing into mem *)
      slot_list : int array;   (* pointing into mem *)
      sem : semaphores;        (* pointing into mem *)
      is_new : bool array;     (* local *)
    }

type camlbox_sender = camlbox

exception Empty
exception Message_too_big

let align = 128
  (* header and slots are aligned by this *)

let ws_bytes = 
    Sys.word_size / 8

let create_header capacity msg_size =
  let dummy = Bigarray.Array1.create Bigarray.char Bigarray.c_layout  0 in
  let hdr =
    { address = 0;
      sem_offset = 0;
      sl_offset = 0;
      offset = 0;
      capacity = capacity;
      msg_size = msg_size;
      split_idx = 0
    } in
  let _, hdr_bytelen = init_value dummy 0 hdr [ Copy_simulate ] in
  hdr.sem_offset <- hdr_bytelen;
  let sem_bytelen = (capacity + 4) * Netsys_posix.sem_size in
  let slot_list = Array.init capacity (fun k -> k) in
  let _, sl_bytelen = init_value dummy 0 slot_list [ Copy_simulate ] in
  hdr.sl_offset <- hdr_bytelen + sem_bytelen;
  let bytelen = hdr_bytelen + sem_bytelen + sl_bytelen in
  hdr.offset <- (((bytelen-1) / align) + 1) * align;
  (hdr, slot_list)

let init_semaphores hdr mem =
  let offset = hdr.sem_offset in
  let init_sem k v =
    ignore(
      Netsys_posix.sem_init mem (offset+k*Netsys_posix.sem_size) true v) in
  init_sem 0 0;            (* notifications *)
  init_sem 1 hdr.capacity; (* free_slots *)
  init_sem 2 1;            (* slot_lock *)
  for k = 0 to hdr.capacity-1 do
    init_sem (k+3) 1       (* slot_ignore.(k) *)
  done

let destroy_semaphores hdr mem =
  (* Must be called before shm is unmapped (only in the receiver) *)
  let offset = hdr.sem_offset in
  for k = 0 to hdr.capacity+2 do
    let s = Netsys_posix.as_sem mem (offset+k*Netsys_posix.sem_size) in
    Netsys_posix.sem_destroy s
  done

let free_mem mem =
  let hdr =
    (as_value mem ws_bytes : header) in
  destroy_semaphores hdr mem

let get_semaphores hdr mem =
  let offset = hdr.sem_offset in
  let get_sem k =
    Netsys_posix.as_sem mem (offset+k*Netsys_posix.sem_size) in
  { s_notifications = get_sem 0;          (* notifications *)
    s_free_slots = get_sem 1;             (* free_slots *)
    s_slot_lock = get_sem 2;              (* slot_lock *)
    s_slot_ignore_a = 
      Array.init hdr.capacity (fun k -> get_sem (k+3));
  }


let create_camlbox addr capacity msg_size =
  if not (Netsys_posix.have_posix_shm()) then
    invalid_arg "create_camlbox (no POSIX shm)";
  if not (Netsys_posix.have_posix_semaphores()) then
    invalid_arg "create_camlbox (no POSIX semaphores)";
  if capacity < 1 || capacity > Sys.max_array_length || msg_size < 1 then
    invalid_arg "create_camlbox (bad params)";
  if String.contains addr '/' then
    invalid_arg "create_camlbox (bad name)";
  if capacity > Netsys_posix.sem_value_max then
    invalid_arg "create_camlbox (capacity exceeds sem_value_max)";
  let slot_size = (((msg_size-1) / align) + 1) * align in
  if slot_size < msg_size then (* overflow *)
    invalid_arg "create_camlbox (too large)";
  let (hdr0, slot_list0) = create_header capacity msg_size in
  let shm_size = hdr0.offset + capacity * slot_size in
  if shm_size < hdr0.offset then (* overflow *)
    invalid_arg "create_camlbox (too large)";
  if (shm_size - hdr0.offset) / slot_size <> capacity then (* overflow *)
    invalid_arg "create_camlbox (too large)";
  
  let shm =
    Netsys_posix.shm_open
      ("/" ^ addr)
      [ Netsys_posix.SHM_O_RDWR;
	Netsys_posix.SHM_O_CREAT;
	Netsys_posix.SHM_O_EXCL
      ]
      0o600 in
  let mem = 
    try
      Unix.ftruncate shm shm_size;
      let mem = Netsys_mem.memory_map_file shm true shm_size in
      Unix.close shm;
      mem
    with
      | error ->
	  Unix.close shm;
	  raise error in
  
  value_area mem;
  hdr0.address <- 
    Nativeint.to_int(Nativeint.shift_right (memory_address mem) 1);
  let hdr_voffs, _ = init_value mem 0 hdr0 [] in
  let hdr = (as_value mem hdr_voffs : header) in
  init_semaphores hdr mem;
  let sl_voffs, _ = init_value mem hdr.sl_offset slot_list0 [] in
  let slot_list = (as_value mem sl_voffs : int array) in
  Gc.finalise free_mem mem;
  
  { mem = mem;
    hdr = hdr;
    slot_list = slot_list;
    sem = get_semaphores hdr mem;
    is_new = Array.make capacity true;
  }


let unlink_camlbox addr =
  Netsys_posix.shm_unlink ("/" ^ addr)


let camlbox_fd addr =
  Netsys_posix.shm_open
    ("/" ^ addr)
    [ Netsys_posix.SHM_O_RDWR ]
    0o600

let with_fd fd f =
  try
    let r = f fd in
    Unix.close fd;
    r
  with
    | error ->
	Unix.close fd;
	raise error

let header_size = lazy(
  let dummy = Bigarray.Array1.create Bigarray.char Bigarray.c_layout  0 in
  let hdr =
    { address = 0;
      sem_offset = 0;
      sl_offset = 0;
      offset = 0;
      capacity = 0;
      msg_size = 0;
      split_idx = 0;
    } in
  let voffset, bytelen =
    init_value
      dummy 0 hdr 
      [ Copy_simulate ] in
  bytelen
)


let camlbox_open fd =
  (* does not initialize is_new! *)
  let hdr_size = Lazy.force header_size in
  let mini_mem = Netsys_mem.memory_map_file fd true hdr_size in
  let hdr0 = (as_value mini_mem ws_bytes : header) in
  let offset = hdr0.offset in
  let slot_size = (((hdr0.msg_size-1) / align) + 1) * align in
  let shm_size = offset + hdr0.capacity * slot_size in
  let mem = Netsys_mem.memory_map_file fd true shm_size in
  let hdr = (as_value mem ws_bytes : header) in
  let sl = (as_value mem (hdr.sl_offset+ws_bytes) : int array) in
  { mem = mem;
    hdr = hdr;
    slot_list = sl;
    sem = get_semaphores hdr mem;
    is_new = [| |]
  }

let camlbox_sender addr =
  with_fd (camlbox_fd addr) camlbox_open

let camlbox_sender_of_fd =
  camlbox_open

let camlbox_bcapacity box =
  box.hdr.capacity

let camlbox_scapacity = camlbox_bcapacity

let camlbox_capacity addr =
  with_fd
    (camlbox_fd addr)
    (fun fd ->
       let box = camlbox_open fd in
       camlbox_bcapacity box
    )

let camlbox_bmsg_size box =
  box.hdr.msg_size

let camlbox_smsg_size = camlbox_bmsg_size

let camlbox_msg_size addr =
  with_fd
    (camlbox_fd addr)
    (fun fd ->
       let box = camlbox_open fd in
       camlbox_bmsg_size box
    )

let camlbox_bmessages box =
  box.hdr.capacity - Netsys_posix.sem_getvalue box.sem.s_free_slots

let camlbox_smessages = camlbox_bmessages

let camlbox_messages addr =
  with_fd
    (camlbox_fd addr)
    (fun fd ->
       let box = camlbox_open fd in
       camlbox_bmessages box
    )

let camlbox_get box k =
  if k < 0 || k >= box.hdr.capacity then
    invalid_arg "camlbox_get";
  let v = Netsys_posix.sem_getvalue box.sem.s_slot_ignore_a.(k) in
  if v <> 0 then raise Empty;
  let slot_size = (((box.hdr.msg_size-1) / align) + 1) * align in
  let slot_offset = box.hdr.offset + k * slot_size in
  as_value box.mem (slot_offset + ws_bytes)

let swap (x:int array) p q =
  let u = x.(p) in
  x.(p) <- x.(q);
  x.(q) <- u

let camlbox_delete box k =
  if k < 0 || k >= box.hdr.capacity then
    invalid_arg "camlbox_delete";
  let v = Netsys_posix.sem_getvalue box.sem.s_slot_ignore_a.(k) in
  if v <> 0 then raise Empty;
  box.is_new.(k) <- true;
  Netsys_posix.sem_post box.sem.s_slot_ignore_a.(k);
  Netsys_posix.sem_wait box.sem.s_slot_lock Netsys_posix.SEM_WAIT_BLOCK;
  let l = box.slot_list in
  let j = ref 0 in
  ( try
      for i = 0 to box.hdr.split_idx - 1 do
	if l.(i) = k then ( j := i; raise Exit )
      done;
      assert false
    with
      | Exit -> ()
  );
  swap l !j (box.hdr.split_idx-1);
  box.hdr.split_idx <- box.hdr.split_idx - 1;
  Netsys_posix.sem_post box.sem.s_slot_lock;
  Netsys_posix.sem_post box.sem.s_free_slots
  
let camlbox_wait box =
  Netsys_posix.sem_wait box.sem.s_notifications Netsys_posix.SEM_WAIT_BLOCK;
  ( try
      while true do   (* decrement to 0 *)
	Netsys_posix.sem_wait 
	  box.sem.s_notifications Netsys_posix.SEM_WAIT_NONBLOCK;
      done
    with
      | Unix.Unix_error(Unix.EAGAIN,_,_) -> ()
  );
  Netsys_posix.sem_wait box.sem.s_slot_lock Netsys_posix.SEM_WAIT_BLOCK;
  let l = box.slot_list in
  let filled = ref [] in
  for i = 0 to box.hdr.split_idx - 1 do
    filled := l.(i) :: !filled
  done;
  Netsys_posix.sem_post box.sem.s_slot_lock;
  let r =
    List.filter
      (fun i ->
	 let v = Netsys_posix.sem_getvalue box.sem.s_slot_ignore_a.(i) in
	 v = 0 && box.is_new.(i)
      )
      !filled in
  List.iter
    (fun i -> box.is_new.(i) <- false)
    r;
  r

let camlbox_cancel_wait box =
  Netsys_posix.sem_post box.sem.s_notifications

let camlbox_wake = camlbox_cancel_wait

let camlbox_send box value =
  Netsys_posix.sem_wait box.sem.s_free_slots Netsys_posix.SEM_WAIT_BLOCK;
  Netsys_posix.sem_wait box.sem.s_slot_lock Netsys_posix.SEM_WAIT_BLOCK;
  assert(box.hdr.split_idx < box.hdr.capacity);
  let k = box.slot_list.(box.hdr.split_idx) in
  box.hdr.split_idx <- box.hdr.split_idx + 1;
  Netsys_posix.sem_post box.sem.s_slot_lock;
  let v = Netsys_posix.sem_getvalue box.sem.s_slot_ignore_a.(k) in
  assert(v <> 0);
  let slot_size = (((box.hdr.msg_size-1) / align) + 1) * align in
  let slot_offset = box.hdr.offset + k * slot_size in
  let (_, _) =
    try
      init_value 
	~targetaddr:(Nativeint.shift_left (Nativeint.of_int box.hdr.address) 1)
	box.mem 
	slot_offset
	value
	[ ] 
    with
	Out_of_space -> raise Message_too_big in
  ( try
      Netsys_posix.sem_wait 
	box.sem.s_slot_ignore_a.(k) 
	Netsys_posix.SEM_WAIT_NONBLOCK
    with
      | Unix.Unix_error(Unix.EAGAIN,_,_) -> assert false
  );
  Netsys_posix.sem_post box.sem.s_notifications

    
