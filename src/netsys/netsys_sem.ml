(* $Id$ *)

type sem_open_flag = Netsys_posix.sem_open_flag =
  | SEM_O_CREAT
  | SEM_O_EXCL

type sem_wait_behavior = Netsys_posix.sem_wait_behavior =
  | SEM_WAIT_BLOCK
  | SEM_WAIT_NONBLOCK

type prefix = string


module Emu = struct
  let n = 16384
    (* We support at most this number of semaphores per container.
       This number can be increased to at most 65535 without changing
       the code.
     *)

  (* Note that there is a basic problem with [mutex]: If the process dies
     while the mutex is in locked state, no other process will ever
     again get the lock. TODO: We could protect against this by recording
     the PID of the lock holder.
   *)

  type anon_semaphore =
      { sem : Netsys_posix.named_semaphore;
	num : int
      }

  type container =
      { prefix : string;
	used : (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t;
	active : (int, anon_semaphore) Hashtbl.t;
	mutex : Netsys_posix.named_semaphore;
      }

  let have_anon_semaphores() =
    Netsys_posix.have_named_posix_semaphores()

  let prefix cont = cont.prefix

  let container prefix =
    let fd = 
      Netsys_posix.shm_open
	(prefix ^ "_sems")
	[ Netsys_posix.SHM_O_RDWR;
	  Netsys_posix.SHM_O_CREAT
	]
	0o600 in
    ( try
	let st = Unix.fstat fd in
	if st.Unix.st_size = 0 then
	  Unix.ftruncate fd (8 * n);
	let used = 
	  Netsys_mem.memory_map_file fd true n in
	Unix.close fd;
	{ prefix = prefix;
	  used = used;
	  active = Hashtbl.create 47;
	  mutex = (Netsys_posix.sem_open
		     (prefix ^ "_contsem")
		     [ Netsys_posix.SEM_O_CREAT ]
		     0o600
		     1)
	}
      with error ->
	Unix.close fd;
	raise error
    )

  let lock cont =
    Netsys_posix.sem_wait cont.mutex Netsys_posix.SEM_WAIT_BLOCK

  let unlock cont =
    Netsys_posix.sem_post cont.mutex

  let sem_name cont k =
    cont.prefix ^ "_sem" ^ string_of_int k

  let unlink prefix =
    (* A radical way to get rid of all persistent objects, without acquiring
       mutex
     *)
    let attempt f arg =
      try f arg
      with _ -> () in
    attempt Netsys_posix.shm_unlink (prefix ^ "_sems");
    attempt Netsys_posix.sem_unlink (prefix ^ "_contsem");
    for k = 0 to n-1 do
      attempt Netsys_posix.sem_unlink (prefix ^ "_sem" ^ string_of_int k)
    done

  let create_container prefix =
    unlink prefix;
    container prefix

  let lookup cont k =
    (* Look the existing semaphore #k up. ENOENT if not found *)
    lock cont;
    try
      try 
        let usem = Hashtbl.find cont.active k in
        unlock cont;
        usem
      with Not_found ->
	let sem =
	  Netsys_posix.sem_open
	    (sem_name cont k)
	    []
	    0
	    0 in
	let usem =
	  { sem = sem;
	    num = k
	  } in
	Hashtbl.add cont.active k usem;
	unlock cont;
	usem
    with error ->
      unlock cont;
      raise error


  let find_unused cont =
    (* Find a free k, and mark it as used *)
    lock cont;
    let k = ref 0 in
    while !k < n && cont.used.{ !k } = '\001' do
      incr k
    done;
    if !k < n then (
      cont.used.{ !k } <- '\001';
      unlock cont;
      !k
    )
    else (
      unlock cont;
      raise(Unix.Unix_error(Unix.ENOMEM,
			    "Netsys_shm.find_unused (too many semaphores)",
			    ""))
    )

  let mark_unused cont k =
    cont.used.{ k } <- '\000'


  let as_sem cont mem pos =
    let k =
      Char.code mem.{ pos } + 256 * Char.code mem.{ pos+1 } in
    lookup cont k

  let sem_init cont mem pos pshared init_value =
    let k = find_unused cont in
    let sem =
      Netsys_posix.sem_open
	(sem_name cont k)
	[ Netsys_posix.SEM_O_CREAT; Netsys_posix.SEM_O_EXCL ]
	0o600
	init_value in
    let usem =
      { sem = sem;
	num = k
      } in
    lock cont;
    Hashtbl.replace cont.active k usem;
    unlock cont;
    mem.{ pos } <- Char.chr (k land 0xff);
    mem.{ pos+1 } <- Char.chr (k lsr 8);
    usem

  let sem_idestroy cont k =
    Hashtbl.remove cont.active k;
    ( try
	Netsys_posix.sem_unlink
	  (sem_name cont k)
      with _ -> ()
    );
    mark_unused cont k

  let sem_destroy cont usem =
    Netsys_posix.sem_close usem.sem;
    lock cont;
    sem_idestroy cont usem.num;
    unlock cont

  let drop_container cont =
    lock cont;
    for k = 0 to n-1 do
      if cont.used.{k} = '\001' then
	sem_idestroy cont k
    done;
    ( try
	Netsys_posix.shm_unlink (cont.prefix ^ "_sems")
      with _ -> ()
    );
    ( try
	Netsys_posix.sem_unlink (cont.prefix ^ "_contsem")
      with _ -> ()
    );
    unlock cont

  let sem_getvalue usem =
    Netsys_posix.sem_getvalue usem.sem

  let sem_post usem =
    Netsys_posix.sem_post usem.sem

  let sem_wait usem wb =
    Netsys_posix.sem_wait usem.sem wb

end


module Native = struct
  type container = string (* the prefix *)

  type anon_semaphore =
      Netsys_posix.anon_semaphore

  let have_anon_semaphores() =
    Netsys_posix.have_anon_posix_semaphores()

  let container p = p

  let create_container p = p

  let drop_container _ = ()

  let unlink _ = ()

  let prefix p = p

  let sem_init _ = 
    Netsys_posix.sem_init

  let sem_destroy _ =
    Netsys_posix.sem_destroy

  let as_sem _ =
    Netsys_posix.as_sem

  let sem_getvalue =
    Netsys_posix.sem_getvalue

  let sem_post =
    Netsys_posix.sem_post

  let sem_wait =
    Netsys_posix.sem_wait

end


type container =
  [ `E of Emu.container
  | `N of Native.container
  ]

type anon_semaphore =
  [ `E of Emu.anon_semaphore
  | `N of Native.anon_semaphore
  ]


let force_emu = ref false

let force_emulation() =
  force_emu := true

let emu() =
  !force_emu || 
    (Netsys_posix.have_named_posix_semaphores() && 
       not (Netsys_posix.have_anon_posix_semaphores()))

let have_anon_semaphores() =
  if emu() then
    Emu.have_anon_semaphores()
  else
    Native.have_anon_semaphores()


let container prefix : container =
  if emu() then
    `E(Emu.container prefix)
  else
    `N(Native.container prefix)

let create_container prefix =
  if emu() then
    `E(Emu.create_container prefix)
  else
    `N(Native.create_container prefix)

let unlink prefix =
  if emu() then
    Emu.unlink prefix
  else
    Native.unlink prefix

let prefix =
  function
    | `E c -> Emu.prefix c
    | `N c -> Native.prefix c

let drop cont =
  match cont with
    | `E c -> Emu.drop_container c
    | `N c -> Native.drop_container c

let sem_init cont mem pos pshared init_value =
  match cont with
    | `E c -> `E(Emu.sem_init c mem pos pshared init_value)
    | `N c -> `N(Native.sem_init c mem pos pshared init_value)

let sem_destroy cont sem =
  match (cont,sem) with
    | `E c, `E s -> Emu.sem_destroy c s
    | `N c, `N s -> Native.sem_destroy c s
    | _ -> assert false

let as_sem cont mem pos =
  match cont with
    | `E c -> `E(Emu.as_sem c mem pos)
    | `N c -> `N(Native.as_sem c mem pos)

let sem_getvalue sem =
  match sem with
    | `E s -> Emu.sem_getvalue s
    | `N s -> Native.sem_getvalue s

let sem_post sem =
  match sem with
    | `E s -> Emu.sem_post s
    | `N s -> Native.sem_post s

let sem_wait sem =
  match sem with
    | `E s -> Emu.sem_wait s
    | `N s -> Native.sem_wait s

let sem_value_max =
  Netsys_posix.sem_value_max

let sem_size =
  max 4 Netsys_posix.sem_size

