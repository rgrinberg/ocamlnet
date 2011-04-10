(* $Id$ *)

(* Literature:
   "Implementing Condition Variables with Semaphores", by Andrew D. Birrell,
   Microsoft Research Silicon Valley, January 2003

   http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.125.3384&rep=rep1&type=pdf
 *)

type condition =
    { dummy_cond : bool;
      mutable waiters : wait_entry;
      mutable null : wait_entry;   (* means: "no more entry" *)
      mutable lock : Netmcore_sem.semaphore;
    }

and wait_entry =
    { mutable empty : bool;  
      (* if empty, this entry is the last in the set, and it is considered
	 as representing no waiter
       *)
      mutable sem : Netmcore_sem.semaphore;
      mutable next : wait_entry;      (* may be cond.null *)
      mutable set_next : wait_entry;  (* not meaningful if [empty] *)
    }

and wait_set = 
    { dummy_set : bool;
      mutable alloc_lock : Netmcore_sem.semaphore;
      mutable head : wait_entry;
    }

let empty_wait_entry() =
  let rec we =
    { empty = true;
      sem = Netmcore_sem.dummy();
      next = we;
      set_next = we;
    } in
  we


let dummy_condition () =
  { dummy_cond = true;
    waiters = empty_wait_entry ();
    null = empty_wait_entry ();
    lock = Netmcore_sem.dummy()
  }


let dummy_wait_set() =
  { dummy_set = true;
    alloc_lock = Netmcore_sem.dummy();
    head = empty_wait_entry()
  }


let create_condition mut =
  let null = empty_wait_entry () in
  let cond_orig =
    { dummy_cond = false;
      waiters = null;
      null = null;
      lock = Netmcore_sem.dummy()
    } in
  let cond = Netmcore_heap.add mut cond_orig in
  Netmcore_heap.pin mut cond;
  cond.lock <- Netmcore_sem.create mut 1;
  cond

let destroy_condition c =
  if not c.dummy_cond then (
    Netmcore_sem.destroy c.lock
  )


let create_wait_set mut =
  let wset_orig =
    { dummy_set = false;
      alloc_lock = Netmcore_sem.dummy();
      head = empty_wait_entry()
    } in
  let wset = Netmcore_heap.add mut wset_orig in
  Netmcore_heap.pin mut wset;
  wset.alloc_lock <- Netmcore_sem.create mut 1;
  wset
  

let destroy_wait_set wset =
  if not wset.dummy_set then (
    let we = ref wset.head in
    Netmcore_sem.destroy !we.sem;
    while not !we.empty do
      we := !we.set_next;
      Netmcore_sem.destroy !we.sem;
    done;
    Netmcore_sem.destroy wset.alloc_lock
  )


let with_alloc_lock wset f =
  Netmcore_sem.wait wset.alloc_lock Netsys_posix.SEM_WAIT_BLOCK;
  try
    let r = f () in
    Netmcore_sem.post wset.alloc_lock;
    r
  with error ->
    Netmcore_sem.post wset.alloc_lock;
    raise error


let alloc_wait_entry mut wset =
  if wset.dummy_set then
    failwith "Netmcore_condition.alloc_wait_entry: dummy wait_set";
  with_alloc_lock wset
    (fun () ->
       (* not really fast *)
       let we = ref wset.head in
       while not !we.empty do
	 we := !we.set_next
       done;
       let tail_orig = empty_wait_entry () in
       let tail = Netmcore_heap.add mut tail_orig in
       !we.set_next <- tail;
       !we.empty <- false;
       !we.sem <- Netmcore_sem.create mut 0;
       !we
    )


let free_wait_entry mut wset we_to_free =
  if wset.dummy_set then
    failwith "Netmcore_condition.free_wait_entry: dummy wait_set";
  with_alloc_lock wset
    (fun () ->
       (* not really fast *)
       let we = ref wset.head in
       let prev = ref None in
       while not !we.empty && !we != we_to_free do
	 prev := Some !we;
	 we := !we.set_next
       done;
       if !we.empty then
	 failwith "Netmcore_condition.free_wait_entry: not found";
       ( match !prev with
	   | None ->
	       wset.head <- !we.set_next
	   | Some p ->
	       p.set_next <- !we.set_next
       );
       !we.set_next <- !we;
       !we.next <- !we
    )


let wait we c m =
  if c.dummy_cond then
    failwith "Netmcore_condition.wait: dummy condition";
  if we.next != we then
    failwith "Netmcore_condition.wait: the wait entry is being used";
  Netmcore_sem.wait c.lock Netsys_posix.SEM_WAIT_BLOCK;
  let old_waiters = c.waiters in
  c.waiters <- we;
  we.next <- old_waiters;
  Netmcore_sem.post c.lock;
  Netmcore_mutex.unlock m;
  Netmcore_sem.wait we.sem Netsys_posix.SEM_WAIT_BLOCK;
  Netmcore_mutex.lock m


let signal c =
  if c.dummy_cond then
    failwith "Netmcore_condition.signal: dummy condition";
  Netmcore_sem.wait c.lock Netsys_posix.SEM_WAIT_BLOCK;
  if c.waiters != c.null then (
    let we = c.waiters in
    c.waiters <- we.next;
    we.next <- we;
    Netmcore_sem.post we.sem
  );
  Netmcore_sem.post c.lock

let broadcast c =
  if c.dummy_cond then
    failwith "Netmcore_condition.broadcast: dummy condition";
  Netmcore_sem.wait c.lock Netsys_posix.SEM_WAIT_BLOCK;
  while c.waiters != c.null do
    let we = c.waiters in
    c.waiters <- we.next;
    we.next <- we;
    Netmcore_sem.post we.sem
  done;
  Netmcore_sem.post c.lock
