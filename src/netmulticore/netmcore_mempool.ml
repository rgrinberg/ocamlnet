(* $Id$ *)

exception Out_of_pool_memory

(* Header:
   - The first 8 bytes are simply: MEMPOOL\n
   - Now the offset to the [header] follows (8 bytes) - 0 if not yet
     initialized. The offset is relative to the start of the pool buffer
   - Now the semaphore for locking the pool
   - Now the space for the [header] value
 *)

type header =
    { pool_size : int;
      mutable pool_free : int;
      mutable pool_free_contiguous : int;
      
      (* Freelists: for k>=1 the array element pool_fl.(k) points to the
	 beginning of the free list for blocks with exactly k free pages.
	 pool_fl.(0) points to the free list for blocks with more
	 free pages than this array is long.

	 A free block has a record free_list at the beginning, see below.
       *)
      pool_fl : int array;

      (* Binary tree of allocated blocks. The tree is represented in an
	 array with a fixed number of elements. The tree is
	 AVL-balanced. Keys of the tree are offsets, and the values contain
	 the bsize.
       *)
      mutable pool_alloc : Netmcore_util.AVL.header;

      mutable pool_start : int; (* offset of the first payload page *)
    }

(* Free list entry: Unused blocks are initialized as follows:
   - The first 8 bytes are simply: MEMFREE\n 
   - The offset to [pool_fl] (8 bytes). The offset is relative to the start 
     of the free block
   - Now space for [pool_fl]
 *)
and pool_fl =
    { mutable pool_fl_next : int;       (* 0 if end *)
      mutable pool_fl_bsize : int;      (* size of block (multiple of page size) *)
    }

let pool_fl_size = 16
  (* size of pool_fl *)

let management_factor = 10
  (* Every this number of pages another alloc_tree element is preallocated *)

let magic = "MEMPOOL\n"
  (* must have length 8 *)

let fl_magic = "MEMFREE\n"
  (* must have length 8 *)

let page_size =
  try Netsys_mem.getpagesize() 
  with Invalid_argument _ -> 4096 

let alloc_mem n =
  Bigarray.Array1.create Bigarray.c_layout Bigarray.char n

let to_page n =
  n / page_size

let prec_block mem hdr offs =
  (* If there is an allocated block preceding the block at offs, the
     preceding block with the highest address is returned as
     [Some (p_offs,p_bsize)]. Otherwise [None] is returned.
   *)
  Netmcore_util.AVL.find_pred hdr.pool_alloc offs

let check_block mem hdr offs =
  (* If there is an allocated block at offs or at a succeeding address, the
     allocated block with the lowest address is returned as
     [Some (s_offs,s_bsize)]. Otherwise [None] is returned.
   *)
  Netmcore_util.AVL.find hdr.pool_alloc offs


let assert_is_free mem hdr offs =
  (* Check that the block at offs has the magic of a free block *)
  let u = String.create(String.length fl_magic) in
  Netsys_mem.blit_memory_to_string mem offs u 0 (String.length fl_magic);
  if u <> fl_magic then
    failwith 
      "Netmcore_mempool: Mem block does not have the signature of free blocks"


let lookup_fl_entry mem hdr offs =
  (* Return the pool_fl value of this free block *)
  assert_is_free mem hdr offs;
  let p = String.length fl_magic in
  let eoffs = Netnumber.int_of_int8 (Netnumber.HO.read_int8 mem (offs+p)) in
  let entry = (Netsys_mem.as_value mem (offs+eoffs) : pool_fl) in
  entry
  

let prec_adj_free_block mem hdr offs =
  (* If there is a free block immediately preceding the block at
     offs, it is returned as [Some(p_offs,p_bsize)]
   *)
  if offs > hdr.pool_start then (
    match prec_block mem hdr offs with
      | None ->
	  assert_is_free mem hdr hdr.pool_start;
	  Some(hdr.pool_start, offs - hdr.pool_start)
      | Some(p_offs,p_bsize) ->
	  let q = p_offs + p_bsize in
	  if q < offs then (
	    assert_is_free mem hdr q;
	    Some(q, offs - q)
	  )
	  else
	    None
  )
  else
    None


let check_free_block mem hdr offs =
  (* If the block at offs is a free block, it is returned as
     [Some(s_offs,s_bsize)]
   *)
  let pool_end = Bigarray.Array1.dim mem in
  let q = offs in
  if q < pool_end then (
    match check_block mem hdr offs with
      | None ->
	  assert_is_free mem hdr q;
	  Some(q, pool_end-q)
      | Some(s_offs,s_bsize) ->
	  if q < s_offs then (
	    assert_is_free mem hdr q;
	    Some(q, s_offs - q)
	  )
	  else
	    None
  )
  else
    None

let del_in_pool_fl_at mem hdr offs bsize prev =
  (* Delete this block from the freelist, and remove fl_magic.
     prev points to the preceding entry in the free list (or 0).
   *)
  let psize = bsize / page_size in
  let k = if psize < pool_fl_size then psize else 0 in
  let entry = lookup_fl_entry mem hdr offs in
  if prev = 0 then
    hdr.pool_fl.(k) <- entry.pool_fl_next
  else (
    let prev_entry = lookup_fl_entry mem hdr prev in
    prev_entry.pool_fl_next <- entry.pool_fl_next
  );
  let u = String.make (String.length fl_magic) ' ' in
  Netsys_mem.blit_string_to_memory u 0 mem offs (String.length fl_magic)


let del_in_pool_fl mem hdr offs bsize =
  (* Delete this block from the freelist, and remove fl_magic *)
  let psize = bsize / page_size in
  let k = if psize < pool_fl_size then psize else 0 in
  let prev = ref 0 in
  let cur = ref hdr.pool_fl.(k) in
  while !cur <> offs && !cur <> 0 do
    let e = lookup_fl_entry mem hdr !cur in
    prev := !cur;
    cur := e.pool_fl_next
  done;
  if !cur = 0 then
    failwith "Netmcore_mempool: Cannot find free block in free list";
  del_in_pool_fl_at mem hdr offs bsize prev


let add_to_pool_fl mem hdr offs bsize =
  (* Add the block at mem+offs to the free list. This function does not
     check whether the block can be joined with adjacent blocks.
     bsize is the length of the block.
   *)
  let p = String.length fl_magic in
  Netsys_mem.blit_string_to_memory fl_magic 0 mem offs p;
  let entry_orig =
    { pool_fl_next = 0;
      pool_fl_bsize = bsize
    } in
  let (voffs, _) = Netsys_mem.init_value mem (offs+p+8) entry_orig [] in
  let entry = (Netsys_mem.as_value mem (offs+p+8+voffs) : pool_fl) in
  let voffs_s =
    Netnumber.HO.int8_as_string (Netnumber.int8_of_int (p+8+voffs)) in
  Netsys_mem.blit_string_to_memory voffs_s 0 mem (offs+p) 8;
  let psize = bsize / page_size in
  let k = if psize < pool_fl_size then psize else 0 in
  entry.pool_fl_next <- hdr.pool_fl.(k);
  hdr.pool_fl.(k) <- offs


let merge_with_pool_fl mem hdr offs bsize =
  (* Same as add_to_pool_fl, but it is also checked whether the block
     can be joined with adjacent free blocks
   *)
  let prec_opt = prec_adj_free_block mem hdr offs in
  ( match prec_opt with
      | None -> ()
      | Some(p_offs,p_bsize) -> del_in_pool_fl mem hdr p_offs p_bsize	  
  );
  let succ_opt = check_free_block mem hdr (offs + bsize) in
  ( match succ_opt with
      | None -> ()
      | Some(s_offs,s_bsize) -> del_in_pool_fl mem hdr s_offs s_bsize	  
  );
  match prec_opt, succ_opt with
    | None, None ->
	add_to_pool_fl mem hdr offs bsize
    | Some(p_offs,p_bsize), None ->
	add_to_pool_fl mem hdr p_offs (p_bsize + bsize)
    | None, Some(s_offs,s_bsize) ->
	add_to_pool_fl mem hdr offs (bsize + s_bsize)
    | Some(p_offs,p_bsize), Some(s_offs,s_bsize) ->
	add_to_pool_fl mem hdr p_offs (bsize + p_bsize + s_bsize)


let find_free_block mem hdr bsize =
  (* Looks a free block up with at least bsize length, or raises 
     Out_of_pool_memory. The block is returned as (offs, prev) 
     where offs is the mem offset to the block, and prev is the 
     mem offset to the block preceding in the free list (or 0 if
     this is the first block in the free list)
   *)
  let psize = bsize / page_size in
  let k = ref(if psize < pool_fl_size then psize else 0) in
  let prev = ref 0 in
  let cur = ref hdr.pool_fl.( !k ) in
  let found = ref false in
  let best = ref 0 in
  let best_prev = ref 0 in
  while not !found && !cur <> 0 do
    let e = lookup_fl_entry mem hdr !cur in
    if e.pool_fl_bsize >= bsize then (
      best := !cur;
      best_prev := !prev
    );
    if e.pool_fl_bsize=bsize then found := true;
    prev := !cur;
    cur := e.pool_fl_next;
    if not !found && !cur = 0 && !k > 0 then (
      incr k;
      if !k = pool_fl_size then k := 0;
      prev := 0;
      cur := hdr.pool_fl.( !k )
    )
  done;
  if !best = 0 then
    raise Out_of_pool_memory;
  (!best, !best_prev)


let really_alloc_mem mem hdr bsize =
  (* bsize must here be a multiple of the page size *)
  let (offs, prev) = find_free_block mem hdr bsize in
  let e = lookup_fl_entry mem hdr offs in
  let alloc_offs =
    if e.pool_fl_bsize = bsize then (
      (* We need to remove this block from the free list *)
      del_in_pool_fl_at mem hdr offs bsize prev;
      offs
    )
    else (
      (* We consume the block only partially. We split the block into two
	 parts: The first part remains a shorter free block, and the second
	 part is the newly allocated block. In some cases, the remaining
	 free part of the block has to be moved to a different free list.
       *)
      let orig_fl_bsize = e.pool_fl_bsize in
      let new_fl_bsize = e.pool_fl_bsize - bsize in

      let orig_fl_psize = orig_fl_bsize / page_size in
      let orig_k = if orig_fl_psize < pool_fl_size then orig_fl_psize else 0 in

      let new_fl_psize = new_fl_bsize / page_size in
      let new_k = if new_fl_psize < pool_fl_size then new_fl_psize else 0 in

      if new_k <> orig_k then (
	(* Move the block to a different free list *)
	del_in_pool_fl_at mem hdr offs orig_fl_bsize prev;
	add_to_pool_fl mem hdr offs new_fl_bsize
      )
      else
	e.e.pool_fl_bsize <- new_fl_bsize;
      
      offs + new_fl_bsize
    ) in
  
  (* Now add the newly allocated block to the tree: *)
  Netmcore_util.AVL.add hdr.pool_alloc alloc_offs bsize;

  (* Return a memory bigarray: *)
  Bigarray.Array1.sub mem alloc_offs bsize
  

let really_free_mem mem hdr offs =
  match check_block mem hdr offs with
    | None ->
	failwith "Netmcore_mempool.free_mem: memory block not found"
    | Some(s_offs,bsize) ->
	if s_offs <> offs then
	  failwith "Netmcore_mempool.free_mem: memory block not found";
	
	(* remove this block from the tree: *)
	Netmcore_util.AVL.remove hdr.pool_alloc offs;

	(* add to the free list: *)
	merge_with_pool_fl mem hdr offs bsize


let really_size_mem mem hdr offs =
  match check_block mem hdr offs with
    | None ->
	failwith "Netmcore_mempool.size_mem: memory block not found"
    | Some(s_offs,bsize) ->
	if s_offs <> offs then
	  failwith "Netmcore_mempool.size_mem: memory block not found";
	bsize


(*
  FIXME

  Needs to be done after mapping:

  Netsys_mem.value_area mem;
 *)

(* Prob: init_pool is called by a process that normally does not have access
   to mem! Idea: delay initialization until first access.
 *)

let delayed_init_pool mem =
  let size = Bigarray.Array1.dim mem in
  let pg = to_page size in
  let size_pool_at = 64 + (pg / management_factor) in
  let dummy = Netmcore_util.AVL.create_node() in
  let pool_alloc = Netmcore_util.AVL.create_header() in
  pool_alloc.Netmcore_util.AVL.nodes <- Array.make size_pool_at dummy;
  let hdr_orig =
    { pool_size = size;
      pool_free = 0; (* later *)
      pool_free_contiguous = 0; (* later *)
      pool_fl = Array.make pool_fl_size (-1);  (* later *)
      pool_alloc = pool_alloc;
      pool_start = 0 (* later *)
    } in
  (* Now move everything to mem: *)
  let p0 = String.length magic in
  let p = ref (p0 + 8 + Netsys_posix.sem_size) in
  let (voffs, n) = Netsys_mem.init_value mem !p hdr_orig [] in
  let hoffs_s =
    Netnumber.HO.int8_as_string (Netnumber.int8_of_int (!p+voffs)) in
  Netsys_mem.blit_string_to_memory hoffs_s 0 mem p0 8;
  let hdr = (Netsys_mem.as_value mem (!p + voffs) : header) in
  p := !p + n;
  (* Now allocate the tree: *)
  for k = 1 to size_pool_at - 1 do
    let (voffs,n) = Netsys_mem.init_value mem !p dummy [] in
    let node = (Netsys_mem.as_value mem (!p + voffs)) in
    p := !p + n;
    hdr.pool_alloc.Netmcore_util.AVL.nodes.(k) <- node;
  done;
  Netmcore_util.AVL.init_header hdr.pool_alloc;
  (* At this point we know how much mem we need for the header *)
  let p_block = ((to_page (!p-1)) + 1) * page_size in
  let remaining = size - p_block in
  add_to_pool_fl mem hdr p_block remaining;
  (* Init remaining fields: *)
  hdr.pool_start <- p_block;
  hdr.pool_free <- remaining;
  hdr.pool_free_contiguous <- remaining;
  hdr


let with_pool mem f =
  let p0 = String.length magic in
  let u = String.create p0 in
  Netsys_mem.blit_memory_to_string mem 0 u 0 p0;
  if u <> magic then
    failwith "Netmcore_mempool: Uninitialized pool";
  let sem = Netsys_posix.as_sem mem (p0+8) in
  Netsys_posix.sem_wait sem Netsys_posix.SEM_WAIT_BLOCK;
  (* CHECK: signals *)
  try
    let hoffs = Netnumber.int_of_int8 (Netnumber.HO.read_int8 mem p0) in
    if hoffs = 0 then
      delayed_init_pool mem;
    let hoffs = Netnumber.int_of_int8 (Netnumber.HO.read_int8 mem p0) in
    assert(hoffs <> 0);
    let hdr = (Netsys_mem.as_value mem hoffs : header) in
    let r = f hdr in
    Netsys_posix.sem_post sem;
    r
  with
    | error ->
	Netsys_posix.sem_post sem;
	raise error
    

let init_pool mem =
  let p0 = String.length magic in
  Netsys_mem.blit_string_to_memory magic 0 mem 0 p0;
  let hoffs_s = Netnumber.HO.int8_as_string (Netnumber.int8_of_int 0) in
  Netsys_mem.blit_string_to_memory hoffs_s 0 mem p0 8;
  ignore(Netsys_posix.sem_init mem (p0+8) true 1)


let get_mem res_id =
  let res = Netmcore.get_resource res_id in
  match res#repr with
    | `Posix_shm_preallocated(_,mem) -> mem
    | _ -> failwith "Netmcore_mempool: this resource is not a pool"
  

let alloc_mem res_id bsize =
  (* round up to multiple of page_size: *)
  if bsize <= 0 then
    invalid_arg "Netmcore_mempool.alloc_mem: bad size";
  let bsize = ((bsize - 1) / page_size) * page_size + 1 in
  let mem = get_mem res_id in
  with_pool mem 
    (fun hdr -> really_alloc_mem mem hdr bsize)


let free_mem res_id m =
  let mem = get_mem res_id in
  let offs =
    Nativeint.to_int
      (Nativeint.sub
	 (Netsys_mem.memory_address m)
	 (Netsys_mem.memory_address mem)) in
 with_pool mem
   (fun hdr -> really_free_mem mem hdr offs)


let size_mem res_id m =
  let mem = get_mem res_id in
  let offs =
    Nativeint.to_int
      (Nativeint.sub
	 (Netsys_mem.memory_address m)
	 (Netsys_mem.memory_address mem)) in
 with_pool mem
   (fun hdr -> really_size_mem mem hdr offs)


let create_mempool size =
  (* round up to multiple of page_size: *)
  if size <= 0 then
    invalid_arg "Netmcore_mempool.create_mempool: bad size";
  let size = ((size - 1) / page_size) * page_size + 1 in
  let res_id, full_name = Netmcore.create_preallocated_shm "/mempool" size in
  (* Map the first page only *)
  let fd = Netsys_posix.shm_open full_name [Netsys_posix.SHM_O_RDWR] 0 in
  ( try
      let mem = Netsys_mem.memory_map_file fd true page_size in
      Unix.close fd
    with
      | error -> Unix.close fd; raise error
  );
  init_pool mem;
  res_id
