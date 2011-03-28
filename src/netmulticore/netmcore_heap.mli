(* $Id$ *)

(** Shared heaps of structured values

    These heaps live in {!Netmcore_mempool}-type shared memory pools,
    and can contain an arbitrary number of Ocaml values. These values
    can be mutable, but special care has to be taken when modifying them.
    The first value pushed onto the heap is called the {i root element}.
    All other values must be (directly or indirectly) reachable from the
    root element.

    Heaps are created with a certain initial size. The heaps remain
    connected with the memory pool, and they are enlarged if necessary
    by allocating more blocks in the pool.

    As the heaps are shared between processes, it must be taken care
    that no reference is made from shared heaps to normal process-local
    memory. These references would only be valid in the process creating
    them, and following such references from other processes would probably
    crash the program (or at least return wrong values). In order to ensure
    this, it is strictly forbidden to directly manipulate mutable
    data structures. The {!Netmcore_heap.modify} function has to be used,
    as this function makes it possible to copy more values to the heap.
    Unfortunately, there is nothing in the type system that would prevent
    direct mutation. so this can only be ensured by the discipline of the
    programmer.

    The values of the heap are also garbage-collected: If all allocated
    space is used and more values need to be added, it is first tried
    to get rid of old unreachable values. The garbarge collection is done
    by the process that happens to initiate the addition of the value
    that does no more fit onto the heap. During garbage collection, no
    other additions can be done, but read accesses are not prevented.
    The garbage collector does not move values (addresses remain unchanged).

    The garabage collector only considers values as reachable that are
    reachable via the root element. It is not sufficient when a value
    is only reachable via a process-specific reference.
 *)

type 'a heap
  (** A heap where the type of the root element is ['a] *)

val create_heap : res_id -> int -> 'a -> 'a heap
  (** [create_heap pool_id size root]: Creates a new heap with [size]
      bytes in the pool identified by [pool_id]. This ID must refer
      to a {!Netmcore_mempool}-managed pool.

      The value [root] is copied to the new heap. This is done by
      deeply duplicating [root] and all values pointed to by [root],
      and storing these duplicates in the heap.
   *)

val minimum_size : 'a -> int
  (** Returns the [size] value one must pass to [create_heap] at minimum
      to put this root element onto the heap.
   *)

val root : 'a heap -> 'a
  (** Returns the root element *)

val modify : 'a heap -> (('b -> 'b) -> unit) -> unit
  (** [modify h mutate]: This function locks the heap so that this process
      has exclusive write access to it for the duration of the [mutate]
      function. The [mutate] function is immediately called back, and
      the argument of [mutate] is a function [add] to deeply copy further values
      to the heap:

      {[ mutate add ]}

      By calling [add x] from the body of [mutate] one can create a copy
      of [x] that is stored in the heap. 
   *)

val copy : 'a -> 'a
  (** Creates a deep copy of the input value, and stores the duplicate
      in normal process memory.
   *)

val with_value : 'a heap -> (unit -> 'b) -> ('b -> 'c) -> 'c
  (** [with_value h find process]: Logically, this runs
      [process (find ())] and returns the result. While [find] is being
      executed, the heap is write-locked. This returns a value [x].
      While [process] is being executed, the value [x] is temporarily
      added to the set of reachable values, so that a parallely running
      garbage collection will not delete it.
   *)


(** {2 Example: Mutable Variable}

    This example creates a heap that stores a single value. (This is
    available as {!Netmcore_ref}.)

    {[
    let shared_ref x =
      (* The shm version of [ref x] *)
      let r = ref x in
      let init_size = minimum_size r in
      let hp = create_heap pool_id init_size r in
      hp

    let deref sref =
      (* The shm version of [!] *)
      !(root sref)

    let assign sref x =
      (* The shm version of [:=] - however, a copy of x is done *)
      modify sref
        (fun add ->
          (root sref) := add x
        )
    ]}

 *)
