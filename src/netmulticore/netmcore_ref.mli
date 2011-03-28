(* $Id$ *)

(** Shared mutable variables *)

type 't sref

val sref : res_id -> 't -> 't sref
  (** The shared version of [ref]: Creates a mutable shared variable in
      the give memory pool
   *)

val deref : 't sref -> 't
  (** Dereferences the variable and returns the contents, comparable to
      [!]. Note that this returns a value that lives in shared memory,
      and there is no guarantee that this value still exists if 
      [assign] operations are done in parallel.
   *)

val with_contents : 't sref -> ('t -> 'a) -> 'a
  (** [with_contents sr f]: Runs [f] with the contents of [sr], and returns
      the result of [f]. While [f] is being executed, the current contents
      are specially protected so that they cannot be garbage collected,
      even if a parallel [assign] changes the current value of the 
      variable.
   *)

val assign : 't sref -> 't -> unit
  (** [assign sr x]: Sets the contents of [sr] to a deep copy of [x].
      While performing the assignment the heap is write-locked,
      and no other [assign] can run.
   *)
