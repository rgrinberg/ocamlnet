(* $Id$ *)

(** Shared arrays *)

type 't sarray

val create : res_id -> 't array -> 't sarray
  (** [create pool_id a]:
      Creates a shared array by deeply copying a normal array *)

val make : res_id -> int -> 't -> 't sarray
  (** [make pool_id n x]:
      Creates a shared array of the passed number of elements, 
      copies the element [x], and initializes each element of the new array
      with the single copy of [x].
   *)

val deref : 't sarray -> 't array
  (** Returns the raw array in shared memory for unprotected access *)

val get : 't sarray -> int -> 't
  (** [get sa k]: Gets the [k]-th element of the shared array [sa].
      Note that there is no guarantee that this value still exists if
      it is returned, and a parallely running [set] changes this element.
   *)

val with_element : 't sarray -> int -> ('t -> 'a) -> 'a
  (** [with_element sa k f]: Runs [f x] when [x] is the k-th element
      of the shared array [sa], and returns the result.
      While [f] is being executed, the current element contents
      are specially protected so that they cannot be garbage collected,
      even if a parallel [set] changes the current value of the 
      element.
   *)

val set : 't sarray -> int -> 't -> unit
  (** [set sa k x]: Sets the [k-th] element of the array [sa] to a
      deep copy of [x].
   *)

