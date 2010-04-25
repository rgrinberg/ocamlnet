(* $Id$ *)

(** Managed Strings *)

(** Managed strings are used in XDR context for constant strings that
    are stored either as string or as memory (bigarray of char)
 *)

open Netsys_mem

class type mstring =
object
  (** Managed strings are used in XDR context for constant strings that
      are stored either as string or as memory (bigarray of char)
   *)

  method length : int
    (** The length of the managed string *)

  method blit_to_string :  int -> string -> int -> int -> unit
    (** [blit_to_string mpos s spos len]: Copies the substring of the
	managed string from [mpos] to [mpos+len-1] to the substring of
	[s] from [spos] to [spos+len-1]
     *)

  method blit_to_memory : int -> memory -> int -> int -> unit
    (** [blit_to_string mpos mem mempos len]: Copies the substring of the
	managed string from [mpos] to [mpos+len-1] to the substring of
	[mem] from [mempos] to [mempos+len-1]
     *)

  method as_string : string * int
    (** Returns the contents as string. It is undefined whether the returned
	string is a copy or the underlying buffer. The int is the position
	where the contents start
     *)

  method as_memory : memory * int
    (** Returns the contents as memory. It is undefined whether the returned
	memory is a copy or the underlying buffer. The int is the position
	where the contents start
     *)

  method preferred : [ `Memory | `String ]
    (** Whether [as_memory] or [as_string] is cheaper *)

end


class type mstring_factory =
object
  method create_from_string : string -> int -> int -> bool -> mstring
    (** [create_from_string s pos len must_copy]: Creates the [mstring] from the
	sub string of s starting at [pos] with length [len]

	If [must_copy] the mstring object must create a copy. Otherwise
	it can just keep the string passed in.
     *)

  method create_from_memory : memory -> int -> int -> bool -> mstring
    (** [create_from_memory m pos len must_copy]: Creates the [mstring] from the
	sub string of m starting at [pos] with length [len]

	If [must_copy] the mstring object must create a copy. Otherwise
	it can just keep the memory passed in.
     *)

end

val string_based_mstrings : mstring_factory
  (** Uses strings to represent mstrings *)

val memory_based_mstrings : mstring_factory
  (** Uses memory to represent mstrings. The memory bigarrays are allocated
      with [Bigarray.Array1.create]
   *)

val paligned_memory_based_mstrings : mstring_factory
  (** Uses memory to represent mstrings. The memory bigarrays are allocated
      with {!Netsys_mem.alloc_memory_pages} if available, and 
      [Bigarray.Array1.create] if not.
   *)

val memory_pool_based_mstrings : Netsys_mem.memory_pool -> mstring_factory
  (** Uses memory to represent mstrings. The memory bigarrays are obtained
      from the pool. The length of these mstrings is limited by the 
      blocksize of the pool.
   *)

val length_mstrings : mstring list -> int
  (** returns the sum of the lengths of the mstrings *)

val concat_mstrings : mstring list -> string
  (** concatenates the mstrings and return them as single string. The returned
      string may be shared with one of the mstrings passed in.
   *)

val prefix_mstrings : mstring list -> int -> string
  (** [prefix_mstrings l n]: returns the first [n] chars of the 
      concatenated mstrings [l] as single string
   *)

val blit_mstrings_to_memory : mstring list -> memory -> unit
  (** blits the mstrings one after the other to the memory, so that
      they appear there concatenated
   *)


type named_mstring_factories =
    (string, mstring_factory) Hashtbl.t
