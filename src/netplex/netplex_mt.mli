(* $Id$ *)

(** Multi-threading provider *)

class mt : unit -> Netplex_types.parallelizer
  (** Uses [Thread.create] to create new threads *)

val mt : unit -> Netplex_types.parallelizer
  (** Uses [Thread.create] to create new threads *)
