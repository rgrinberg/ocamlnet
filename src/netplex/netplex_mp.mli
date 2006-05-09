(* $Id$ *)

(** Multi-processing provider *)

class mp : unit -> Netplex_types.parallelizer
  (** Uses [Unix.fork] to create new threads *)
