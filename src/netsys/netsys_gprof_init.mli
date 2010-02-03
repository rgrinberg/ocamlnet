(* $Id$ *)

(** Initialize GPROF helper

    By linking this module the function {!Netsys.moncontrol} is made
    working. This should only be done if the program is built for
    profiling (ocamlopt -p).
 *)

val init : unit -> unit
  (** Dummy function. *)
