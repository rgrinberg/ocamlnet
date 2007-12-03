(* $Id$ *)

open Netsys_pollset

val best_pollset : unit -> pollset
  (** Returns the best available pollset on this platform *)
