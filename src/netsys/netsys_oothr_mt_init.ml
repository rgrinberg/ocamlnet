(* $Id$ *)

(** Initializer for mt programs *)

let () =
  Netsys_oothr.provider := Netsys_oothr_mt.mtprovider()
