(* $Id$ *)

(** Some helpers for en/decoding XDR faster *)

external s_read_int4_64_unsafe : string -> int -> int
  = "netsys_s_read_int4_64" "noalloc"
  (** For 64 bit platforms only: Decodes 4 bytes at this string position
      as a signed 32 bit int in network byte order

      There is no index bounds check!
   *)

external s_write_int4_64_unsafe : string -> int -> int -> unit
  = "netsys_s_write_int4_64" "noalloc"
  (** For 64 bit platforms only: Encodes 4 bytes at this string position
      as a signed 32 bit int in network byte order

      There is no index bounds check!
   *)
