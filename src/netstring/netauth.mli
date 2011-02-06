(* $Id$ *)

(** Some primitives for authentication *)

val hmac : h:(string->string) ->
           b:int ->
           l:int ->
           k:string ->
           message:string ->
             string
  (** The HMAC algorithm of RFC 2104. The function [h] is the hash function.
      [b] and [l] are properties of [h] (see the RFC or below). The string
      [k] is the key, up to [b] bytes. The [message] is authenticated.

      The key [k] should ideally have length [l]. If this cannot be ensured
      by other means, one should pass [k = h any_k].

      Common values of [b] and [l]:
      - For [h=MD5]: [b=64], [l=16]
      - For [h=SHA-1]: [b=64], [l=20]
   *)


val xor_s : string -> string -> string
  (** Performs the bitwise XOR of these strings (which must have the same
      length)
   *)
