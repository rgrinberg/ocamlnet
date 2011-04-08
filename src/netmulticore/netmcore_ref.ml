(* $Id$ *)

open Netmcore_heap

type 't sref = 't ref heap

let sref res_id x =
  create_heap res_id (minimum_size x) (ref x)

let assign r x =
  modify r (fun mut -> (root r) := add mut x)

let deref r =
  !(root r)

let deref_p r f =
  with_value r (fun () -> !(root r)) f

let deref_c r =
  deref_p r copy

let heap r =
  Obj.magic r
