(* $Id$ *)

open Netmcore_heap

type semaphore = string
    (* An empty string is a dummy sem.  Otherwise, the string is on
       the heap, and it contains the semaphore value *)

let dummy() = ""

let create mut initval =
  let s = String.create Netsys_posix.sem_size in
  let s_heap = add mut s in
  let s_addr = Netsys_mem.obj_address (Obj.repr s_heap) in
  let m = Netsys_mem.grab s_addr Netsys_posix.sem_size in
  ignore(Netsys_posix.sem_init m 0 true initval);
  s_heap

let get_sem s_heap =
  if s_heap = "" then
    failwith "Netmcore_sem: cannot access dummy semaphore";
  let s_addr = Netsys_mem.obj_address (Obj.repr s_heap) in
  let m = Netsys_mem.grab s_addr Netsys_posix.sem_size in
  Netsys_posix.as_sem m 0

let destroy s_heap =
  if s_heap <> "" then (
    let sem = get_sem s_heap in
    Netsys_posix.sem_destroy sem
  )

let getvalue s_heap =
  let sem = get_sem s_heap in
  Netsys_posix.sem_getvalue sem

let post s_heap =
  let sem = get_sem s_heap in
  Netsys_posix.sem_post sem

let wait s_heap swb =
  let sem = get_sem s_heap in
  Netsys.restart (Netsys_posix.sem_wait sem) swb

