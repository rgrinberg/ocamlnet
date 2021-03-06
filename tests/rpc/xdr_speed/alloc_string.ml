(* Speed of string allocation *)

(* Opteron 1354 with 8 GB RAM, 64 bit mode:

   Time for empty loop: 0.013679       (~1ns per loop cycle)
   Time for allocate+free: 0.395558    (40 ns per loop cycle)
   Time for allocate+oldify: 4.013421  (401ns per loop cycle)
   Time for slicing: 3.866392          (387ns per loop cycle)

   Really strange that the last test is faster than allocate+oldify!
 *)

open Printf

type r =
    { mutable contents : string }

let s = "01234567890123456789012345678901"  (* 32 bytes *)

let () =
  (* Test 0: empty loop *)
  let t0 = Unix.gettimeofday() in
  for k = 1 to 10_000_000 do
    ()
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for empty loop: %f\n%!" (t1-.t0);

  (* Test 1: allocate and free by minor GC: *)
  Gc.full_major();
  let r = { contents = "" } in
  let t0 = Unix.gettimeofday() in
  for k = 1 to 10_000_000 do
    r.contents <- String.copy s
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for allocate+free: %f\n%!" (t1-.t0);

  (* Test 2: allocate and oldify *)
  Gc.full_major();
  let a = Array.make 10_000_000 "" in
  let t0 = Unix.gettimeofday() in
  for k = 0 to 9_999_999 do
    (* let x = String.copy s in // the following is a bit faster, probably
       because it does not need to determine the string length first 
     *)
    let x = String.create 32 in
    String.unsafe_blit s 0 x 0 32;
    Array.unsafe_set a k x 
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for allocate+oldify: %f\n%!" (t1-.t0);

  (* Test 3: slicing *)
  Gc.full_major();
  let s0 = "\000\000\000\032" ^ s in
  let u = String.create ((32 + 4) * 10_000_000) in
  for k = 0 to 9_999_999 do
    String.blit s0 0 u (k*36) 36
  done;
  let a = Array.make 10_000_000 "" in
  let t0 = Unix.gettimeofday() in
  let j = ref 0 in
  for k = 0 to 9_999_999 do
    let x = String.create 32 in
    String.unsafe_blit u (!j+4) x 0 32;
    Array.unsafe_set a k x;
    j := !j + 36
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for slicing: %f\n%!" (t1-.t0);


