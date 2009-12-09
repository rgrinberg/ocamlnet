(* Speed of allocation *)

(* Opteron 1354 with 8 GB RAM, 64 bit mode:

   Time for empty loop: 0.009132
   Time for allocate+free: 0.046222
   Time for allocate+oldify: 3.345699

   So roughly:
   - 3.5 ns for the allocation of a temporary nativeint (w/o loop costs)
   - 330 ns for the allocation of a nativeint in the major heap

   If the size of the minor heap is extended, allocate+free becomes
   slower (!), and allocate+oldify becomes faster.
 *)

open Printf

type r =
    { mutable contents : nativeint }

let () =
  (* Test 0: empty loop *)
  let t0 = Unix.gettimeofday() in
  for k = 1 to 10_000_000 do
    ()
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for empty loop: %f\n%!" (t1-.t0);

  (* Test 1: allocate and free by minor GC: *)
  let r = { contents = 0n } in
  let t0 = Unix.gettimeofday() in
  for k = 1 to 10_000_000 do
    r.contents <- Nativeint.succ 41n
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for allocate+free: %f\n%!" (t1-.t0);

  (* Test 2: allocate and oldify *)
  let a = Array.make 10_000_000 0n in
  let t0 = Unix.gettimeofday() in
  for k = 0 to 9_999_999 do
    let x = Nativeint.succ 41n in
    Array.unsafe_set a k x 
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for allocate+oldify: %f\n%!" (t1-.t0)

