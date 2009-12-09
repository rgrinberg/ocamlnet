(* Speed of string allocation *)

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
  let r = { contents = "" } in
  let t0 = Unix.gettimeofday() in
  for k = 1 to 10_000_000 do
    r.contents <- String.copy s
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for allocate+free: %f\n%!" (t1-.t0);

  (* Test 2: allocate and oldify *)
  let a = Array.make 10_000_000 "" in
  let t0 = Unix.gettimeofday() in
  for k = 0 to 9_999_999 do
    let x = String.copy s in
    Array.unsafe_set a k x 
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for allocate+oldify: %f\n%!" (t1-.t0)

