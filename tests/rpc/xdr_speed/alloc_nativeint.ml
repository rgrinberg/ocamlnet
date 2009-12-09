(* Speed of allocation *)

open Printf

type r =
    { mutable contents : nativeint }

let () =
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

