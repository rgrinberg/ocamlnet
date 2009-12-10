(* Speed of decoding XDR ints *)

(* Opteron 1354 with 8 GB RAM, 64 bit mode:

   Time for decoding: 0.131501  (around 13ns per loop cycle)
   Time for reading bigarray: 0.069432 (around 7ns per loop cycle)

   Here, "decode" is only the string access, the conversion to nativeint,
   and the addition to sum. There is no allocation of a temporary heap
   block.

   For reading bigarrays, the code generator emits code for allocating
   a heap block for the nativeint read from the bigarray. From the
   alloc_nativeint tests we know that this allocation is quite cheap
   (3.5 ns), around 50% of the time spent in a loop cycle. In the
   bigarray test there is no byte swapping (XDR uses big endian).

   So we get:
   - 13ns for decoding 32 bit ints from a string
   - 3.5ns for reading 32 bit ints from a bigarray

   The alloc_nativeint test also says that it is much more expensive
   when we let the decoded nativeint move to the major heap.
   This is around 330ns for a loop cycle. This plays a role for any
   bigger/longer-living XDR message, and dominates the costs. The
   13ns for the non-optimal decode operation (compared to 1.6ns in C)
   can be neglected.

 *)

open Printf

(* Note: 64 bit only! *)

let read_int4_unsafe s pos = (* this function is inlined *)
  let n3 = Nativeint.of_int (Char.code (String.unsafe_get s pos)) in
  let x = Nativeint.shift_right (Nativeint.shift_left n3 56) 32 in

  let n2 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+1))) in
  let x = Nativeint.logor x (Nativeint.shift_left n2 16) in

  let n1 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+2))) in
  let x = Nativeint.logor x (Nativeint.shift_left n1 8) in

  let n0 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+3))) in
  Nativeint.logor x n0


external decode_nativeint : string -> int -> nativeint
  = "decode_nativeint" 

let () =
  let s = String.create 40_000_000 in
  for k = 0 to 9_999_999 do
    let p = k * 4 in
    s.[p] <- '\x10';
    s.[p+1] <- '\x11';
    s.[p+2] <- '\x12';
    s.[p+3] <- '\x13';
  done;

  let t0 = Unix.gettimeofday() in
  let k = ref 0 in
  let sum = ref 0n in
  while !k < 40_000_000 do
    let i = read_int4_unsafe s !k in
    sum := Nativeint.add !sum i;
    k := !k +4
  done;
  let t1 = Unix.gettimeofday() in
  printf "Sum: %ld\n" (Nativeint.to_int32 !sum);
  printf "Time for decoding: %f\n%!" (t1-.t0);

  let b = 
    Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 10_000_000 in
  for p = 0 to 9_999_999 do
    b.{p} <- 0x10111213l;
  done;

  let t0 = Unix.gettimeofday() in
  let k = ref 0 in
  let sum = ref 0l in
  while !k < 10_000_000 do
    let i = Bigarray.Array1.unsafe_get b !k in
    sum := Int32.add !sum i;
    incr k
  done;
  let t1 = Unix.gettimeofday() in
  printf "Sum: %ld\n" !sum;
  printf "Time for reading bigarray: %f\n%!" (t1-.t0);

  let t0 = Unix.gettimeofday() in
  let k = ref 0 in
  let sum = ref 0n in
  while !k < 40_000_000 do
    let i = decode_nativeint s !k in
    sum := Nativeint.add !sum i;
    k := !k +4
  done;
  let t1 = Unix.gettimeofday() in
  printf "Sum: %ld\n" (Nativeint.to_int32 !sum);
  printf "Time for decoding w/ C helper: %f\n%!" (t1-.t0);

;;
