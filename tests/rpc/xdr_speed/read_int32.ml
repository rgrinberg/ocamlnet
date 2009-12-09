open Printf

(* Note: 64 bit only! *)

let read_int4_unsafe_1 s pos =
  let n3 = Int32.of_int (Char.code (String.unsafe_get s pos)) in
  let x = Int32.shift_left n3 24 in

  let n2 = Int32.of_int (Char.code (String.unsafe_get s (pos+1))) in
  let x = Int32.logor x (Int32.shift_left n2 16) in

  let n1 = Int32.of_int (Char.code (String.unsafe_get s (pos+2))) in
  let x = Int32.logor x (Int32.shift_left n1 8) in

  let n0 = Int32.of_int (Char.code (String.unsafe_get s (pos+3))) in
  Int32.logor x n0

let read_int4_unsafe_2 s pos =
  let n3 = Nativeint.of_int (Char.code (String.unsafe_get s pos)) in
  let x = Nativeint.shift_right (Nativeint.shift_left n3 56) 32 in

  let n2 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+1))) in
  let x = Nativeint.logor x (Nativeint.shift_left n2 16) in

  let n1 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+2))) in
  let x = Nativeint.logor x (Nativeint.shift_left n1 8) in

  let n0 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+3))) in
  Nativeint.logor x n0


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
    let i = read_int4_unsafe_2 s !k in
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
  printf "Time for reading bigarray: %f\n%!" (t1-.t0)
;;
