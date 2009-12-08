open Printf

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
  while !k < 40_000_000 do
    let _i = Rtypes.read_int4 s !k in
    k := !k +4
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for decoding: %f\n%!" (t1-.t0);

  let t0 = Unix.gettimeofday() in
  let b = 
    Bigarray.Array1.create Bigarray.int32 Bigarray.c_layout 10_000_000 in
  let k = ref 0 in
  while !k < 10_000_000 do
    let _i = Bigarray.Array1.unsafe_get b !k in
    incr k
  done;
  let t1 = Unix.gettimeofday() in
  printf "Time for reading bigarray: %f\n%!" (t1-.t0)
;;
