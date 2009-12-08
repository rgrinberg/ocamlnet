open Printf
let () =
  let a = 81l in
  let b = 80l in
  let t0 = Unix.gettimeofday() in
  for k = 1 to 1000_0000 do
    (* cmp a b unsigned: *)
    let _p = (* a < b *)
(*
      let a1 = Int32.shift_right_logical a 1 in
      let b1 = Int32.shift_right_logical b 1 in
      a1 < b1 || (a1 = b1 && (Int32.logand a 1l < Int32.logand b 1l))
 *)
(*
      (a >= 0l && (a < b || b < 0l)) || (a < 0l && b < a)
 *)
(*
      if a < b then a >= 0l else (* b <= a *) b < a && b < 0l
 *)
      a < b
    in
    ()      
  done;
  let t1 = Unix.gettimeofday() in
  printf "t=%f\n%!" (t1-.t0)

