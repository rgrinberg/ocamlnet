open Rtypes;;
open Convtest2_aux;;


let id_b1 x  = _to_b1 (_of_b1 x);;
let id_b2 x  = _to_b2 (_of_b2 x);;
let id_b3 x  = _to_b3 (_of_b3 x);;
let id_b4 x  = _to_b4 (_of_b4 x);;
let id_b5 x  = _to_b5 (_of_b5 x);;
let id_b6 x  = _to_b6 (_of_b6 x);;
let id_b7 x  = _to_b7 (_of_b7 x);;
let id_b8 x  = _to_b8 (_of_b8 x);;
let id_f x   = _to_B'B'f'arg (_of_B'B'f'arg x);;

let xdrt = Xdr.validate_xdr_type xdrt_B'B'f'arg;;

let matched_id_f x =
  let y = _of_B'B'f'arg x in
  assert(Xdr.value_matches_type y xdrt []);
  _to_B'B'f'arg y
;;

let packed_id_f x =
  let y = _of_B'B'f'arg x in
  let z = Xdr.pack_xdr_value_as_string y xdrt [] in
  let y' = Xdr.unpack_xdr_value z xdrt [] in
  _to_B'B'f'arg y'
;;

let counter = ref 1;;

let check id x =
  let x' = id x in
  if x <> x' then
    print_endline ("Test " ^ string_of_int !counter ^ " failed");
  incr counter
;;


let test() =
  check id_b1  [| int4_of_int (-5); int4_of_int 0; int4_of_int 6 |];
  check id_b2  [| int4_of_int (-5); int4_of_int 0 |];
  check id_b2  [| |];
  check id_b3  [| int4_of_int (-5); int4_of_int 0; int4_of_int 6 |];
  check id_b3  [| |];
  check id_b4  [| "abc"; "xyz" |];
  check id_b5  None;
  check id_b5  (Some (int4_of_int 18));
  check id_b6  x6;
  check id_b6  y6;
  check id_b6  z6;
  check id_b7  [| x7; y7; z7 |];
  check id_b8  None;
  check id_b8  (Some x8);

  let x1 =
    ( [| int4_of_int (-5); int4_of_int 0; int4_of_int 6 |],
      [| int4_of_int (-5); int4_of_int 0 |],
      [| |],
      [| "abc"; "xyz" |],
      (Some (int4_of_int 18)),
      y6,
      [| x7; y7; z7 |],
      None
    )
  in
  check id_f x1;
  check matched_id_f x1;
  check packed_id_f x1;
  ()
;;



test();
print_endline "TEST PASSED"
;;
