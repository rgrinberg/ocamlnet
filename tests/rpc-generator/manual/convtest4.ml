open Rtypes;;
open Convtest4_aux;;


let id_b1 x  = _to_b1 (_of_b1 x);;
let id_b2 x  = _to_b2 (_of_b2 x);;
let id_b3 x  = _to_b3 (_of_b3 x);;
let id_b4 x  = _to_b4 (_of_b4 x);;
let id_b5 x  = _to_b5 (_of_b5 x);;
let id_b6 x  = _to_b6 (_of_b6 x);;
let id_b7 x  = _to_b7 (_of_b7 x);;
let id_b8 x  = _to_b8 (_of_b8 x);;
let id_b9 x  = _to_b9 (_of_b9 x);;
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

let fpcheck id x =
  let x' = id x in
  if (x -. x') /. x > 1E-5 then
    print_endline ("Test " ^ string_of_int !counter ^ " failed");
  incr counter
;;


let test() =
  check id_b1 (`__1 "abc");
  check id_b1 (`_0);
  check id_b1 (`_1 [| int4_of_int 5 |]);
  check id_b2 (`__1 "abc");
  check id_b2 (`_0);
  check id_b2 (`_1 [| int4_of_int 5 |]);
  check id_b2 (`default(int4_of_int 5, "opaque"));
  check id_b3 (`__1 "abc");
  check id_b3 (`_0);
  check id_b3 (`_1 [| int4_of_int 5 |]);
  check id_b3 (`default(int4_of_int 5));
  check id_b4 (`_2 "abc");
  check id_b4 (`_0);
  check id_b4 (`_1 [| int4_of_int 5 |]);
  check id_b5 (`_2 "abc");
  check id_b5 (`_0);
  check id_b5 (`_1 [| int4_of_int 5 |]);
  check id_b5 (`default(uint4_of_int 5, "opaque"));
  check id_b6 (`_2 "abc");
  check id_b6 (`_0);
  check id_b6 (`_1 [| int4_of_int 5 |]);
  check id_b6 (`default(uint4_of_int 5));
  check id_b7 (`a "abc");
  check id_b7 (`b);
  check id_b8 (`a "abc");
  check id_b8 (`b);
  check id_b8 (`c "opaque");
  check id_b9 (`a "abc");
  check id_b9 (`b);
  check id_b9 (`c);
  
  let x1 =
    ( (`__1 "abc"),
      `_0,
      (`_1 [| int4_of_int 5 |]),
      (`_2 "abc"),
      (`default(uint4_of_int 5, "opaque")),
      (`default(uint4_of_int 5)),
      `b,
      (`c "opaque"),
      `c
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
