open Rtypes;;
open Convtest3_aux;;


let id_b1 x  = _to_b1 (_of_b1 x);;
let id_b2 x  = _to_b2 (_of_b2 x);;
let id_b3 x  = _to_b3 (_of_b3 x);;
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
  let y' = Xdr.unpack_xdr_value ~fast:true z xdrt [] in
  _to_B'B'f'arg y'
;;

let failed = ref false ;;
let counter = ref 1;;

let check id x =
  print_endline("Running test " ^ string_of_int !counter);
  let x' = id x in
  if x <> x' then (
    failed := true;
    print_endline ("Test " ^ string_of_int !counter ^ " failed");
  );
  incr counter
;;


let test() =
  check id_b1 { x1 = int4_of_int 1;
		y1 = "abc";
		z1 = z1a;
	      };
  check id_b1 { x1 = int4_of_int (-1);
		y1 = "";
		z1 = z1c;
	      };
  check id_b2 [| |];
  check id_b2 [| { x2 = int4_of_int (-5) } |];
  check id_b3 None;
  check id_b3 (Some { x3 = "a"; y3 = None });
  check id_b3 (Some { x3 = "a"; y3 = Some { x3 = "b"; y3 = None }
		    }
	      );

  check id_b3 (Some { x3 = "a"; y3 = Some { x3 = "b"; y3 = Some { x3 = "c";
								  y3 = None;
								}
					  }
		    }
	      );

  let x1 =
    ( { x1 = int4_of_int 1;
	y1 = "abc";
	z1 = z1a;
      },
      [| { x2 = int4_of_int (-5) } |],
      (Some { x3 = "a"; y3 = Some { x3 = "b"; y3 = None }
	    }
      )
    )
  in
  check id_f x1;
  check matched_id_f x1;
  check packed_id_f x1;

  ()
;;


test();
print_endline (if !failed then "TEST FAILED" else "TEST PASSED");
exit (if !failed then 1 else 0)
;;

