
open Netconversion;;

(* Measure the speed of certain character set conversions.
 *
 * Test Data Strings:
 *
 * tds_ascii: consists of 10000 ASCII characters
 * tds_latin1: consists of 10000 characters:
 *    80% ASCII characters
 *    20% characters from 160 to 255
 * tds_unicode: consists of 10000 characters:
 *    60% ASCII characters
 *    30% characters from 160 to 255
 *    10% characters from 256 to 0xfff
 *    10% characters from 0x1000 to 0xffff
 *)

let repeat n s =
  (* Repeat the string s n times *)
  let l = String.length s in
  let u = String.create (n * l) in
  for k = 0 to n - 1 do
    String.blit s 0 u (k*l) l
  done;
  u
;;


let tds_ascii_rest =
  repeat 10000 "A" ;;

let tds_ascii_utf16be =
  repeat 10000 (makechar `Enc_utf16_be 65) ;;

let tds_ascii =
  [ `Enc_iso88591, tds_ascii_rest;
    `Enc_iso88592, tds_ascii_rest;
    `Enc_utf8, tds_ascii_rest;
    `Enc_utf16_be, tds_ascii_utf16be
  ] ;;


let tds_latin1_rest =
  repeat 100
    (repeat 80 "A" ^ repeat 20 "\160") ;;

let tds_latin1_utf8 =
  repeat 100
    (repeat 80 "A" ^ repeat 20 (makechar `Enc_utf8 160)) ;;

let tds_latin1_utf16be =
  repeat 100
    (repeat 80 (makechar `Enc_utf16_be 65) ^ 
     repeat 20  (makechar `Enc_utf16_be 160)) ;;

let tds_latin1 =
  [ `Enc_iso88591, tds_latin1_rest;
    `Enc_iso88592, tds_latin1_rest;
    `Enc_utf8, tds_latin1_utf8;
    `Enc_utf16_be, tds_latin1_utf16be
  ] ;;

let tds_unicode_utf8 =
  repeat 100
    (repeat 60 "A" ^
     repeat 30 (makechar `Enc_utf8 160) ^
     repeat 10 (makechar `Enc_utf8 1600) ^
     repeat 10 (makechar `Enc_utf8 20000)) ;;


let tds_unicode_utf16 =
  repeat 100
    (repeat 60 (makechar `Enc_utf16_be 65) ^
     repeat 30 (makechar `Enc_utf16_be 160) ^
     repeat 10 (makechar `Enc_utf16_be 1600) ^
     repeat 10 (makechar `Enc_utf16_be 20000)) ;;

let tds_unicode =
  [ `Enc_iso88591, "";
    `Enc_iso88592, "";
    `Enc_utf8, tds_unicode_utf8;
    `Enc_utf16_be, tds_unicode_utf16
  ] ;;

let matrix tds_name tds =
  let subst = fun _ -> "" in
  List.iter
    (fun (in_enc, in_tds) ->
       if in_tds <> "" then begin
	 List.iter
	   (fun (out_enc, _) ->
	      (* Get the median of 3 trials *)
	      let t = Array.make 3 0.0 in
	      for k = 0 to 2 do
		Gc.full_major();
		let t0 = Unix.gettimeofday() in
		for i = 1 to 100 do
		  ignore(recode_string ~in_enc ~out_enc ~subst in_tds)
		done;
		let t1 = Unix.gettimeofday() in
		t.(k) <- t1 -. t0;
	      done;
	      Array.sort compare t;
	      Printf.printf "%-11s %-10s %-10s %1.3f\n"
		tds_name
		(string_of_encoding in_enc)
		(string_of_encoding out_enc)
		t.(1);
	      flush stdout
	   )
	   tds
       end
    )
    tds
;;

matrix "tds_ascii" tds_ascii;;
matrix "tds_latin1" tds_latin1;;
matrix "tds_unicode" tds_unicode;;
