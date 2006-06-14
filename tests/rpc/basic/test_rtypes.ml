#directory "../../src/rpc";;
#load "rtypes.cmo";;

open Rtypes;;

let test name f =
  print_string ("Test " ^ name ^ ": ");
  flush stdout;
  begin try
    if f() then
      print_endline "OK"
    else
      print_endline "FAILED"
  with
      ex ->
	print_endline ("FAILED WITH " ^ Printexc.to_string ex)
  end;
  flush stdout
;;


let overflow f x =
  try ignore(f x); false
  with
      Cannot_represent _ -> true
;;


let test_sth_of_int4 () =
  let n1 = mk_int4 ('\000', '\000', '\000', '\001') in
  let n2 = mk_int4 ('\000', '\000', '\001', '\000') in
  let n3 = mk_int4 ('\000', '\001', '\000', '\000') in
  let n4 = mk_int4 ('\001', '\000', '\000', '\000') in
  let n5 = mk_int4 ('\032', '\000', '\000', '\000') in
  let n6 = mk_int4 ('\064', '\000', '\000', '\000') in
  let n7 = mk_int4 ('\128', '\000', '\000', '\000') in
  let n8 = mk_int4 ('\255', '\255', '\255', '\255') in

  (int_of_int4 n1 = 1) &&
  (int_of_int4 n2 = 256) &&
  (int_of_int4 n3 = 65536) &&
  (int_of_int4 n4 = 0x01000000) &&
  (int_of_int4 n5 = 0x20000000) &&
  (overflow int_of_int4 n6) && 
  (overflow int_of_int4 n7) &&
  (int_of_int4 n8 = -1) &&

  (int32_of_int4 n1 = Int32.of_int 1) &&
  (int32_of_int4 n2 = Int32.of_int 256) &&
  (int32_of_int4 n3 = Int32.of_int 65536) &&
  (int32_of_int4 n4 = Int32.of_int 0x01000000) &&
  (int32_of_int4 n5 = Int32.of_int 0x20000000) &&
  (int32_of_int4 n6 = Int32.shift_left (Int32.of_int 0x20000000) 1) && 
  (int32_of_int4 n7 = Int32.shift_left (Int32.of_int 0x20000000) 2) &&
  (int32_of_int4 n8 = Int32.of_int (-1)) &&

  (int64_of_int4 n1 = Int64.of_int 1) &&
  (int64_of_int4 n2 = Int64.of_int 256) &&
  (int64_of_int4 n3 = Int64.of_int 65536) &&
  (int64_of_int4 n4 = Int64.of_int 0x01000000) &&
  (int64_of_int4 n5 = Int64.of_int 0x20000000) &&
  (int64_of_int4 n6 = Int64.shift_left (Int64.of_int 0x20000000) 1) && 
  (int64_of_int4 n7 = Int64.pred (Int64.neg (Int64.of_int32 (Int32.max_int))))&&
  (int64_of_int4 n8 = Int64.of_int (-1))
;;


let test_int4_of_sth() =
  let n1 = mk_int4 ('\000','\000','\000','\001') in
  let n2 = mk_int4 ('\032','\000','\000','\000') in
  let n3 = mk_int4 ('\064','\000','\000','\000') in
  let n4 = mk_int4 ('\255','\255','\255','\255') in

  (int4_of_int 1 = n1) &&
  (int4_of_int 0x20000000 = n2) &&
  (int4_of_int (-1) = n4) &&

  (int4_of_int32 (Int32.of_string "1") = n1) &&
  (int4_of_int32 (Int32.of_string "0x20000000") = n2) &&
  (int4_of_int32 (Int32.of_string "0x40000000") = n3) &&
  (int4_of_int32 (Int32.of_string "-1") = n4) &&

  (int4_of_int64 (Int64.of_string "1") = n1) &&
  (int4_of_int64 (Int64.of_string "0x20000000") = n2) &&
  (int4_of_int64 (Int64.of_string "0x40000000") = n3) &&
  (overflow int4_of_int64 (Int64.of_string "0x80000000")) &&
  (int4_of_int64 (Int64.of_string "-1") = n4)
;;

  
let test_sth_of_uint4 () =
  let n1 = mk_uint4 ('\000', '\000', '\000', '\001') in
  let n2 = mk_uint4 ('\000', '\000', '\001', '\000') in
  let n3 = mk_uint4 ('\000', '\001', '\000', '\000') in
  let n4 = mk_uint4 ('\001', '\000', '\000', '\000') in
  let n5 = mk_uint4 ('\032', '\000', '\000', '\000') in
  let n6 = mk_uint4 ('\064', '\000', '\000', '\000') in
  let n7 = mk_uint4 ('\128', '\000', '\000', '\000') in
  let n8 = mk_uint4 ('\255', '\255', '\255', '\255') in

  (int_of_uint4 n1 = 1) &&
  (int_of_uint4 n2 = 256) &&
  (int_of_uint4 n3 = 65536) &&
  (int_of_uint4 n4 = 0x01000000) &&
  (int_of_uint4 n5 = 0x20000000) &&
  (overflow int_of_uint4 n6) && 
  (overflow int_of_uint4 n7) &&
  (overflow int_of_uint4 n8) &&

  (int32_of_uint4 n1 = Int32.of_int 1) &&
  (int32_of_uint4 n2 = Int32.of_int 256) &&
  (int32_of_uint4 n3 = Int32.of_int 65536) &&
  (int32_of_uint4 n4 = Int32.of_int 0x01000000) &&
  (int32_of_uint4 n5 = Int32.of_int 0x20000000) &&
  (int32_of_uint4 n6 = Int32.shift_left (Int32.of_int 0x20000000) 1) && 
  (overflow int32_of_uint4 n7) &&
  (overflow int32_of_uint4 n8) &&

  (int64_of_uint4 n1 = Int64.of_int 1) &&
  (int64_of_uint4 n2 = Int64.of_int 256) &&
  (int64_of_uint4 n3 = Int64.of_int 65536) &&
  (int64_of_uint4 n4 = Int64.of_int 0x01000000) &&
  (int64_of_uint4 n5 = Int64.of_int 0x20000000) &&
  (int64_of_uint4 n6 = Int64.shift_left (Int64.of_int 0x20000000) 1) && 
  (int64_of_uint4 n7 = Int64.shift_left (Int64.of_int 0x20000000) 2) && 
  (int64_of_uint4 n8 = Int64.logor 
			 (Int64.of_int 0xffff)
			 (Int64.shift_left (Int64.of_int 0xffff) 16))
;;


let test_uint4_of_sth() =
  let n1 = mk_uint4 ('\000','\000','\000','\001') in
  let n2 = mk_uint4 ('\032','\000','\000','\000') in
  let n3 = mk_uint4 ('\064','\000','\000','\000') in
  let n4 = mk_uint4 ('\192','\000','\000','\000') in

  (uint4_of_int 1 = n1) &&
  (uint4_of_int 0x20000000 = n2) &&
  (overflow uint4_of_int (-1)) &&

  (uint4_of_int32 (Int32.of_string "1") = n1) &&
  (uint4_of_int32 (Int32.of_string "0x20000000") = n2) &&
  (uint4_of_int32 (Int32.of_string "0x40000000") = n3) &&
  (overflow uint4_of_int32 (Int32.of_string "-1")) ;; (*&&

  (uint4_of_int64 (Int64.of_string "1") = n1) &&
  (uint4_of_int64 (Int64.of_string "0x20000000") = n2) &&
  (uint4_of_int64 (Int64.of_string "0x40000000") = n3) &&
  (uint4_of_int64 (Int64.of_string "0xc0000000") = n4) &&
  (overflow uint4_of_int64 (Int64.of_string "0x100000000")) &&
  (overflow uint4_of_int64 (Int64.of_string "-1"))
;;*)


let test_sth_of_int8 () =
  let n1 = mk_int8 ('\000','\000','\000','\000','\000','\000','\000','\001') in
  let n2 = mk_int8 ('\000','\000','\000','\000','\001','\000','\000','\000') in
  let n3 = mk_int8 ('\000','\000','\000','\000','\064','\000','\000','\000') in
  let n4 = mk_int8 ('\000','\000','\000','\000','\128','\000','\000','\000') in
  let n5 = mk_int8 ('\001','\000','\000','\000','\000','\000','\000','\000') in
  let n6 = mk_int8 ('\128','\000','\000','\000','\000','\000','\000','\000') in
  let n7 = mk_int8 ('\255','\255','\255','\255','\255','\255','\255','\255') in

  (int_of_int8 n1 = 1) &&
  (int_of_int8 n2 = 0x01000000) &&
  (overflow int_of_int8 n3) &&
  (overflow int_of_int8 n4) &&
  (overflow int_of_int8 n5) &&
  (overflow int_of_int8 n6) &&
  (int_of_int8 n7 = -1) &&

  (int32_of_int8 n1 = Int32.of_int 1) &&
  (int32_of_int8 n2 = Int32.of_int 0x01000000) &&
  (int32_of_int8 n3 = Int32.shift_left (Int32.of_int 0x20000000) 1) && 
  (overflow int32_of_int8 n4) &&
  (overflow int32_of_int8 n5) &&
  (overflow int32_of_int8 n6) &&
  (int32_of_int8 n7 = Int32.of_int (-1)) &&

  (int64_of_int8 n1 = Int64.of_int 1) &&
  (int64_of_int8 n2 = Int64.of_int 0x01000000) &&
  (int64_of_int8 n3 = Int64.shift_left (Int64.of_int 1) 30) && 
  (int64_of_int8 n4 = Int64.shift_left (Int64.of_int 1) 31) && 
  (int64_of_int8 n5 = Int64.shift_left (Int64.of_int 1) 56) && 
  (int64_of_int8 n6 = Int64.shift_left (Int64.of_int 1) 63) && 
  (int64_of_int8 n7 = Int64.of_int (-1))
;;


let test_int8_of_sth() =
  let n1 = mk_int8 ('\000','\000','\000','\000','\000','\000','\000','\001') in
  let n2 = mk_int8 ('\000','\000','\000','\000','\032','\000','\000','\000') in
  let n3 = mk_int8 ('\000','\000','\000','\000','\064','\000','\000','\000') in
  let n4 = mk_int8 ('\064','\000','\000','\000','\000','\000','\000','\000') in
  let n5 = mk_int8 ('\255','\255','\255','\255','\255','\255','\255','\255') in

  (int8_of_int 1 = n1) &&
  (int8_of_int 0x20000000 = n2) &&
  (int8_of_int (-1) = n5) &&

  (int8_of_int32 (Int32.of_string "1") = n1) &&
  (int8_of_int32 (Int32.of_string "0x20000000") = n2) &&
  (int8_of_int32 (Int32.of_string "0x40000000") = n3) &&
  (int8_of_int32 (Int32.of_string "-1") = n5) &&

  (int8_of_int64 (Int64.of_string "1") = n1) &&
  (int8_of_int64 (Int64.of_string "0x20000000") = n2) &&
  (int8_of_int64 (Int64.of_string "0x40000000") = n3) &&
  (int8_of_int64 (Int64.of_string "0x4000000000000000") = n4) &&
  (int8_of_int64 (Int64.of_string "-1") = n5)
;;


let test_sth_of_uint8 () =
  let n1 = mk_uint8 ('\000','\000','\000','\000','\000','\000','\000','\001') in
  let n2 = mk_uint8 ('\000','\000','\000','\000','\001','\000','\000','\000') in
  let n3 = mk_uint8 ('\000','\000','\000','\000','\064','\000','\000','\000') in
  let n4 = mk_uint8 ('\000','\000','\000','\000','\128','\000','\000','\000') in
  let n5 = mk_uint8 ('\001','\000','\000','\000','\000','\000','\000','\000') in
  let n6 = mk_uint8 ('\128','\000','\000','\000','\000','\000','\000','\000') in
  let n7 = mk_uint8 ('\255','\255','\255','\255','\255','\255','\255','\255') in

  (int_of_uint8 n1 = 1) &&
  (int_of_uint8 n2 = 0x01000000) &&
  (overflow int_of_uint8 n3) &&
  (overflow int_of_uint8 n4) &&
  (overflow int_of_uint8 n5) &&
  (overflow int_of_uint8 n6) &&
  (overflow int_of_uint8 n7) &&

  (int32_of_uint8 n1 = Int32.of_int 1) &&
  (int32_of_uint8 n2 = Int32.of_int 0x01000000) &&
  (int32_of_uint8 n3 = Int32.shift_left (Int32.of_int 0x20000000) 1) && 
  (overflow int32_of_uint8 n4) &&
  (overflow int32_of_uint8 n5) &&
  (overflow int32_of_uint8 n6) &&
  (overflow int32_of_uint8 n7) &&

  (int64_of_uint8 n1 = Int64.of_int 1) &&
  (int64_of_uint8 n2 = Int64.of_int 0x01000000) &&
  (int64_of_uint8 n3 = Int64.shift_left (Int64.of_int 1) 30) && 
  (int64_of_uint8 n4 = Int64.shift_left (Int64.of_int 1) 31) && 
  (int64_of_uint8 n5 = Int64.shift_left (Int64.of_int 1) 56) && 
  (overflow int64_of_uint8 n6) &&
  (overflow int64_of_uint8 n7) 
;;


let test_uint8_of_sth() =
  let n1 = mk_uint8 ('\000','\000','\000','\000','\000','\000','\000','\001') in
  let n2 = mk_uint8 ('\000','\000','\000','\000','\032','\000','\000','\000') in
  let n3 = mk_uint8 ('\000','\000','\000','\000','\064','\000','\000','\000') in
  let n4 = mk_uint8 ('\064','\000','\000','\000','\000','\000','\000','\000') in
  let n5 = mk_uint8 ('\127','\255','\255','\255','\255','\255','\255','\255') in

  (uint8_of_int 1 = n1) &&
  (uint8_of_int 0x20000000 = n2) &&
  (overflow uint8_of_int (-1)) &&

  (uint8_of_int32 (Int32.of_string "1") = n1) &&
  (uint8_of_int32 (Int32.of_string "0x20000000") = n2) &&
  (uint8_of_int32 (Int32.of_string "0x40000000") = n3) &&
  (overflow uint8_of_int32 (Int32.of_string "0x80000000")) &&
  (overflow uint8_of_int32 (Int32.of_string "-1")) &&

  (uint8_of_int64 (Int64.of_string "1") = n1) &&
  (uint8_of_int64 (Int64.of_string "0x20000000") = n2) &&
  (uint8_of_int64 (Int64.of_string "0x40000000") = n3) &&
  (uint8_of_int64 (Int64.of_string "0x4000000000000000") = n4) &&
  (uint8_of_int64 (Int64.of_string "0x7fffffffffffffff") = n5) &&
  (overflow uint8_of_int64 (Int64.of_string "0x8000000000000000")) &&
  (overflow uint8_of_int64 (Int64.of_string "-1"))
;;

(* TODO: fp4, fp8 testen *)


test "sth_of_int4" test_sth_of_int4;;
test "int4_of_sth" test_int4_of_sth;;
test "sth_of_uint4" test_sth_of_uint4;;
test "uint4_of_sth" test_uint4_of_sth;;
test "sth_of_int8" test_sth_of_int8;;
test "int8_of_sth" test_int8_of_sth;;
test "sth_of_uint8" test_sth_of_uint8;;
test "uint8_of_sth" test_uint8_of_sth;;
