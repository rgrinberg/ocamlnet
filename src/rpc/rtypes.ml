(*
 * $Id$
 *
 *)

(* NOTE: Parts of this implementation depend very much of representation
 * details of O'Caml 3.xx. It is not guaranteed that this works in future
 * versions of O'Caml as well.
 *)

(* representation types *)

type int4 = nativeint;;   (* faster on 64 bit platforms! *)
type int8 = int64;;

type uint4 = nativeint;;
type uint8 = int64;;

type fp4 = int32 (* string;; *)    (* IEEE representation of fp numbers *)
type fp8 = int64;;

exception Cannot_represent of string;;
(* raised if numbers are too big to map them to other type *)

exception Out_of_range;;

let rec cannot_represent s = 
  (* "rec" because this prevents this function from being inlined *)
  raise (Cannot_represent s);;


let lt_uint4 x y =
  if x < y then 
    x >= 0n
      (* because:
         - if x < 0  && y < 0   ==> x >u y
         - if x < 0  && y >= 0  ==> x >u y
         - if x >= 0 && y => 0  ==> x <u y
       *)
  else (* ==>  y <= x *) 
    y < x && y < 0n
      (* because:
         - if y < 0  && x < 0  ==> x <u y
         - if y < 0  && x >= 0 ==> x <u y
         - if y >= 0 && x >= 0 ==> x >u y
       *)


(**********************************************************************)
(* mk_[u]intn                                                         *)
(**********************************************************************)

(* compatibility interface *)

let mk_int4 (c3,c2,c1,c0) =
  let n3 = Nativeint.of_int (Char.code c3) in
  let n2 = Nativeint.of_int (Char.code c2) in
  let n1 = Nativeint.of_int (Char.code c1) in
  let n0 = Nativeint.of_int (Char.code c0) in

IFDEF WORDSIZE_64 THEN
  Nativeint.logor
    (Nativeint.shift_right (Nativeint.shift_left n3 56) 32)  (* sign! *)
    (Nativeint.logor
       (Nativeint.shift_left n2 16)
       (Nativeint.logor
	  (Nativeint.shift_left n1 8)
	  n0))
ELSE
  Nativeint.logor
    (Nativeint.shift_left n3 24)
    (Nativeint.logor
       (Nativeint.shift_left n2 16)
       (Nativeint.logor
	  (Nativeint.shift_left n1 8)
	  n0))
END
;;

let mk_int8 (c7,c6,c5,c4,c3,c2,c1,c0) =
  let n7 = Int64.of_int (Char.code c7) in
  let n6 = Int64.of_int (Char.code c6) in
  let n5 = Int64.of_int (Char.code c5) in
  let n4 = Int64.of_int (Char.code c4) in
  let n3 = Int64.of_int (Char.code c3) in
  let n2 = Int64.of_int (Char.code c2) in
  let n1 = Int64.of_int (Char.code c1) in
  let n0 = Int64.of_int (Char.code c0) in

  Int64.logor
    (Int64.shift_left n7 56)
    (Int64.logor
       (Int64.shift_left n6 48)
       (Int64.logor
	  (Int64.shift_left n5 40)
	  (Int64.logor
	     (Int64.shift_left n4 32)
	     (Int64.logor
		(Int64.shift_left n3 24)
		(Int64.logor
		   (Int64.shift_left n2 16)
		   (Int64.logor
		      (Int64.shift_left n1 8)
		      n0))))))
;;

let mk_uint4 = mk_int4;;
let mk_uint8 = mk_int8;;

(**********************************************************************)
(* read_[u]intn                                                       *)
(**********************************************************************)

let read_int4_unsafe s pos =
  let n3 = Nativeint.of_int (Char.code (String.unsafe_get s pos)) in
  let x = 
IFDEF WORDSIZE_64 THEN
    Nativeint.shift_right (Nativeint.shift_left n3 56) 32  (* sign! *)
ELSE
    Nativeint.shift_left n3 24
END in
  
  let n2 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+1))) in
  let x = Nativeint.logor x (Nativeint.shift_left n2 16) in

  let n1 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+2))) in
  let x = Nativeint.logor x (Nativeint.shift_left n1 8) in

  let n0 = Nativeint.of_int (Char.code (String.unsafe_get s (pos+3))) in
  Nativeint.logor x n0

(*
  seems to be slightly better than

  Int32.logor
    (Int32.shift_left n3 24)
    (Int32.logor
       (Int32.shift_left n2 16)
       (Int32.logor
	  (Int32.shift_left n1 8)
	  n0))
*)
;;

let read_int4 s pos =
  if pos < 0 || pos + 4 > String.length s then
    raise Out_of_range;
  read_int4_unsafe s pos



let read_int8_unsafe s pos =
  let n7 = Int64.of_int (Char.code (String.unsafe_get s pos)) in
  let x = Int64.shift_left n7 56 in

  let n6 = Int64.of_int (Char.code (String.unsafe_get s (pos+1))) in
  let x = Int64.logor x (Int64.shift_left n6 48) in

  let n5 = Int64.of_int (Char.code (String.unsafe_get s (pos+2))) in
  let x = Int64.logor x (Int64.shift_left n5 40) in

  let n4 = Int64.of_int (Char.code (String.unsafe_get s (pos+3))) in
  let x = Int64.logor x (Int64.shift_left n4 32) in

  let n3 = Int64.of_int (Char.code (String.unsafe_get s (pos+4))) in
  let x = Int64.logor x (Int64.shift_left n3 24) in

  let n2 = Int64.of_int (Char.code (String.unsafe_get s (pos+5))) in
  let x = Int64.logor x (Int64.shift_left n2 16) in

  let n1 = Int64.of_int (Char.code (String.unsafe_get s (pos+6))) in
  let x = Int64.logor x (Int64.shift_left n1 8) in

  let n0 = Int64.of_int (Char.code (String.unsafe_get s (pos+7))) in
  Int64.logor x n0
;;


let read_int8 s pos =
  if pos < 0 || pos + 8 > String.length s then
    raise Out_of_range;
  read_int8_unsafe s pos


let read_uint4 = read_int4;;
let read_uint8 = read_int8;;
let read_uint4_unsafe = read_int4_unsafe;;
let read_uint8_unsafe = read_int8_unsafe;;


let read_fp4 s pos =
  Nativeint.to_int32(read_int4 s pos)

let read_fp8 s pos =
  read_int8 s pos


(**********************************************************************)
(* dest_[u]intn                                                       *)
(**********************************************************************)

(* compatibility interface *)

let dest_int4 x =
  let n3 = Nativeint.to_int (Nativeint.shift_right_logical x 24) land 0xff in
  let n2 = Nativeint.to_int (Nativeint.shift_right_logical x 16) land 0xff in
  let n1 = Nativeint.to_int (Nativeint.shift_right_logical x 8) land 0xff in
  let n0 = Nativeint.to_int (Nativeint.logand x 0xffn) in
  (Char.chr n3, Char.chr n2, Char.chr n1, Char.chr n0)
;;


let dest_int8 x =
  let n7 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 56)
			   0xffL) in
  let n6 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 48)
			   0xffL) in
  let n5 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 40)
			   0xffL) in
  let n4 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 32)
			   0xffL) in
  let n3 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 24)
			   0xffL) in
  let n2 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 16)
			   0xffL) in
  let n1 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 8)
			   0xffL) in
  let n0 = Int64.to_int (Int64.logand x 0xffL) in
  (Char.chr n7, Char.chr n6, Char.chr n5, Char.chr n4,
   Char.chr n3, Char.chr n2, Char.chr n1, Char.chr n0)
;;


let dest_uint4 = dest_int4;;
let dest_uint8 = dest_int8;;

(**********************************************************************)
(* write_[u]intn                                                      *)
(**********************************************************************)

let write_int4_unsafe s pos x =
  let n3 = Nativeint.to_int (Nativeint.shift_right_logical x 24) land 0xff in
  String.unsafe_set s pos (Char.unsafe_chr n3);
  let n2 = Nativeint.to_int (Nativeint.shift_right_logical x 16) land 0xff in
  String.unsafe_set s (pos+1) (Char.unsafe_chr n2);
  let n1 = Nativeint.to_int (Nativeint.shift_right_logical x 8) land 0xff in
  String.unsafe_set s (pos+2) (Char.unsafe_chr n1);
  let n0 = Nativeint.to_int (Nativeint.logand x 0xffn) in
  String.unsafe_set s (pos+3) (Char.unsafe_chr n0);
  ()
;;


let write_int4 s pos x =
  if pos < 0 || pos + 4 > String.length s then
    raise Out_of_range;
  write_int4_unsafe s pos x
;;


let write_int8_unsafe s pos x =
  let n7 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 56)
			   0xffL) in
  String.unsafe_set s pos (Char.unsafe_chr n7);

  let n6 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 48)
			   0xffL) in
  String.unsafe_set s (pos+1) (Char.unsafe_chr n6);

  let n5 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 40)
			   0xffL) in
  String.unsafe_set s (pos+2) (Char.unsafe_chr n5);

  let n4 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 32)
			   0xffL) in
  String.unsafe_set s (pos+3) (Char.unsafe_chr n4);

  let n3 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 24)
			   0xffL) in
  String.unsafe_set s (pos+4) (Char.unsafe_chr n3);

  let n2 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 16)
			   0xffL) in
  String.unsafe_set s (pos+5) (Char.unsafe_chr n2);

  let n1 = Int64.to_int (Int64.logand (Int64.shift_right_logical x 8)
			   0xffL) in
  String.unsafe_set s (pos+6) (Char.unsafe_chr n1);

  let n0 = Int64.to_int (Int64.logand x 0xffL) in
  String.unsafe_set s (pos+7) (Char.unsafe_chr n0);
  ()
;;


let write_int8 s pos x =
  if pos < 0 || pos + 8 > String.length s then
    raise Out_of_range;
  write_int8_unsafe s pos x
;;


let write_uint4 = write_int4;;
let write_uint8 = write_int8;;
let write_uint4_unsafe = write_int4_unsafe;;
let write_uint8_unsafe = write_int8_unsafe;;

(**********************************************************************)
(* [u]intn_as_string                                                  *)
(**********************************************************************)

let int4_as_string x =
  let s = String.create 4 in
  write_int4 s 0 x;
  s
;;

let uint4_as_string x =
  let s = String.create 4 in
  write_uint4 s 0 x;
  s
;;

let int8_as_string x =
  let s = String.create 8 in
  write_int8 s 0 x;
  s
;;

let uint8_as_string x =
  let s = String.create 8 in
  write_int8 s 0 x;
  s
;;

(**********************************************************************)
(* int_of_[u]intn                                                     *)
(**********************************************************************)

let c_max_int_64 = Int64.of_int max_int;;
let c_min_int_64 = Int64.of_int min_int;;


let name_int_of_int4 = "int_of_int4"

let int_of_int4 x =
IFDEF WORDSIZE_64 THEN
  Nativeint.to_int x
ELSE
  if x < (-0x4000_0000n) || x > 0x3fff_ffffn then
    cannot_represent name_int_of_int4;
  Nativeint.to_int x
END
;;


let name_int_of_uint4 = "int_of_uint4"

let int_of_uint4 x =
IFDEF WORDSIZE_64 THEN
  Nativeint.to_int (Nativeint.logand x 0xffff_ffffn)
ELSE
  if x >= 0n && x <= 0x3fff_ffffn then
    Nativeint.to_int x
  else
    cannot_represent name_int_of_uint4
END
;;


let name_int_of_int8 = "int_of_int8"

let int_of_int8 x =
  if x >= c_min_int_64 && x <= c_max_int_64 then
    Int64.to_int x
  else
    cannot_represent name_int_of_int8
;;


let name_int_of_uint8 = "int_of_uint8"

let int_of_uint8 x =
  if x >= Int64.zero && x <= c_max_int_64 then
    Int64.to_int x
  else
    cannot_represent name_int_of_uint8
;;

(**********************************************************************)
(* intn_of_int                                                        *)
(**********************************************************************)

let name_int4_of_int = "int4_of_int"

let int4_of_int i =
IFDEF WORDSIZE_64 THEN
  let j = i asr 31 in
  if j = 0 || j = (-1) then
    Nativeint.of_int i
  else
    cannot_represent name_int4_of_int
ELSE
    Nativeint.of_int i
END
;;


let name_uint4_of_int = "uint4_of_int"

let uint4_of_int i =
IFDEF WORDSIZE_64 THEN
  let j = i asr 32 in
  if j = 0 then
    Nativeint.of_int i
  else
    cannot_represent name_uint4_of_int
ELSE
  if i >= 0 then
    Nativeint.of_int i
  else
    cannot_represent name_uint4_of_int
END
;;


let int8_of_int = Int64.of_int ;;


let name_uint8_of_int = "uint8_of_int"

let uint8_of_int i =
  if i >= 0 then
    Int64.of_int i
  else
    cannot_represent name_uint8_of_int
;;


(**********************************************************************)
(* Int32 and Int64 support: int[32|64]_of_[u]intn                     *)
(**********************************************************************)

let int32_of_int4 x = Nativeint.to_int32 x ;;

let name_int32_of_uint4 = "int32_of_uint4"

let int32_of_uint4 x =
  if x >= 0n then
    Nativeint.to_int32 x
  else
    cannot_represent name_int32_of_uint4
;;


let c_int32_min_int_64 = Int64.of_int32 Int32.min_int ;;
let c_int32_max_int_64 = Int64.of_int32 Int32.max_int ;;

let name_int32_of_int8 = "int32_of_int8"

let int32_of_int8 x =
  if x >= (-0x8000_0000L) && x <= 0x7fff_0000L then
    Int64.to_int32 x
  else
    cannot_represent name_int32_of_int8
;;


let name_int32_of_uint8 = "int32_of_uint8"

let int32_of_uint8 x =
  if x >= 0L && x <= 0x7fff_0000L then
    Int64.to_int32 x
  else
    cannot_represent name_int32_of_uint8
;;


let int64_of_int4 = Int64.of_nativeint ;;


let int64_of_uint4 x =
  if x >= 0n then
    Int64.of_nativeint x
  else
    Int64.add (Int64.of_nativeint x) 0x1_0000_0000L
;;

let int64_of_int8 x = x ;;

let name_int64_of_uint8 = "int64_of_uint8"

let int64_of_uint8 x =
  if x >= 0L then
    x
  else
    cannot_represent name_int64_of_uint8
;;


(**********************************************************************)
(* Int32 and Int64 support: [u]intn_of_int[32|64]                     *)
(**********************************************************************)

let int4_of_int32 = Nativeint.of_int32 ;;

let name_uint4_of_int32 = "uint4_of_int32"

let uint4_of_int32 i =
  if i < 0l then
    cannot_represent name_uint4_of_int32;
  Nativeint.of_int32 i
;;

let int8_of_int32 =
  Int64.of_int32
;;

let name_uint8_of_int32 = "uint8_of_int32"

let uint8_of_int32 i =
  if i < 0l then
    cannot_represent name_uint8_of_int32;
  Int64.of_int32 i
;;

let name_int4_of_int64 = "int4_of_int64"

let int4_of_int64 i =
  if i >= (-0x8000_0000L) && i <= 0x7fff_ffffL then
    Int64.to_nativeint i
  else cannot_represent name_int4_of_int64
;;

let name_uint4_of_int64 = "uint4_of_int64"

let uint4_of_int64 i =
  if i < 0L || i > 0xffff_ffffL then
    cannot_represent name_uint4_of_int64;
IFDEF WORDSIZE_64 THEN
  Int64.to_nativeint(Int64.shift_right (Int64.shift_left i 32) 32)  (* sign! *)
ELSE
  Int64.to_nativeint i
END
;;

let int8_of_int64 i = i ;;

let name_uint8_of_int64 = "uint8_of_int64"

let uint8_of_int64 i =
  if i < 0L then
    cannot_represent name_uint8_of_int64;
  i
;;

(**********************************************************************)
(* logical_xxx_of_xxx                                                 *)
(**********************************************************************)

let logical_uint4_of_int32 x = Nativeint.of_int32 x;;
let logical_int32_of_uint4 x = Nativeint.to_int32 x;;
let logical_uint8_of_int64 x = x;;
let logical_int64_of_uint8 x = x;;

(**********************************************************************)
(* floating-point numbers                                             *)
(**********************************************************************)

(* Old fp8_of_fp4 implementation, the only way until O'Caml 3.07.
 * 
 * let n23 = Int64.of_string "0x7fffff";;
 * let n32 = Int64.of_string "0x80000000";;
 * let n52 = Int64.of_string "0x10000000000000";;
 * 
 * let fp8_of_fp4_int64 x =
 *   let m = Int64.logand x n23 in
 *   let exp = Int64.to_int (Int64.shift_right_logical x 23) land 0xff in
 *   let s = Int64.logand x n32 in
 * 
 *   let x1 = Int64.shift_left s 32 in
 *   if exp = 255 then begin
 *     (* Infinity, NaN *)
 *     Int64.logor
 *       x1
 *       (Int64.logor
 * 	 (Int64.shift_left (Int64.of_int 0x7ff) 52)
 * 	 (Int64.shift_left m 29)
 *       )
 *   end
 *   else begin
 *     let m' = ref (Int64.shift_left m 29) in
 *     let exp' = ref (exp + 1023 - 127) in
 *     if exp = 0 && !m' <> Int64.zero then begin
 *       (* We need normalization *)
 *       while Int64.logand !m' n52 = Int64.zero do
 * 	m' := Int64.shift_left !m' 1;
 * 	decr exp'
 *       done
 *     end;
 *     Int64.logor
 *       x1
 *       (Int64.logor
 * 	 (Int64.shift_left (Int64.of_int !exp') 52)
 * 	 !m'
 *       )
 *   end
 * ;;
 *)


let fp8_of_fp4 x =
  (* Requires O'Caml >= 3.08 *)
  Int64.bits_of_float (Int32.float_of_bits x)
  (* Old:
   *  let x_int32 = x in
   *  let x_int64 = Int64.of_int32 x_int32 in
   *  let x'_int64 = fp8_of_fp4_int64 x_int64 in
   *  let x' = x'_int64 in
   *  x'
   *)
;;



(* Old fp4_of_fp8 implementation, the only way until O'Caml 3.07.
 *
 * let n51 = Int64.of_string "0xfffffffffffff";;
 * let n64 = Int64.of_string "0x8000000000000000";;
 * let n22 = Int64.of_string "0x400000";;
 *
 * let fp4_of_fp8_int64 x =
 *  let m = Int64.logand x n51 in
 *  let exp = Int64.to_int (Int64.shift_right_logical x 52) land 0x7ff in
 *  let s = Int64.logand x n64 in
 *
 *  let x1 = Int64.shift_right_logical s 32 in
 *  if exp = 0x7ff then begin
 *    (* infinity, NaN *)
 *    if m = Int64.zero then
 *      (* infinity *)
 *      Int64.logor
 *	x1
 *	(Int64.shift_left (Int64.of_int 0xff) 23)
 *    else
 *      (* NaN *)
 *      Int64.logor
 *	x1
 *	(Int64.logor
 *	   (Int64.shift_left (Int64.of_int 0xff) 23)
 *	   (Int64.logor
 *	      (Int64.shift_right_logical m 29)
 *	      (Int64.one)))
 *  end
 *  else begin
 *    let m' = ref (Int64.shift_right_logical m 29) in
 *    let exp' = ref (exp - 1023 + 127) in
 *    if !exp' <= 0 then begin
 *      m' := Int64.logor (Int64.shift_right_logical !m' 1) n22;
 *      while !exp' < 0 do
 *	m' := Int64.shift_right_logical !m' 1;
 *	incr exp'
 *      done
 *    end;
 *    if !exp' >= 255 then
 *      cannot_represent "fp4_of_fp8";
 *
 *    Int64.logor
 *      x1
 *      (Int64.logor
 *	 (Int64.shift_left (Int64.of_int !exp') 23)
 *	 !m'
 *      )
 *  end
 *;;
 *)

let fp4_of_fp8 x =
  (* Requires O'Caml >= 3.08 *)
  Int32.bits_of_float (Int64.float_of_bits x)
  (* Old:
   * let x_int64 = x in
   * let x'_int64 = fp4_of_fp8_int64 x_int64 in
   * let x'_int32 = Int64.to_int32 x'_int64 in
   * let x' = x'_int32 in
   * x'
   *)
;;


let float_of_fp8 x =
  (* Requires O'Caml >= 3.01 *)
  Int64.float_of_bits x
;;


let float_of_fp4 x =
  (* Requires O'Caml >= 3.08 *)
  Int32.float_of_bits x
  (* Old:
   *  float_of_fp8 (fp8_of_fp4 x)
   *)
;;


let fp8_of_float x =
  (* Requires O'Caml >= 3.01 *)
  Int64.bits_of_float x
;;


let fp4_of_float x =
  (* Requires O'Caml >= 3.08 *)
  Int32.bits_of_float x
  (* Old:
   * fp4_of_fp8 (fp8_of_float x)
   *)
;;

let mk_fp4 x = Nativeint.to_int32 (mk_int4 x) ;;
let mk_fp8 = mk_int8 ;;
let dest_fp4 x = dest_int4 (Nativeint.of_int32 x) ;;
let dest_fp8 = dest_int8 ;;

let fp4_as_string x = int4_as_string (Nativeint.of_int32 x);;
let fp8_as_string = int8_as_string;;

