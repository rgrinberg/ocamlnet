open Netconversion;;

let make_iso enc =
  let s = ref "" in
  for i = 0 to 255 do
    let u = try Netconversion.makechar (enc :> Netconversion.encoding) i 
            with Not_found -> "" in
    s := !s ^ u
  done;
  !s
;;

let make_ucs2 start stop =
  let s = String.create ((stop - start) * 2) in
  for i = 0 to stop-start-1 do
    let k = 2 * i in
    let c = i + start in
    s.[k]   <- Char.chr(c lsr 8);
    s.[k+1] <- Char.chr(c land 0xff);
  done;
  s
;;

let make_ucs4 start stop =
  let s = String.create ((stop - start) * 4) in
  for i = 0 to stop-start-1 do
    let k = 4 * i in
    let c = i + start in
    s.[k]   <- Char.chr(c lsr 24);
    s.[k+1] <- Char.chr((c lsr 16) land 0xff);
    s.[k+2] <- Char.chr((c lsr 8) land 0xff);
    s.[k+3] <- Char.chr(c land 0xff);
  done;
  s
;;

let name_of_encoding enc =
  match enc with
      `Enc_iso88591 -> "ISO_8859-1"
    | `Enc_iso88592 -> "ISO_8859-2"
    | `Enc_iso88593 -> "ISO_8859-3"
    | `Enc_iso88594 -> "ISO_8859-4"
    | `Enc_iso88595 -> "ISO_8859-5"
    | `Enc_iso88596 -> "ISO_8859-6"
    | `Enc_iso88597 -> "ISO_8859-7"
    | `Enc_iso88598 -> "ISO_8859-8"
    | `Enc_iso88599 -> "ISO_8859-9"
    | `Enc_iso885910 -> "ISO_8859-10"
    | `Enc_iso885913 -> "ISO_8859-13"
    | `Enc_iso885914 -> "ISO_8859-14"
    | `Enc_iso885915 -> "ISO_8859-15"
    | `Enc_utf8     -> "UTF-8"
    | `Enc_ucs4     -> "UCS-4"
    | `Enc_utf16    -> "UTF-16BE"
    | `Enc_eucjp    -> "EUC-JP"
;;

let iconv_recode_string in_enc out_enc in_s =
  let in_enc_name  = name_of_encoding in_enc in
  let out_enc_name = name_of_encoding out_enc in
  let out_s = ref "" in

  let out_ch,in_ch = Unix.open_process ("iconv -f " ^ in_enc_name ^ " -t " ^ 
					out_enc_name) in
  (* Write in_s to in_ch in a new thread: *)
  let writer =
    (Thread.create
       (fun () ->
	  output_string in_ch in_s;
	  flush in_ch;
	  Unix.close (Unix.descr_of_out_channel in_ch);
       )
       ()
    ) in
  (* Read the result in the current thread: *)
  let buf = String.create 1024 in
  let n = ref 1 in
  while !n <> 0 do
    let n' = input out_ch buf 0 1024 in
    out_s := !out_s ^ String.sub buf 0 n';
    n := n'
  done;
  Thread.join writer;
  ignore(Unix.close_process (out_ch,in_ch));
  !out_s
;;

let test_iso_and_utf8 enc  =
  let name = name_of_encoding enc in
  print_string ("Recode: " ^ name ^ " and UTF-8... "); flush stdout;
  let s = make_iso enc in
  let s1' = Netconversion.recode_string (enc :> Netconversion.encoding) 
                                        `Enc_utf8 s in
  let s2' = iconv_recode_string         enc `Enc_utf8 s in
  assert(s1' = s2');
  let s1  = Netconversion.recode_string `Enc_utf8 
	                                (enc :> Netconversion.encoding) s1' in
  let s2  = iconv_recode_string         `Enc_utf8 enc s1' in
  assert(s1 = s2 && s1 = s);
  print_endline "OK"; flush stdout
;;

let test_utf16_and_utf8_0000_d7ff () =
  print_string "Recode: UTF-16-BE and UTF-8, #0000-#D7FF... "; 
  flush stdout;
  let s = make_ucs2 0 0xd800 in
  let s1' = Netconversion.recode_string `Enc_utf16_be `Enc_utf8 s in
  let s2' = iconv_recode_string        `Enc_utf16    `Enc_utf8 s in
  assert(s1' = s2');
  let s1  = Netconversion.recode_string `Enc_utf8 `Enc_utf16_be s1' in
  let s2  = iconv_recode_string        `Enc_utf8 `Enc_utf16 s1' in
  assert(s1 = s2 && s1 = s);
  print_endline "OK"; flush stdout
;;

let test_utf16_and_utf8_e000_fffd () =
  print_string "Recode: UTF-16-BE and UTF-8, #E000-#FFFD... "; 
  flush stdout;
  let s = make_ucs2 0xe000 0xfffe in
  let s1' = Netconversion.recode_string `Enc_utf16_be `Enc_utf8 s in
  let s2' = iconv_recode_string        `Enc_utf16    `Enc_utf8 s in
  assert(s1' = s2');
  let s1  = Netconversion.recode_string `Enc_utf8 `Enc_utf16_be s1' in
  let s2  = iconv_recode_string        `Enc_utf8 `Enc_utf16 s1' in
  assert(s1 = s2 && s1 = s);
  print_endline "OK"; flush stdout
;;

let test_utf16_and_utf8_10000_10FFFF () =
  print_string "Recode: UTF-16-BE and UTF-8, #10000-#10FFFF... "; 
  flush stdout;
  for i = 1 to 16 do
    let s0  = make_ucs4 (i * 0x10000) (i * 0x10000 + 0x10000) in
    let s   = iconv_recode_string        `Enc_ucs4     `Enc_utf16 s0 in
    let s1' = Netconversion.recode_string `Enc_utf16_be `Enc_utf8 s in
    let s2' = iconv_recode_string        `Enc_utf16    `Enc_utf8 s in
    assert(s1' = s2');
    let s1  = Netconversion.recode_string `Enc_utf8 `Enc_utf16_be s1' in
    let s2  = iconv_recode_string        `Enc_utf8 `Enc_utf16 s1' in
    assert(s1 = s2 && s1 = s);
    print_string "+"; flush stdout;
  done;
  print_endline "OK"; flush stdout
;;


let test_utf16_and_eucjp () =
  print_string "Recode: UTF-16-BE and EUC-JP... "; flush stdout;
  let s = make_ucs2 0 0xd800 ^ make_ucs2 0xe000 0xfffe in
  (* Transform this to EUC-JP, and remove all non-representable codes: *)
  let s_euc = Netconversion.convert ~subst:(fun _ -> "") 
		~in_enc:`Enc_utf16_be ~out_enc:`Enc_eucjp s in
  print_string "+"; flush stdout;
  let l = Netconversion.ustring_length `Enc_eucjp s_euc in
  print_string (string_of_int l ^ " characters... ");
  let s1 = Netconversion.convert `Enc_eucjp `Enc_utf16_be s_euc in
  print_string "+"; flush stdout;
  let s2 = iconv_recode_string `Enc_eucjp `Enc_utf16 s_euc in
  print_string "+"; flush stdout;
  assert (s1 = s2);
  let s1' = Netconversion.convert `Enc_utf16_be `Enc_eucjp s1 in
  print_string "+"; flush stdout;
  let s2' = iconv_recode_string `Enc_utf16 `Enc_eucjp s1 in
  assert (s1' = s_euc && s1' = s2');
  print_endline "OK"; 
  flush stdout
;;


let test_recode_traps () =
  print_string "Recode: testing some traps... "; flush stdout;
  let auml = 
    Netconversion.convert ~in_enc:`Enc_iso88591 ~out_enc:`Enc_utf8 "ä" in
  let s = auml ^ "ä" in
  (* Test: "recode" must not run into the third byte, 'ä' ! *)

  (* Test1: limit by in_len *)
  let out_buf = String.make 256 ' ' in
  let (in_k, out_k, _) = 
    Netconversion.recode ~in_enc:`Enc_utf8 ~in_buf:s ~in_pos:0
      ~in_len:(String.length auml) ~out_enc:`Enc_iso88591 ~out_buf ~out_pos:0 
      ~out_len:(String.length out_buf) ~max_chars:max_int
      ~subst:(fun _ -> assert false) in
  assert(in_k = 2);
  assert(out_k = 1);
  assert(out_buf.[0] = 'ä');
  print_string "1OK "; flush stdout;

  (* Test2: limit by max_chars *)
  out_buf.[0] <- ' ';
  let (in_k, out_k, _) = 
    Netconversion.recode ~in_enc:`Enc_utf8 ~in_buf:s ~in_pos:0
      ~in_len:(String.length s) ~out_enc:`Enc_iso88591 ~out_buf ~out_pos:0 
      ~out_len:(String.length out_buf) ~max_chars:1
      ~subst:(fun _ -> assert false) in
  assert(in_k = 2);
  assert(out_k = 1);
  assert(out_buf.[0] = 'ä');
  print_endline "2OK "; flush stdout
;;


let make_test_string enc n =
  (* Code points from 0 to n-1 as enc-encoded string *)
  let b = Buffer.create (n*3) in
  for i = 0 to n-1 do
    Buffer.add_string b (ustring_of_uchar enc i)
  done;
  Buffer.contents b
;;


let test_cursor1 () =
  (* Move forth and back, test every position: *)
  print_string "Cursors: Move char-by-char... "; flush stdout;
  let s = make_test_string `Enc_utf8 16384 in
  let cs = create_cursor `Enc_utf8 s in
  for i = 0 to 16383 do
    assert(uchar_at cs = i);
    move cs;
  done;
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  assert(try move cs; false with Cursor_out_of_range -> true);
  for i = 16383 downto 0 do
    move ~num:(-1) cs;
    assert(uchar_at cs = i);
  done;
  assert(try move ~num:(-1) cs; false with Cursor_out_of_range -> true);
  print_endline "OK"; flush stdout;
;;


let test_cursor2 () =
  (* Move forth and back, skip as much as possible: *)
  print_string "Cursors: Move far... "; flush stdout;
  let s = make_test_string `Enc_utf8 16384 in
  let cs = create_cursor `Enc_utf8 s in
  assert(uchar_at cs = 0);
  move ~num:16383 cs;
  assert(uchar_at cs = 16383);
  move cs;
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  assert(try move cs; false with Cursor_out_of_range -> true);
  move ~num:(-16384) cs;
  assert(uchar_at cs = 0);
  assert(try move ~num:(-1) cs; false with Cursor_out_of_range -> true);
  print_endline "OK"; flush stdout;
;;


let test_cursor3() =
  (* Move forth and back, stop at borders *)
  print_string "Cursors: Move border-to-border... "; flush stdout;
  let s = make_test_string `Enc_utf8 16384 in
  let cs = create_cursor `Enc_utf8 s in
  assert(uchar_at cs = 0);
  assert(try move ~num:max_int cs; false with Cursor_out_of_range -> true);
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  move ~num:(-1) cs;
  assert(uchar_at cs = 16383);
  assert(try move ~num:(-max_int) cs; false with Cursor_out_of_range -> true);
  assert(uchar_at cs = 0);
  print_endline "OK"; flush stdout;
;;
  

let test_cursor4() =
  (* Cursor for empty string *)
  print_string "Cursors: Empty strings... "; flush stdout;
  let cs = create_cursor `Enc_utf8 "" in
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  print_endline "OK"; flush stdout;
;;


let test_cursor5() =
  (* No cursor when initial character is bad *)
  print_string "Cursors: Bad char at pos 0... "; flush stdout;
  let s = "\192\128" in
  assert(try ignore(create_cursor `Enc_utf8 s); false with
	     Malformed_code -> true);
  print_endline "OK"; flush stdout;
;;


let test_cursor6() =
  (* Cursor work for other bad chars *)
  print_string "Cursors: Bad char at pos 1... "; flush stdout;
  let s = "A\192\128" in
  let cs = create_cursor `Enc_utf8 s in
  assert(uchar_at cs = 65);
  assert(try move cs; false with Malformed_code -> true);
  assert(uchar_at cs = 65);
  print_endline "OK"; flush stdout;
;;


let test_cursor7() =
  (* Cursor work for other bad chars *)
  print_string "Cursors: Bad char at pos 2... "; flush stdout;
  let s = "AB\192\128" in
  let cs = create_cursor `Enc_utf8 s in
  assert(uchar_at cs = 65);
  assert(try move ~num:2 cs; false with Malformed_code -> true);
  assert(uchar_at cs = 66);
  print_endline "OK"; flush stdout;
;;


let test_cursor8() =
  (* Cursor work for other bad chars *)
  print_string "Cursors: Bad char at pos -1... "; flush stdout;
  let s = "\192\128AB" in
  let cs = create_cursor ~initial_rel_pos:2 `Enc_utf8 s in
  assert(uchar_at cs = 65);
  assert(try move ~num:(-1) cs; false with Malformed_code -> true);
  assert(uchar_at cs = 65);
  print_endline "OK"; flush stdout;
;;


let test_cursor9() =
  (* Cursor work for other bad chars *)
  print_string "Cursors: Bad char at pos -2... "; flush stdout;
  let s = "\192\128AB" in
  let cs = create_cursor ~initial_rel_pos:3 `Enc_utf8 s in
  assert(uchar_at cs = 66);
  assert(try move ~num:(-2) cs; false with Malformed_code -> true);
  assert(uchar_at cs = 65);
  print_endline "OK"; flush stdout;
;;


let test_cursor10() =
  (* Imbchar within slice *)
  print_string "Cursors: Incomplete multi-byte character... "; flush stdout;
  let s = "A\192" in
  let cs = create_cursor `Enc_utf8 s in
  assert(uchar_at cs = 65);
  move cs;
  assert(try ignore(uchar_at cs); false with Partial_character -> true);
  move cs;
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  print_endline "OK"; flush stdout;
;;


let test_cursor11() =
  (* Imbchar at the end of the slice; this especially tests cursor_imbchar_len
   *)
  print_string "Cursors: Incomplete multi-byte character at end of slice... ";
  flush stdout;
  let s = make_test_string `Enc_utf8 (big_slice-2) ^ "A\192" in
  let cs = create_cursor `Enc_utf8 s in
  assert(uchar_at cs = 0);
  move ~num:(big_slice-2) cs;
  assert(uchar_at cs = 65);
  move cs;
  assert(try ignore(uchar_at cs); false with Partial_character -> true);
  assert(cursor_byte_length cs = 1);
  move cs;
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  move ~num:(-1) cs;
  assert(try ignore(uchar_at cs); false with Partial_character -> true);
  move ~num:(-1) cs;
  assert(uchar_at cs = 65);
  print_endline "OK"; flush stdout;
;;


let test_cursor12() =
  (* Single imbchar *)
  print_string "Cursors: Incomplete multi-byte character at beginning... ";
  let s = "\192" in
  let cs = create_cursor `Enc_utf8 s in
  assert(try ignore(uchar_at cs); false with Partial_character -> true);
  assert(cursor_byte_length cs = 1);
  move cs;
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  print_endline "OK"; flush stdout;
;;


let test_cursor13() =
  print_string "Cursors: UTF-16 and BOM... "; flush stdout;
  let s = "\254\255" ^ make_test_string `Enc_utf16_be (big_slice+1) in
  let cs = create_cursor `Enc_utf16 s in
  assert(cursor_encoding cs = `Enc_utf16);
  assert(try ignore(uchar_at cs); false with Byte_order_mark -> true);
  assert(cursor_byte_length cs = 2);
  move cs;
  assert(cursor_encoding cs = `Enc_utf16_be);
  assert(uchar_at cs = 0);
  move ~num:(-1) cs;
  assert(cursor_encoding cs = `Enc_utf16);
  move ~num:big_slice cs;
  assert(cursor_encoding cs = `Enc_utf16_be);
  assert(uchar_at cs = big_slice - 1);
  move ~num:(-1) cs;
  assert(cursor_encoding cs = `Enc_utf16_be);
  assert(uchar_at cs = big_slice - 2);
  move ~num:(-big_slice+1) cs;
  assert(cursor_encoding cs = `Enc_utf16);
  print_endline "OK"; flush stdout;
;;


let test_cursor14() =
  print_string "Cursors: Incomplete BOM... ";
  let s = "\254" in
  let cs = create_cursor `Enc_utf16 s in
  assert(cursor_encoding cs = `Enc_utf16);
  assert(try ignore(uchar_at cs); false with Partial_character -> true);
  assert(cursor_byte_length cs = 1);
  move cs;
  assert(cursor_encoding cs = `Enc_utf16);
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  print_endline "OK"; flush stdout;
;;


let test_cursor15 () =
  (* Same as test_cursor1, but for single-byte encoding *)
  print_string "Cursors: Single-byte encoding... "; flush stdout;
  let s = make_test_string `Enc_iso88591 256 in
  let cs = create_cursor `Enc_iso88591 s in
  for i = 0 to 255 do
    assert(uchar_at cs = i);
    assert(cursor_byte_length cs = 1);
    move cs;
  done;
  assert(try ignore(uchar_at cs); false with End_of_string -> true);
  assert(try move cs; false with Cursor_out_of_range -> true);
  for i = 255 downto 0 do
    move ~num:(-1) cs;
    assert(uchar_at cs = i);
    assert(cursor_byte_length cs = 1);
  done;
  assert(try move ~num:(-1) cs; false with Cursor_out_of_range -> true);
  print_endline "OK"; flush stdout;
;;


let test_cursor16() =
  print_string "Cursors: UTF-16-BE / no BOM... "; flush stdout;
  let s = make_test_string `Enc_utf16_be (big_slice+1) in
  let cs = create_cursor `Enc_utf16_be s in
  assert(cursor_encoding cs = `Enc_utf16_be);
  assert(uchar_at cs = 0);
  move ~num:big_slice cs;
  assert(cursor_encoding cs = `Enc_utf16_be);
  assert(uchar_at cs = big_slice);
  print_endline "OK"; flush stdout;
;;


print_endline "Warning: You need the command 'iconv' to run this test!";
flush stdout;
test_iso_and_utf8 `Enc_iso88591;
test_iso_and_utf8 `Enc_iso88592;
test_iso_and_utf8 `Enc_iso88593;
test_iso_and_utf8 `Enc_iso88594;
test_iso_and_utf8 `Enc_iso88595;
test_iso_and_utf8 `Enc_iso88596;
test_iso_and_utf8 `Enc_iso88597;
(* test_iso_and_utf8 `Enc_iso88598; *)
test_iso_and_utf8 `Enc_iso88599;
test_iso_and_utf8 `Enc_iso885910;
(* test_iso_and_utf8 `Enc_iso885913; *)
(* test_iso_and_utf8 `Enc_iso885914; *)
(* test_iso_and_utf8 `Enc_iso885915; *)
test_utf16_and_utf8_0000_d7ff();
test_utf16_and_utf8_e000_fffd();
(* This test does not work because iconv does not support the surrogate
 * representation of UTF-16:
 * test_utf16_and_utf8_10000_10FFFF();
 *)
test_utf16_and_eucjp ();
test_recode_traps();
test_cursor1();
test_cursor2();
test_cursor3();
test_cursor4();
test_cursor5();
test_cursor6();
test_cursor7();
test_cursor8();
test_cursor9();
test_cursor10();
test_cursor11();
test_cursor12();
test_cursor13();
test_cursor14();
test_cursor15();
test_cursor16();
()
;;
