#use "topfind";;
#require "pcre";;
#require "unix";;
#directory "..";;
#load "netstring.cma";;

open Mimestring;;

(**********************************************************************)
(* scan_structured_value                                              *)
(**********************************************************************)

let t001() =
  let r = scan_structured_value "user@domain.com" [ '@'; '.' ] [] in
  r = [ Atom "user"; Special '@'; Atom "domain"; Special '.'; Atom "com" ]
;;


let t002() =
  let r = scan_structured_value "user @ domain . com" [ '@'; '.' ]  [] in
  r = [ Atom "user"; Special '@'; Atom "domain"; Special '.'; Atom "com" ]
;;


let t003() =
  let r = scan_structured_value "user(Do you know him?)@domain.com" [ '@'; '.' ]
 []  in
  r = [ Atom "user"; Special '@'; Atom "domain"; Special '.'; Atom "com" ]
;;


let t004() =
  let r = scan_structured_value "user @ domain . com" [ '@'; '.'; ' ' ] []  in
  r = [ Atom "user"; Special ' '; Special '@'; Special ' '; Atom "domain";
	Special ' '; Special '.'; Special ' '; Atom "com" ]
;;


let t005() =
  let r = scan_structured_value "user(Do you know him?)@domain.com"
	                        ['@'; '.'; '(']  [] in
  r = [ Atom "user"; Special '('; Atom "Do"; Atom "you"; Atom "know";
	Atom "him?)"; Special '@'; Atom "domain"; Special '.'; Atom "com" ]
;;


let t006() =
  let r = scan_structured_value "\"My.name\"@domain.com" [ '@'; '.' ]  [] in
  r = [ QString "My.name"; Special '@'; Atom "domain"; Special '.';
	Atom "com" ]
;;


let t007() =
  let r = scan_structured_value "\"\\\"()@. \"@domain.com" [ '@'; '.' ]  [] in
  r = [ QString "\"()@. "; Special '@'; Atom "domain"; Special '.';
	Atom "com" ]
;;


let t008() =
  let r = scan_structured_value "a(b(c(d)e)f)g" [] [] in
  r = [ Atom "a"; Atom "g" ]
;;


let t009() =
  let r = scan_structured_value "a(b(c(d)e)f" [] [] in
  r = [ Atom "a" ]
;;


let t010() =
  let r = scan_structured_value "a(b\\(c\\(d\\)e)f" [] [] in
  r = [ Atom "a"; Atom "f" ]
;;


let t011() =
  let r = scan_structured_value "a(b(c(d)e)f\\" [] [] in
  r = [ Atom "a" ]
;;


let t012() =
  let r = scan_structured_value "\"abc" [] [] in
  r = [ QString "abc" ]
;;


let t013() =
  let r = scan_structured_value "\"abc\\" [] [] in
  r = [ QString "abc\\" ]
;;


(* New tests for netstring-0.9: *)

let t020() =
  let r = scan_structured_value "user(Do you know him?)@domain.com" 
	    [ '@'; '.' ] [ Return_comments ] in
  r = [ Atom "user"; Comment; Special '@'; Atom "domain"; Special '.'; 
	Atom "com" ]
;;

let t021() =
  let r = scan_structured_value "user (Do you know him?) @ domain . com"
	    [ '@'; '.'; ' ' ] [] in
  r = [ Atom "user"; Special ' '; Special ' '; Special ' '; Special '@';
	Special ' '; Atom "domain";
	Special ' '; Special '.'; Special ' '; Atom "com" ]
;;

let t022() =
  let r = scan_structured_value "user (Do you know him?) @ domain . com"
	    [ '@'; '.'; ' ' ] [ Return_comments ] in
  r = [ Atom "user"; Special ' '; Comment; Special ' '; Special '@'; 
	Special ' '; Atom "domain";
	Special ' '; Special '.'; Special ' '; Atom "com" ]
;;

let t023() =
  let r = scan_structured_value "=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?=" 
	    [] [] in
  r = [ Atom "=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?=" ]
;;

let t024() =
  let r = scan_structured_value "=?ISO-8859-1?Q?Keld_J=F8rn_Simonsen?=" 
	    [ ] [ Recognize_encoded_words ] in
  r = [ EncodedWord(("ISO-8859-1",""), "Q", "Keld_J=F8rn_Simonsen") ]
;;

let t025() =
  let r = scan_structured_value 
	    "=?ISO-8859-1?B?SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=?= =?ISO-8859-2?B?dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==?="
	    []
	    [ Recognize_encoded_words ] in
  r = [ EncodedWord
	  (("ISO-8859-1", ""), "B", "SWYgeW91IGNhbiByZWFkIHRoaXMgeW8=");
	EncodedWord
	  (("ISO-8859-2", ""), "B", "dSB1bmRlcnN0YW5kIHRoZSBleGFtcGxlLg==")
      ]
;;

(**********************************************************************)
(* s_extended_token                                                   *)
(**********************************************************************)

let scan specials options str =
  let scn = create_mime_scanner specials options str in
  scan_token_list scn;;

let t100() =
  let r = scan [] [] "Two atoms" in
  match r with
      [ a1, Atom "Two"; a2, Atom "atoms" ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 3) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 4) &&
	(get_line a2 = 1) &&
	(get_column a2 = 4) &&
	(get_length a2 = 5) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;


let t101() =
  let r = scan [] [] "  Two  atoms  " in
  match r with
      [ a1, Atom "Two"; a2, Atom "atoms" ] ->

	(get_pos a1 = 2) &&
	(get_line a1 = 1) &&
	(get_column a1 = 2) &&
	(get_length a1 = 3) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 7) &&
	(get_line a2 = 1) &&
	(get_column a2 = 7) &&
	(get_length a2 = 5) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;


let t102() =
  let r = scan [] [] "  Two\n atoms  " in
  match r with
      [ a1, Atom "Two"; a2, Atom "atoms" ] ->

	(get_pos a1 = 2) &&
	(get_line a1 = 1) &&
	(get_column a1 = 2) &&
	(get_length a1 = 3) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 7) &&
	(get_line a2 = 2) &&
	(get_column a2 = 1) &&
	(get_length a2 = 5) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t110() =
  let r = scan [] [] "\"Two\" \"qstrings\"" in
  match r with
      [ a1, QString "Two"; a2, QString "qstrings" ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 5) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 6) &&
	(get_line a2 = 1) &&
	(get_column a2 = 6) &&
	(get_length a2 = 10) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t111() =
  let r = scan [] [] "  \"Two\"  \"qstrings\"  " in
  match r with
      [ a1, QString "Two"; a2, QString "qstrings" ] ->

	(get_pos a1 = 2) &&
	(get_line a1 = 1) &&
	(get_column a1 = 2) &&
	(get_length a1 = 5) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 9) &&
	(get_line a2 = 1) &&
	(get_column a2 = 9) &&
	(get_length a2 = 10) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t112() =
  let r = scan [] [] "  \"Two\nlines\"  \"and\nqstrings\"  " in
  match r with
      [ a1, QString "Two\nlines"; a2, QString "and\nqstrings" ] ->

	(get_pos a1 = 2) &&
	(get_line a1 = 1) &&
	(get_column a1 = 2) &&
	(get_length a1 = 11) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 15) &&
	(get_line a2 = 2) &&
	(get_column a2 = 8) &&
	(get_length a2 = 14) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t113() =
  let r = scan [] [] "  \"Two\\\nlines\"  \"and\\\nqstrings\"  " in
  match r with
      [ a1, QString "Two\nlines"; a2, QString "and\nqstrings" ] ->

	(get_pos a1 = 2) &&
	(get_line a1 = 1) &&
	(get_column a1 = 2) &&
	(get_length a1 = 12) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 16) &&
	(get_line a2 = 2) &&
	(get_column a2 = 8) &&
	(get_length a2 = 15) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t120() =
  (* Domain literals are implemented like quoted strings, so only the
   * most complicated test case.
   *)
  let r = scan [] [] "  [Two\\\nlines]  [and\\\nliterals]  " in
  match r with
      [ a1, DomainLiteral "Two\nlines"; a2, DomainLiteral "and\nliterals" ] ->

	(get_pos a1 = 2) &&
	(get_line a1 = 1) &&
	(get_column a1 = 2) &&
	(get_length a1 = 12) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 16) &&
	(get_line a2 = 2) &&
	(get_column a2 = 8) &&
	(get_length a2 = 15) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t130() =
  let r = scan [] [ Return_comments ] "(Two) (comments)" in
  match r with
      [ a1, Comment; a2, Comment ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 5) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 6) &&
	(get_line a2 = 1) &&
	(get_column a2 = 6) &&
	(get_length a2 = 10) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t131() =
  let r = scan [] [ Return_comments ] "(Two\nlines) (and\ncomments)" in
  match r with
      [ a1, Comment; a2, Comment ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 11) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 12) &&
	(get_line a2 = 2) &&
	(get_column a2 = 7) &&
	(get_length a2 = 14) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t132() =
  let r = scan [] [ Return_comments ] "(Two\\\nlines) (and\\\ncomments)" in
  match r with
      [ a1, Comment; a2, Comment ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 12) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 13) &&
	(get_line a2 = 2) &&
	(get_column a2 = 7) &&
	(get_length a2 = 15) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t133() =
  let r = scan [] [ Return_comments ] "(a\n(b\nc)d\ne(f)) atom" in
  match r with
      [ a1, Comment; a2, Atom "atom" ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 15) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 16) &&
	(get_line a2 = 4) &&
	(get_column a2 = 6) &&
	(get_length a2 = 4) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t140() = 
  let r = scan [] [] "\031\031" in
  match r with
      [ a1, Control '\031'; a2, Control '\031' ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 1) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 1) &&
	(get_line a2 = 1) &&
	(get_column a2 = 1) &&
	(get_length a2 = 1) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t150() = 
  let r = scan [ '\t'; '\n' ] [] " \t\n  \n  \t" in
  match r with
      [ a1, Special '\t'; _, Special '\n'; _, Special '\n'; a2, Special '\t'] ->

	(get_pos a1 = 1) &&
	(get_line a1 = 1) &&
	(get_column a1 = 1) &&
	(get_length a1 = 1) &&
	(separates_adjacent_encoded_words a1 = false) &&

	(get_pos a2 = 8) &&
	(get_line a2 = 3) &&
	(get_column a2 = 2) &&
	(get_length a2 = 1) &&
	(separates_adjacent_encoded_words a2 = false)

    | _ ->
	false
;;

let t160() =
  let r = scan [] [ Recognize_encoded_words ] 
	    "=?iso8859-1?q?G=F6rd?= =?iso8859-1?q?G=F6rd?=" in
  match r with
      [ a1, EncodedWord(("ISO8859-1", ""), "Q", "G=F6rd"); 
	a2, EncodedWord(("ISO8859-1", ""), "Q", "G=F6rd"); ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 22) &&
	(separates_adjacent_encoded_words a1 = false) &&
	(get_decoded_word a1 = "Görd") &&
	(get_charset a1 = "ISO8859-1") &&

	(get_pos a2 = 23) &&
	(get_line a2 = 1) &&
	(get_column a2 = 23) &&
	(get_length a2 = 22) &&
	(separates_adjacent_encoded_words a2 = false) &&
	(get_decoded_word a2 = "Görd") &&
	(get_charset a2 = "ISO8859-1")

    | _ ->
	false
;;

let t161() =
  let r = scan [ ' ' ] [ Recognize_encoded_words ] 
	    "=?iso8859-1?q?G=F6rd?= =?iso8859-1?q?G=F6rd?=" in
  match r with
      [ a1, EncodedWord(("ISO8859-1", ""), "Q", "G=F6rd"); 
	sp, Special ' ';
	a2, EncodedWord(("ISO8859-1", ""), "Q", "G=F6rd"); ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 22) &&
	(separates_adjacent_encoded_words a1 = false) &&
	(get_decoded_word a1 = "Görd") &&
	(get_charset a1 = "ISO8859-1") &&

	(get_pos a2 = 23) &&
	(get_line a2 = 1) &&
	(get_column a2 = 23) &&
	(get_length a2 = 22) &&
	(separates_adjacent_encoded_words a2 = false) &&
	(get_decoded_word a2 = "Görd") &&
	(get_charset a2 = "ISO8859-1") &&

	(separates_adjacent_encoded_words sp = true)

    | _ ->
	false
;;

let t162() =
  let r = scan [ ' ' ] [ Recognize_encoded_words ] 
	    "=?iso8859-1?q?G=F6rd?=  =?iso8859-1?q?G=F6rd?=" in
  match r with
      [ a1, EncodedWord(("ISO8859-1", ""), "Q", "G=F6rd"); 
	sp1, Special ' ';
	sp2, Special ' ';
	a2, EncodedWord(("ISO8859-1", ""), "Q", "G=F6rd"); ] ->

	(get_pos a1 = 0) &&
	(get_line a1 = 1) &&
	(get_column a1 = 0) &&
	(get_length a1 = 22) &&
	(separates_adjacent_encoded_words a1 = false) &&
	(get_decoded_word a1 = "Görd") &&
	(get_charset a1 = "ISO8859-1") &&

	(get_pos a2 = 24) &&
	(get_line a2 = 1) &&
	(get_column a2 = 24) &&
	(get_length a2 = 22) &&
	(separates_adjacent_encoded_words a2 = false) &&
	(get_decoded_word a2 = "Görd") &&
	(get_charset a2 = "ISO8859-1") &&

	(separates_adjacent_encoded_words sp1 = true) &&
	(separates_adjacent_encoded_words sp2 = true)

    | _ ->
	false
;;



(**********************************************************************)

let test f n =
  if f() then
    print_endline ("Test " ^ n ^ " ok")
  else
    print_endline ("Test " ^ n ^ " FAILED!!!!");
  flush stdout
;;

test t001 "001";;
test t002 "002";;
test t003 "003";;
test t004 "004";;
test t005 "005";;
test t006 "006";;
test t007 "007";;
test t008 "008";;
test t009 "009";;
test t010 "010";;
test t011 "011";;
test t012 "012";;
test t013 "013";;

test t020 "020";;
test t021 "021";;
test t022 "022";;
test t023 "023";;
test t024 "024";;
test t025 "025";;

test t100 "100";;
test t101 "101";;
test t102 "102";;
test t110 "110";;
test t111 "111";;
test t112 "112";;
test t113 "113";;
test t120 "120";;
test t130 "130";;
test t131 "131";;
test t132 "132";;
test t133 "133";;
test t140 "140";;
test t150 "150";;
test t160 "160";;
test t161 "161";;
test t162 "162";;
