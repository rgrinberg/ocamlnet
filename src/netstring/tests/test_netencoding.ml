#use "topfind";;
#require "netstring";;


open Netencoding;;

(**********************************************************************)
(* Base64                                                             *)
(**********************************************************************)

(* Test strings:
 * "", "a", "ab", "abc", "abcd", "abcde",
 * "abcdefghijklmnopqrstuvwxyz".
 *)

let t001() =
  (* ENCODE. No line breaks. *)
  Base64.encode "" = "" &
  Base64.encode "a" = "YQ==" &
  Base64.encode "ab" = "YWI=" &
  Base64.encode "abc" = "YWJj" &
  Base64.encode "abcd" = "YWJjZA==" &
  Base64.encode "abcde" = "YWJjZGU=" &
  Base64.encode "abcdefghijklmnopqrstuvwxyz" =
                "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo="
;;


let t002() =
  (* ENCODE. Lines with length of 4, separated by LF *)
  let abc = "abcdefghijklmnopqrstuvwxyz" in
  let enc = Base64.encode ~linelength:4 in
  enc ~len:0 abc = "" &
  enc ~len:1 abc = "YQ==\n" &
  enc ~len:2 abc = "YWI=\n" &
  enc ~len:3 abc = "YWJj\n" &
  enc ~len:4 abc = "YWJj\nZA==\n" &
  enc ~len:5 abc = "YWJj\nZGU=\n" &
  enc abc = "YWJj\nZGVm\nZ2hp\namts\nbW5v\ncHFy\nc3R1\ndnd4\neXo=\n"
;;


let t003() =
  (* ENCODE. Lines with length of 5, separated by LF *)
  let abc = "abcdefghijklmnopqrstuvwxyz" in
  let enc = Base64.encode ~linelength:5 in
  enc ~len:0 abc = "" &
  enc ~len:1 abc = "YQ==\n" &
  enc ~len:2 abc = "YWI=\n" &
  enc ~len:3 abc = "YWJj\n" &
  enc ~len:4 abc = "YWJj\nZA==\n" &
  enc ~len:5 abc = "YWJj\nZGU=\n" &
  enc abc = "YWJj\nZGVm\nZ2hp\namts\nbW5v\ncHFy\nc3R1\ndnd4\neXo=\n"
;;


let t004() =
  (* ENCODE. Lines with length of 7, separated by LF *)
  let abc = "abcdefghijklmnopqrstuvwxyz" in
  let enc = Base64.encode ~linelength:7 in
  enc ~len:0 abc = "" &
  enc ~len:1 abc = "YQ==\n" &
  enc ~len:2 abc = "YWI=\n" &
  enc ~len:3 abc = "YWJj\n" &
  enc ~len:4 abc = "YWJj\nZA==\n" &
  enc ~len:5 abc = "YWJj\nZGU=\n" &
  enc abc = "YWJj\nZGVm\nZ2hp\namts\nbW5v\ncHFy\nc3R1\ndnd4\neXo=\n"
;;


let t005() =
  (* ENCODE. Lines with length of 8, separated by LF *)
  let abc = "abcdefghijklmnopqrstuvwxyz" in
  let enc = Base64.encode ~linelength:8 in
  enc ~len:0 abc = "" &
  enc ~len:1 abc = "YQ==\n" &
  enc ~len:2 abc = "YWI=\n" &
  enc ~len:3 abc = "YWJj\n" &
  enc ~len:4 abc = "YWJjZA==\n" &
  enc ~len:5 abc = "YWJjZGU=\n" &
  enc abc = "YWJjZGVm\nZ2hpamts\nbW5vcHFy\nc3R1dnd4\neXo=\n"
;;


let t006() =
  (* ENCODE. Lines with length of 8, separated by CRLF *)
  let abc = "abcdefghijklmnopqrstuvwxyz" in
  let enc = Base64.encode ~linelength:8 ~crlf:true in
  enc ~len:0 abc = "" &
  enc ~len:1 abc = "YQ==\r\n" &
  enc ~len:2 abc = "YWI=\r\n" &
  enc ~len:3 abc = "YWJj\r\n" &
  enc ~len:4 abc = "YWJjZA==\r\n" &
  enc ~len:5 abc = "YWJjZGU=\r\n" &
  enc abc = "YWJjZGVm\r\nZ2hpamts\r\nbW5vcHFy\r\nc3R1dnd4\r\neXo=\r\n"
;;


let old_input p x k l =
  try p#input x k l with End_of_file -> 0
;;


let t007() =
  (* ENCODING PIPE *)
  let x = String.make 30 ' ' in
  let p = new Base64.encoding_pipe () in
  ( try ignore(old_input p x 0 30); failwith "code A" 
    with Netchannels.Buffer_underrun -> ()
  );
  p # output_string "abcd";
  if old_input p x 0 30 <> 4 then failwith "code B";
  if String.sub x 0 4 <> "YWJj" then failwith "code C";
  ( try ignore(old_input p x 0 30); failwith "code D" 
    with Netchannels.Buffer_underrun -> ()
  );
  p # output_string "efg";
  if old_input p x 0 30 <> 4 then failwith "code E";
  if String.sub x 0 4 <> "ZGVm" then failwith "code F";
  ( try ignore(old_input p x 0 30); failwith "code G" 
    with Netchannels.Buffer_underrun -> ()
  );
  p # close_out();
  if old_input p x 0 30 <> 4 then failwith "code H";
  if String.sub x 0 4 <> "Zw==" then failwith "code I";
  if old_input p x 0 30 <> 0 then failwith "code J";
  true
;;


let t008() =
  (* ENCODING PIPE w/ multiple lines *)
  let x = String.make 30 ' ' in
  let p1 = new Base64.encoding_pipe ~linelength:8 () in
  p1 # output_string "abcd";
  p1 # close_out();
  if old_input p1 x 0 30 <> 9 then failwith "code A";
  if String.sub x 0 9 <> "YWJjZA==\n" then failwith "code B";
  if old_input p1 x 0 30 <> 0 then failwith "code C";

  let p2 = new Base64.encoding_pipe ~linelength:8 () in
  p2 # close_out();
  if old_input p2 x 0 30 <> 0 then failwith "code D";

  let p3 = new Base64.encoding_pipe ~linelength:8 () in
  p3 # output_string "abcd";
  if old_input p3 x 0 30 <> 4 then failwith "code E";
  if String.sub x 0 4 <> "YWJj" then failwith "code F";
  p3 # close_out();
  if old_input p3 x 0 30 <> 5 then failwith "code G";
  if String.sub x 0 5 <> "ZA==\n" then failwith "code H";
  if old_input p3 x 0 30 <> 0 then failwith "code I";

  let p4 = new Base64.encoding_pipe ~linelength:8 () in
  p4 # output_string "abc";
  if old_input p4 x 0 30 <> 4 then failwith "code J";
  if String.sub x 0 4 <> "YWJj" then failwith "code K";
  p4 # close_out();
  if old_input p4 x 0 30 <> 1 then failwith "code L";
  if String.sub x 0 1 <> "\n" then failwith "code M";
  if old_input p4 x 0 30 <> 0 then failwith "code N";

  let p5 = new Base64.encoding_pipe ~linelength:8 () in
  p5 # output_string "abc";
  if old_input p5 x 0 30 <> 4 then failwith "code O";
  if String.sub x 0 4 <> "YWJj" then failwith "code P";
  p5 # output_string "defghi";
  if old_input p5 x 0 30 <> 9 then failwith "code Q";
  if String.sub x 0 9 <> "ZGVm\nZ2hp" then failwith "code R";
  p5 # output_string "jklmno";
  if old_input p5 x 0 30 <> 9 then failwith "code S";
  if String.sub x 0 9 <> "amts\nbW5v" then failwith "code T";
  p5 # close_out();
  if old_input p5 x 0 30 <> 1 then failwith "code U";
  if String.sub x 0 1 <> "\n" then failwith "code V";
  if old_input p5 x 0 30 <> 0 then failwith "code W";

  true
;;


let t020() =
  (* DECODE. First test without spaces *)
  let dec = Base64.decode ~url_variant:false in
  dec "" = "" &
  dec "YQ==" = "a" &
  dec "YWI=" = "ab" &
  dec "YWJj" = "abc" &
  dec "YWJjZA==" = "abcd" &
  dec "YWJjZGU=" = "abcde" &
  dec "YWJjZGVmZ2hpamtsbW5vcHFyc3R1dnd4eXo=" = "abcdefghijklmnopqrstuvwxyz"
;;


let t021() =
  (* DECODE. With spaces *)
  let dec = Base64.decode ~url_variant:false ~accept_spaces:true in
  dec " \r\n\t" = "" &
  dec " Y W J j\n Z G U = " = "abcde"
;;
 

let t022() =
  (* DECODE. With URL characters and spaces *)
  let dec = Base64.decode ~url_variant:true ~accept_spaces:true in
  dec " Y W J j\n Z G U = " = "abcde" &
  dec " Y W J j\n Z G U . " = "abcde"
;;


let t023() =
  (* DECODING PIPE *)
  let p = new Base64.decoding_pipe ~url_variant:false ~accept_spaces:false() in
  let x = String.make 30 ' ' in
  p#output_string "YWJ";
  ( try ignore(p#input_char()); failwith "code A" 
    with Netchannels.Buffer_underrun -> ()
  );
  p#output_string "jZGV";
  if old_input p x 0 30 <> 3 then failwith "code B";
  if String.sub x 0 3 <> "abc" then failwith "code C";
  p#output_string "mZ2hpamtsbW5vcHFyc3R1dnd4eXo";
  if old_input p x 0 30 <> 21 then failwith "code D";
  if String.sub x 0 21 <> "defghijklmnopqrstuvwx" then failwith "code E";
  p#output_string "=";
  if old_input p x 0 30 <> 2 then failwith "code F";
  if String.sub x 0 2 <> "yz" then failwith "code G";
  p#close_out();
  if old_input p x 0 30 <> 0 then failwith "code H";
  true
;;


let t024() =
  (* DECODING PIPE w/ spaces *)
  let p = new Base64.decoding_pipe ~url_variant:false ~accept_spaces:true () in
  let x = String.make 30 ' ' in
  p#output_string " Y W J ";
  ( try ignore(p#input_char()); failwith "code A" 
    with Netchannels.Buffer_underrun -> ()
  );
  p#output_string " j Z G V ";
  if old_input p x 0 30 <> 3 then failwith "code B";
  if String.sub x 0 3 <> "abc" then failwith "code C";
  p#output_string "  m Z 2 h p a m t s b W 5 v c H F y c 3 R 1 d n d 4 e X o ";
  if old_input p x 0 30 <> 21 then failwith "code D";
  if String.sub x 0 21 <> "defghijklmnopqrstuvwx" then failwith "code E";
  p#output_string " =      ";
  if old_input p x 0 30 <> 2 then failwith "code F";
  if String.sub x 0 2 <> "yz" then failwith "code G";
  p#close_out();
  if old_input p x 0 30 <> 0 then failwith "code H";
  true
;;


(**********************************************************************)
(* Quoted Printable                                                   *)
(**********************************************************************)

let t100() =
  (* ENCODE. *)
  QuotedPrintable.encode "a %= 12345 &$[]\"" = "a %=3D 12345 &=24=5B=5D=22" &
  QuotedPrintable.encode "\000\001\002" = "=00=01=02" &
  QuotedPrintable.encode "abc\r\ndef\nghi" = "abc\r\ndef\r\nghi" &
  QuotedPrintable.encode " abc\r\n def\n ghi" = " abc\r\n def\r\n ghi" &
  QuotedPrintable.encode "abc \r\n def\nghi " = "abc=20\r\n def\r\nghi=20"
;;


let t120() =
  (* DECODE. *)
  QuotedPrintable.decode "a %=3D 12345 &=24=5B=5D=22" = "a %= 12345 &$[]\"" &
  QuotedPrintable.decode "=00=01=02" = "\000\001\002" &
  QuotedPrintable.decode "abc\r\ndef\nghi" = "abc\r\ndef\nghi" &
  QuotedPrintable.decode " abc\r\n def\n ghi" = " abc\r\n def\n ghi" &
  QuotedPrintable.decode "abc=20\r\n def\nghi=20" = "abc \r\n def\nghi " &
  QuotedPrintable.decode "abc=\r\n def\nghi=20" = "abc def\nghi "
;;

(**********************************************************************)
(* Q                                                                  *)
(**********************************************************************)

let t200() =
  (* ENCODE. *)
  Q.encode "a %= 12345 &$[]\"" = "a=20=25=3D=2012345=20=26=24=5B=5D=22" &
  Q.encode "\000\001\002\r\n" = "=00=01=02=0D=0A"
;;


let t220() =
  (* DECODE. *)
  Q.decode "a=20=25=3D=2012345=20=26=24=5B=5D=22" = "a %= 12345 &$[]\"" &
  Q.decode "=00=01=02=0D=0A" = "\000\001\002\r\n" &
  Q.decode "a=20=25=3d=2012345=20=26=24=5b=5d=22" = "a %= 12345 &$[]\"" 
;;

(**********************************************************************)
(* Url                                                                *)
(**********************************************************************)

(* Already tested for Cgi *)

(**********************************************************************)
(* Html                                                               *)
(**********************************************************************)

let t300() =
  Html.encode_from_latin1 "<>&\"abcdefäöÜ\160\025'" = 
    "&lt;&gt;&amp;&quot;abcdef&auml;&ouml;&Uuml;&nbsp;&#25;'"
;;


let t320() =
  Html.decode_to_latin1 
    "&lt;&gt;&amp;&quot;abcdef&auml;&ouml;&Uuml;&nbsp;&#25;" =
    "<>&\"abcdefäöÜ\160\025" &
  Html.decode_to_latin1 "&apos;" = "'" &
  Html.decode_to_latin1 "&nonsense;" = "&nonsense;" &
  Html.decode_to_latin1 "&#256;" = "&#256;"
;;


(**********************************************************************)

let test f n =
  if f() then
    print_endline ("Test " ^ n ^ " ok")
  else 
    print_endline ("Test " ^ n ^ " FAILED!!!!");
  flush stdout
;;

test t001 "001";
test t002 "002";
test t003 "003";
test t004 "004";
test t005 "005";
test t006 "006";
test t007 "007";
test t008 "008";

test t020 "020";
test t021 "021";
test t022 "022";
test t023 "023";
test t024 "024";

test t100 "100";
test t120 "120";

test t200 "200";
test t220 "220";

test t300 "300";
test t320 "320";
