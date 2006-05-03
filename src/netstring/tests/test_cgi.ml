#use "topfind";;
#require "pcre";;
#require "unix";;
#directory "..";;
#load "netstring.cma";;


open Cgi;;

(**********************************************************************)
(* dest_form_encoded_parameters                                       *)
(**********************************************************************)

let t001 f =
  let r =
    f
      "blah blah
--snip
Content-Disposition: form-data; name=blupp

This is a text
--snip--
blah blah"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text"]
;;


let t002 f =
  let r =
    f
      "blah blah
--snip
Content-Disposition: form-data; name=blupp

This is a text
--snip--
blah blah"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text"]
;;


let t003 f =
  let r =
    f
      "--snip
Content-Disposition: form-data; name=blupp

This is a text
--snip--"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text"]
;;


let t004 f =
  let r =
    f
      "--snip
Content-Disposition: form-data; name=blupp

This is a text

--snip--"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text\013\n"]
;;


let t005 f =
  let r =
    f
      "--snip
Content-Disposition: form-data; name=blupp

This is a text

--snip--"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text\n"]
;;


let t006 f =
  let r =
    f
      "blah blah
--snip
Content-Disposition: form-data;name= \"blupp\"

This is a text
--snip--
blah blah"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text"]
;;


let t007 f =
  let r =
    f
      "blah blah
--snip
Content-Disposition: form-data;name= \"name=blupp\"

This is a text
--snip--
blah blah"
      "snip"
  in
  r = ["name=blupp", "text/plain", "This is a text"]
;;


let t008 f =
  let r =
    f
      "blah blah
--snip
Content-Disposition: form-data; strange=\"name=blop\"; name= \"blupp\"

This is a text
--snip--
blah blah"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text"]
;;


let t009 f =
  let r =
    f
      "blah blah
--snip
Content-Disposition: form-data; strange=\" name=blop \";  name=blupp

This is a text
--snip--
blah blah"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text"]
;;


let t010 f =
  (* There is a space after "octet-stream"! *)
  let r =
    f
      "--snip
Content-Disposition: form-data; name=blupp
Content-type:  application/octet-stream

This is a text
--snip--"
      "snip"
  in
  r = ["blupp", "application/octet-stream", "This is a text"]
;;


let t011 f =
  let r =
    f
      "blah blah
--snip
Content-Disposition: form-data; name=blupp

This is a text
--snip
Content-Disposition: form-data; name=blipp

Another line
--snip-- blah
blah blah"
      "snip"
  in
  r = ["blupp", "text/plain", "This is a text";
       "blipp", "text/plain", "Another line" ]
;;


let t012 f =
  (* A real example *)
   let r =
     f
"-----------------------------10843891265508332411092264958
Content-Disposition: form-data; name=\"line\"

aaa
-----------------------------10843891265508332411092264958
Content-Disposition: form-data; name=\"submit\"

Submit
-----------------------------10843891265508332411092264958--
"
      "---------------------------10843891265508332411092264958"
   in
   r = [ "line", "text/plain", "aaa";
	 "submit", "text/plain", "Submit";
       ]
;;


(**********************************************************************)
(* encode/decode                                                      *)
(**********************************************************************)

let t100() =
  let s = String.create 256 in
  for i = 0 to 255 do s.[i] <- Char.chr i done;
  let r = encode s in
  r = ("%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F" ^
       "%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F" ^
       "+!%22%23$%25%26'()*%2B,-.%2F" ^
       "0123456789%3A%3B%3C%3D%3E%3F" ^
       "%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_" ^
       "%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D%7E%7F" ^
       "%80%81%82%83%84%85%86%87%88%89%8A%8B%8C%8D%8E%8F" ^
       "%90%91%92%93%94%95%96%97%98%99%9A%9B%9C%9D%9E%9F" ^
       "%A0%A1%A2%A3%A4%A5%A6%A7%A8%A9%AA%AB%AC%AD%AE%AF" ^
       "%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA%BB%BC%BD%BE%BF" ^
       "%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD%CE%CF" ^
       "%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF" ^
       "%E0%E1%E2%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF" ^
       "%F0%F1%F2%F3%F4%F5%F6%F7%F8%F9%FA%FB%FC%FD%FE%FF")
;;


let t101() =
  let r = String.create 256 in
  for i = 0 to 255 do r.[i] <- Char.chr i done;
  let s = decode
	    ("%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F" ^
	     "%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F" ^
	     "+!%22%23$%25%26'()*%2B,-.%2F" ^
	     "0123456789%3A%3B%3C%3D%3E%3F" ^
	     "%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_" ^
	     "%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D%7E%7F" ^
	     "%80%81%82%83%84%85%86%87%88%89%8A%8B%8C%8D%8E%8F" ^
	     "%90%91%92%93%94%95%96%97%98%99%9A%9B%9C%9D%9E%9F" ^
	     "%A0%A1%A2%A3%A4%A5%A6%A7%A8%A9%AA%AB%AC%AD%AE%AF" ^
	     "%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA%BB%BC%BD%BE%BF" ^
	     "%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD%CE%CF" ^
	     "%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF" ^
	     "%E0%E1%E2%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF" ^
	     "%F0%F1%F2%F3%F4%F5%F6%F7%F8%F9%FA%FB%FC%FD%FE%FF") in
  r = s
;;


let t102() =
  let r = String.create 256 in
  for i = 0 to 255 do r.[i] <- Char.chr i done;
  let s = decode
	    ((String.lowercase
		("%00%01%02%03%04%05%06%07%08%09%0A%0B%0C%0D%0E%0F" ^
		 "%10%11%12%13%14%15%16%17%18%19%1A%1B%1C%1D%1E%1F" ^
		 "+!%22%23$%25%26'()*%2B,-.%2F" ^
		 "0123456789%3A%3B%3C%3D%3E%3F")) ^
	     "%40ABCDEFGHIJKLMNOPQRSTUVWXYZ%5B%5C%5D%5E_" ^
	     (String.lowercase
		("%60abcdefghijklmnopqrstuvwxyz%7B%7C%7D%7E%7F" ^
		 "%80%81%82%83%84%85%86%87%88%89%8A%8B%8C%8D%8E%8F" ^
		 "%90%91%92%93%94%95%96%97%98%99%9A%9B%9C%9D%9E%9F" ^
		 "%A0%A1%A2%A3%A4%A5%A6%A7%A8%A9%AA%AB%AC%AD%AE%AF" ^
		 "%B0%B1%B2%B3%B4%B5%B6%B7%B8%B9%BA%BB%BC%BD%BE%BF" ^
		 "%C0%C1%C2%C3%C4%C5%C6%C7%C8%C9%CA%CB%CC%CD%CE%CF" ^
		 "%D0%D1%D2%D3%D4%D5%D6%D7%D8%D9%DA%DB%DC%DD%DE%DF" ^
		 "%E0%E1%E2%E3%E4%E5%E6%E7%E8%E9%EA%EB%EC%ED%EE%EF" ^
		 "%F0%F1%F2%F3%F4%F5%F6%F7%F8%F9%FA%FB%FC%FD%FE%FF"))) in
  r = s
;;

(**********************************************************************)
(* dest_url_encoded_parameters                                        *)
(**********************************************************************)

let t200() =
  let r = dest_url_encoded_parameters "a=b&c=d" in
  r = ["a", "b"; "c", "d" ]
;;


let t201() =
  let r = dest_url_encoded_parameters "a=&c=d" in
  r = ["a", ""; "c", "d" ]
;;


let t202() =
  let r = dest_url_encoded_parameters "a=&c=" in
  r = ["a", ""; "c", "" ]
;;


let t203() =
  let r = dest_url_encoded_parameters "" in
  r = []
;;


let t204() =
  let r = dest_url_encoded_parameters "%41=%42" in
  r = ["A", "B"]
;;


(**********************************************************************)

let test f n =
  if f() then
    print_endline ("Test " ^ n ^ " ok")
  else
    print_endline ("Test " ^ n ^ " FAILED!!!!");
  flush stdout
;;


let test_dest_form_encoded_parameters f n =
  let dest s b =
    let args = dest_form_encoded_parameters s b default_config in
    List.map
      (fun a -> arg_name a, arg_mimetype a, arg_value a)
      args
  in
  if f dest then
    print_endline ("Test dest_form_encoded_parameters " ^ n ^ " ok")
  else
    print_endline ("Test dest_form_encoded_parameters " ^ n ^ " FAILED!!!!");
  flush stdout
;;


let fill_stream s =
  (* Returns a channel that reads from string s.
   * This requires forking.
   *)
  let rd, wr = Unix.pipe() in
  let pid = Unix.fork() in
  if pid = 0 then begin
    Unix.close rd;
    let out = Unix.out_channel_of_descr wr in
    output_string out s;
    close_out out;
    exit(0);
  end;
  Unix.close wr;
  Unix.in_channel_of_descr rd
;;


let test_dest_form_encoded_parameters_from_netstream f n =
  let dest s b =
    let fd = fill_stream s in
    let bs = String.length b * 2 in
    let stream = new Netstream.input_stream
		   ~block_size:bs
		   (new Netchannels.input_channel fd) in

    let args = dest_form_encoded_parameters_from_netstream
		 stream b default_config in

(*
    List.iter
      (fun a ->
	 Printf.printf "name=%s mimetype=%s value=%s\n"
	   (arg_name a) (arg_mimetype a) (arg_value a))
      args;
*)
    List.map
      (fun a -> arg_name a, arg_mimetype a, arg_value a)
      args
  in
  if f dest then
    Printf.printf
      "Test dest_form_encoded_parameters_from_netstream %s ok\n"
      n
  else
    print_endline ("Test dest_form_encoded_parameters_from_netstream " ^ n ^ " FAILED!!!!");
  flush stdout
;;



test_dest_form_encoded_parameters t001 "001";;
test_dest_form_encoded_parameters t002 "002";;
test_dest_form_encoded_parameters t003 "003";;
test_dest_form_encoded_parameters t004 "004";;
test_dest_form_encoded_parameters t005 "005";;
test_dest_form_encoded_parameters t006 "006";;
test_dest_form_encoded_parameters t007 "007";;
test_dest_form_encoded_parameters t008 "008";;
test_dest_form_encoded_parameters t009 "009";;
test_dest_form_encoded_parameters t010 "010";;
test_dest_form_encoded_parameters t011 "011";;
test_dest_form_encoded_parameters t012 "012";;

test_dest_form_encoded_parameters_from_netstream t001 "001";;
test_dest_form_encoded_parameters_from_netstream t002 "002";;
test_dest_form_encoded_parameters_from_netstream t003 "003";;
test_dest_form_encoded_parameters_from_netstream t004 "004";;
test_dest_form_encoded_parameters_from_netstream t005 "005";;
test_dest_form_encoded_parameters_from_netstream t006 "006";;
test_dest_form_encoded_parameters_from_netstream t007 "007";;
test_dest_form_encoded_parameters_from_netstream t008 "008";;
test_dest_form_encoded_parameters_from_netstream t009 "009";;
test_dest_form_encoded_parameters_from_netstream t010 "010";;
test_dest_form_encoded_parameters_from_netstream t011 "011";;
test_dest_form_encoded_parameters_from_netstream t012 "012";;


test t100 "100";;
test t101 "101";;
test t102 "102";;

test t200 "200";;
test t201 "201";;
test t202 "202";;
test t203 "203";;
test t204 "204";;
