#use "topfind";;
#require "netstring";;

open Neturl;;

let details = false

let expect_malformed_url f =
  if details then print_string ".";
  try ignore(f()); false with Malformed_URL -> true;;

let works f =
  if details then print_string ".";
  not (expect_malformed_url f)
;;

(**********************************************************************)
(* extract_url_scheme                                                 *)
(**********************************************************************)

let t001 () =
  extract_url_scheme "a:bc" = "a" &&
  extract_url_scheme "A:bc" = "a" &&
  extract_url_scheme "a:b:c" = "a" &&
  extract_url_scheme "a+b-c:d:e" = "a+b-c"
;;


let t002 () =
  let test s =
    try ignore(extract_url_scheme s); false with Malformed_URL -> true
  in
  test "a" &&
  test "a/b:c" &&
  test "%61:b" &&
  test "a%3ab"
;;

(**********************************************************************)
(* url_syntax                                                         *)
(**********************************************************************)

let hashtbl_for_all f h =
  let b = ref true in
  Hashtbl.iter
    (fun k v -> b := !b && f k v)
    h;
  !b
;;

let t010 () =
  url_syntax_is_valid null_url_syntax &&
  url_syntax_is_valid ip_url_syntax &&
  hashtbl_for_all
    (fun _ syn ->
       url_syntax_is_valid syn
    )
    common_url_syntax
;;

let t011 () =
  (* url_syntax_is_valid (partial_url_syntax null_url_syntax) && *)
  url_syntax_is_valid (partial_url_syntax ip_url_syntax)

(* The following no longer works, as there are schemes that don't
 * support partial URLs:
 *)
(*
 &&
  hashtbl_for_all
    (fun _ syn ->
       url_syntax_is_valid (partial_url_syntax syn)
    )
    common_url_syntax
*)
;;

let t012 () =
  let f = fun _ -> true in
  let syn =
    { url_enable_scheme    = Url_part_not_recognized;
      url_enable_user      = Url_part_required;
      url_enable_password  = Url_part_allowed;
      url_enable_host      = Url_part_required;
      url_enable_port      = Url_part_not_recognized;
      url_enable_path      = Url_part_required;
      url_enable_param     = Url_part_not_recognized;
      url_enable_query     = Url_part_not_recognized;
      url_enable_fragment  = Url_part_required;
      url_enable_other     = Url_part_not_recognized;
      url_accepts_8bits    = false;
      url_is_valid         = f;
      url_enable_user_param= Url_part_not_recognized;
      url_enable_relative  = true;
    } in
  let syn' = partial_url_syntax syn in
  
  (syn'.url_enable_scheme    = Url_part_not_recognized) &&
  (syn'.url_enable_user      = Url_part_allowed) &&
  (syn'.url_enable_password  = Url_part_allowed) &&
  (syn'.url_enable_host      = Url_part_allowed) &&
  (syn'.url_enable_port      = Url_part_not_recognized) &&
  (syn'.url_enable_path      = Url_part_allowed) &&
  (syn'.url_enable_param     = Url_part_not_recognized) &&
  (syn'.url_enable_query     = Url_part_not_recognized) &&
  (syn'.url_enable_fragment  = Url_part_allowed) &&
  (syn'.url_enable_other     = Url_part_not_recognized) &&
  (syn'.url_is_valid        == f) &&

  url_syntax_is_valid syn &&
  url_syntax_is_valid syn'
;;

(**********************************************************************)
(* make_url                                                           *)
(**********************************************************************)

let t020 () =
  (* Basic functionality: *)
  let http_syn = Hashtbl.find common_url_syntax "http" in

  let u1 = make_url
	     (* default: not encoded *)
	     ~scheme:"http"
	     ~user:"U"
	     ~password:"%()~$@"
	     ~host:"a.b.c"
	     ~port:81
	     ~path:["";"?";""]
	     http_syn in

  url_provides 
    ~scheme:true ~user:true ~password:true ~host:true ~port:true ~path:true 
    u1 &&

  not
    (url_provides
       ~scheme:true ~user:true ~password:true ~host:true ~port:true ~path:true 
       ~query:true u1) &&

  (url_syntax_of_url u1 == http_syn) &&

  (url_scheme   u1 = "http") &&
  (url_user     u1 = "U") &&
  (url_password u1 = "%()~$@") &&
  (url_host     u1 = "a.b.c") &&
  (url_port     u1 = 81) &&
  (url_path     u1 = ["";"?";""]) &&

  (url_user     ~encoded:true u1 = "U") &&
  (url_password ~encoded:true u1 = "%25%28%29%7E%24%40") &&
  (url_path     ~encoded:true u1 = ["";"%3F";""]) &&

  (string_of_url u1 = "http://U:%25%28%29%7E%24%40@a.b.c:81/%3F/")
;;


let t021 () =
  (* Basic functionality: *)
  let http_syn = Hashtbl.find common_url_syntax "http" in

  let u1 = make_url
	     ~encoded:true
	     ~scheme:"http"
	     ~user:"%55"
	     ~password:"%25()%7e$%40"
	     ~host:"a.b.c"
	     ~port:81
	     ~path:["";"%3F";""]
	     http_syn in

  url_provides 
    ~scheme:true ~user:true ~password:true ~host:true ~port:true ~path:true 
    u1 &&

  not
    (url_provides
       ~scheme:true ~user:true ~password:true ~host:true ~port:true ~path:true 
       ~query:true u1) &&

  (url_syntax_of_url u1 == http_syn) &&

  (url_scheme   u1 = "http") &&
  (url_user     u1 = "U") &&
  (url_password u1 = "%()~$@") &&
  (url_host     u1 = "a.b.c") &&
  (url_port     u1 = 81) &&
  (url_path     u1 = ["";"?";""]) &&

  (url_user     ~encoded:true u1 = "%55") &&
  (url_password ~encoded:true u1 = "%25()%7e$%40") &&
  (url_path     ~encoded:true u1 = ["";"%3F";""]) &&

  string_of_url u1 = "http://%55:%25()%7e$%40@a.b.c:81/%3F/"
;;


(* NEGATIVE TESTS *)

let t030 () =
  (* It is not possible to add a component which is not recognized *)
  let http_syn = Hashtbl.find common_url_syntax "http" in

  expect_malformed_url
    (fun () ->
       make_url
	 ~scheme:"http"
	 ~user:"U"
	 ~password:"%()~$@"
	 ~host:"a.b.c"
	 ~port:81
	 ~path:["";"?";""]
	 ~fragment:"abc"
	 http_syn)
;;


let t031 () =
  (* It is not possible to put malformed '%'-encodings into the URL *)
  let http_syn = Hashtbl.find common_url_syntax "http" in

  works                      (* reference *)
    (fun () ->
       make_url
	 ~encoded:true
	 ~scheme:"http"
	 ~user:"U"
	 ~password:"XX"
	 ~host:"a.b.c"
	 ~port:81
	 ~path:["";"a";""]
	 http_syn) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~encoded:true
	 ~scheme:"http"
	 ~user:"U"
	 ~password:"%XX"
	 ~host:"a.b.c"
	 ~port:81
	 ~path:["";"a";""]
	 http_syn) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~encoded:true
	 ~scheme:"http"
	 ~user:"U"
	 ~password:"%X"
	 ~host:"a.b.c"
	 ~port:81
	 ~path:["";"a";""]
	 http_syn) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~encoded:true
	 ~scheme:"http"
	 ~user:"U"
	 ~password:"%"
	 ~host:"a.b.c"
	 ~port:81
	 ~path:["";"a";""]
	 http_syn) 
;;

let t032 () =
  (* It is not possible to put unsafe characters into the URL *)
  let http_syn = Hashtbl.find common_url_syntax "http" in

  let make c =
    make_url
      ~encoded:true
      ~scheme:"http"
      ~user:"U"
      ~password:(String.make 1 c)
      ~host:"a.b.c"
      ~port:81
      ~path:["";"a";""]
      http_syn
  in

  works (fun () -> make 'a') &&                   (* reference *)

  (* List of unsafe characters taken from RFC1738: *)
  expect_malformed_url (fun () -> make '<') && 
  expect_malformed_url (fun () -> make '>') && 
  expect_malformed_url (fun () -> make '"') && 
  expect_malformed_url (fun () -> make '#') && 
    (* Note: '#' would be considered as reserved if fragments were enabled *)
  expect_malformed_url (fun () -> make '%') && 
  expect_malformed_url (fun () -> make '{') && 
  expect_malformed_url (fun () -> make '}') && 
  expect_malformed_url (fun () -> make '|') && 
  expect_malformed_url (fun () -> make '\\') && 
  expect_malformed_url (fun () -> make '^') && 
  expect_malformed_url (fun () -> make '[') && 
  expect_malformed_url (fun () -> make ']') && 
  expect_malformed_url (fun () -> make '`') &&
  expect_malformed_url (fun () -> make '~') &&
    (* Note: '~' is considered as safe in paths: *)
  works 
    (fun () ->
    make_url
      ~encoded:true
      ~scheme:"http"
      ~user:"U"
      ~password:"a"
      ~host:"a.b.c"
      ~port:81
      ~path:["";"~";""]
      http_syn)
;;

let t033 () =
  (* It is not possible to put reserved characters into the URL *)
  let http_syn = Hashtbl.find common_url_syntax "http" in

  let make_password c =
    make_url
      ~encoded:true
      ~scheme:"http"
      ~user:"U"
      ~password:(String.make 1 c)
      ~host:"a.b.c"
      ~port:81
      ~path:["";"a";""]
      http_syn
  in
  let make_path c =
    make_url
      ~encoded:true
      ~scheme:"http"
      ~user:"U"
      ~password:"a"
      ~host:"a.b.c"
      ~port:81
      ~path:["";String.make 1 c;""]
      http_syn
  in
  let make_query c =
    make_url
      ~encoded:true
      ~scheme:"http"
      ~user:"U"
      ~password:"a"
      ~host:"a.b.c"
      ~port:81
      ~path:["";"a";""]
      ~query:(String.make 1 c)
      http_syn
  in

  (* Note: There is a difference between RFC 1738 and RFC 1808 regarding
   * which characters are reserved. RFC 1808 defines a fixed set of characters
   * as reserved while RFC 1738 defines the reserved characters depending
   * on the scheme.
   * This implementation of URLs follows RFC 1738 (because of practical
   * reasons).
   *)

  works (fun () -> make_password 'a') &&                   (* reference *)
  works (fun () -> make_path 'a') &&
  works (fun () -> make_query 'a') &&

  expect_malformed_url (fun () -> make_password ':') && 
  expect_malformed_url (fun () -> make_password '@') && 
  expect_malformed_url (fun () -> make_password '/') && 
  expect_malformed_url (fun () -> make_password '?') && (* Change in 0.98 *)
  works                (fun () -> make_password ';') &&
  works                (fun () -> make_password '=') &&
  works                (fun () -> make_password '&') &&

  (* Note: ';' is allowed in path and query because parameters are not
   * recognized in HTTP syntax.
   *)

  expect_malformed_url (fun () -> make_path '/') && 
  expect_malformed_url (fun () -> make_path '?') && 
  works                (fun () -> make_path ':') && 
  works                (fun () -> make_path '@') && 
  works                (fun () -> make_path ';') && 
  works                (fun () -> make_path '=') && 
  works                (fun () -> make_path '&') && 

  expect_malformed_url (fun () -> make_query '?') && 
  works                (fun () -> make_query '/') && 
  works                (fun () -> make_query ':') && 
  works                (fun () -> make_query '@') && 
  works                (fun () -> make_query ';') && 
  works                (fun () -> make_query '=') && 
  works                (fun () -> make_query '&')
;;


let t034 () =
  (* It is not possible to create a URL with a password, but without user;
   * and neither to create a URL with a port, but without host;
   * and neither to create a URL with a user, but without host
   *)

  expect_malformed_url
    (fun () ->
       make_url
	 ~scheme:"http"
	 ~password:"a"
	 ~host:"a.b.c"
	 ~path:["";"a";""]
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~scheme:"http"
	 ~user:"U"
	 ~path:["";"a";""]
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~scheme:"http"
	 ~port:81
	 ~path:["";"a";""]
	 ip_url_syntax)
;;


let t035 () =
  (* It is not possible to create a URL with illegal scheme prefix *)
  
  (* reference: *)
  works
    (fun () ->
       make_url
	 ~scheme:"a"
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~scheme:":"
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~scheme:"a=b"
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~scheme:"a%62b"
	 ip_url_syntax) &&
 
  expect_malformed_url
    (fun () ->
       make_url
	 ~scheme:"a&b"
	 ip_url_syntax)
;;


let t036 () =
  (* It is not possible to have a path that begins with double slashes *)
  
  (* reference: *)
  works
    (fun () ->
       make_url
	 ~path:["";"a";""]
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~path:["";""]
	 ip_url_syntax) &&

  works
    (fun () ->
       make_url
	 ~path:["a";"";""]
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~path:["";"";"a"]
	 ip_url_syntax) &&

  works
    (fun () ->
       make_url
	 ~path:["a";"";"a"]
	 ip_url_syntax)
;;


let t037 () =
  (* It is not possible to have port numbers outside 0..65535 *)
  
  (* reference: *)
  works
    (fun () ->
       make_url
	 ~host:"a"
	 ~port:1
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~host:"a"
	 ~port:(-1)
	 ip_url_syntax) &&

  expect_malformed_url
    (fun () ->
       make_url
	 ~host:"a"
	 ~port:65536
	 ip_url_syntax)
;;


let t038 () =
  (* Several cases which are not allowed. *)
  
  expect_malformed_url
    (fun () ->
       make_url
	 ~host:"a"
	 ~path:["a"]
	 ip_url_syntax
    ) &&                       (* illegal: host + relative path *)

  expect_malformed_url
    (fun () ->
       make_url
	 ~host:"a"
	 ~path:[]
	 ~param:["x"]
	 ip_url_syntax
    ) (* && *)                 (* illegal: host + no path + params *)

(* Now legal (RFC 2396):

  expect_malformed_url
    (fun () ->
       make_url
	 ~host:"a"
	 ~path:[]
	 ~query:"x"
	 ip_url_syntax
    )                          (* illegal: host + no path + query *)

*)
;;

(**********************************************************************)
(* url_of_string                                                      *)
(**********************************************************************)

let t050 () =
  (* absolute URLs with ip_url_syntax *)
  let identical s =
    string_of_url (url_of_string ip_url_syntax s) = s in

  let fails s =
    try ignore(url_of_string ip_url_syntax s); false 
    with Malformed_URL -> true
  in

  identical "http:" &&

  identical "http://host" &&
  identical "http://user@host" &&
  identical "http://user:password@host" &&
  identical "http://user@host:99" &&
  identical "http://user:password@host:99" &&

  identical "http://host/" &&
  identical "http://user@host/" &&
  identical "http://user:password@host/" &&
  identical "http://user@host:99/" &&
  identical "http://user:password@host:99/" &&

  identical "http://host/a/b" &&
  identical "http://user@host/a/b" &&
  identical "http://user:password@host/a/b" &&
  identical "http://user@host:99/a/b" &&
  identical "http://user:password@host:99/a/b" &&

  identical "http://host/a/b/" &&
  identical "http://user@host/a/b/" &&
  identical "http://user:password@host/a/b/" &&
  identical "http://user@host:99/a/b/" &&
  identical "http://user:password@host:99/a/b/" &&

  identical "http://host/?a=b&c=d" &&
  identical "http://user@host/?a=b&c=d" &&
  identical "http://user:password@host/?a=b&c=d" &&
  identical "http://user@host:99/?a=b&c=d" &&
  identical "http://user:password@host:99/?a=b&c=d" &&

  (* The following changed in 0.98: *)
  identical "http://host?a=b&c=d" &&
  identical "http://user@host?a=b&c=d" &&
  identical "http://user:password@host?a=b&c=d" &&
  identical "http://user@host:99?a=b&c=d" &&
  identical "http://user:password@host:99?a=b&c=d" &&

  identical "http://host/?a=/&c=/" &&
  identical "http://user@host/?a=/&c=/" &&
  identical "http://user:password@host/?a=/&c=/" &&
  identical "http://user@host:99/?a=/&c=/" &&
  identical "http://user:password@host:99/?a=/&c=/" &&

  identical "http://host/;a;b" &&
  identical "http://user@host/;a;b" &&
  identical "http://user:password@host/;a;b" &&
  identical "http://user@host:99/;a;b" &&
  identical "http://user:password@host:99/;a;b" &&

  fails "http://host;a;b" &&
  fails "http://user@host;a;b" &&
  fails "http://user:password@host;a;b" &&
  fails "http://user@host:99;a;b" &&
  fails "http://user:password@host:99;a;b" &&

  identical "http://host/;a;b?a=b&c=d" &&
  identical "http://user@host/;a;b?a=b&c=d" &&
  identical "http://user:password@host/;a;b?a=b&c=d" &&
  identical "http://user@host:99/;a;b?a=b&c=d" &&
  identical "http://user:password@host:99/;a;b?a=b&c=d" &&

  identical "http:#f" &&

  identical "http://host#f" &&
  identical "http://user@host#f" &&
  identical "http://user:password@host#f" &&
  identical "http://user@host:99#f" &&
  identical "http://user:password@host:99#f" &&

  identical "http://host/;a;b?a=b&c=d#f" &&
  identical "http://user@host/;a;b?a=b&c=d#f" &&
  identical "http://user:password@host/;a;b?a=b&c=d#f" &&
  identical "http://user@host:99/;a;b?a=b&c=d#f" &&
  identical "http://user:password@host:99/;a;b?a=b&c=d#f" &&

  true
;;


let t051 () =
  (* relative URLs with ip_url_syntax *)
  let identical s =
    string_of_url (url_of_string ip_url_syntax s) = s in

(*
  let fails s =
    try ignore(url_of_string ip_url_syntax s); false 
    with Malformed_URL -> true
  in
 *)

  identical "//host" &&
  identical "//user@host" &&
  identical "//user:password@host" &&
  identical "//user@host:99" &&
  identical "//user:password@host:99" &&

  identical "//host/" &&
  identical "//user@host/" &&
  identical "//user:password@host/" &&
  identical "//user@host:99/" &&
  identical "//user:password@host:99/" &&

  identical "//host#f" &&
  identical "//user@host#f" &&
  identical "//user:password@host#f" &&
  identical "//user@host:99#f" &&
  identical "//user:password@host:99#f" &&

  identical "/" &&
  identical "/a" &&
  identical "/a/" &&
  identical "/a/a" &&

  identical "/;a;b" &&
  identical "/a;a;b" &&
  identical "/a/;a;b" &&
  identical "/a/a;a;b" &&

  identical "/?a=b&c=d" &&
  identical "/a?a=b&c=d" &&
  identical "/a/?a=b&c=d" &&
  identical "/a/a?a=b&c=d" &&

  identical "/;a;b?a=b&c=d" &&
  identical "/a;a;b?a=b&c=d" &&
  identical "/a/;a;b?a=b&c=d" &&
  identical "/a/a;a;b?a=b&c=d" &&

  identical "/#f" &&
  identical "/a#f" &&
  identical "/a/#f" &&
  identical "/a/a#f" &&

  identical "/;a;b#f" &&
  identical "/a;a;b#f" &&
  identical "/a/;a;b#f" &&
  identical "/a/a;a;b#f" &&

  identical "/;a;b?a=b&c=d#f" &&
  identical "/a;a;b?a=b&c=d#f" &&
  identical "/a/;a;b?a=b&c=d#f" &&
  identical "/a/a;a;b?a=b&c=d#f" &&

  identical "" &&
  identical "a" &&
  identical "a/" &&
  identical "a/a" &&

  identical ";a;b" &&
  identical "a;a;b" &&
  identical "a/;a;b" &&
  identical "a/a;a;b" &&

  identical "?a=b&c=d" &&
  identical "a?a=b&c=d" &&
  identical "a/?a=b&c=d" &&
  identical "a/a?a=b&c=d" &&

  identical ";a;b?a=b&c=d" &&
  identical "a;a;b?a=b&c=d" &&
  identical "a/;a;b?a=b&c=d" &&
  identical "a/a;a;b?a=b&c=d" &&

  identical "#f" &&
  identical "a#f" &&
  identical "a/#f" &&
  identical "a/a#f" &&

  identical ";a;b#f" &&
  identical "a;a;b#f" &&
  identical "a/;a;b#f" &&
  identical "a/a;a;b#f" &&

  identical ";a;b?a=b&c=d#f" &&
  identical "a;a;b?a=b&c=d#f" &&
  identical "a/;a;b?a=b&c=d#f" &&
  identical "a/a;a;b?a=b&c=d#f" &&

  identical "." &&
  identical "./" &&
  identical "./a" &&

  identical ".;a;b" &&
  identical "./;a;b" &&
  identical "./a;a;b" &&

  identical ".?a=b&c=d" &&
  identical "./?a=b&c=d" &&
  identical "./a?a=b&c=d" &&

  identical ".;a;b?a=b&c=d" &&
  identical "./;a;b?a=b&c=d" &&
  identical "./a;a;b?a=b&c=d" &&

  identical ".#f" &&
  identical "./#f" &&
  identical "./a#f" &&

  identical ".;a;b#f" &&
  identical "./;a;b#f" &&
  identical "./a;a;b#f" &&

  identical ".;a;b?a=b&c=d#f" &&
  identical "./;a;b?a=b&c=d#f" &&
  identical "./a;a;b?a=b&c=d#f" &&

  identical ".." &&
  identical "../" &&
  identical "../a" &&

  identical "..;a;b" &&
  identical "../;a;b" &&
  identical "../a;a;b" &&

  identical "..?a=b&c=d" &&
  identical "../?a=b&c=d" &&
  identical "../a?a=b&c=d" &&

  identical "..;a;b?a=b&c=d" &&
  identical "../;a;b?a=b&c=d" &&
  identical "../a;a;b?a=b&c=d" &&

  identical "..#f" &&
  identical "../#f" &&
  identical "../a#f" &&

  identical "..;a;b#f" &&
  identical "../;a;b#f" &&
  identical "../a;a;b#f" &&

  identical "..;a;b?a=b&c=d#f" &&
  identical "../;a;b?a=b&c=d#f" &&
  identical "../a;a;b?a=b&c=d#f" &&

  string_of_url
    (make_url ~path:["a:b"] ip_url_syntax) = "a%3Ab" &&

  string_of_url
    (make_url ~encoded:true ~path:["a:b"] ip_url_syntax) = "./a:b" &&

  true
;;


let t052 () =
  (* mailto: URLs *)
  let mailto_syn = Hashtbl.find common_url_syntax "mailto" in

  let identical s =
    string_of_url (url_of_string mailto_syn s) = s in

  let fails s =
    try ignore(url_of_string mailto_syn s); false 
    with Malformed_URL -> true
  in

  identical "mailto:user@host" &&
  identical "mailto:user@host;?;?" &&
  fails     "mailto:user@host#f"
;;

(**********************************************************************)
(* split_path/join_path/norm_path:                                    *)
(**********************************************************************)

let t060 () =
  (split_path "" = []) &&
  (split_path "/" = [ "" ]) &&
  (split_path "/a" = [ ""; "a" ]) &&
  (split_path "a" = [ "a" ]) &&
  (split_path "a/" = [ "a"; "" ]) &&
  (split_path "/a/" = [ ""; "a"; "" ]) &&
  (split_path "/a/b" = [ ""; "a"; "b" ]) &&
  (split_path "/a/b/" = [ ""; "a"; "b"; "" ]) &&
  (split_path "/a/b/c" = [ ""; "a"; "b"; "c" ]) &&

  (join_path [] = "") &&
  (join_path [ "" ] = "/") &&
  (join_path [ ""; "a" ] = "/a") &&
  (join_path [ "a" ] = "a") &&
  (join_path [ "a"; "" ] = "a/") &&
  (join_path [ ""; "a"; "" ] = "/a/") &&
  (join_path [ ""; "a"; "b" ] = "/a/b") &&
  (join_path [ ""; "a"; "b"; "" ] = "/a/b/") &&
  (join_path [ ""; "a"; "b"; "c" ] = "/a/b/c") &&

  true
;;


let t061 () =
  (norm_path ["."] = []) &&
  (norm_path ["."; ""] = []) &&
  (norm_path ["a"; "."] = ["a"; ""]) &&
  (norm_path ["a"; "b"; "."] = ["a"; "b"; ""]) &&
  (norm_path ["a"; "b"; ".."] = ["a"; ""]) &&
  (norm_path ["a"; "."; "b"; "."] = ["a"; "b"; ""]) &&
  (norm_path [".."] = [".."; ""]) &&
  (norm_path [".."; ""] = [".."; ""]) &&
  (norm_path ["a"; "b"; ".."; "c" ] = ["a"; "c"]) &&
  (norm_path ["a"; "b"; ".."; "c"; ""] = ["a"; "c"; ""]) &&
  (norm_path ["";"";"a";"";"b"] = [""; "a"; "b"]) &&
  (norm_path ["a"; "b"; ""; ".."; "c"; ""] = ["a"; "c"; ""]) &&
  (norm_path ["a"; ".."] = []) &&
  (norm_path ["";""] = [""]) &&
  (norm_path [""] = [""]) &&
  (norm_path [] = []) &&

  true
;;
		  
(**********************************************************************)
(* apply_relative_url:                                                *)
(**********************************************************************)

let url_eq u1 u2 =
  let get f arg =
    try Some(f arg) with Not_found -> None in
  get url_scheme u1 = get url_scheme u2 &&
  get url_user u1 = get url_user u2 &&
  get url_user_param u1 = get url_user_param u2 &&
  get url_password u1 = get url_password u2 &&
  get url_host u1 = get url_host u2 &&
  get url_port u1 = get url_port u2 &&
  get url_path u1 = get url_path u2 &&
  get url_param u1 = get url_param u2 &&
  get url_query u1 = get url_query u2 &&
  get url_fragment u1 = get url_fragment u2 &&
  get url_other u1 = get url_other u2

let ( <=> ) = url_eq


let t070() =
  (* Examples taken from RFC 1808 *)
  let url = url_of_string ip_url_syntax in
  let base = url "http://a/b/c/d;p?q#f" in
  let aru = apply_relative_url base in

  (aru (url "g:h")     <=> url "g:h") &&
  (aru (url "g")       <=> url "http://a/b/c/g") &&
  (aru (url "./g")     <=> url "http://a/b/c/g") &&
  (aru (url "g/")      <=> url "http://a/b/c/g/") &&
  (aru (url "/g")      <=> url "http://a/g") &&
  (aru (url "//g")     <=> url "http://g") &&
  (aru (url "?y")      <=> url "http://a/b/c/d;p?y") &&
  (aru (url "g?y")     <=> url "http://a/b/c/g?y") &&
  (aru (url "g?y/./x") <=> url "http://a/b/c/g?y/./x") &&
  (aru (url "#s")      <=> url "http://a/b/c/d;p?q#s") &&
  (aru (url "g#s")     <=> url "http://a/b/c/g#s") &&
  (aru (url "g#s/./x") <=> url "http://a/b/c/g#s/./x") &&
  (aru (url "g?y#s")   <=> url "http://a/b/c/g?y#s") &&
  (aru (url ";x")      <=> url "http://a/b/c/d;x") &&
  (aru (url "g;x")     <=> url "http://a/b/c/g;x") &&
  (aru (url "g;x?y#s") <=> url "http://a/b/c/g;x?y#s") &&
  (aru (url ".")       <=> url "http://a/b/c/") &&
  (aru (url "./")      <=> url "http://a/b/c/") &&
  (aru (url "..")      <=> url "http://a/b/") &&
  (aru (url "../")     <=> url "http://a/b/") &&
  (aru (url "../g")    <=> url "http://a/b/g") &&
  (aru (url "../..")   <=> url "http://a/") &&
  (aru (url "../../")  <=> url "http://a/") &&
  (aru (url "../../g") <=> url "http://a/g") &&

  (aru (url "")              <=> url "http://a/b/c/d;p?q#f") &&
  (aru (url "../../../g")    <=> url "http://a/../g") &&
  (aru (url "../../../../g") <=> url "http://a/../../g") &&
  (aru (url "/./g")          <=> url "http://a/./g") &&
  (aru (url "/../g")         <=> url "http://a/../g") &&
  (aru (url "g.")            <=> url "http://a/b/c/g.") &&
  (aru (url ".g")            <=> url "http://a/b/c/.g") &&
  (aru (url "g..")           <=> url "http://a/b/c/g..") &&
  (aru (url "..g")           <=> url "http://a/b/c/..g") &&
  (aru (url "./../g")        <=> url "http://a/b/g") &&
  (aru (url "./g/.")         <=> url "http://a/b/c/g/") &&
  (aru (url "g/./h")         <=> url "http://a/b/c/g/h") &&
  (aru (url "g/../h")        <=> url "http://a/b/c/h") &&
  (aru (url "http:g")        <=> url "http:g") &&
  (aru (url "http:")         <=> url "http:") &&

  true
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

test t010 "010";
test t011 "011";
test t012 "012";

test t020 "020";
test t021 "021";

test t030 "030";
test t031 "031";
test t032 "032";
test t033 "033";
test t034 "034";
test t035 "035";
test t036 "036";
test t037 "037";
test t038 "038";

test t050 "050";
test t051 "051";
test t052 "052";

test t060 "060";
test t061 "061";

test t070 "070";
()
;;
