(* $Id$ *)

open Netcgi_env
open Netcgi_types
open Netcgi
open Netchannels
open Netmime
;;


let expect_equal ~printer ?(msg = "unknown") expected value =
  if expected = value then
    print_endline (msg ^ ": passed")
  else (
    print_endline (msg ^ ": failed");
    print_endline ("  Expected value: " ^ printer expected);
    print_endline ("  Found value:    " ^ printer value);
  );
  flush stdout
;;


let expect_true ?(msg = "unknown") value =
  if value then
    print_endline (msg ^ ": passed")
  else (
    print_endline (msg ^ ": failed");
    print_endline ("  Expected value: true");
    print_endline ("  Found value:    false");
  );
  flush stdout
;;


let expect_pass header f =
  print_endline ("*** Function " ^ header);  
  flush stdout;
  try
    f()
  with
      any ->
	print_endline("Exception: " ^ Printexc.to_string any);
	print_endline("Stopping this test!");
	flush stdout;
	raise any
;;


let expect_fail header f =
  print_endline ("*** Function " ^ header);
  flush stdout;
  try
    f();
    print_endline "This function is successful although it should fail!";
    flush stdout
  with
      any ->
	()
;;


let fail = failwith;;


let equal_str   = expect_equal ~printer:(fun s -> s);;
let equal_bool  = expect_equal ~printer:(function true -> "true" 
					   | false -> "false");;
let equal_int_opt = expect_equal ~printer:(function None -> "None" 
					   | Some n -> "Some" ^ string_of_int n);;



let make_env config =
  let e = new custom_environment ~config () in
  e # set_cgi 
    ~gateway_interface:"CGI/1.1"
    ~server_software:"SERVER/1"
    ~server_name:"testname"
    ~server_protocol:"HTTP/1.0"
    ~server_port:(Some 8080)
    ~request_method:"GET"
    ~path_info:"/additional/path"
    ~path_translated:"/serverroot/cgi/additional/path"
    ~script_name:"/test.cgi"
    ~remote_host:"client"
    ~remote_addr:"1.2.3.4"
    ();
  e # set_input_header_field "user-agent" "fort/test_netcgi";
  e
;;


expect_pass "initial custom environment"
begin fun() ->

  let e = make_env default_config in

  equal_str ~msg:"cgi1" "CGI/1.1" e#cgi_gateway_interface ;
  equal_str ~msg:"cgi2" "SERVER/1" e#cgi_server_software;
  equal_str ~msg:"cgi3" "testname" e#cgi_server_name;
  equal_str ~msg:"cgi4" "HTTP/1.0" e#cgi_server_protocol;
  equal_int_opt ~msg:"cgi5" (Some 8080) e#cgi_server_port;
  equal_str ~msg:"cgi6" "GET" e#cgi_request_method;
  equal_str ~msg:"cgi7" "/additional/path" e#cgi_path_info;
  equal_str ~msg:"cgi8" "/serverroot/cgi/additional/path" e#cgi_path_translated;
  equal_str ~msg:"cgi9" "/test.cgi" e#cgi_script_name;
  equal_str ~msg:"cgi10" "" e#cgi_query_string;
  equal_str ~msg:"cgi11" "client" e#cgi_remote_host;
  equal_str ~msg:"cgi12" "1.2.3.4" e#cgi_remote_addr;
  equal_str ~msg:"cgi13" "" e#cgi_auth_type;
  equal_str ~msg:"cgi14" "" e#cgi_remote_user;
  equal_str ~msg:"cgi15" "" e#cgi_remote_ident;

  equal_str ~msg:"cgi16" 
    "CGI/1.1" (e#cgi_property ~default:"xxx" "GATEWAY_INTERFACE");
  equal_str ~msg:"cgi17"
    "xxx" (e#cgi_property ~default:"xxx" "GATEWAY-INTERFACE");
  equal_str ~msg:"cgi18"
    "xxx" (e#cgi_property ~default:"xxx" "gateway_interface");
  equal_str ~msg:"cgi19"
    "xxx" (e#cgi_property ~default:"xxx" "gateway-interface");

  equal_bool ~msg:"cgi20" false e#cgi_https;

  expect_true ~msg:"protocol1" (e#protocol = `Http((1,0),[]));

  equal_str ~msg:"ihdr1"
    "fort/test_netcgi"
    (e#input_header_field ~default:"xxx" "user-agent");
  equal_str ~msg:"ihdr2"
    "fort/test_netcgi"
    (e#input_header_field ~default:"xxx" "USER-AGENT");
  equal_str ~msg:"ihdr3"
    "xxx"
    (e#input_header_field ~default:"xxx" "USER_AGENT");
  equal_str ~msg:"ihdr4"
    "xxx"
    (e#input_header_field ~default:"xxx" "user_agent");

  equal_str ~msg:"user_agent1" "fort/test_netcgi" e#user_agent;

  expect_true ~msg:"input_state1" (e#input_state = `Start);
  expect_true ~msg:"output_state1" (e#output_state = `Start);

  expect_true ~msg:"ohdr1" (e#output_header_fields = []);
end ;;


expect_pass "output header"
begin fun () ->
  let e = make_env default_config in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  equal_str ~msg:"ohdr1" "xxx" (e # output_header_field ~default:"xxx" "f1");
  
  e # set_output_header_field "f1" "a";
  e # set_multiple_output_header_field "f2" [ "b1"; "b2" ];

  equal_str ~msg:"ohdr2" "a"  (e # output_header_field ~default:"xxx" "f1");
  equal_str ~msg:"ohdr3" "b1" (e # output_header_field ~default:"xxx" "f2");

  expect_true ~msg:"ohdr4" 
    (e # multiple_output_header_field "f2" = ["b1";"b2"]);

  e # send_output_header();

  let expected_header =
    "f1: a\n" ^ 
    "f2: b1\n" ^ 
    "f2: b2\n" ^
    "\n" in
  (* The order is not specified, except that b1 is before b2.
   * The case of the field names is not specified, too.
   *)

  expect_true ~msg:"ostate1"
    (e#output_state = `Sent_header);
  equal_str ~msg:"full_ohdr" expected_header (Buffer.contents b);

  expect_true ~msg:"send_again"
    (try e # send_output_header(); false with Failure _ -> true);
end ;;


expect_pass "cgi_activation GET no-arguments direct"
begin fun () ->
  let e = make_env default_config in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation ~env:(e :> cgi_environment) () in

  expect_true ~msg:"method1" (cgi#request_method = `GET);
  expect_true ~msg:"iargs1" (cgi#initial_arguments = []);
  expect_true ~msg:"args1" (cgi#arguments = []);

  equal_str ~msg:"url1"
    "http://testname:8080/test.cgi/additional/path"
    (cgi # url());

  equal_str ~msg:"url2"
    "/test.cgi/additional/path"
    (cgi # url ~with_authority:`None ());

  equal_str ~msg:"url3"
    "http://testname:8080/test.cgi"
    (cgi # url ~with_path_info:`None ());

  equal_str ~msg:"url4"
    "http://testname:8080/test.cgi/additional/path"
    (cgi # url ~with_query_string:`Initial ());

  equal_str ~msg:"url5"
    "http://testname:8080/test.cgi/additional/path"
    (cgi # url ~with_query_string:`Current ());

  let x = new simple_argument "x" "a b" in
  equal_str ~msg:"url6"
    "http://testname:8080/test.cgi/additional/path?x=a+b"
    (cgi # url ~with_query_string:(`Args [x]) ());

  cgi # set_header();

  (* Because this is direct mode, the header is sent immediately: *)

  let expected_header =
    "Content-type: text/html\n\n" in

  equal_str ~msg:"full_ohdr1" 
    expected_header (Buffer.contents b);

  cgi # output # output_string "<html></html>";
  cgi # output # commit_work();                 (* flushes buffers if any *)

  equal_str ~msg:"full_ohdr2" 
    (expected_header ^ "<html></html>") (Buffer.contents b);

end ;;


let trans_test b (cgi : cgi_activation) =
  (* Common routine for the transaction tests *)

  cgi # set_header();

  (* Nothing is sent! *)

  equal_str ~msg:"trans1" "" (Buffer.contents b);

  cgi # set_header ~content_type:"text/plain" ();

  (* Nothing is sent! *)

  equal_str ~msg:"trans2" "" (Buffer.contents b);

  cgi # output # output_string "This is my first output";

  (* Nothing is sent! *)

  equal_str ~msg:"trans3" "" (Buffer.contents b);

  cgi # output # rollback_work();

  cgi # output # output_string "This is my second output";

  (* Nothing is sent! *)

  equal_str ~msg:"trans4" "" (Buffer.contents b);

  (* Now commit everything: *)

  cgi # output # commit_work();

  let expected_output =
    "Content-type: text/plain\n\nThis is my second output" in

  equal_str ~msg:"trans5" expected_output (Buffer.contents b);

  (* And append something: *)

  cgi # output # output_string " appendix1";
  cgi # output # rollback_work();
  cgi # output # output_string " appendix2";
  cgi # output # commit_work();

  let expected_output' = 
    expected_output ^ " appendix2" in

  equal_str ~msg:"trans6" expected_output' (Buffer.contents b)
;;


expect_pass "cgi_activation GET no-arguments transactional(buffer)"
begin fun () ->
  let e = make_env default_config in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment)
	      ~operating_type: buffered_transactional_optype
	      () in

  trans_test b cgi
end ;;


expect_pass "cgi_activation GET no-arguments transactional(tempfile)"
begin fun () ->
  let e = make_env default_config in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment)
	      ~operating_type: tempfile_transactional_optype
	      () in

  trans_test b cgi
end ;;


expect_pass "cgi_activation GET arguments no-output"
begin fun () ->
  let e = make_env default_config in
  e # set_cgi ~query_string:"a=%20&b=other" ();

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment) () in

  equal_str ~msg:"iarg1" " " (cgi # initial_argument_value "a");
  equal_str ~msg:"iarg2" "other" (cgi # initial_argument_value "b");

  let a = cgi#initial_argument "a" in
  let b = cgi#initial_argument "b" in

  equal_str ~msg:"iarg3" "a" a#name;
  equal_str ~msg:"iarg4" " " a#value;
  expect_true ~msg:"iarg5" (a#store = `Memory);
  equal_str ~msg:"iarg6" "text/plain" a#content_type;
  expect_true ~msg:"iarg7" (a#filename = None);
  expect_true ~msg:"iarg8" 
    (match a#representation with `Simple _ -> true | _ -> false);

  (* Modify value *)
  expect_true ~msg:"iarg9" 
    (try a # set_value "changed"; false with Immutable _ -> true);

  let ma = cgi#argument "a" in
  ma # set_value "changed";
  equal_str ~msg:"arg1" "changed" (cgi # argument_value "a");
  equal_str ~msg:"arg2" " " (cgi # initial_argument_value "a");

  (* Add a new value, and delete b *)

  cgi # update_argument (new simple_argument "c" "cvalue");
  cgi # delete_argument "b";

  equal_str ~msg:"url1"
    "?c=cvalue&a=changed"
    (cgi # url ~with_authority:`None ~with_script_name:`None
               ~with_path_info:`None ~with_query_string:`Current ());

  equal_str ~msg:"url2"
    "?a=+&b=other"
    (cgi # url ~with_authority:`None ~with_script_name:`None
               ~with_path_info:`None ~with_query_string:`Initial ());

end ;;


expect_pass "cgi_activation POST(urlencoded) arguments no-output"
begin fun () ->
  let e = make_env default_config in

  let msg = "a=%20&b=other" in
  e # set_input_ch (new input_string msg);

  e # set_input_content_length (String.length msg);
  e # set_input_content_type "application/x-www-form-urlencoded";
  e # set_cgi ~request_method:"POST" ();

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment) () in

  equal_str ~msg:"iarg1" " " (cgi # initial_argument_value "a");
  equal_str ~msg:"iarg2" "other" (cgi # initial_argument_value "b");

end ;;


expect_pass "cgi_activation POST(simple form-data) arguments no-output"
begin fun () ->
  let e = make_env default_config in

  let msg = 
    "--SNIP\n" ^ 
    "content-disposition: form-data; name=\"a\"\n" ^ 
    "\n" ^ 
    " \n" ^
    "--SNIP\n" ^ 
    "content-disposition: form-data; name=\"b\"\n" ^ 
    "\n" ^ 
    "other\n"^ 
    "--SNIP--\n"
  in
  e # set_input_ch (new input_string msg);

  e # set_input_content_length (String.length msg);
  e # set_input_content_type "multipart/form-data; boundary=SNIP";
  e # set_cgi ~request_method:"POST" ();

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment) () in

  equal_str ~msg:"iarg1" " " (cgi # initial_argument_value "a");
  equal_str ~msg:"iarg2" "other" (cgi # initial_argument_value "b");

end ;;


expect_pass "cgi_activation POST(form-data) arguments no-output"
begin fun () ->
  let e = make_env default_config in

  let msg = 
    "--SNIP\n" ^ 
    "content-disposition: form-data; name=\"a\"; filename=\"x\"\n" ^ 
    "content-type: r/s\n" ^ 
    "\n" ^ 
    " \n" ^
    "--SNIP\n" ^ 
    "content-disposition: form-data; name=\"b\"; filename=y\n" ^ 
    "content-type: image/gif\n" ^ 
    "\n" ^ 
    "other\n"^ 
    "--SNIP--\n"
  in
  e # set_input_ch (new input_string msg);

  e # set_input_content_length (String.length msg);
  e # set_input_content_type "multipart/form-data; boundary=SNIP";
  e # set_cgi ~request_method:"POST" ();

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment) () in

  equal_str ~msg:"iarg1" " " (cgi # initial_argument_value "a");
  equal_str ~msg:"iarg2" "other" (cgi # initial_argument_value "b");

  let a = cgi#initial_argument "a" in
  let b = cgi#initial_argument "b" in

  equal_str ~msg:"iarg3" "a" a#name;
  equal_str ~msg:"iarg4" " " a#value;
  expect_true ~msg:"iarg5" (a#store <> `Memory);
     (* `File because there is a "filename" parameter *)
  equal_str ~msg:"iarg6" "r/s" a#content_type;
  expect_true ~msg:"iarg7" (a#filename = Some "x");
  expect_true ~msg:"iarg8" 
    (match a#representation with `MIME _ -> true | _ -> false);

  equal_str ~msg:"iarg9" "b" b#name;
  equal_str ~msg:"iarg10" "other" b#value;
  expect_true ~msg:"iarg11" (b#store <> `Memory);
     (* `File because there is a "filename" parameter *)
  equal_str ~msg:"iarg12" "image/gif" b#content_type;
  expect_true ~msg:"iarg13" (b#filename = Some "y");
  expect_true ~msg:"iarg14" 
    (match b#representation with `MIME _ -> true | _ -> false);


end ;;


expect_pass "cgi_activation POST(backslash form-data) arguments no-output"
begin fun () ->
  let e = make_env default_config in

  let msg = 
    "--SNIP\n" ^ 
    "content-disposition: form-data; name=\"a\"; filename=C:\\filename\n" ^ 
    "content-type: r/s\n" ^ 
    "\n" ^ 
    "\\\n" ^
    "--SNIP\n" ^ 
    "content-disposition: form-data; name=\"b\"; filename=\"C:\\\\filename\"\n" ^ 
    "content-type: r/s\n" ^ 
    "\n" ^ 
    "\"\n" ^
    "--SNIP--\n"
  in
  e # set_input_ch (new input_string msg);

  e # set_input_content_length (String.length msg);
  e # set_input_content_type "multipart/form-data; boundary=SNIP";
  e # set_cgi ~request_method:"POST" ();

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment) () in

  equal_str ~msg:"iarg1" "\\" (cgi # initial_argument_value "a");

  let a = cgi#initial_argument "a" in
  let b = cgi#initial_argument "b" in

  equal_str ~msg:"iarg2" "a" a#name;
  equal_str ~msg:"iarg3" "\\" a#value;
  expect_true ~msg:"iarg4" (a#store <> `Memory);
  equal_str ~msg:"iarg5" "r/s" a#content_type;
  expect_true ~msg:"iarg6" (a#filename = Some "C:\\filename");
  expect_true ~msg:"iarg7" 
    (match a#representation with `MIME _ -> true | _ -> false);

  equal_str ~msg:"iarg8" "b" b#name;
  equal_str ~msg:"iarg9" "\"" b#value;
  expect_true ~msg:"iarg10" (b#store <> `Memory);
  equal_str ~msg:"iarg11" "r/s" b#content_type;
  expect_true ~msg:"iarg12" (b#filename = Some "C:\\\\filename");
    (* maybe we should return only one backslash here? *)
  expect_true ~msg:"iarg13" 
    (match b#representation with `MIME _ -> true | _ -> false);

  cgi # finalize();

end ;;


expect_pass "cgi_activation POST(bigtext form-data) arguments no-output"
begin fun () ->
  let e = make_env default_config in

  let line = String.make 79 'X' ^ "\n" in
  let value = String.make 800000 'x' in
  for k = 0 to 9999 do
    String.blit line 0 value (k*80) 80
  done;
  assert (try ignore(String.index value 'x');false with Not_found -> true);

  let msg = 
    "--SNIP\n" ^ 
    "content-disposition: form-data; name=\"a\"\n" ^ 
    "\n" ^ 
    value ^ "\n" ^ 
    "--SNIP--\n"
  in
  e # set_input_ch (new input_string msg);

  e # set_input_content_length (String.length msg);
  e # set_input_content_type "multipart/form-data; boundary=SNIP";
  e # set_cgi ~request_method:"POST" ();

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment) () in

  equal_str ~msg:"iarg1" value (cgi # initial_argument_value "a");

  cgi # finalize();

end ;;


expect_pass "cgi_activation POST(bigbinary form-data) arguments no-output"
begin fun () ->
  let e = make_env default_config in

  let block = String.make 256 'X' in
  for k = 0 to 255 do
    block.[k] <- Char.chr k
  done;

  let value = String.make (5000*256) 'x' in
  for k = 0 to 4999 do
    String.blit block 0 value (k*256) 256
  done;

  let msg = 
    "--SNIP\n" ^ 
    "content-disposition: form-data; name=\"a\"\n" ^ 
    "\n" ^ 
    value ^ "\n" ^ 
    "--SNIP--\n"
  in
  e # set_input_ch (new input_string msg);

  e # set_input_content_length (String.length msg);
  e # set_input_content_type "multipart/form-data; boundary=SNIP";
  e # set_cgi ~request_method:"POST" ();

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment) () in

  equal_str ~msg:"iarg1" value (cgi # initial_argument_value "a");

  cgi # finalize();

end ;;


expect_pass "cgi_activation GET no-arguments set_header"
begin fun () ->
  let e = make_env default_config in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation 
	      ~env:(e :> cgi_environment) 
	      ~operating_type: buffered_transactional_optype () in

  cgi # set_header
    ~status:`No_content
    ~content_type:"x/y"
    ~cache:`Unspecified
    ~filename:"qwertz"
    ~language:"en"
    ~script_type:"application/javascript"
    ~style_type:"text/css"
    ~set_cookie:[]
    ~fields:[ "p", ["r"] ]
    ();

  (* Check: *)

  equal_str ~msg:"ohdr1" "204 No content" 
    (e # output_header_field ~default:"xxx" "status");
  equal_str ~msg:"ohdr2" "x/y"
    (e # output_header_field ~default:"xxx" "content-type");
  equal_str ~msg:"ohdr3" "xxx"
    (e # output_header_field ~default:"xxx" "cache-control");
  equal_str ~msg:"ohdr4" "attachment; filename=\"qwertz\""
    (e # output_header_field ~default:"xxx" "content-disposition");
  equal_str ~msg:"ohdr5" "en"
    (e # output_header_field ~default:"xxx" "content-language");
  equal_str ~msg:"ohdr6" "application/javascript"
    (e # output_header_field ~default:"xxx" "content-script-type");
  equal_str ~msg:"ohdr7" "text/css"
    (e # output_header_field ~default:"xxx" "content-style-type");
  equal_str ~msg:"ohdr8" "xxx"
    (e # output_header_field ~default:"xxx" "set-cookie");
  equal_str ~msg:"ohdr9" "r"
    (e # output_header_field ~default:"xxx" "p");

  (* Further checks: *)

  cgi # set_header ~status:`Ok ();

  equal_str ~msg:"ohdr10" "200 OK" 
    (e # output_header_field ~default:"xxx" "status");
  equal_str ~msg:"ohdr11" "text/html"                   (* the default *)
    (e # output_header_field ~default:"xxx" "content-type");

  cgi # set_header ~fields:[ "r", ["q"; "s"]] ();

  expect_true ~msg:"ohdr12"
    (e # multiple_output_header_field "r" = [ "q"; "s" ]);

  (* Cache: *)

  cgi # set_header ~cache:`No_cache();

  equal_str ~msg:"ohdr13" "no-cache"
    (e # output_header_field ~default:"xxx" "cache-control");
  equal_str ~msg:"ohdr14" "no-cache"
    (e # output_header_field ~default:"xxx" "pragma");
  expect_true ~msg:"ohdr15"
    (let date = e # output_header_field ~default:"xxx" "expires" in
     let t = Netdate.parse_epoch date in
     abs_float(t -. Unix.gettimeofday()) < 10.0
    );

  (* Cookies: *)

  cgi # set_header 
    ~set_cookie:[ { cookie_name = "x";
		    cookie_value = "y";
		    cookie_expires = None;
		    cookie_domain = None;
		    cookie_path = None;
		    cookie_secure = false
		  } ] ();
  equal_str ~msg:"ohdr16" "x=y"
    (e # output_header_field ~default:"xxx" "set-cookie");

  let t0 = 1002469943.0 in
  cgi # set_header 
    ~set_cookie:[ { cookie_name = "x";
		    cookie_value = "y";
		    cookie_expires = Some t0;
		    cookie_domain = None;
		    cookie_path = None;
		    cookie_secure = false
		  } ] ();
  equal_str ~msg:"ohdr17" "x=y;EXPIRES=Sunday, 07-Oct-01 15:52:23 +0000"
    (e # output_header_field ~default:"xxx" "set-cookie");

  cgi # set_header 
    ~set_cookie:[ { cookie_name = "x";
		    cookie_value = "y";
		    cookie_expires = None;
		    cookie_domain = Some ".bogus.com";
		    cookie_path = None;
		    cookie_secure = false
		  } ] ();
  equal_str ~msg:"ohdr18" "x=y;DOMAIN=.bogus.com"
    (e # output_header_field ~default:"xxx" "set-cookie");

  cgi # set_header 
    ~set_cookie:[ { cookie_name = "x";
		    cookie_value = "y";
		    cookie_expires = None;
		    cookie_domain = None;
		    cookie_path = Some "/";
		    cookie_secure = false
		  } ] ();
  equal_str ~msg:"ohdr19" "x=y;PATH=/"
    (e # output_header_field ~default:"xxx" "set-cookie");

  cgi # set_header 
    ~set_cookie:[ { cookie_name = "x";
		    cookie_value = "y";
		    cookie_expires = None;
		    cookie_domain = None;
		    cookie_path = None;
		    cookie_secure = true;
		  } ] ();
  equal_str ~msg:"ohdr19" "x=y;SECURE"
    (e # output_header_field ~default:"xxx" "set-cookie");

  (* Redirections: *)

  cgi # set_redirection_header "/other.cgi";
  equal_str ~msg:"ohdr20" "/other.cgi"
    (e # output_header_field ~default:"xxx" "location");

end ;;
  
expect_fail "finalize/1"
begin fun () ->
  let a = new simple_argument "x" "y" in
  a # finalize();
  try ignore(a # value) with Failure s -> fail s
end ;;

expect_pass "finalize/2"
begin fun () ->
  let a = new simple_argument "x" "y" in
  a # finalize();
  a # finalize();
end ;;

expect_pass "finalize/3"
begin fun () ->
  let e = make_env default_config in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  let cgi = new std_activation ~env:(e :> cgi_environment) () in

  let x1 = new simple_argument "x" "1" in
  let x2 = new simple_argument "x" "2" in

  cgi # update_argument x1;
  cgi # update_argument x2;

  equal_str ~msg:"arg1"
    "2" (cgi # argument_value "x");

  expect_true ~msg:"arg2"
    (try ignore(x1 # value); false with Failure _ -> true);
  (* meaning: x1 is finalized! *)
end ;;


expect_pass "parse cookies 1"
begin fun () ->
  let e = make_env default_config in
  e # set_input_header_field "cookie" "x=y; y=a+b";
  
  let cookies = e # cookies in

  expect_true ~msg:"cookie1"
    (cookies = [ "x", "y"; "y", "a b" ])

end ;;


expect_pass "parse cookies 2"
begin fun () ->
  let e = make_env default_config in
  e # set_multiple_input_header_field "cookie" [ "x=y"; "y=a+b" ];
  
  let cookies = e # cookies in

  expect_true ~msg:"cookie1"
    (cookies = [ "x", "y"; "y", "a b" ])

end ;;


expect_pass "config/1"
begin fun () ->
  let e = make_env { default_config with
		       permitted_http_methods = [ "POST" ] }
  in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  expect_true
    (try ignore(new std_activation ~env:(e :> cgi_environment) ()); false
     with Failure _ -> true);
end ;;


expect_pass "config/2"
begin fun () ->
  let e = make_env { default_config with
		       permitted_input_content_types = [ "a/b" ] }
  in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;

  e # set_input_state `Received_header;

  e # set_input_content_type "text/xxx";      (* <> "a/b" *)
  e # set_cgi ~request_method:"POST" ();

  expect_true
    (try ignore(new std_activation ~env:(e :> cgi_environment) ()); false
     with Failure _ -> true);
end ;;


expect_pass "config/3"
begin fun () ->
  let e = make_env { default_config with
		       input_content_length_limit = 3 } 
  in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;
  e # set_input_state `Received_header;

  e # set_input_content_length 3;
  e # set_input_content_type "application/x-www-form-urlencoded";
  e # set_cgi ~request_method:"POST" ();
  e # set_input_ch (new input_string "a=b");

  expect_true ~msg:"cgi1"
    (ignore(new std_activation ~env:(e :> cgi_environment) ()); true);

  let e = make_env { default_config with
		       input_content_length_limit = 3 } 
  in

  let b = Buffer.create 100 in
  let bch = new output_buffer b in

  e # set_output_ch bch;
  e # set_input_state `Received_header;
  e # set_input_content_length 4;
  e # set_input_content_type "application/x-www-form-urlencoded";
  e # set_cgi ~request_method:"POST" ();
  e # set_input_ch (new input_string "a=bc");

  expect_true ~msg:"cgi2"
    (try ignore(new std_activation ~env:(e :> cgi_environment) ()); false
     with Resources_exceeded -> true);
end ;;
