(* $Id$ *)

open Netcgi;;
open Netcgi_env;;
open Netcgi_types;;
open Netchannels;;


(**********************************************************************)
(* CONFIGURATION                                                      *)
(**********************************************************************)

(* let datadir = "/home/groups/o/oc/ocamlnet/data/browsertest";; *)
let datadir = "/tmp/browsertest";;

  (* datadir: The directory where to store the following files:
   * "lock": an empty file used as mutex
   * "requests": the logged requests
   * "results": the logged results
   *
   * These files must be writable by the user running the web server.
   * They are created if necessary.
   *)

(**********************************************************************)

type test_rec =
    { tname : string;
      tspec : (string * bool) list;
    } ;;

exception Test_failed of test_rec;;

let text = Netencoding.Html.encode_from_latin1;;

let hex s =
  let u = ref "" in
  for k = 0 to String.length s - 1 do
    u := !u ^ Printf.sprintf "%02x" (Char.code s.[k])
  done;
  !u
;;


let details out (env : cgi_environment) request =
  out "<P><B>cgi_property:</B>\n";
  out "<PRE>\n";
  List.iter
    (fun n ->
       out (n ^ "=" ^ text(env # cgi_property ~default:"(none)" n) ^ "\n")
    )
    [ "GATEWAY_INTERFACE";
      "SERVER_SOFTWARE";
      "SERVER_NAME";
      "SERVER_PROTOCOL";
      "REQUEST_METHOD";
      "PATH_INFO";
      "SCRIPT_NAME";
      "QUERY_STRING";
      "REMOTE_HOST";
      "REMOTE_ADDR"
    ];
  out "</PRE>\n";
  out "<P><B>input_header_field:</B>\n";
  out "<PRE>\n";
  List.iter
    (fun (n,v) ->
       out (n ^ "=" ^ text v ^ "\n")
    )
    (env # input_header_fields);
  out "</PRE>\n";
  out "<P><B>Request body:</B>\n";
  out "<PRE>\n";
  out (text request);
  out "\n</PRE>\n"
;;


(* Logging: *)

let current_lock = ref None;;

let lock() =
  match !current_lock with
      None ->
	let f = Unix.openfile 
		  (Filename.concat datadir "lock") 
		  [ Unix.O_RDWR; Unix.O_CREAT ]
		  0o644
	in
	Unix.lockf f Unix.F_LOCK 1;  (* lock exactly one byte *)
	current_lock := Some f
    | Some _ ->
	()
;;

let unlock() =
  match !current_lock with
      None -> ()
    | Some f ->
	Unix.lockf f Unix.F_ULOCK 1;  (* unlock exactly one byte *)
	current_lock := None
;;

let log_request env req =
  (* Returns the byte position in the log file *)
  lock();
  let log = open_out_gen
	      [ Open_wronly; Open_append; Open_creat ]
	      0o644
	      (Filename.concat datadir "requests")
  in
  let pos = out_channel_length log in
  let out = output_string log in
  details out env req;
  out (String.make 76 '#' ^ "\n");
  close_out log;
  unlock();
  pos
;;

let log_result title agent reqpos result =
  lock();
  let log = open_out_gen
	      [ Open_wronly; Open_append; Open_creat ]
	      0o644
	      (Filename.concat datadir "results")
  in
  let out = output_string log in
  out ("Test-title: " ^ title ^ "\n");
  out ("Test-date: " ^ Netdate.mk_mail_date(Unix.gettimeofday()) ^ "\n");
  out ("Test-agent: " ^ agent ^ "\n");
  out ("Test-request: " ^ (string_of_int reqpos) ^ "\n");
  out ("Test-result: " ^ String.escaped result ^ "\n");
  out "\n";
  close_out log;
  unlock()
;;


let output_begin_page out title =
  out "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
  out "<HTML>\n";
  out "<HEAD>\n";
  out ("<TITLE>" ^ text title ^ "</TITLE>\n");
  out ("<STYLE TYPE=\"text/css\">\n");
  out "body { background: white; color: black; }\n";
  out "</STYLE>\n";
  out "</HEAD>\n";
  out "<BODY>\n";
  out ("<H1>" ^ text title ^ "</H1>\n")
;;


let begin_page cgi title =
  let out = cgi # output # output_string in
  output_begin_page out title
;;


let env_begin_page env title =
  env # set_output_header_fields [ "Content-type", "text/html" ];
  env # send_output_header();
  output_begin_page (env # output_ch # output_string) title
;;


let end_page cgi =
  cgi # output # output_string "</BODY>\n";
  cgi # output # output_string "</HTML>\n"
;;


let env_end_page env =
  env # output_ch # output_string "</BODY>\n";
  env # output_ch # output_string "</HTML>\n"
;;
  

let test_passed cgi title request =
  (* Log: *)
  let reqpos = log_request cgi#environment request in
  log_result 
    (cgi#environment#cgi_path_info)
    (cgi#environment#user_agent)
    reqpos
    "passed";
  (* User message: *)
  begin_page cgi (title ^ ": Passed");
  cgi # output # output_string "<A HREF=\"../browsertest.cgi\">Back</A>";
  end_page cgi
;;
  

let test_failed (cgi : cgi_activation) request title spec =
  (* Log: *)
  let failed = List.map fst (List.find_all (fun (m,p) -> not p) spec) in
  let message = String.concat "/" failed in
  let reqpos = log_request cgi#environment request in
  log_result 
    (cgi#environment#cgi_path_info)
    (cgi#environment#user_agent)
    reqpos
    ("PROBLEM: " ^ message);
  (* User message: *)
  let out = cgi # output # output_string in
  begin_page cgi (title ^ ": Failed");
  out "The following problems have occurred:\n";
  out "<UL>\n";
  List.iter
    (fun (name,passed) ->
       if not passed then begin
	 out "<LI> ";
	 out name;
	 out "\n";
       end
    )
    spec;
  out "</UL>\n";
  out "<H2>Details</H2>\n";
  out "The following request sent by your browser is incorrect or incorrectly interpreted by Netcgi:\n";
  details out cgi#environment request
;;


let test1_request (cgi : cgi_activation) request =
  let is_post = cgi # request_method = `POST in
  let is_urlencoded = 
    let content_type =
      cgi # environment # input_header_field ~default:"xxx" "content-type" in
    let mime_type, params =
      Mimestring.scan_mime_type content_type [] in
    mime_type = "application/x-www-form-urlencoded" in
  let args_ok = (cgi # argument_value ~default:"xxx" "X" = "42") &&
		(cgi # argument_value ~default:"xxx" "S" = "Do this test") in

  if is_post && is_urlencoded && args_ok then begin
    test_passed cgi "test1" request
  end 
  else begin
    raise(Test_failed { tname = "test1";
			tspec = [ "Wrong request method", is_post;
				  "Wrong request encoding", is_urlencoded;
				  "Wrong argument values", args_ok;
				]
		      })
  end
;;


let test2_request (cgi : cgi_activation) request =
  let is_post = cgi # request_method = `POST in
  let is_formencoded = 
    let content_type =
      cgi # environment # input_header_field ~default:"xxx" "content-type" in
    let mime_type, params =
      Mimestring.scan_mime_type content_type [] in
    mime_type = "multipart/form-data" in
  let args_ok = (cgi # argument_value ~default:"xxx" "X" = "42") &&
		(cgi # argument_value ~default:"xxx" "S" = "Do this test") in

  if is_post && is_formencoded && args_ok then begin
    test_passed cgi "test2" request
  end 
  else begin
    raise(Test_failed { tname = "test2";
			tspec = [ "Wrong request method", is_post;
				  "Wrong request encoding", is_formencoded;
				  "Wrong argument values", args_ok;
				] } )
  end
;;


let test3_request (cgi : cgi_activation) request =
  let out = cgi # output # output_string in
  let is_post = cgi # request_method = `POST in
  let is_formencoded = 
    let content_type =
      cgi # environment # input_header_field ~default:"xxx" "content-type" in
    let mime_type, params =
      Mimestring.scan_mime_type content_type [] in
    mime_type = "multipart/form-data" in
  let args_ok = (cgi # argument_value ~default:"xxx" "X Y" = "42") &&
		(cgi # argument_value ~default:"xxx" "X\\Y" = "43") &&
		(cgi # argument_value ~default:"xxx" "X\"Y" = "44") &&
		(cgi # argument_value ~default:"xxx" "S" = "Do this test") in

  if is_post && is_formencoded && args_ok then begin
    test_passed cgi "test3" request;
  end 
  else begin
    raise(Test_failed { tname = "test3";
			tspec = [ "Wrong request method", is_post;
				  "Wrong request encoding", is_formencoded;
				  "Wrong argument values", args_ok;
				] } )
  end
;;


let test4_request (cgi : cgi_activation) request =
  let out = cgi # output # output_string in
  let is_post = cgi # request_method = `POST in
  let is_formencoded = 
    let content_type =
      cgi # environment # input_header_field ~default:"xxx" "content-type" in
    let mime_type, params =
      Mimestring.scan_mime_type content_type [] in
    mime_type = "multipart/form-data" in
  let arg_exists = try ignore(cgi # argument "X"); true with Not_found -> false in
  let arg_has_filename =
    arg_exists && (cgi # argument "X") # filename <> None in

  if is_post && is_formencoded && arg_exists && arg_has_filename then begin
    let arg = cgi # argument "X" in
    let filename = match arg # filename with Some x -> x | None -> "" in
    let digest = Digest.string (arg # value) in
    (* Log: *)
    let reqpos = log_request cgi#environment request in
    log_result 
      (cgi#environment#cgi_path_info)
      (cgi#environment#user_agent)
      reqpos
      ("results (filename=\"" ^ String.escaped filename ^ "\" " ^
       "filetype=\"" ^ String.escaped(arg # content_type) ^ "\" " ^ 
       "length=" ^ string_of_int (String.length arg#value) ^ " " ^ 
       "digest=" ^ hex digest ^ ")");
    (* User message: *)
    begin_page cgi "Results of test4";
    out "A file with the following properties has been uploaded:<BR><BR>\n";
    out "<FORM ACTION=\"test4-done\" METHOD=\"POST\" ENCTYPE=\"application/x-www-form-urlencoded\">\n";
    out ("<INPUT TYPE=\"HIDDEN\" NAME=\"reqpos\" VALUE=\"" ^ string_of_int reqpos ^ 
	 "\">\n");
    out "<TABLE BORDER=1>\n";

    out "<TR><TD>File name:</TD>"; 
    out "<TD><TT>"; out (text filename); out "</TD>\n";
    out "<TD>Is this ok? <INPUT NAME=\"name\" TYPE=\"CHECKBOX\" VALUE=\"ON\" CHECKED></TD>\n";
    out "</TR>\n";

    out "<TR><TD>File type:</TD>"; 
    out "<TD><TT>"; out (text (arg # content_type)); out "</TD>\n";
    out "<TD>Is this ok? <INPUT NAME=\"type\" TYPE=\"CHECKBOX\" VALUE=\"ON\" CHECKED></TD>\n";
    out "</TR>\n";

    out "<TR><TD>File length:</TD>"; 
    out "<TD><TT>"; out (string_of_int (String.length arg#value));
    out "</TD>\n";
    out "<TD>Is this ok? <INPUT NAME=\"length\" TYPE=\"CHECKBOX\" VALUE=\"ON\" CHECKED></TD>\n";
    out "</TR>\n";

    out "<TR><TD>MD5 Digest:</TD>"; 
    out "<TD><TT>"; out (hex digest); out "</TD>\n";
    out "<TD>Is this ok? <INPUT NAME=\"digest\" TYPE=\"CHECKBOX\" VALUE=\"ON\" CHECKED></TD>\n";
    out "</TR>\n";
    out "</TABLE><BR><BR>\n";

    out "Are these values reasonable? If not, please remove the tick from the wrong\n";
    out "value and press the following button:<BR>\n";
    out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"The values are NOT ok\">\n";
    out "<BR><BR>\n";
    out "Note that you can check the digest by the Unix program <TT>md5sum</TT>.\n";
    out "<BR><BR>\n";
    cgi # output # output_string "<A HREF=\"../browsertest.cgi\">All values are ok</A>";
    end_page cgi
  end 
  else begin
    raise(Test_failed { tname = "test4";
			tspec = [ "Wrong request method", is_post;
				  "Wrong request encoding", is_formencoded;
				  "File argument is missing", arg_exists;
				  "File name is missing", arg_has_filename;
				] } )
  end
;;

let test4done_request (cgi : cgi_activation) =
  (* Log outcome: *)
  let reqpos    =  cgi # argument_value ~default:"X" "reqpos" in
  let name_ok   = (cgi # argument_value ~default:"X" "name") = "ON" in
  let type_ok   = (cgi # argument_value ~default:"X" "type") = "ON" in
  let length_ok = (cgi # argument_value ~default:"X" "length") = "ON" in
  let digest_ok = (cgi # argument_value ~default:"X" "digest") = "ON" in
  log_result 
    (cgi#environment#cgi_path_info)
    (cgi#environment#user_agent)
    (try int_of_string reqpos with _ -> -1)
    ("rating (name=" ^ string_of_bool name_ok ^ 
     " type=" ^ string_of_bool type_ok ^ 
     " length=" ^ string_of_bool length_ok ^ 
     " digest=" ^ string_of_bool digest_ok ^ ")");
  (* User message: *)
  let out = cgi # output # output_string in
  begin_page cgi "Again test4";
  out "Your interpretation has been recorded.<BR>\n";
  out "<A HREF=\"../browsertest.cgi\">Back to main page</A>";
  end_page cgi
;;


let cookie_val = " @+*<>{}[]()\\\"'# ";;

let test5_phase1 (cgi : cgi_activation) =
  log_result 
    (cgi#environment#cgi_path_info)
    (cgi#environment#user_agent)
    (-1)
    "phase1";
  let out = cgi # output # output_string in
  let now = Unix.gettimeofday() in
  let script_name = cgi # environment # cgi_script_name in
  cgi # set_header ~set_cookie:[ { cookie_name = "BROWSERTEST";
				   cookie_value = cookie_val;
				   cookie_expires = (Some (now +. 86400.0));
				   cookie_domain = None;   (* only host *)
				   cookie_path = Some script_name;
				   cookie_secure = false;
				 } ] ();
  begin_page cgi "Test 5 (Phase 1)";
  out "The cookie has been sent. Please press the following button to\n";
  out "check whether the cookie can be retrieved.<BR>\n";
  out "<FORM ACTION=\"test5_phase2\" METHOD=\"POST\" ENCTYPE=\"application/x-www-form-urlencoded\">\n";
  out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"Check the cookie\">\n";
  out "</FORM>\n";
  end_page cgi
;;


let test5_phase2 (cgi : cgi_activation) request = 
  let cookies = List.map snd
		  (List.filter 
		     (fun (n,v) -> n = "BROWSERTEST")
		     (cgi # environment # cookies)) in
  let good = (cookies = [ cookie_val ]) in
  if good then begin
    test_passed cgi "test5" request;
  end 
  else begin
    raise(Test_failed { tname = "test5";
			tspec = [ "Browser did not send cookie", (cookies<>[]);
				  "Browser has sent too many cookies",
				     (List.length cookies <> 1);
				  "The value of the cookie is wrong",
				     (not(List.mem cookie_val cookies))
				] } )
  end
;;


let test6_phase1 (cgi : cgi_activation) =
  let out = cgi # output # output_string in
  begin_page cgi "Test 6 (Phase 1)";
  out "Please click on the following button. There are a number of possible\n";
  out "reactions:\n";
  out "<UL>\n";
  out "<LI>The browser asks you whether you want to save the file on your\n";
  out "local disk, and the 'save as' dialog offers the specified file name\n";
  out "<LI>The 'save as' dialog appears, but the file name is wrong (for\n";
  out "example, there are quotes around the file name)\n";
  out "<LI>The browser displays the file\n";
  out "</UL><BR><BR>\n";

  out "<FORM ACTION=\"test6_download\" METHOD=\"POST\" ENCTYPE=\"application/x-www-form-urlencoded\">\n";
  out "<INPUT TYPE=\"TEXT\" NAME=\"name\" VALUE=\"sample file.html\">\n";
  out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"Download the file\">\n";
  out "</FORM><BR><BR>\n";

  out "Because we try to fool the browser, the downloaded file is a normal\n";
  out "HTML file. If you see the HTML page entitled &quot;Content-disposition\n";
  out "not supported&quot; the browser cannot interpret this HTTP directive.\n";
  out "In this case, please follow the instructions you see. If the\n";
  out "'save as' dialog appears, please check whether the file name is\n";
  out "the demanded one (by default, <TT>sample file.html</TT>, containing\n";
  out "a space). Fill out the\n";
  out "following result form, and submit it.<BR><BR>\n";

  out "<FORM ACTION=\"test6_phase2\" METHOD=\"POST\" ENCTYPE=\"application/x-www-form-urlencoded\">\n";
  out "<INPUT NAME=\"result\" TYPE=\"RADIO\" VALUE=\"notyet\" CHECKED>\n";
  out "  I have not yet tried to download the file<BR>\n";
  out "<INPUT NAME=\"result\" TYPE=\"RADIO\" VALUE=\"right\">\n";
  out "  I have seen the 'save as' dialog and the file name was right<BR>\n";
  out "<INPUT NAME=\"result\" TYPE=\"RADIO\" VALUE=\"wrong\">\n";
  out "  I have seen the 'save as' dialog and the file name was wrong.\n";
  out "  It was: ";
  out "<INPUT NAME=\"filename\" TYPE=\"TEXT\"><BR>\n";
  out "<INPUT NAME=\"result\" TYPE=\"RADIO\" VALUE=\"else\">\n";
  out "  Something else has happened.<BR>\n";
  out "<INPUT NAME=\"S\" TYPE=\"SUBMIT\" VALUE=\"Submit your rating\">";
  out "</FORM>\n";

  end_page cgi
;;


let test6_download (cgi : cgi_activation) =
  let out = cgi # output # output_string in
  let filename = cgi # argument_value "name" in
  (* Note: Many browsers accept the content-disposition directive if the
   * content type is application/octet-stream, but do not accept the directive
   * if the content type is an "internal type" like text/html.
   * application/octet-stream is usually sufficient, so we use it for the
   * test.
   *)
  cgi # set_header (* ~content_type:"text/html" *)
                   ~content_type:"application/octet-stream"
                   ~filename
                   ();
  begin_page cgi "Test 6: Content-disposition not supported";
  out "If your browser displays this page without asking you whether you\n";
  out "want to save it into a file, it does not support the content-disposition\n";
  out "directive. In this case, please press the following button such that we\n";
  out "get this result, too:<BR>\n";
  out "<FORM ACTION=\"test6_phase2\" METHOD=\"POST\" ENCTYPE=\"application/x-www-form-urlencoded\">\n";
  out "<INPUT NAME=\"result\" TYPE=\"HIDDEN\" VALUE=\"not_supported\">\n";
  out "<INPUT NAME=\"S\" TYPE=\"SUBMIT\" VALUE=\"Submit this observation\">";
  out "</FORM>\n";
  end_page cgi
;;


let test6_phase2 (cgi : cgi_activation) =
  (* Log outcome: *)
  let result   =  cgi # argument_value ~default:"(none)" "result" in
  let filename =  cgi # argument_value ~default:"(none)" "filename" in
  log_result 
    (cgi#environment#cgi_path_info)
    (cgi#environment#user_agent)
    (-1)
    ("rating (result=" ^ result ^ 
     " filename=" ^ filename ^ ")");
  (* User message: *)
  let out = cgi # output # output_string in
  begin_page cgi "Test 6 (Phase 2)";
  out "Your interpretation has been recorded.<BR>\n";
  out "<A HREF=\"../browsertest.cgi\">Back to main page</A>";
  end_page cgi
;;


let welcome_request (cgi : cgi_activation) =
  (* Simply output the start page *)
  let out = cgi # output # output_string in
  begin_page cgi "The Browser Test for Netcgi";
  out "<P>The following tests help us finding out whether the Netcgi module works\n";
  out "together with real WWW browsers. Please run them with every browser you have\n";
  out "access to. We can analyze the results and check whether there are\n";
  out "incompatibilities that needs to be solved.\n";
  out "<P>We have already checked that Netcgi conforms to web standards. However,\n";
  out "many browsers do not comply to these standards and behave differently.\n";
  out "There are often simple workarounds for these problems, but we must know which\n";
  out "browser runs into which problem.\n";
  out "<P>The results of the tests are automatically recorded, so it is not\n";
  out "necessary to mail us the results.\n";
  out "<H2>About your browser</H2>\n";
  out ("Your are using: <TT>" ^ text (cgi # environment # user_agent) ^ "</TT>");

  out "<H2>Test 1: Simple POST Request</H2>\n";
  out "This test checks whether your browser can generate POST requests at all.\n";
  out "After pressing the button, you will see a page telling you that\n";
  out "the test has been passed. If you do not see this page, you browser\n";
  out "is not able to produce POST requests, and we are not interested in\n";
  out "such browsers.<BR>\n";
  out "<FORM ACTION=\"browsertest.cgi/test1\" METHOD=\"POST\" ENCTYPE=\"application/x-www-form-urlencoded\">\n";
  out "<INPUT TYPE=\"HIDDEN\" NAME=\"X\" VALUE=\"42\">\n";
  out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"Do this test\">\n";
  out "</FORM>\n";

  out "<H2>Test 2: Form-encoded POST Request</H2>\n";
  out "This test checks whether your browser can generate form-encoded POST requests.\n";
  out "This type of POST request is necessary to perform file uploads.<BR>\n";
  out "<FORM ACTION=\"browsertest.cgi/test2\" METHOD=\"POST\" ENCTYPE=\"multipart/form-data\">\n";
  out "<INPUT TYPE=\"HIDDEN\" NAME=\"X\" VALUE=\"42\">\n";
  out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"Do this test\">\n";
  out "</FORM>\n";

  out "<H2>Test 3: Strict Form-encoded POST Request</H2>\n";
  out "This test checks whether your browser produces requests that strictly\n";
  out "conform to the web standards. It is interesting if this test fails or succeeds,\n";
  out "but the tested property does usually not cause interoperability\n";
  out "problems. (We expect that most browsers behave incorrectly here.)<BR>\n";
  out "<FORM ACTION=\"browsertest.cgi/test3\" METHOD=\"POST\" ENCTYPE=\"multipart/form-data\">\n";
  out "<INPUT TYPE=\"HIDDEN\" NAME=\"X Y\" VALUE=\"42\">\n";
  out "<INPUT TYPE=\"HIDDEN\" NAME=\"X\\Y\" VALUE=\"43\">\n";
  out "<INPUT TYPE=\"HIDDEN\" NAME=\"X&quot;Y\" VALUE=\"44\">\n";
  out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"Do this test\">\n";
  out "</FORM>\n";

  out "<H2>Test 4: File upload</H2>\n";
  out "This test checks whether your browser produces reasonable requests\n";
  out "for file uploads. Please upload a file and start the test. The file\n";
  out "must be smaller than 20k (bigger files are not accepted). Please\n";
  out "do not upload empty or non-existent files. You can stress your browser\n";
  out "by uploading files with names that contain backslashes, double quotes or spaces.<BR>\n";
  out "<FORM ACTION=\"browsertest.cgi/test4\" METHOD=\"POST\" ENCTYPE=\"multipart/form-data\">\n";
  out "<INPUT TYPE=\"FILE\" NAME=\"X\">\n";
  out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"Do this test\">\n";
  out "</FORM>\n";

  out "<H2>Test 5: Cookies</H2>\n";
  out "This test checks whether your browser understands the cookie\n";
  out "protocol. Please ensure that your browser accepts cookies before\n";
  out "you start the test (this can usually be selected in the\n";
  out "preferences/options dialog). The cookie expires tomorrow.<BR>\n";
  out "<FORM ACTION=\"browsertest.cgi/test5_phase1\" METHOD=\"POST\" ENCTYPE=\"application/x-www-form-urlencoded\">\n";
  out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"Do this test\">\n";
  out "</FORM>\n";

  out "<H2>Test 6: Download</H2>\n";
  out "This test checks whether your browser supports an important\n";
  out "HTTP directive for file downloads (content-disposition). The\n";
  out "directive demands that the file must not be displayed by the\n";
  out "browser itself nor by configured helper applications, but that\n";
  out "it is preferred to store the file on the local disk.<BR>\n";
  out "<FORM ACTION=\"browsertest.cgi/test6_phase1\" METHOD=\"POST\" ENCTYPE=\"application/x-www-form-urlencoded\">\n";
  out "<INPUT TYPE=\"SUBMIT\" NAME=\"S\" VALUE=\"Do this test\">\n";
  out "</FORM>\n";
 
  end_page cgi
;;


class special_environment buffer config =
object (self)
  inherit std_environment ~config () as super

  (* This special environment records the incoming request in [buffer] *)

  method input_ch =
    let real_ch = super # input_ch in
    let b = new output_buffer buffer in
    let len = try self # input_content_length with Not_found -> 0 in
    b # output_channel ~len real_ch;
    new input_string (Buffer.contents buffer)
end
;;


let process_request() =
  let config = { default_config with 
		   input_content_length_limit = 30000;
		   workarounds = [];
	       }
  in
  let buffer = Buffer.create 1000 in
  let env = new special_environment buffer config in
  (* let env = new test_environment ~config () in *)

  (* Find out the test ID and call the function serving the test: *)
  let test_id = env # cgi_path_info in
  
  let cgi = 
    try
      new std_activation
            ~env
            ~processing:(fun _ -> `Memory)
            ~operating_type:buffered_transactional_optype () 
    with
	Resources_exceeded ->
	  let out = env # output_ch # output_string in
	  env_begin_page env "Request too large";
	  out "Sorry, the request was too large.<BR>\n";
	  out "<A HREF=\"../browsertest.cgi\">Back</A>";
	  env_end_page env;
	  env # output_ch # flush();
	  (* This event is not logged! *)
	  exit 0
      | error ->
	  (* Cannot parse arguments *)
	  (* Log this error: *)
	  let reqpos = log_request env (Buffer.contents buffer) in
	  log_result test_id (env#user_agent) reqpos 
	    ("ERROR: " ^ Printexc.to_string error);
	  (* User feedback: *)
	  let out = env # output_ch # output_string in
	  env_begin_page env "Software error";
	  out "While processing the request an O'Caml exception has been raised:<BR>";
	  out ("<TT>" ^ text(Printexc.to_string error) ^ "</TT><BR>");
	  out "The abnormal condition has been recorded, and will be checked soon.\n";
	  out "<H2>Details</H2>\n";
	  out "The following request sent by your browser is incorrect or incorrectly interpreted by Netcgi:\n";
	  details out env (Buffer.contents buffer);
	  env_end_page env;
	  env # output_ch # flush();
	  exit 0
  in

  let request = Buffer.contents buffer in
  try
    cgi # set_header();
    (* Every request function logs *)
    begin match test_id with
	"" 
      | "/" -> welcome_request cgi
      | "/test1" -> test1_request cgi request
      | "/test2" -> test2_request cgi request
      | "/test3" -> test3_request cgi request
      | "/test4" -> test4_request cgi request
      | "/test4-done" -> test4done_request cgi
      | "/test5_phase1" -> test5_phase1 cgi
      | "/test5_phase2" -> test5_phase2 cgi request
      | "/test6_phase1" -> test6_phase1 cgi
      | "/test6_phase2" -> test6_phase2 cgi
      | "/test6_download" -> test6_download cgi
      | _ -> failwith "Unknown test_id"
    end;
    cgi # output # commit_work();
  with
    | Test_failed trec ->
	cgi # output # rollback_work();
	cgi # set_header();
	test_failed cgi request trec.tname trec.tspec;
	(* test_failed: logs already *)
	cgi # output # commit_work();
    | error ->
	(* Log: *)
	let reqpos = log_request env request in
	log_result test_id (env#user_agent) reqpos 
	  ("ERROR: " ^ Printexc.to_string error);
	(* User message: *)
	cgi # output # rollback_work();
	cgi # set_header();
	begin_page cgi "Software error";
	cgi # output # output_string "While processing the request an O'Caml exception has been raised:<BR>";
	cgi # output # output_string ("<TT>" ^ text(Printexc.to_string error) ^ "</TT><BR>");
	cgi # output # output_string "The abnormal condition has been recorded, and will be checked soon.";
	end_page cgi;
	cgi # output # commit_work()
;;


process_request();;

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.3  2001/10/14 15:46:53  stolpmann
 * 	Now using the strict HTML 4.0 DTD.
 * 	Download test: the filename can be entered by the tester.
 *
 * Revision 1.2  2001/10/09 22:53:18  stolpmann
 * 	Added tests 5 and 6.
 * 	[lock] and [unlock] are now called from the logging functions
 * themselves.
 *
 * Revision 1.1  2001/10/08 22:37:51  stolpmann
 * 	initial revision
 *
 * 
 *)
