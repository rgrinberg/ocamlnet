(* browsertest.ml *)
(* $Id: browsertest.ml,v 1.4 2005/10/19 20:28:56 chris_77 Exp $ *)

open Netcgi
open Printf

(* Configuration
 ***********************************************************************)

let datadir = "/tmp/browsertest";;
(* datadir: The directory where to store the following files:
   "lock": an empty file used as mutex
   "requests": the logged requests
   "results": the logged results

   These files must be writable by the user running the web server.
   They are created if necessary.  *)

(***********************************************************************)

(* Information about the failure of a test *)
type test_rec = {
  tname : string;
  (* name of the test *)
  tspec : (string * bool) list;
  (* [(desc, passed)] where [desc] is a description of what can go
     wrong and [passed = false] if it indeed went wrong ([passed =
     true] means it passed that part). *)
}

exception Test_failed of test_rec

let text = Netencoding.Html.encode_from_latin1

let hex s =
  let u = ref "" in
  for k = 0 to String.length s - 1 do
    u := !u ^ sprintf "%02x" (Char.code s.[k])
  done;
  !u



let details (env: cgi_environment) request =
  let buf = Buffer.create 300 in
  let out = Buffer.add_string buf in
  out "<p><b>CGI property:</b>\n";
  out "<pre>\n";
  List.iter
    (fun n -> out(n ^ "=" ^ text(env#cgi_property ~default:"(none)" n) ^ "\n"))
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
  out "</pre>\n";
  out "<p><b>input_header_fields:</b>\n";
  out "<pre>\n";
  List.iter (fun (n,v) -> out(n ^ "=" ^ text v ^ "\n"))
    (env#input_header_fields);
  out "</pre>\n";
  out "<p><b>Request body:</b>\n";
  out "<pre>\n";
  out (text request);
  out "\n</pre>\n";
  Buffer.contents buf


(************************************************************************)
(** Logging *)

(* Log the results of the tests in [datadir] *)
module Log = struct
  let current_lock = ref None

  let () =
    (* Creates the directory if it does not exists. *)
    try Unix.mkdir datadir 0o775
    with _ -> ()


  let lock() =
    match !current_lock with
    | None ->
	let lockfile = Filename.concat datadir "lock" in
	let fd = Unix.openfile lockfile [Unix.O_RDWR; Unix.O_CREAT] 0o644 in
	Unix.lockf fd Unix.F_LOCK 1;  (* lock exactly one byte *)
	current_lock := Some fd
    | Some _ -> ()


  let unlock() =
    match !current_lock with
    | None -> ()
    | Some f ->
	Unix.lockf f Unix.F_ULOCK 1;  (* unlock exactly one byte *)
	current_lock := None

  (* log the output of the [details] function (above) *)
  let request env req =
    (* Returns the byte position in the log file *)
    lock();
    let logfile = Filename.concat datadir "requests.html" in
    let log =
      open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 logfile in
    let pos = out_channel_length log in
    output_string log (details env req);
    output_string log "<hr>\n";
    close_out log;
    unlock();
    pos


  let result title agent reqpos result =
    lock();
    let logfile = Filename.concat datadir "results.txt" in
    let log =
      open_out_gen [Open_wronly; Open_append; Open_creat] 0o644 logfile in
    let out = output_string log in
    out ("Test-title: " ^ title ^ "\n");
    out ("Test-date: " ^ Netdate.mk_mail_date(Unix.gettimeofday()) ^ "\n");
    out ("Test-agent: " ^ agent ^ "\n");
    let reqpos =
      if reqpos < 0 then "(none)"
      else string_of_int reqpos in
    out ("Test-request: " ^ reqpos ^ "\n");
    out ("Test-result: " ^ String.escaped result ^ "\n");
    out "\n";
    close_out log;
    unlock()
end


(************************************************************************)
(** General outpout functions *)

let output_begin_page out title =
  out ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \
        \"http://www.w3.org/TR/REC-html40/strict.dtd\">
<html>
<head>
<title>" ^ text title ^ "</title>
<style type=\"text/css\">
  body { background: white; color: black; }
</style>
</head>
<body>
<h1>" ^ text title ^ "</h1>\n")


let html_page (cgi:cgi) title (body:string) =
  let out = cgi#out_channel#output_string in
  output_begin_page out title;
  out body;
  out "</body>\n</html>\n"

let env_html_page (env:cgi_environment) title body =
  let out = env#out_channel#output_string in
  env#set_output_header_fields [ "Content-type", "text/html" ];
  env#send_output_header();
  output_begin_page out title;
  out body;
  out "</body>\n</html>\n"

let anchor s =
  let s = String.lowercase s in
  (* Replace ' ' by '_' *)
  for i = 0 to String.length s - 1 do
    if s.[i] = ' ' then s.[i] <- '_'
  done;
  s

let test_passed (cgi:cgi) title request =
  (* Log: *)
  let env = cgi#environment in
  let reqpos = Log.request env request in
  Log.result env#cgi_path_info env#user_agent reqpos "passed";
  (* User message: *)
  html_page cgi (title ^ ": Passed")
    ("<a href=\"" ^ (cgi#url ~with_path_info:`None ())
     ^ "#" ^ (anchor title) ^ "\">Back</a>")



let test_failed (cgi: cgi) request title spec =
  (* Log: *)
  let env = cgi#environment in
  let failed = List.map fst (List.find_all (fun (m,p) -> not p) spec) in
  let message = String.concat "/" failed in
  let reqpos = Log.request env request in
  Log.result env#cgi_path_info env#user_agent reqpos ("PROBLEM: " ^ message);
  (* User message: *)
  html_page cgi (title ^ ": Failed")
    ("The following problems have occurred: <ul>\n"
     ^ (String.concat ""
	  (List.map (fun (name, passed) ->
		       if not passed then ("<li> " ^ name ^ "</li>\n")
		       else "") spec))
     ^ "</ul>
<h2>Details</h2>
The following request sent by your browser is incorrect or incorrectly
interpreted by Netcgi:\n"
     ^ details env request)


(* Tests *)


(* Output the start page *)
let welcome (cgi: cgi) =
  let env = cgi#environment in
  let url p = cgi#url ~with_path_info:(`This p) () in
  html_page cgi "The Browser Test for Netcgi"
    ("<p>The following tests help us finding out whether the Netcgi
module works together with real WWW browsers.  Please run them with every
browser you have access to.  We can analyze the results and check whether
there are incompatibilities that needs to be solved.
</p>
<p>We have already checked that Netcgi conforms to web standards. However,
many browsers do not comply to these standards and behave differently.
There are often simple workarounds for these problems, but we must know
which browser runs into which problem.
</p>
<p>The results of the tests are automatically recorded, so it is not
necessary to mail us the results.

<h2>About your browser</h2>
Your are using: <tt>" ^ text(env#user_agent) ^ "</tt>

<h2><a name=\"test_1\">Test 1: Simple POST Request</a></h2>
<p>This test checks whether your browser can generate POST requests at all.
After pressing the button, you will see a page telling you that
the test has been passed.  If you do not see this page, your browser
is not able to produce POST requests, and we are not interested in
such browsers.<br />
<form action=\"" ^ (url "/test1") ^ "\" method=\"post\" \
  enctype=\"application/x-www-form-urlencoded\">
<input type=\"hidden\" name=\"X\" value=\"42\">
<input type=\"submit\" name=\"S\" value=\"Do this test\">
</form>
</p>

<h2><a name=\"test_2\">Test 2: Form-encoded POST Request</a></h2>
<p>This test checks whether your browser can generate form-encoded POST
requests.  This type of POST request is necessary to perform file uploads.
<br />
<form action=\"" ^ (url "/test2") ^ "\" method=\"post\" \
  enctype=\"multipart/form-data\">
<input type=\"hidden\" name=\"X\" value=\"42\">
<input type=\"submit\" name=\"S\" value=\"Do this test\">
</form>
</p>

<h2><a name=\"test_3\">Test 3: Strict Form-encoded POST Request</a></h2>
<p>This test checks whether your browser produces requests that strictly
conform to the web standards.  It is interesting if this test fails or
succeeds, but the tested property does usually not cause interoperability
problems.  (We expect that most browsers behave incorrectly here.)
<br />
<form action=\"" ^ (url "/test3") ^ "\" method=\"post\" \
  enctype=\"multipart/form-data\">
<input type=\"hidden\" name=\"X Y\" value=\"42\">
<input type=\"hidden\" name=\"X\\Y\" value=\"43\">
<input type=\"hidden\" name=\"X&quot;Y\" value=\"44\">
<input type=\"submit\" name=\"S\" value=\"Do this test\">
</form>
</p>

<h2><a name=\"test_4\">Test 4: File upload</a></h2>
<p>This test checks whether your browser produces reasonable requests
for file uploads.  Please upload a file and start the test. The file
must be smaller than 20k (bigger files are not accepted).  Please
do not upload empty or non-existent files.  You can stress your browser
by uploading files with names that contain backslashes, double quotes
or spaces.
<br />
<form action=\"" ^ (url "/test4") ^ "\" method=\"post\" \
  enctype=\"multipart/form-data\">
<input type=\"file\" name=\"X\">
<input type=\"submit\" name=\"S\" value=\"Do this test\">
</form>
</p>

<h2><a name=\"test_5\">Test 5: Cookies</a></h2>
<p>This test checks whether your browser understands the cookie
protocol.  Please ensure that your browser accepts cookies before
you start the test (this can usually be selected in the
preferences/options dialog).  The cookie expires tomorrow.
<br />
<form action=\"" ^ (url "/test5_phase1") ^ "\" method=\"post\" \
  enctype=\"application/x-www-form-urlencoded\">
<input type=\"submit\" name=\"S\" value=\"Do this test\">
</form>
</p>

<h2><a name=\"test_6\">Test 6: Download</a></h2>
<p>This test checks whether your browser supports an important
HTTP directive for file downloads (content-disposition). The
directive demands that the file must not be displayed by the
browser itself nor by configured helper applications, but that
it is preferred to store the file on the local disk.
<br />
<form action=\"" ^ (url "test6_phase1") ^ "\" method=\"post\" \
  enctype=\"application/x-www-form-urlencoded\">
<input type=\"submit\" name=\"S\" value=\"Do this test\">
</form>
</p>\n")



let test1_request (cgi: cgi) request =
  let is_post = cgi#request_method = `POST in
  let is_urlencoded =
    fst(cgi#environment#input_content_type())
    = "application/x-www-form-urlencoded" in
  let args_ok =
    cgi#argument_value "X" = "42" &&
    cgi#argument_value "S" = "Do this test" in
  if is_post && is_urlencoded && args_ok then
    test_passed cgi "Test 1" request
  else
    raise(Test_failed { tname = "test1";
			tspec = [ "Wrong request method", is_post;
				  "Wrong request encoding", is_urlencoded;
				  "Wrong argument values", args_ok;
				] } )


let test2_request (cgi: cgi) request =
  let is_post = cgi#request_method = `POST in
  let is_formencoded =
    fst(cgi#environment#input_content_type()) = "multipart/form-data" in
  let args_ok =
    cgi#argument_value "X" = "42" &&
    cgi#argument_value "S" = "Do this test" in
  if is_post && is_formencoded && args_ok then
    test_passed cgi "Test 2" request
  else
    raise(Test_failed { tname = "test2";
			tspec = [ "Wrong request method", is_post;
				  "Wrong request encoding", is_formencoded;
				  "Wrong argument values", args_ok;
				] } )


let test3_request (cgi: cgi) request =
  let is_post = cgi#request_method = `POST in
  let is_formencoded =
    fst(cgi#environment#input_content_type()) = "multipart/form-data" in
  let args_ok =
    cgi#argument_value "X Y" = "42" &&
    cgi#argument_value "X\\Y" = "43" &&
    cgi#argument_value "X\"Y" = "44" &&
    cgi#argument_value "S" = "Do this test" in
  if is_post && is_formencoded && args_ok then
    test_passed cgi "Test 3" request
  else
    raise(Test_failed { tname = "test3";
			tspec = [ "Wrong request method", is_post;
				  "Wrong request encoding", is_formencoded;
				  "Wrong argument values", args_ok;
				] } )


let test4_request (cgi: cgi) request =
  let is_post = cgi#request_method = `POST in
  let is_formencoded =
    fst(cgi#environment#input_content_type()) = "multipart/form-data" in
  let arg_has_filename =
    cgi#argument_exists "X" && (cgi#argument "X")#filename <> None in

  if is_post && is_formencoded && arg_has_filename then begin
    let arg = cgi#argument "X" in
    let filename = match arg#filename with Some x -> x | None -> "" in
    let digest = Digest.string arg#value in
    (* Log: *)
    let reqpos = Log.request cgi#environment request in
    Log.result (cgi#environment#cgi_path_info) (cgi#environment#user_agent)
      reqpos
      ("results (filename=\"" ^ String.escaped filename ^ "\" "
       ^ "filetype=\"" ^ String.escaped(fst(arg#content_type())) ^ "\" "
       ^ "length=" ^ string_of_int (String.length arg#value) ^ " "
       ^ "digest=" ^ hex digest ^ ")");
    (* User message: *)
    html_page cgi "Results of test4"
      ("A file with the following properties has been uploaded:<br /><br />
<form action=\"test4_done\" method=\"post\" \
  enctype=\"application/x-www-form-urlencoded\">
<input type=\"hidden\" name=\"reqpos\" value=\"" ^ string_of_int reqpos ^ "\">
<table border=\"1\">
  <tr><td>File name:</td><td><tt>" ^ (text filename) ^ "</td>
<td>Is this ok? <input name=\"name\" type=\"checkbox\" value=\"ON\" checked>
</td></TR>
<tr><td>File type:</td><td><tt>" ^ (text(fst(arg#content_type()))) ^ "</td>
<td>Is this ok? <input name=\"type\" type=\"checkbox\" value=\"ON\" checked>
</td></TR>
<tr><td>File length:</td><td><tt>" ^ (string_of_int(String.length arg#value))
^ "</td><td>Is this ok? <input name=\"length\" type=\"checkbox\" value=\"ON\"
checked>
</td></tr>
<tr><td>MD5 Digest:</td><td><tt>" ^ (hex digest) ^ "</td>
<td>Is this ok? <input name=\"digest\" type=\"checkbox\" value=\"ON\"
checked>
</td></tr>
</table><br /><br />

Are these values reasonable? If not, please remove the tick from the wrong
value and press the following button:<br />
<input type=\"submit\" name=\"S\" value=\"The values are NOT ok\">
<br /><br />
Note that you can check the digest by the Unix program <tt>md5sum</tt>.
<br /><br />
<a href=\"" ^ (cgi#url ~with_path_info:`None ()) ^ "\">All values are ok</a>");
  end
  else raise(Test_failed {
	       tname = "test4";
	       tspec = [ "Wrong request method", is_post;
			 "Wrong request encoding", is_formencoded;
			 "File argument is missing", cgi#argument_exists "X";
			 "File name is missing", arg_has_filename;
		       ] } )


let test4done_request (cgi: cgi) =
  (* Log outcome: *)
  let reqpos    = cgi#argument_value "reqpos" in
  let name_ok   = cgi#argument_value "name" = "ON" in
  let type_ok   = cgi#argument_value "type" = "ON" in
  let length_ok = cgi#argument_value "length" = "ON" in
  let digest_ok = cgi#argument_value "digest" = "ON" in
  Log.result (cgi#environment#cgi_path_info) (cgi#environment#user_agent)
    (try int_of_string reqpos with _ -> -1)
    ("rating (name=" ^ string_of_bool name_ok ^
     " type=" ^ string_of_bool type_ok ^
     " length=" ^ string_of_bool length_ok ^
     " digest=" ^ string_of_bool digest_ok ^ ")");
  (* User message: *)
  html_page cgi "Again test4"
    ("Your interpretation has been recorded.<br />
<a href=\"" ^ (cgi#url ~with_path_info:`None ()) ^ "\">Back to main page</a>")



let cookie_val = " @+*<>{}[]()\\\"'# "

let test5_phase1 (cgi: cgi) =
  Log.result (cgi#environment#cgi_path_info)
    (cgi#environment#user_agent) (-1) "phase1";
  let cookie = Cookie.make "BROWSERTEST" cookie_val
    ~max_age:86400 ~path:cgi#environment#cgi_script_name in
  cgi#set_header ~set_cookies:[cookie] ();
  html_page cgi "Test 5 (Phase 1)"
    ("The cookie has been sent.  Please press the following button to
check whether the cookie can be retrieved.<br />
<form action=\"test5_phase2\" method=\"post\" \
	enctype=\"application/x-www-form-urlencoded\">
<input type=\"submit\" name=\"S\" value=\"Check the cookie\">
</form>\n")


let test5_phase2 (cgi: cgi) request =
  let cookies =
    List.map Cookie.value
      (List.filter (fun c -> Cookie.name c = "BROWSERTEST")
	 cgi#environment#cookies) in
  let good = (cookies = [ cookie_val ]) in
  if good then
    test_passed cgi "Test 5" request
  else
    let ncookies = List.length cookies in
    raise(Test_failed {
	    tname = "test5";
	    tspec = [ "Browser did not send cookie", (ncookies <> 0);
		      (sprintf "Browser has sent too many cookies (%i)"
			ncookies, ncookies = 1);
		      (sprintf "The value of the cookie (%s) is wrong"
			 (match cookies with [s] -> s | _ -> ""),
		       (List.mem cookie_val cookies))
		    ] } )


let test6_phase1 (cgi: cgi) =
  html_page cgi "Test 6 (Phase 1)"
    ("Please click on the following button. There are a number of possible
reactions:
<ul>
<li>The browser asks you whether you want to save the file on your local
disk, and the 'save as' dialog offers the specified file name;
<li>The 'save as' dialog appears, but the file name is wrong (for example,
there are quotes around the file name);
<li>The browser displays the file.
</ul>
<br />
<form action=\"test6_download\" method=\"post\" \
  enctype=\"application/x-www-form-urlencoded\">
<input type=\"text\" name=\"name\" value=\"sample file.html\" />
<input type=\"submit\" name=\"S\" value=\"Download the file\" />
</form>
<br /><br />
Because we try to fool the browser, the downloaded file is a normal
HTML file.  If you see the HTML page entitled &quot;Content-disposition
not supported&quot; the browser cannot interpret this HTTP directive.
In this case, please follow the instructions you see. If the
'save as' dialog appears, please check whether the file name is
the demanded one (by default, <tt>sample file.html</tt>, containing
a space). Fill out the following result form, and submit it.
<br /><br />
<form action=\"test6_phase2\" method=\"post\" \
  enctype=\"application/x-www-form-urlencoded\">
  <input id=1 name=\"result\" type=\"radio\" value=\"notyet\" checked />
  <label for=1>I have not yet tried to download the file.</label><br />
  <input id=2 name=\"result\" type=\"radio\" value=\"right\" />
  <label for=2>I have seen the 'save as' dialog and the file name was
  right.</label><br />
  <input id=3 name=\"result\" type=\"radio\" value=\"wrong\" />
  <label for=3>I have seen the 'save as' dialog and the file name was wrong.
  It was:</label>
  <input name=\"filename\" type=\"text\" /><br />
  <input id=4 name=\"result\" type=\"radio\" value=\"else\" />
  <label for=4>Something else has happened.</label><br />
  <input name=\"S\" type=\"submit\" value=\"Submit your rating\" />
</form>\n")



let test6_download (cgi: cgi) =
  let filename = cgi#argument_value "name" in
  (* Note: Many browsers accept the content-disposition directive if
     the content type is application/octet-stream, but do not accept the
     directive if the content type is an "internal type" like text/html.
     application/octet-stream is usually sufficient, so we use it for
     the test.  *)
  cgi#set_header (* ~content_type:"text/html" *)
    ~content_type:"application/octet-stream" ~filename ();
  html_page cgi "Test 6: Content-disposition not supported"
    ("If your browser displays this page without asking you whether you
want to save it into a file, it does not support the content-disposition
directive.  In this case, please press the following button such that we
get this result, too:
<br />
<form action=\"test6_phase2\" method=\"post\" \
  enctype=\"application/x-www-form-urlencoded\">
<input name=\"result\" type=\"hidden\" value=\"not_supported\" />
<input name=\"S\" type=\"submit\" value=\"Submit this observation\" />
</form>\n")


let test6_phase2 (cgi: cgi) =
  (* Log outcome: *)
  let result   =  cgi#argument_value ~default:"(none)" "result" in
  let filename =  cgi#argument_value ~default:"(none)" "filename" in
  Log.result (cgi#environment#cgi_path_info) (cgi#environment#user_agent) (-1)
    ("rating (result=" ^ result
     ^ (if filename = "" then "" else " filename=" ^ filename)
     ^ ")");
  (* User message: *)
  html_page cgi "Test 6 (Phase 2)"
    ("Your interpretation has been recorded.<br />
<a href=\"" ^ (cgi#url ~with_path_info:`None ()) ^ "\">Back to main page</a>")





(* The Netcgi_cgi module is small and the new interface allow very
   little peeking into the internals (for the user good!); so redefine
   it here.  *)
module Cgi =
struct
  open Netcgi_common

  let split_name_val s =
    try
      let i = String.index s '=' in
      (String.sub s 0 i, String.sub s (i+1) (String.length s - i - 1))
    with Not_found ->
      (s, "")

  let in_buf = Buffer.create 30000
    (* Copy the stdin data to this buffer so we can log it. *)

  let run ?(config=Netcgi.default_config)
      ?(output_type=(`Direct "":Netcgi.output_type))
      ?(arg_store=(fun _ _ _ -> `Automatic)) f =
    let (properties, input_header) =
      Array.fold_left
	(fun l e -> update_props_inheader (split_name_val e) l)
	([], []) (Unix.environment()) in
    let out_obj = new Netchannels.output_channel stdout in
    let env = new cgi_environment ~config ~properties ~input_header out_obj in

    (* Custom exception handler *)
    let exn_handler env f =
      try f()
      with
      | HTTP(_,_) as exn ->
          let reqpos = Log.request env (Buffer.contents in_buf) in
	  Log.result env#cgi_path_info env#user_agent reqpos
	    ("HTTP ERROR: " ^ Printexc.to_string exn);
          raise exn (* leave it to the default exception handler *)
      | exn ->
	  (* Log this error: *)
	  let reqpos = Log.request env (Buffer.contents in_buf) in
	  Log.result env#cgi_path_info env#user_agent reqpos
	    ("ERROR: " ^ Printexc.to_string exn);
	  (* User feedback: *)
	  env_html_page env "Software error"
	    ("While processing the request an O'Caml exception has been
raised:<br /><tt>" ^ text(Printexc.to_string exn) ^ "</tt><br />
The abnormal condition has been recorded, and will be checked soon.
<h2>Details</h2>
The following request sent by your browser is incorrect or is incorrectly
interpreted by Netcgi:"
	     ^ details env (Buffer.contents in_buf));
	  out_obj#flush();
    in
    exn_handler_default env ~exn_handler
      (fun () ->
         (* Record stdin into [in_buf] *)
         let len = try env#input_content_length with Not_found -> 0 in
         if len > 30000 then
	   raise(HTTP(`Request_entity_too_large,
		      "A maximum of 30000  bytes is allowed"))
         else (
	   Buffer.add_channel in_buf stdin len;
	   let in_obj = new Netchannels.input_string(Buffer.contents in_buf) in
	   let cgi = cgi_with_args (new cgi) env output_type in_obj arg_store in
	   f (cgi:Netcgi.cgi);
           cgi#finalize();
           None (* no "special" exceptions *)
         )
      )
      ~finally:(fun () ->
                  (try env#out_channel#close_out() with _ -> ());
                  exit 0
               )
end



let process_request (cgi:cgi) =
  (* Find out the test ID and call the function serving the test: *)
  let test_id = cgi#environment#cgi_path_info in
  let request = Buffer.contents Cgi.in_buf in
  try
    cgi#set_header();
    (* Every request function logs *)
    begin match test_id with
	""
      | "/" -> welcome cgi
      | "/test1" -> test1_request cgi request
      | "/test2" -> test2_request cgi request
      | "/test3" -> test3_request cgi request
      | "/test4" -> test4_request cgi request
      | "/test4_done" -> test4done_request cgi
      | "/test5_phase1" -> test5_phase1 cgi
      | "/test5_phase2" -> test5_phase2 cgi request
      | "/test6_phase1" -> test6_phase1 cgi
      | "/test6_phase2" -> test6_phase2 cgi
      | "/test6_download" -> test6_download cgi
      | _ -> raise(Netcgi_common.HTTP(`Not_found,
				      test_id ^ " does not exists"))
    end;
    cgi#out_channel#commit_work();
  with
    | Test_failed trec ->
	cgi#out_channel#rollback_work();
	cgi#set_header();
	test_failed cgi request trec.tname trec.tspec; (* => log *)
	cgi#out_channel#commit_work();
    | Netcgi_common.HTTP(_,_) as e -> raise e
    | exn ->
	(* Log: *)
	let reqpos = Log.request cgi#environment request in
	Log.result test_id cgi#environment#user_agent reqpos
	  ("ERROR: " ^ Printexc.to_string exn);
	(* User message: *)
	cgi#out_channel#rollback_work();
	cgi#set_header();
	html_page cgi "Software error"
	  ("While processing the request an O'Caml exception has been
raised:<br /><tt>" ^ text(Printexc.to_string exn) ^ "</tt><br />
The abnormal condition has been recorded, and will be checked soon.");
	cgi#out_channel#commit_work()



let () =
  let config = { default_config with
		   input_content_length_limit = 30000;
		   workarounds = [];
	       } in
  let output_type =
    `Transactional(fun _ ch -> new Netchannels.buffered_trans_channel ch) in
  if Netcgi_cgi.is_cgi() then
    Cgi.run ~config ~output_type process_request
  else (
    prerr_endline("** Test mode.  Use \"-help\" for more info **");
    Netcgi_test.run ~config ~output_type process_request
  )

