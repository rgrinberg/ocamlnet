(* This is a port of the "adder" of the Ocamlnet examples to Nethttpd, 
 * using only the reactor module.
 *)

open Netcgi;;
open Netcgi_types;;
open Netcgi_env;;
open Nethttpd_reactor;;
open Printf;;


let rec service_loop reactor netcgi_processor =
  match reactor # next_request () with
    | Some req ->
	( try
	    req # accept_body();   (* Always! *)
	    let env =
	      req # environment in
	    let cgi = 
	      new std_activation 
		   ~env:(env :> cgi_environment) 
		   ~operating_type:buffered_transactional_optype () in
	    netcgi_processor cgi
	  with
	      e ->
		printf "Uncaught exception: %s\n" (Printexc.to_string e);
		flush stdout
	);
	req # finish();
	service_loop reactor netcgi_processor

    | None ->
	()
;;


let serve fd netcgi_processor =
  let config : http_reactor_config = 
    object
      method config_timeout_next_request = 15.0
      method config_timeout = 300.0
      method config_reactor_synch = `Write
      method config_cgi = Netcgi_env.default_config 
      method config_error_response n = "<html>Error " ^ string_of_int n ^ "</html>"
      method config_log_error _ _ _ _ msg = 
	printf "Error log: %s\n" msg
      method config_max_reqline_length = 256
      method config_max_header_length = 32768
      method config_max_trailer_length = 32768
      method config_limit_pipeline_length = 5
      method config_limit_pipeline_size = 250000
      method config_announce_server = `Ocamlnet
    end in
  let reactor = new http_reactor config fd in
  service_loop reactor netcgi_processor;
  reactor # close()
;;


let start netcgi_processor =
  let master_sock = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.setsockopt master_sock Unix.SO_REUSEADDR true;
  Unix.bind master_sock (Unix.ADDR_INET(Unix.inet_addr_any, 8765));
  Unix.listen master_sock 100;
  printf "Listening on port 8765\n";
  flush stdout;

    while true do
    try
      let conn_sock, _ = Unix.accept master_sock in
      Unix.set_nonblock conn_sock;
      serve conn_sock netcgi_processor
    with
	Unix.Unix_error(Unix.EINTR,_,_) -> ()  (* ignore *)
  done
;;



(**********************************************************************
 * The following is copied, almost verbatim, from add.ml
 **********************************************************************)


(***********************************************************************
 * This example demonstrates a very simple CGI page that refers to itself
 * using the GET method.
 ***********************************************************************)

let text = Netencoding.Html.encode_from_latin1;;
(* This function encodes "<", ">", "&", double quotes, and Latin 1 characters 
 * as character entities. E.g. text "<" = "&lt;", and text "ä" = "&auml;"
 *)

let begin_page cgi title =
  (* Output the beginning of the page with the passed [title]. *)
  let out = cgi # output # output_string in
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


let end_page cgi =
  let out = cgi # output # output_string in
  out "</BODY>\n";
  out "</HTML>\n"
;;


let generate_query_page (cgi : cgi_activation) =
  (* Display the query form. *)
  begin_page cgi "Add Two Numbers";
  let out = cgi # output # output_string in
  out "<P>This CGI page can perform additions. Please enter two integers,\n";
  out "and press the button!\n";
  out (sprintf "<P>GET: <FORM METHOD=GET ACTION=\"%s\">\n" 
	 (text (cgi#url())));
  (* Note that cgi#url() returns the URL of this script (without ? clause).
   * We pass this string through the text function to avoid problems with
   * some characters.
   *)
  out "<INPUT TYPE=TEXT NAME=\"x\"> + <INPUT TYPE=TEXT NAME=\"y\"> = ";
  out "<INPUT TYPE=SUBMIT NAME=\"button\" VALUE=\"Go!\">\n";
  (* The hidden field only indicates that now the result page should
   * be consulted.
   *)
  out "<INPUT TYPE=HIDDEN NAME=\"page\" VALUE=\"result\">\n";
  out "</FORM>\n";
  out (sprintf "<P>POST: <FORM METHOD=POST ACTION=\"%s\">\n" 
	 (text (cgi#url())));
  out "<INPUT TYPE=TEXT NAME=\"x\"> + <INPUT TYPE=TEXT NAME=\"y\"> = ";
  out "<INPUT TYPE=SUBMIT NAME=\"button\" VALUE=\"Go!\">\n";
  out "<INPUT TYPE=HIDDEN NAME=\"page\" VALUE=\"result\">\n";
  out "</FORM>\n";
  end_page cgi
;;


let generate_result_page (cgi : cgi_activation) =
  (* Compute the result, and display it *)
  begin_page cgi "Sum";
  let out = cgi # output # output_string in
  out "<P>The result is:\n";
  let x = cgi # argument_value "x" in
  let y = cgi # argument_value "y" in
  let sum = (int_of_string x) + (int_of_string y) in
  out (sprintf "<P>%s + %s = %d\n" x y sum);
  out (sprintf "<P><A HREF=\"%s\">Add further numbers</A>\n" 
	 (text (cgi#url 
		  ~with_query_string:
		                   (`Args [new simple_argument "page" "query"])
		  ()
	       )));
  (* Here, the URL contains the CGI argument "page", but no other arguments. *)
  end_page cgi
;;


let generate_page (cgi : cgi_activation) =
  (* Check which page is to be displayed. This is contained in the CGI
   * argument "page".
   *)
  match cgi # argument_value "page" with
      "" ->
	(* The argument is the empty string, or the argument is missing.
	 * This is the same like the page "query".
	 *)
	generate_query_page cgi
    | "query" ->
	generate_query_page cgi
    | "result" ->
	generate_result_page cgi
    | _ ->
	assert false
;;


let process (cgi : cgi_activation) =
  (* A [cgi_activation] is an object that allows us to program pages
   * in a quite abstract way. By creating the [std_activation] object
   * the CGI/1.1 protocol is used to communicate with the outer world.
   * The CGI arguments are read in, and further properties of the protocol
   * are available by method calls.
   *
   * The parameter [~operating_type] specifies that the generated HTML
   * page is buffered, and sent to the browser when it is complete. This
   * has the advantage that you can catch errors while the page is generated,
   * and can output error messages. Other [~operating_type]s make it
   * possible that the HTML page is buffered in a temporary file, and it
   * can also be specified that the HTML page is not buffered at all.
   *)
  
  (* The [try] block catches errors during the page generation. *)
  try
    (* Set the header. The header specifies that the page must not be
     * cached. This is important for dynamic pages called by the GET
     * method, otherwise the browser might display an old version of
     * the page.
     * Furthermore, we set the content type and the character set.
     * Note that the header is not sent immediately to the browser because
     * we have enabled HTML buffering.
     *)
    cgi # set_header 
      ~cache:`No_cache 
      ~content_type:"text/html; charset=\"iso-8859-1\""
      ();

    generate_page cgi;

    (* After the page has been fully generated, we can send it to the
     * browser. 
     *)
    cgi # output # commit_work();
  with
      error ->
	(* An error has happened. Generate now an error page instead of
	 * the current page. By rolling back the output buffer, any 
	 * uncomitted material is deleted.
	 *)
	cgi # output # rollback_work();

	(* We change the header here only to demonstrate that this is
	 * possible.
	 *)
	cgi # set_header 
	  ~status:`Forbidden                  (* Indicate the error *)
	  ~cache:`No_cache 
	  ~content_type:"text/html; charset=\"iso-8859-1\""
	  ();

	begin_page cgi "Software error";
        cgi # output # output_string "While processing the request an O'Caml exception has been raised:<BR>";
        cgi # output # output_string ("<TT>" ^ text(Printexc.to_string error) ^ "</TT><BR>");
	end_page cgi;

	(* Now commit the error page: *)
	cgi # output # commit_work()
;;


(* main: *)
start process ;;
