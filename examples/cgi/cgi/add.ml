(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Netcgi;;
open Netcgi_types;;
open Printf;;

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
  out (sprintf "<P><FORM METHOD=GET ACTION=\"%s\">\n" 
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


let process() =
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
  let cgi =
    new std_activation ~operating_type:buffered_transactional_optype () in
  
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


let main() =
  (* Call the function that processes the request, and catch any remaining
   * errors. These include protocol errors, and insufficient memory.
   * Because we cannot do anything else the error is simply logged.
   *
   * Note: Some web servers (e.g. iPlanet) do not always write the stderr output
   * to the error log. By prepending an empty line, the response is invalid
   * anyway, and chances are high that it is logged.
   *)
  try
    process()
  with
      error ->
	prerr_newline();
	prerr_endline ("O'Caml exception: " ^ Printexc.to_string error)
;;


main();;

  

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.1  2001/10/18 22:16:48  stolpmann
 * 	Initial revision.
 *
 * 
 *)
