#!/usr/bin/ocamlrun ocaml
(* netcgi.cgi                                               -*-tuareg-*-  *)
(* $Id: netcgi.cgi,v 1.2 2005/08/16 02:32:44 chris_77 Exp $ *)

#directory "+pcre";;
#directory "+netstring";;
#directory "+netcgi";;
#load "pcre.cma";;
#load "unix.cma";;
#load "netstring.cma";;
#load "netcgi.cma";;

open Printf

let main (cgi:Netcgi.cgi) =
  cgi#set_header ~content_type:"text/html; charset=iso-8859-1" ();
  (* Redefine [printf] to print to the script out channel -- this is
     portable i.e. it will work with all connectors.  *)
  let printf fmt = kprintf cgi#out_channel#output_string fmt in
  printf "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\" lang=\"en\">\n";
  printf "<html>\n";
  printf "<head><style>\n";
  printf "var { color: #355aba; }\n";
  printf "pre { color: #108a35; }\n";
  printf "</style></head>\n<body>\n";
  printf "<h2>Parameters:</h2>\n";
  printf "<ul>\n";
  List.iter
    (fun a ->
       let ct, _ = a#content_type() in
       printf "<li><var>%s</var>: <tt>%s</tt> = %s</li>\n" a#name ct
	 (match a#filename with
	  | None -> a#value
	  | Some fn ->
	      sprintf "[filename=<tt>%s</tt>] <pre>%s</pre>" fn a#value)
    ) cgi#args;
  printf "</ul>\n";
  printf "<tt>QUERY_STRING</tt>=%s<br />\n" cgi#env#query_string;
  printf "<tt>PATH_INFO</tt>=%s<br />\n" cgi#env#path_info;
  printf "Request method: %s<br />\n"
    (match cgi#request_method with
     | `GET -> "GET"
     | `HEAD -> "HEAD"
     | `POST -> "POST"
     | `DELETE -> "DELETE"
     | `PUT a -> sprintf "PUT with argument:<pre>%s</pre>" a#value);
  cgi#finalize();

  let url = cgi#url() in

  printf "<h2>GET URL-encoded form</h2>\n";
  printf "<form action=\"%s\" method=\"GET\">\n" url;
  printf "  <input type=\"text\" name=\"line\">\n";
  printf "  <input type=\"submit\" name=\"submit\" value=\"Submit\">\n";
  printf "</form>\n";

  printf "<h2>POST URL-encoded form</h2>\n";
  printf "<form action=\"%s\" method=\"POST\">\n" url;
  printf "  <input type=\"text\" name=\"line\">\n";
  printf "  <input type=\"submit\" name=\"submit\" value=\"Submit\">\n";
  printf "  <input type=\"hidden\" name=\"gâché\" value=\"éträñge\">\n";
  printf "</form>\n";

  printf "<h2>POST FORM-encoded form</h2>\n";
  printf "<form action=\"%s\" method=\"POST\" \
           enctype=\"multipart/form-data\">\n" url;
  printf "  <input type=\"text\" name=\"line\">\n";
  printf "  <input type=\"text\" name=\"sträange\">\n";
  printf "  <input type=\"submit\" name=\"submit\" value=\"Submit\">\n";
  printf "</form>\n";

  printf "<h2>File upload</h2>\n";
  printf
    "<form action=\"%s\" method=POST enctype=\"multipart/form-data\">\n" url;
  printf "  <input type=\"text\" name=\"line\">\n";
  printf "  <input type=\"file\" name=\"file\">\n";
  printf "  <input type=\"submit\" name=\"submit\" value=\"Submit\">\n";
  printf "</form>\n";

  printf "</body>\n</html>\n";

  cgi#out_channel#commit_work()


let () =
  Sys.catch_break true;   (* easier debugging *)
  let config = {
    Netcgi.default_config with
      Netcgi.permitted_http_methods = [`GET; `HEAD; `POST; `DELETE; `PUT];
      permitted_input_content_types = [
	"multipart/form-data";
	"application/x-www-form-urlencoded";
	"application/octet-stream" (* for PUT *) ];
      input_content_length_limit = 1024;
  } in
  if Netcgi_cgi.is_cgi() then
    Netcgi_cgi.run ~config main
  else (
    prerr_endline("** Test mode.  Use \"-help\" for more info **");
    Netcgi_test.run main
  )
