(* args_show.ml -- show arguments of the scripts (part generic for all
   connectors).  *)
open Netcgi
open Printf

let text = Netencoding.Html.encode ~in_enc:`Enc_iso88591 ()

let main (cgi: cgi) =
  let env = cgi#environment in
  let cookies = [
    Cookie.make "Netcgi" "y" ~max_age:3600;
    Cookie.make "no-value" "";
    Cookie.make "" "no-name";
    Cookie.make "v2" "U\000e" ~comment:"For our amusement only";
  ] in
  cgi#set_header
    ~set_cookies:cookies
    ~fields:[("P3P", ["CP=\"NOI ADM DEV PSAi COM NAV OUR OTRo STP IND DEM\""])]
    ();

  let printf fmt = Printf.kprintf cgi#out_channel#output_string fmt in
  printf "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"\n\
    \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\" lang=\"en\">\n";
  printf "<html>
<head>
<style>
  code { color: rgb(255, 100, 100); }
  var { color: #355aba; }
  pre { color: #108a35; }
  pre.file { font-size: x-small; }
</style>
</head>
<body bgcolor=\"white\" text=\"black\">
<h2>Arguments</h2>
<ul>\n";
  List.iter
    (fun a ->
       let ct, _ = a#content_type() in
       printf "<li><var>%s</var>&nbsp;: <tt>%s</tt> = %s</li>\n" a#name ct
	 (match a#filename with
	  | None -> a#value
	  | Some fn ->
	      sprintf "[filename=<tt>%s</tt>] <pre class=\"file\">%s</pre>"
		fn (text a#value))
    ) cgi#arguments;
  printf "</ul>\n";
  printf "Request method: %s<br />\n"
    (match cgi#request_method with
     | `GET -> "GET"
     | `HEAD -> "HEAD"
     | `POST -> "POST"
     | `DELETE -> "DELETE"
     | `PUT a -> sprintf "PUT with argument:<pre>%s</pre>" a#value);


  let comp_name (n1,_) (n2,_) = compare n1 n2 in
  printf "<h2>Properties</h2>\n";
  let props = List.sort comp_name env#cgi_properties in
  List.iter (fun (n,v) -> printf "<tt>%s=</tt>%s<br />\n" n v) props;

  printf "<h2>Input HTTP header</h2>\n";
  let inheader = List.sort comp_name env#input_header_fields in
  List.iter (fun (n,v) -> printf "<tt>%s=</tt>%s<br />\n" n v) inheader;

  printf "<br /><b>Cookies</b> (<tt>cgi#environment#cookies</tt>):<ul>\n";
  List.iter (fun c ->
               printf "<li><var>%s</var><tt>&nbsp;&rarr;&nbsp;</tt>%s</li>\n"
	         (text(Cookie.name c)) (text(Cookie.value c))) env#cookies;
  printf "</ul>\n";

  (* Display forms pointing back to this script itself *)
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
  printf "<form action=\"%s\" method=POST \
	   enctype=\"multipart/form-data\">\n" url;
  printf "  <input type=\"text\" name=\"line\">\n";
  printf "  <input type=\"file\" name=\"file\">\n";
  printf "  <input type=\"submit\" name=\"submit\" value=\"Submit\">\n";
  printf "</form>\n";

  printf "</body>\n</html>\n";

  cgi#out_channel#commit_work()


let config = {
  Netcgi.default_config with
    Netcgi.permitted_http_methods = [`GET; `HEAD; `POST; `DELETE; `PUT];
    permitted_input_content_types = [
      "multipart/form-data";
      "application/x-www-form-urlencoded";
      "application/octet-stream" (* for PUT *)
    ];
(*     input_content_length_limit = 50 * 1024; *)
}

