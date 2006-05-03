#! /bin/sh
# (*
exec ocaml "$0" "$@"
*) use "topfind";;

#require "pcre";;
#require "unix";;
#directory "..";;
#load "netstring.cma";;

Cgi.header "";
Cgi.parse_arguments 
  { Cgi.default_config with
      Cgi.how_to_process_arguments = (fun _ -> Cgi.File)
  };
let params = Cgi.arguments() in
print_string "<html><body>\n";
print_string "<h1>Parameters:</h1>\n";
print_string "<ul>\n";
List.iter
  (fun (n,a) ->
     print_string "<li>";
     print_string n;
     print_string ":";
     print_string (Cgi.arg_mimetype a);
     print_string "=";
     (match Cgi.arg_filename a with
	  None -> ()
	| Some fn -> print_string ("[filename=" ^ fn ^ "]")
     );
     print_string (Cgi.arg_value a);
     print_string "</li>\n";

  )
  params;

Cgi.cleanup();

print_string "</ul>\n";

print_string "<h1>GET URL-encoded form</h1>\n";
print_string "<form action=\"test_encoding.cgi\" method=GET>\n";
print_string "<input type=text name=line>\n";
print_string "<input type=submit name=submit value=\"Submit\">\n";
print_string "</form>\n";

print_string "<h1>POST URL-encoded form</h1>\n";
print_string "<form action=\"test_encoding.cgi\" method=POST>\n";
print_string "<input type=text name=line>\n";
print_string "<input type=submit name=submit value=\"Submit\">\n";
print_string "</form>\n";

print_string "<h1>POST FORM-encoded form</h1>\n";
print_string "<form action=\"test_encoding.cgi\" method=POST enctype=\"multipart/form-data\">\n";
print_string "<input type=text name=line>\n";
print_string "<input type=text name=\"sträange\">\n";
print_string "<input type=submit name=submit value=\"Submit\">\n";
print_string "</form>\n";

print_string "<h1>File upload</h1>\n";
print_string "<form action=\"test_encoding.cgi\" method=POST enctype=\"multipart/form-data\">\n";
print_string "<input type=text name=line>\n";
print_string "<input type=file name=file>\n";
print_string "<input type=submit name=submit value=\"Submit\">\n";
print_string "</form>\n";

print_string ("QUERY_STRING=" ^ Cgi.Env.query_string() ^ "\n");


print_string "</body></html>\n";

flush stdout
;;

     
