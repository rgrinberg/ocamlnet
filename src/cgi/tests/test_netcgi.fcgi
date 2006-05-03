#! /bin/sh
# (*
# Execute the right toploop: (do "make toploop" to create it)
exec ./toploop "$0" "$@"
*) directory ".";;

#directory "../../netstring";;
#directory "..";;
#load "netstring.cma";;
#load "cgi.cma";;

Sys.catch_break true;;   (* easier debugging *)

let process (cgi : Netcgi_types.cgi_activation) =
  cgi # set_header();
  let params = cgi # arguments in
  let print_string = cgi # output # output_string in
  print_string "<html><body>\n";
  print_string "<h1>Parameters:</h1>\n";
  print_string "<ul>\n";
  List.iter
    (fun (n,a) ->
       print_string "<li>";
       print_string n;
       print_string ":";
       print_string (a # content_type);
       print_string "=";
       (match a # filename with
	    None -> ()
	  | Some fn -> print_string ("[filename=" ^ fn ^ "]")
       );
       print_string (a # value);
       print_string "</li>\n";
    )
    params;
  
  cgi # finalize();
  (* Cgi.cleanup(); *)

  print_string "</ul>\n";
  
  print_string "<h1>GET URL-encoded form</h1>\n";
  print_string "<form action=\"test_netcgi.fcgi\" method=GET>\n";
  print_string "<input type=text name=line>\n";
  print_string "<input type=submit name=submit value=\"Submit\">\n";
  print_string "</form>\n";
  
  print_string "<h1>POST URL-encoded form</h1>\n";
  print_string "<form action=\"test_netcgi.fcgi\" method=POST>\n";
  print_string "<input type=text name=line>\n";
  print_string "<input type=submit name=submit value=\"Submit\">\n";
  print_string "</form>\n";
  
  print_string "<h1>POST FORM-encoded form</h1>\n";
  print_string "<form action=\"test_netcgi.fcgi\" method=POST enctype=\"multipart/form-data\">\n";
  print_string "<input type=text name=line>\n";
  print_string "<input type=text name=\"sträange\">\n";
  print_string "<input type=submit name=submit value=\"Submit\">\n";
  print_string "</form>\n";
  
  print_string "<h1>File upload</h1>\n";
  print_string "<form action=\"test_netcgi.fcgi\" method=POST enctype=\"multipart/form-data\">\n";
  print_string "<input type=text name=line>\n";
  print_string "<input type=file name=file>\n";
  print_string "<input type=submit name=submit value=\"Submit\">\n";
  print_string "</form>\n";
  
  print_string ("QUERY_STRING=" ^ cgi # environment # cgi_query_string ^ "\n");
  

  print_string "</body></html>\n";
  
  cgi # output # commit_work()
;;


Netcgi_fcgi.serv process Netcgi.buffered_transactional_optype
;;

     
