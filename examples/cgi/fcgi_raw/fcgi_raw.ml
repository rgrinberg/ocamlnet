open Netcgi_fcgi_10

let _ =
  while true
  do
    let req = fcgi_accept () in
    let print s = 
      output_string stderr s;
      fcgi_write_stdout req s
    in
      fcgi_write_stdout req "Content-Type: text/plain\n\n";
      print "fastcgi request information\n";
      
      (* request type, app type *)
      print ("id: " ^ (string_of_int req.id) ^ "\n" ^
	     "app_type: " ^ (string_of_int req.app_type) ^ "\n");

      (* the params *)
      print "enviornment variables\n";
      List.iter
	(fun (name, valu) -> 
	   print (name ^ ": " ^ valu ^ "\n"))
	req.params;
      print "\n";

      (* stdin stderr records *)
      print ("stdin: " ^ req.stdin ^ "\n");
      print ("data: " ^ req.data ^ "\n");
      fcgi_write_end_request req {astatus=0;pstatus=0};
      fcgi_destroy req;
      flush_all()
  done
