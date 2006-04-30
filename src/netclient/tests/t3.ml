#directory "..";;
#require "equeue";;
#require "unix";;
#require "cgi";;
#require "base64";;
#load "netclient.cma";;
open Http_client;;

try
  let p = new pipeline in


  let a = new basic_auth_method in
  a # set_realm "Adminbereich" "testuser" "testpassword";

  let a' = new digest_auth_method in
  a' # set_realm "testrealm" "testuser" "testpassword";

  p # add_authentication_method a;
  p # add_authentication_method a';

  let opts = p # get_options in
  p # set_options
    { opts with 
	verbose_connection = true;
	verbose_status = true;
    };

  let m1 = new get "http://localhost/techdocs/informix-online/admin.pdf" in

  p # add m1;
 
  p # run();

(*  print_string ("Reply:\n" ^ m1 # get_resp_body()); *)

with
    Assert_failure (where, first, last) ->
      Printf.eprintf ("Assert failure: %s %d %d\n") where first last;
      flush stderr;
      raise Not_found
;;
