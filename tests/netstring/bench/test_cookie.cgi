#! /bin/sh
# (*
exec /opt/ocaml-3.01/bin/ocamlfattop "$0" "$@"
*) directory ".";;

#directory "..";;
#load "netstring.cma";;

open Cgi.Operators;;

Cgi.parse_arguments Cgi.default_config;

let old_cookies = Cgi.Env.cookies() in

let name_new_cookie  = try !$ "name" with Not_found -> "" in
let value_new_cookie = try !$ "value" with Not_found -> "" in
let exp_new_cookie   = try !$ "expiration" with Not_found -> "" in
let exp_new_cookie_t = 
  if exp_new_cookie = "" then
    None
  else
    Some (Unix.time() +. float_of_string exp_new_cookie)
in
let new_cookies = 
  if name_new_cookie <> "" then
    [ {Cgi.cookie_name = name_new_cookie;
       Cgi.cookie_value = value_new_cookie;
       Cgi.cookie_expires = exp_new_cookie_t;
       Cgi.cookie_domain = None;
       Cgi.cookie_path = None;
       Cgi.cookie_secure = false
      }]
  else
    []
in

Cgi.header ~set_cookie:new_cookies "";

print_string "<html><body>\n";
print_string "<h1>Found cookies</h1>\n";

print_string "<pre>\n";
List.iter
  (fun (n,v) ->
     print_string n;
     print_string "=";
     print_string v;
     print_string "\n";
  )
  old_cookies;
print_string "</pre>\n";

print_string "<h1>Set cookie</h1>\n";

print_string ("<form method=get action=\"" ^ Cgi.this_url() ^ "\">\n");
print_string "Name:  <input type=text name=name><br>\n";
print_string "Value: <input type=text name=value><br>\n";
print_string "Expires in: <input type=text name=expiration> seconds<br>\n";
print_string "<input type=submit>\n";
print_string "</form>\n";
print_string "</body></html>\n";

flush stdout
;;


