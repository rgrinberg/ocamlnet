
(* Bad request (a newline in the request URL - the server sees a HTTP 0.9
 * request, but the HEAD method is not defined for HTTP 0.9).
 * In the callback function, a second request is pushded onto the queue.
 *)

open Http_client;;

let server = Sys.argv.(1) in

let exn_to_string x =
  match x with
    | Http_protocol any ->
	"Http_protocol(" ^ Printexc.to_string any ^ ")"
    | any ->
	Printexc.to_string any
in


try
  let p = new pipeline in

  let opts = p # get_options in
  p # set_options
    { opts with 
	verbose_connection = true;
	verbose_status = true;
	number_of_parallel_connections = 1;
    };
	
  let m1 = new head ("http://" ^ server ^ "/techdocs/corba/OMG-2.0/mailto:pubs@omg.org\n") in
  let m2 = new get ("http://" ^ server ^ "/") in

  p # add_with_callback 
    m1
    (fun m ->
       try
	 let _, _, _ = m # dest_status() in ()
       with
	   any ->
	     print_endline ("Serious error: " ^
			    exn_to_string any);
	     flush stdout;
	     p # add m2;
    );

  let rec r() =
    try
      p # run()
    with
	Http_protocol x ->
	  print_endline ("Exception: "  ^ Printexc.to_string x);
	  flush stdout;
	  r()
  in
  r();

  print_string ("Reply:\n" ^ m2 # get_resp_body()); 

with
    Assert_failure (where, first, last) ->
      Printf.printf ("Assert failure: %s %d %d\n") where first last;
      flush stdout;
      ()
  | Http_protocol any ->
      Printf.printf "Uncaught exception: Http_protocol(%s)\n" (Printexc.to_string any);
      flush stdout;
      ()
  | any ->
      Printf.printf "Uncaught exception: %s\n" (Printexc.to_string any);
      flush stdout;
      ()
;;
