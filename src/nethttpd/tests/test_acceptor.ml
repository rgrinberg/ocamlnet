(* Test the request acceptor of Nethttpd_kernel.http_protocol *)

(* Note: All tests are written with just LF as line terminator. It is optionally
 * replaced by CRLF just before feeding the data to the acceptor.
 *)

open Nethttpd_types

(* ---- Individual data cases ---- *)

let d_emptyline () =
  "\n" ;;

let d_empty_empty_request() =
  "GET / HTTP/1.0\n\n" ;;

let d_badreq_empty_request() =
  "foo\n\n" ;;

let d_header_empty_request_1() =
  "GET / HTTP/1.1\n" ^
  "Host: foo\n\n" ;;

let d_header_empty_request_2() =
  "GET / HTTP/1.1\n" ^
  "Content-Length: 0\n" ^
  "Host: foo\n\n" ;;

let d_largeheader_empty_request n () =   (* n = length of large field *)
  "GET / HTTP/1.1\n" ^
  "X-foo: " ^ (String.make n 'X') ^ "\n" ^ 
  "Host: foo\n\n" ;;

let d_badheader_empty_request () =
  "GET / HTTP/1.1\n" ^
  "Host: foo\n" ^
  "foo foo foo\n\n" ;;

let d_noheader_request () =
  "GET / HTTP/1.1\n" ^
  "Host: foo\n" ;;

let d_header_identity_request n () =   (* n = number of bytes in the body *)
  assert(n >= 2);
  "PUT / HTTP/1.1\n" ^
  ("Content-Length: " ^ string_of_int n ^ "\n") ^
  "Host: foo\n\n" ^ 
  "P" ^ String.make (n-2) 'X' ^ "Q" ;;

let d_header100_identity_request n () =   (* n = number of bytes in the body *)
  assert(n >= 2);
  "PUT / HTTP/1.1\n" ^
  ("Content-Length: " ^ string_of_int n ^ "\n") ^
  "Expect: 100-continue\n" ^
  "Host: foo\n\n" ^ 
  "P" ^ String.make (n-2) 'X' ^ "Q" ;;

let d_header_shortidentity_request n () =   (* n = number of bytes in the body *)
  assert(n >= 2);
  "PUT / HTTP/1.1\n" ^
  ("Content-Length: " ^ string_of_int (n+1) ^ "\n") ^
  "Host: foo\n\n" ^ 
  "P" ^ String.make (n-2) 'X' ^ "Q" ;;

let d_header_chunk_request m n () =   (* m = number of chunks, n = length of chunk *)
  assert (n >= 2);
  let b = Buffer.create 1000 in
  Buffer.add_string b
    ("PUT / HTTP/1.1\n" ^
     "Transfer-Encoding: chunked\n" ^ 
     "Host: foo\n\n");
  for k = 1 to m do
    Printf.bprintf b "%x\n" n;
    Buffer.add_string b ("P" ^ String.make (n-2) 'X' ^ "Q");
    Buffer.add_string b "\n";
  done;
  Buffer.add_string b "0\n";
  Buffer.add_string b "\n";
  Buffer.contents b
;;

let d_header_shortchunkheader_request () =
  "PUT / HTTP/1.1\n" ^
  "Transfer-Encoding: chunked\n" ^ 
  "Host: foo\n\n" ^
  "5" ;;

let d_header_longchunkheader_request () =
  "PUT / HTTP/1.1\n" ^
  "Transfer-Encoding: chunked\n" ^ 
  "Host: foo\n\n" ^
  (String.make 1000 '0') ^ "5\n" ^
  "PXXXQ\n" ^
  "0\n\n" ;;

let d_header_badchunkheader_request () =
  "PUT / HTTP/1.1\n" ^
  "Transfer-Encoding: chunked\n" ^ 
  "Host: foo\n\n" ^
  "0x5\n" ^
  "PXXXQ\n" ^
  "0\n\n" ;;

let d_header_badchunk_request () =
  "PUT / HTTP/1.1\n" ^
  "Transfer-Encoding: chunked\n" ^ 
  "Host: foo\n\n" ^
  "5\n" ^
  "PXXXQ" ^  (* LF missing *)
  "0\n\n" ;;

let d_header_shortchunk_request () =
  "PUT / HTTP/1.1\n" ^
  "Transfer-Encoding: chunked\n" ^ 
  "Host: foo\n\n" ^
  "5\n" ^
  "PXX" ;;

let d_header_trailer_request m n () =   (* m = number of chunks, n = length of chunk *)
  assert(n>=2);
  let b = Buffer.create 1000 in
  Buffer.add_string b
    ("PUT / HTTP/1.1\n" ^
     "Transfer-Encoding: chunked\n" ^ 
     "Host: foo\n\n");
  for k = 1 to m do
    Printf.bprintf b "%x\n" n;
    Buffer.add_string b ("P" ^ String.make (n-2) 'X' ^ "Q");
    Buffer.add_string b "\n";
  done;
  Buffer.add_string b "0\n";
  Buffer.add_string b "X-foo: ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ\n";
  Buffer.add_string b "\n";
  Buffer.contents b
;;


let d_header_largetrailer_request m n () = (* m = number of chunks, n = length of chunk *)
  assert(n>=2);
  let b = Buffer.create 1000 in
  Buffer.add_string b
    ("PUT / HTTP/1.1\n" ^
     "Transfer-Encoding: chunked\n" ^ 
     "Host: foo\n\n");
  for k = 1 to m do
    Printf.bprintf b "%x\n" n;
    Buffer.add_string b ("P" ^ String.make (n-2) 'X' ^ "Q");
    Buffer.add_string b "\n";
  done;
  Buffer.add_string b "0\n";
  Buffer.add_string b ("X-foo: " ^ String.make 10000 'Z' ^ "\n");
  Buffer.add_string b "\n";
  Buffer.contents b
;;

let d_header_shorttrailer_request () =
  "PUT / HTTP/1.1\n" ^
  "Transfer-Encoding: chunked\n" ^ 
  "Host: foo\n\n" ^
  "5\n" ^
  "PXXXQ\n" ^
  "0\n" ^ 
  "foo: X\n"
;;

let d_header_badtrailer_request () =
  "PUT / HTTP/1.1\n" ^
  "Transfer-Encoding: chunked\n" ^ 
  "Host: foo\n\n" ^
  "5\n" ^
  "PXXXQ\n" ^
  "0\n" ^ 
  "foo bar\n\n"
;;



(* ---- Case combinators ---- *)

let repeat f n () =
  let b = Buffer.create 20000 in
  for k = 1 to n do
    Buffer.add_string b ( f() )
  done;
  Buffer.contents b
;;


let seq l () =
  String.concat "" (List.map (fun f -> f()) l) ;;


(* ---- Config ---- *)

class config 
        ?(max_reqline_length = 256)
        ?(max_header_length = 32768)
        ?(max_trailer_length = 32768)
        ?(limit_pipeline_length = 99)
        ?(limit_pipeline_size = max_int)
        () : Nethttpd_kernel.http_protocol_config =
  Nethttpd_kernel.modify_http_protocol_config
    ~config_max_reqline_length:max_reqline_length
    ~config_max_header_length:max_header_length
    ~config_max_trailer_length:max_trailer_length
    ~config_limit_pipeline_length:limit_pipeline_length
    ~config_limit_pipeline_size:limit_pipeline_size
    Nethttpd_kernel.default_http_protocol_config

(* ---- Test driver ---- *)

let lf_to_crlf s =
  Netstring_str.global_replace 
    (Netstring_str.regexp "\n")
    "\r\n"
    s
;;
       

let perform config f (name,print_req,quiet,crlf) =
  let fd0, fd1 = Unix.socketpair Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  Unix.set_nonblock fd0;
  Unix.set_nonblock fd1;
  let proto = new Nethttpd_kernel.http_protocol config fd1 in
  let data = if crlf then lf_to_crlf (f()) else f() in
  let data_pos = ref 0 in
  let data_eof = ref false in

  let print_header prefix hdr =
    List.iter
      (fun (hn, hv) ->
	 let hv' =
	   if String.length hv > 50 then
	     Printf.sprintf "(%d bytes)" (String.length hv)
	   else hv in
	 Printf.printf "     %s %s: %s\n" prefix hn hv')
      hdr#fields;
  in

  let rec next_token () =
    (* Feed data to [fd0] *)
    let n = 
      try Unix.single_write fd0 data !data_pos (String.length data - !data_pos) 
      with
	  Unix.Unix_error((Unix.EAGAIN|Unix.EWOULDBLOCK),_,_) -> 0
    in
    data_pos := !data_pos + n;
    if !data_pos = String.length data && not !data_eof then (
      Unix.shutdown fd0 Unix.SHUTDOWN_SEND;
      data_eof := true
    );
    proto # cycle ();   (* without blocking! *)
    if proto # recv_queue_len = 0 then
      next_token()
    else (
      let tok = proto # receive() in
      ( match tok with
	  | `Req_header (((meth, uri), v), hdr, resp) ->
	      if not quiet then (
		Printf.printf "- `Req_header:\n";
		Printf.printf "     method = %s, uri = %s, version = %s\n" 
		  meth uri (Nethttp.string_of_protocol v);
		print_header "header" hdr;
	      );
	  | `Req_expect_100_continue ->
	      if not quiet then
		Printf.printf "- `Req_expect_100_continue\n"
	  | `Req_body(s, pos, len) ->
	      if not quiet then (
		Printf.printf "- `Req_body: len=%d first=%c last=%c\n"
				   len (s.[pos]) (s.[pos+len-1])
	      );
	  | `Req_trailer tr ->
	      if not quiet then (
		Printf.printf "- `Req_trailer:\n";
		print_header "trailer" tr;
	      );
	  | `Req_end ->
	      if not quiet then
		Printf.printf "- `Req_end\n"
	  | `Eof ->
	      Printf.printf "- `Eof\n";
	  | `Fatal_error e ->
	      let name = Nethttpd_kernel.string_of_fatal_error e in
	      Printf.printf "- `Fatal_error: %s\n" name
	  | `Bad_request_error (e, resp) ->
	      let name = Nethttpd_kernel.string_of_bad_request_error e in
	      Printf.printf "- `Bad_request_error: %s\n" name
	  | `Timeout ->
	      Printf.printf "- `Timeout\n"
      );
      flush stdout;
      tok
    )
  in

  Printf.printf "Starting test: %s\n" name; flush stdout;

  if print_req then (
    Printf.printf "Test data: <<<%s>>>\n" data;
    flush stdout
  );
  
  let cur_tok = ref (next_token()) in
  while !cur_tok <> `Eof do
    cur_tok := next_token()
  done;
  
  Unix.close fd0;
  Unix.close fd1;

  Printf.printf "Test coverage for %s:\n" name;

  List.iter
    (fun tok ->
       Printf.printf "  %s\n" tok)
    proto # test_coverage;

  print_string "\n";

  flush stdout
;;


(* --- Test definitions ---- *)

let tests =
  [ "1x_empty_empty_request",
    perform (new config()) d_empty_empty_request;

    "1x_header_empty_request_1",
    perform (new config()) d_header_empty_request_1;

    "1x_header_empty_request_2",
    perform (new config()) d_header_empty_request_2;

    "1x_header_identity_request",
    perform (new config()) (d_header_identity_request 100);

    "1x_header_chunk_request",
    perform (new config()) (d_header_chunk_request 1 100);

    "1x_header_trailer_request",
    perform (new config()) (d_header_trailer_request 1 100);

    "5x_empty_empty_request",
    perform (new config()) (repeat d_empty_empty_request 5);

    "1x_largeheader_empty_request",
    perform (new config()) (d_largeheader_empty_request 20000);

    "1x_emptyline_empty_empty_request",
    perform (new config()) (seq [d_emptyline; d_empty_empty_request]);

    "5x_emptyline_empty_empty_request",
    perform (new config()) (repeat (seq [d_emptyline; d_empty_empty_request]) 5);

    "1x_largeheader_empty_request_fail",
    perform (new config ~max_header_length:19000 ()) (d_largeheader_empty_request 20000);

    "1x_badreq_empty_request_fail",
    perform (new config()) d_badreq_empty_request;

    "1x_badheader_empty_request_fail",
    perform (new config()) d_badheader_empty_request;

    "1x_noheader_request_fail",
    perform (new config()) d_noheader_request;

    "1x_header100_identity_request",
    perform (new config()) (d_header100_identity_request 100);

    "1x_header_largeidentity_request",
    perform (new config()) (d_header_identity_request 20000);

    "1x_header_hugeidentity_request",
    perform (new config()) (d_header_identity_request 200000);

    "5x_header_bigidentity_request",
    perform (new config()) (repeat (d_header_identity_request 5000) 5);

    "1x_header_shortidentity_request_fail",
    perform (new config()) (d_header_shortidentity_request 100);

    "1x_header_manychunks_request",
    perform (new config()) (d_header_chunk_request 10 100);

    "1x_header_bigchunks_request",
    perform (new config()) (d_header_chunk_request 3 5000);

    "1x_header_shortchunkheader_request_fail",
    perform (new config()) d_header_shortchunkheader_request;

    (* This is only detected at the buffer boundary = 8192 bytes: *)
    "1x_header_longchunkheader_request_fail",
    perform (new config()) (seq [ d_header_identity_request 7200;
		                d_header_longchunkheader_request; ]);

    "1x_header_badchunkheader_request_fail",
    perform (new config()) d_header_badchunkheader_request;

    "1x_header_badchunk_request_fail",
    perform (new config()) d_header_badchunk_request;

    "1x_header_shortchunk_request_fail",
    perform (new config()) d_header_shortchunk_request;

    "1x_header_toolargetrailer_request_fail",
    perform (new config ~max_trailer_length:10 ()) (d_header_trailer_request 1 100);

    "1x_header_largetrailer_request",
    perform (new config ()) (d_header_largetrailer_request 1 100);

    "1x_header_shorttrailer_request_fail",
    perform (new config ()) d_header_shorttrailer_request;

    "1x_header_badtrailer_request_fail",
    perform (new config ()) d_header_badtrailer_request;

    "5x_header_manychunks_request",
    perform (new config()) (repeat (d_header_chunk_request 10 100) 5);

    "5x_header_trailer_request",
    perform (new config()) (repeat (d_header_trailer_request 1 100) 5);

    "5x_mixed_requests",
    perform (new config()) (repeat 
			  (seq [ (d_empty_empty_request);
		                  (d_header_empty_request_1);
 		                  (d_header_empty_request_2);
			         (d_header_identity_request 100);
			         (d_header_trailer_request 1 100);
			         (d_largeheader_empty_request 20000);
			         d_emptyline; d_empty_empty_request;
			         (d_header_identity_request 20000);
			         (d_header_chunk_request 10 100);
			         (d_header_chunk_request 3 5000);
			         (d_header_largetrailer_request 1 100) ]) 5);
				
  ]
;;


(* --- main --- *)

let main() =
  let do_list = ref false in
  let do_all = ref false in
  let do_tests = ref [] in
  let do_print_req = ref false in
  let do_quiet = ref false in
  let with_crlf = ref false in

  Arg.parse
    (Arg.align
       [ "-list", Arg.Set do_list, " Output the list of available tests";
	"-all", Arg.Set do_all, " Perform all tests";
	"-print-req", Arg.Set do_print_req, " Print request data";
	"-quiet", Arg.Set do_quiet, " Do not print accepted tokens";
	"-crlf", Arg.Set with_crlf, " Use CR/LF as line terminator instead of LF";
       ])
    (fun t -> 
       if List.mem_assoc t tests then
	 do_tests := !do_tests @ [t]
       else
	 raise(Arg.Bad("Unknown test: " ^ t)))
    "usage: test_acceptor [ <options> ] testname ...";

  if !do_all then
    do_tests := List.map fst tests;

  if !do_list then (
    Printf.printf "Available tests:\n";
    List.iter
      (fun (name, _) ->
	 Printf.printf "- %s\n" name)
      tests;
    flush stdout;
  );

  List.iter
    (fun name ->
       let f = List.assoc name tests in
       f (name,!do_print_req,!do_quiet,!with_crlf))
    !do_tests
;;



main();;



