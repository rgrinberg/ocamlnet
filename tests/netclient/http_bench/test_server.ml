open Printf

type reaction =
    Print_file of string
  | Expect of string
  | End
  | Break_and_reconnect
  | Close_input
  | Close_output
  | Sleep of int
  | Expect_end
  | Reconnect of bool option
  | Message_or_reconnect of int * bool option
  | Starttls
;;


let read_file fname =
  (* read file 'fname' as string *)
  let s = ref "" in
  let b = String.create 8192 in
  let fd = Unix.openfile fname [ Unix.O_RDONLY ] 0 in
  let k = ref (Unix.read fd b 0 8192) in
  while !k > 0 do
    s := !s ^ (String.sub b 0 !k);
    k := Unix.read fd b 0 8192;
  done;
  !s
;;


let prerr_buffer s prefix n0 =
  let out_line n k0 k1 =
    prerr_string prefix;
    prerr_int n;
    prerr_string " ";
    prerr_string (String.sub s k0 (k1-k0));
    prerr_newline();
    flush stderr
  in
  let n = ref n0 in
  let k = ref 0 in
  let l = String.length s - 1 in
  for i = 0 to l do
    if s.[i] = '\n' then begin
      out_line !n !k i;
      incr n;
      k := i+1
    end else if i = l then begin
      out_line !n !k (i+1)
    end
  done
;;


let pidfile = ref "server.pid";;

let main() =
  let line = ref 0 in
  let spec = ref [] in
  let protocol = ref false in
  let portfile = ref "server.port" in
  let ssl = ref false in
  let rssl = ref None in
  Arg.parse
      [ "-portfile", Arg.String
	              (fun s -> portfile := s),
                  " <file>  Writes the port number to this file (default server.port)";
	"-pidfile", Arg.String
	              (fun s -> pidfile := s),
		 " <file>   Writes the process ID to this file (default server.pid)";
        "-line", Arg.Int (fun i -> 
			    if i >= !line then line := i
			    else failwith "Line numbers must grow"),
	      " <n>         React after the n-th line of input";
	"-file", Arg.String 
	           (fun s -> spec := !spec @ [ !line, Print_file s ]),
              " <file>      Output contents of file";
	"-end", Arg.Unit
	          (fun _ -> spec := !spec @ [ !line, End ]),
             "              Output EOF marker";
	"-break", Arg.Unit
	            (fun _ -> spec := !spec @ [ !line, Break_and_reconnect ];
		              line := 0),
	       "            Close the connection immediately; wait for next connection";
	"-close-in", Arg.Unit
	            (fun _ -> spec := !spec @ [ !line, Close_input ] ),
        "                   Close only the input side of the connection; continue";
	"-close-out", Arg.Unit
	            (fun _ -> spec := !spec @ [ !line, Close_output ] ),
        "                   Close only the output side of the connection; continue";
	"-expect", Arg.String (fun s -> spec := !spec @ [ !line, Expect s ]),
	        " <string>  Expect that the last input line has these contents";
	"-expect-end", Arg.Unit 
	                 (fun _ -> spec := !spec @ [ !line, Expect_end ]),
	            "       Ignore input until EOF is read";
	"-reconnect", Arg.Unit 
	                (fun _ -> 
			   spec := !spec @ [ !line, Reconnect !rssl ];
			   rssl := None;
                           line := 0),
	           "        -expect-end + Allow another connection after EOF";
	"-message-or-reconnect", 
	   Arg.Int (fun n ->
		      spec := !spec @ [ !line, Message_or_reconnect(n,!rssl) ];
		      rssl := None;
		      line := 0),
	"<n>  Either get a message of exactly n bytes, or assume -reconnect";
	"-sleep", Arg.Int 
 	                (fun i -> spec := !spec @ [ !line, Sleep i ]),
	       " <n>        Sleeps <n> seconds";
	"-starttls", Arg.Unit (fun () -> spec := !spec @ [ !line, Starttls ]),
	          "         Enables TLS at this point of the connection";
	"-protocol", Arg.Set protocol,
                  "         turn protocol on stderr on";
	"-ssl", Arg.Set ssl,
	     "              enable SSL as default";
	"-no-ssl", Arg.Clear ssl,
	     "              disable SSL as default";
	"-reconnect-ssl", Arg.Unit(fun () -> rssl := Some true),
	     "              enable SSL for next -reconnect";
	"-reconnect-no-ssl", Arg.Unit(fun () -> rssl := Some false),
	     "              disable SSL for next -reconnect";
	               
      ]
      (fun s -> failwith ("Bad argument: " ^ s))
      "Usage: test_server [options]

The test server listens on a TCP port and prints the port number to
the file specified by -portfile. The process ID is printed to the file
specified by -pidfile; this file is removed once the server terminates. 
The server  accepts per default one connection (and for every -reconnect
option one more connection), and terminates then.
While the test server reads input lines from the client
it checks certain conditions, and reacts on such events. The -line
option specifies after which line of input (0 means: at the beginning) 
the next event will happen. -file and -end specify reactions; -file outputs
the contents of the file; and -end outputs an EOF marker. Note that while
a file is being sent to the client the input side of the socket is blocked.

The server terminates also on the SIGTERM signal or after 10 minutes idle time
without connection.
";

  let buffsize = 16384 in
  let buff = String.create buffsize in
  let n0 = ref 0 in            (* Line of the first character of 'buff' *)

  let lines = ref [] in      (* Lines read so far, in reverse order *)
  let n_lines = ref 0 in     (* length of !lines *)
  let this_line = ref "" in  (* Current, not yet complete line *)

  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.listen s 10;
  let s_name = Unix.getsockname s in
  let Unix.ADDR_INET(inetaddr,port) = s_name in
  let f_portfile = open_out !portfile in
  output_string f_portfile (string_of_int port);
  output_string f_portfile "\n";
  close_out f_portfile;
  let f_pidfile = open_out !pidfile in
  output_string f_pidfile (string_of_int (Unix.getpid()));
  output_string f_pidfile "\n";
  close_out f_pidfile;
  let wait_for_connection = ref true in
  let thisspec = ref !spec in

  Ssl.init();

  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Server_context in
  Ssl.use_certificate ctx "ssl-cert-snakeoil.pem" "ssl-cert-snakeoil.key";

  let use_ssl = ref !ssl in

  while !wait_for_connection do
    wait_for_connection := false;   (* set to 'true' by Reconnect reaction *)

    let l_readable,_,_ = Unix.select [ s ] [] [] 600.0 in
    if l_readable = [] then
      exit(99);      (* timeout *)

    let conn, peer = Unix.accept s in
    n0 := 1;

    if !protocol then 
      prerr_endline "! ACCEPTED NEW CONNECTION";

    let upgrade() =
      let ssl_sock = Ssl.embed_socket conn ctx in
      Ssl.accept ssl_sock;
      if !protocol then 
	prerr_endline "! ACCEPTED SSL SESSION";
      ( object
	  method read s p n = 
	    try Ssl.read ssl_sock s p n
	    with Ssl.Read_error Ssl.Error_zero_return -> 0
	      | Ssl.Read_error Ssl.Error_syscall ->
		  if !protocol then
		    prerr_endline "! UNCLEAN SSL STATE";
		  0
	  method write s p n = Ssl.write ssl_sock s p n
	  method shutdown_in () = Ssl.shutdown ssl_sock
	  method shutdown_out () = 
	    let (flag_in, flag_out) = Ssl_exts.get_shutdown ssl_sock in
	    if not flag_out then
		Ssl_exts.single_shutdown ssl_sock
	  method close() = Ssl.shutdown ssl_sock; Unix.close conn
	end
      ) in

    let ops =
      if !use_ssl then 
	ref(upgrade())
      else
	ref
	  ( object
	      method read s p n = Unix.read conn s p n
	      method write s p n = Unix.single_write conn s p n
	      method shutdown_in () = Unix.shutdown conn Unix.SHUTDOWN_RECEIVE
	      method shutdown_out () = Unix.shutdown conn Unix.SHUTDOWN_SEND
	      method close() = Unix.close conn
	    end
	  ) in
    
    let connopen = ref true in
    let eof_sent = ref false in  (* i.e. write side closed *)
    let broken = ref false in    (* i.e. read side closed *)
    while !connopen do

      (* Interpret 'spec' and react *)

      while !connopen & !thisspec <> [] & fst(List.hd !thisspec) < !n0 do
	let (lineno, react) :: rest_spec = !thisspec in
	thisspec := rest_spec;
	if !protocol then
	  prerr_string ("! EVENT ON #" ^ string_of_int lineno ^ ": ");
	match react with
	    Print_file fname ->
	      if !protocol then
		prerr_endline ("SENDING FILE " ^ fname);
	      let fstring = read_file fname in
	      if !protocol then
		prerr_buffer fstring "> " 1;
	      let m = ref 0 in
	      let l = String.length fstring in
              begin try
	        while !m < l do
		  m := !m + !ops#write fstring !m (l - !m)
	        done
              with
                 Unix.Unix_error(Unix.EPIPE,_,_) ->
                   if !protocol then
                     prerr_endline "! BROKEN PIPE";
                   eof_sent := true;
              end
	  | End ->
	      if !protocol then
		prerr_endline "SENDING EOF";
	      !ops#shutdown_out();
	      eof_sent := true;
	  | Break_and_reconnect ->
	      if !protocol then
		prerr_endline "BREAKING CONNECTION";
	      !ops#close();
	      broken := true;
	      wait_for_connection := true;
	      connopen := false;
	  | Close_input ->
	      if !protocol then
		prerr_endline "CLOSING INPUT SIDE";
	      !ops#shutdown_in();
	  | Close_output ->
	      if !protocol then
		prerr_endline "CLOSING OUTPUT SIDE";
	      !ops#shutdown_out();
	  | Expect line ->
	      let actual_line =
		List.nth !lines (!n_lines - lineno) in
	      let l_actual_line =
		String.length actual_line in
	      let actual_line =
		if actual_line <> "" && actual_line.[l_actual_line - 1] = '\r'
		then
		  String.sub actual_line 0 (l_actual_line-1)
		else
		  actual_line in
	      if !protocol then
		prerr_endline ("EXPECTING LINE '" ^ line ^ "'");
	      if actual_line = line then begin
		if !protocol then
		  prerr_endline ("! THE LINE MATCHED")
	      end
	      else begin
		if !protocol then begin
		  prerr_endline ("! GOT LINE '" ^ actual_line ^ "'");
		  prerr_endline ("! THE LINE DOES NOT MATCH. SENDING EOF");
		end;
		!ops#shutdown_out();
		eof_sent := true;
		failwith "Test failure";
	      end
	  | Sleep i ->
	      if !protocol then
		prerr_endline "SLEEPING";
	      Unix.sleep i
	  | Starttls ->
	      if !protocol then
		prerr_endline "STARTTLS";
	      ops := upgrade()
	  | Expect_end ->
	      if !protocol then
		prerr_endline "! IGNORING INPUT UNTIL GETTING EOF";
	      let k = ref 1 in
	      while !k <> 0 do
		k := !ops#read buff 0 buffsize;
		if !k > 0 & !protocol then
		  prerr_endline ("! IGNORING " ^ string_of_int !k ^ " BYTES");
	      done;
	      connopen := false;
	  | Reconnect rssl ->
	      if !protocol then
		prerr_endline "! IGNORING INPUT UNTIL GETTING EOF";
	      let k = ref 1 in
	      while !k <> 0 do
		k := !ops#read buff 0 buffsize;
		if !k > 0 & !protocol then
		  prerr_endline ("! IGNORING " ^ string_of_int !k ^ " BYTES");
	      done;
	      wait_for_connection := true;
	      connopen := false;
	      ( match rssl with
		  | None -> use_ssl := !ssl
		  | Some flag -> use_ssl := flag
	      )
	  | Message_or_reconnect (n,rssl) ->
	      if !protocol then
		eprintf "! WILL IGNORE %d BYTES, OR ANY BYTES UNTIL EOF\n%!" n;
	      let k = ref n in
	      let eof = ref false in
	      while !k > 0 do
		let p = !ops#read buff 0 (min buffsize !k) in
		if p > 0 && !protocol then
		  prerr_endline ("! IGNORING " ^ string_of_int p ^ " BYTES");
		if p = 0 then (
		  if !protocol then
		    prerr_endline "! GOT EOF";
		  k := 0;
		  eof := true
		)
		else k := !k - p
	      done;
	      if !eof then (
		connopen := false;
		wait_for_connection := true;
		( match rssl with
		    | None -> use_ssl := !ssl
		    | Some flag -> use_ssl := flag
		)
	      );
	      n0 := 1    (* The next line has always number 1 *)
      done;

      (* read as much as immediately possible *)

      if not !broken then begin

	(* let _ = Unix.select [ conn ] [] [] (-1.0) in*)
	let k = !ops#read buff 0 buffsize in
      
	if k = 0 then begin
	  (* got EOF *)
	  if !protocol then
	    prerr_endline "! GOT EOF.";
	  connopen := false
	end
	else begin
	  if !protocol then
	    prerr_buffer (String.sub buff 0 k) "< " !n0;
	  (* count number of lines *)
	  let i_start = ref 0 in
	  for i = 0 to k-1 do
	    if buff.[i] = '\n' then begin
	      lines := 
	      (!this_line ^ String.sub buff !i_start (i - !i_start)) :: !lines;
	      incr n_lines;
	      this_line := "";
	      i_start := i+1;
	      incr n0
	    end
	  done;
	  this_line := !this_line ^ String.sub buff !i_start (k - !i_start);
	end
      end;
    done; (* while !connopen *)

    if not !broken then begin
      if not !eof_sent then begin
	if !protocol then prerr_endline "! IMMEDIATELY REPLYING EOF";
	!ops#shutdown_out();
	eof_sent := true;
      end;
      if !protocol then prerr_endline "! CLOSING SOCKET";
      !ops#close();
    end;

  done (* while !wait_for_connection *)
;;

Sys.signal Sys.sigpipe Sys.Signal_ignore;

begin try
  main()
with
    e ->
      prerr_endline ("Exception: "  ^ Printexc.to_string e)
end;

Unix.sleep 1;

if Sys.file_exists !pidfile then (
  Sys.remove !pidfile;
  Netsys.sleep 0.05;   (* makes a race unlikely - between regular termination and kill *)
)
;;

