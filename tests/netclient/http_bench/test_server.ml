
type reaction =
    Print_file of string
  | Expect of string
  | End
  | Break_and_reconnect
  | Close_input
  | Close_output
  | Sleep of int
  | Expect_end
  | Reconnect
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
	                (fun _ -> spec := !spec @ [ !line, Reconnect ];
                                  line := 0),
	           "        -expect-end + Allow another connection after EOF";
	"-sleep", Arg.Int 
 	                (fun i -> spec := !spec @ [ !line, Sleep i ]),
	       " <n>        Sleeps <n> seconds";
	"-protocol", Arg.Set protocol,
                  "         turn protocol on stderr on";
	               
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
  while !wait_for_connection do
    wait_for_connection := false;   (* set to 'true' by Reconnect reaction *)

    let l_readable,_,_ = Unix.select [ s ] [] [] 600.0 in
    if l_readable = [] then
      exit(99);      (* timeout *)

    let conn, peer = Unix.accept s in
    n0 := 1;

    if !protocol then 
      prerr_endline "! ACCEPTED NEW CONNECTION";

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
		  m := !m + Unix.write conn fstring !m (l - !m)
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
	      Unix.shutdown conn Unix.SHUTDOWN_SEND;
	      eof_sent := true;
	  | Break_and_reconnect ->
	      if !protocol then
		prerr_endline "BREAKING CONNECTION";
	      Unix.close conn;
	      broken := true;
	      wait_for_connection := true;
	      connopen := false;
	  | Close_input ->
	      if !protocol then
		prerr_endline "CLOSING INPUT SIDE";
	      Unix.shutdown conn Unix.SHUTDOWN_RECEIVE;
	  | Close_output ->
	      if !protocol then
		prerr_endline "CLOSING OUTPUT SIDE";
	      Unix.shutdown conn Unix.SHUTDOWN_SEND;
	  | Expect line ->
	      let actual_line =
		List.nth !lines (!n_lines - lineno) in
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
		Unix.shutdown conn Unix.SHUTDOWN_SEND;
		eof_sent := true;
		failwith "Test failure";
	      end
	  | Sleep i ->
	      if !protocol then
		prerr_endline "SLEEPING";
	      Unix.sleep i
	  | (Expect_end | Reconnect) ->
	      if !protocol then
		prerr_endline "! IGNORING INPUT UNTIL GETTING EOF";
	      let k = ref 1 in
	      while !k <> 0 do
		k := Unix.read conn buff 0 buffsize;
		if !k > 0 & !protocol then
		  prerr_endline ("! IGNORING " ^ string_of_int !k ^ " BYTES");
	      done;
	      if react = Reconnect then
		wait_for_connection := true;
	      connopen := false;
      done;

      (* read as much as immediately possible *)

      if not !broken then begin

	let _ = Unix.select [ conn ] [] [] (-1.0) in
	let k = Unix.read conn buff 0 buffsize in
      
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
	Unix.shutdown conn Unix.SHUTDOWN_SEND;
	eof_sent := true;
      end;
      if !protocol then prerr_endline "! CLOSING SOCKET";
      Unix.close conn;
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

if Sys.file_exists !pidfile then
  Sys.remove !pidfile
;;

