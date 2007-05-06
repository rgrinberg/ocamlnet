(* $Id$ *)

open Telnet_client
open Ftp_data_endpoint

exception FTP_error of exn
exception FTP_protocol_violation of string

let proto_viol s =
  raise(FTP_protocol_violation s)

let verbose = true

type cmd_state =
    [ `Init
    | `Success
    | `Proto_error
    | `Temp_failure
    | `Perm_failure
    | `Rename_seq
    | `Restart_seq
    | `User_pass_seq
    | `User_acct_seq
    | `Pass_acct_seq
    | `Preliminary
    ]

type port =
    [ `Active of string * int * Unix.file_descr
    | `Passive of string * int
    | `Unspecified
    ]

type form_code =
    [ `Non_print | `Telnet | `ASA ]

type representation =
    [ `ASCII of form_code option
    | `EBCDIC of form_code option
    | `Image
    ]

type structure =
    [ `File_structure
    | `Record_structure
    ]

type transmission_mode =
    Ftp_data_endpoint.transmission_mode

type ftp_state =
    { cmd_state : cmd_state;
      ftp_connected : bool;
      ftp_data_conn : bool;
      ftp_user : string option;
      ftp_password : string option;
      ftp_account : string option;
      ftp_logged_in : bool;
      ftp_port : port;
      ftp_repr : representation;
      ftp_structure : structure;
      ftp_trans : transmission_mode;
      ftp_dir : string list;
      ftp_features : (string * string option) list option;
      ftp_options : (string * string option) list;
    }


type cmd =
    [ `Connect
    | `Dummy
    | `USER of string
    | `PASS of string
    | `ACCT of string
    | `CWD of string
    | `CDUP
    | `SMNT of string
    | `QUIT
    | `REIN
    | `PORT
    | `PASV
    | `TYPE of representation
    | `STRU of structure
    | `MODE of transmission_mode
    | `RETR of string * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `STOR of string * (ftp_state -> Ftp_data_endpoint.local_sender)
    | `STOU of (unit -> Ftp_data_endpoint.local_sender)
    | `APPE of string * (ftp_state -> Ftp_data_endpoint.local_sender)
    | `ALLO of int * int option
    | `REST of string
    | `RNFR of string
    | `RNTO of string
    | `DELE of string
    | `RMD of string
    | `MKD of string
    | `PWD
    | `LIST of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `NLST of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `SITE of string
    | `SYST
    | `STAT of string option
    | `HELP of string option
    | `NOOP
    | `FEAT
    | `OPTS of string * string option
    | `MDTM of string
    ]

let string_of_cmd =
  function
    | `Connect -> ""
    | `Dummy -> ""
    | `USER s -> "USER " ^ s ^ "\r\n"
    | `PASS s -> "PASS " ^ s ^ "\r\n"
    | `ACCT s -> "ACCT " ^ s ^ "\r\n"
    | `CWD s  -> "CWD " ^ s ^ "\r\n"
    | `CDUP   -> "CDUP\r\n"
    | `SMNT s -> "SMNT " ^ s ^ "\r\n"
    | `QUIT   -> "QUIT\r\n"
    | `REIN   -> "REIN\r\n"
    | `PORT   -> assert false   (* not done here *)
    | `PASV   -> "PASV\r\n"
    | `TYPE t -> "TYPE " ^ 
	         ( match t with
		     | `ASCII None -> "A"
		     | `ASCII (Some `Non_print) -> "A N"
		     | `ASCII (Some `Telnet) -> "A T"
		     | `ASCII (Some `ASA) -> "A C"
		     | `EBCDIC None -> "E"
		     | `EBCDIC (Some `Non_print) -> "E N"
		     | `EBCDIC (Some `Telnet) -> "E T"
		     | `EBCDIC (Some `ASA) -> "E C"
		     | `Image -> "I"
		 ) ^ "\r\n"
    | `STRU `File_structure -> "STRU F\r\n"
    | `STRU `Record_structure -> "STRU R\r\n"
    | `MODE `Stream_mode -> "MODE S\r\n"
    | `MODE `Block_mode -> "MODE B\r\n"
    | `RETR (s,_) -> "RETR " ^ s ^ "\r\n"
    | `STOR (s,_) -> "STOR " ^ s ^ "\r\n"
    | `STOU _     -> "STOU\r\n"
    | `APPE (s,_) -> "APPE " ^ s ^ "\r\n"
    | `ALLO(n,r)  -> "ALLO " ^ string_of_int n ^ 
	             (match r with
			| None -> ""
			| Some m -> " R " ^ string_of_int m) ^ "\r\n"
    | `REST s -> "REST " ^ s ^ "\r\n"
    | `RNFR s -> "RNFR " ^ s ^ "\r\n"
    | `RNTO s -> "RNTO " ^ s ^ "\r\n"
    | `DELE s -> "DELE " ^ s ^ "\r\n"
    | `RMD  s -> "RMD " ^ s ^ "\r\n"
    | `MKD  s -> "MKD " ^ s ^ "\r\n"
    | `PWD    -> "PWD\r\n"
    | `LIST(None,_) -> "LIST\r\n"
    | `LIST(Some s,_) -> "LIST " ^ s ^ "\r\n"
    | `NLST(None,_) -> "NLST\r\n"
    | `NLST(Some s,_) -> "NLST " ^ s ^ "\r\n"
    | `SITE s -> "SITE " ^ s ^ "\r\n"
    | `SYST   -> "SYST\r\n"
    | `STAT None -> "STAT\r\n"
    | `STAT(Some s) -> "STAT " ^ s ^ "\r\n"
    | `HELP None -> "HELP\r\n"
    | `HELP(Some s) -> "HELP " ^ s ^ "\r\n"
    | `NOOP -> "NOOP\r\n"
    | `FEAT -> "FEAT\r\n"
    | `OPTS (cmd,None) -> "OPTS " ^ cmd ^ "\r\n"
    | `OPTS (cmd,Some param) -> "OPTS " ^ cmd ^ " " ^ param ^ "\r\n"
    | `MDTM s -> "MDTM " ^ s ^ "\r\n"


let port_re = Netstring_pcre.regexp ".*[^0-9](\\d+),(\\d+),(\\d+),(\\d+),(\\d+),(\\d+)"

let extract_port s =
  match Netstring_pcre.string_match port_re s 0 with
    | None ->
	proto_viol "Cannot parse specification of passive port"
    | Some m ->
	let h1 = Netstring_pcre.matched_group m 1 s in
	let h2 = Netstring_pcre.matched_group m 2 s in
	let h3 = Netstring_pcre.matched_group m 3 s in
	let h4 = Netstring_pcre.matched_group m 4 s in
	let p1 = Netstring_pcre.matched_group m 5 s in
	let p2 = Netstring_pcre.matched_group m 6 s in
	let p = int_of_string p1 * 256 + int_of_string p2 in
	(h1 ^ "." ^ h2 ^ "." ^ h3 ^ "." ^ h4, p)

let addr_re = Netstring_pcre.regexp "^(\\d+)\\.(\\d+)\\.(\\d+)\\.(\\d+)$"

let format_port (addr,p) =
  match Netstring_pcre.string_match addr_re addr 0 with
    | None ->
	failwith "Bad IP address"
    | Some m ->
	let h1 = Netstring_pcre.matched_group m 1 addr in
	let h2 = Netstring_pcre.matched_group m 2 addr in
	let h3 = Netstring_pcre.matched_group m 3 addr in
	let h4 = Netstring_pcre.matched_group m 4 addr in
	let p1 = string_of_int(p lsr 8) in
	let p2 = string_of_int(p land 0xff) in
	h1 ^ "," ^ h2 ^ "," ^ h3 ^ "," ^ h4 ^ "," ^ p1 ^ "," ^ p2

let set_ftp_port state value =
  ( match state.ftp_port with
      | `Active(_,_,fd) ->
	  Unix.close fd
      | _ ->
	  ()
  );
  { state with ftp_port = value }


let feature_line_re = Netstring_pcre.regexp "^ ([\x21-\xff]+)( .*)?$"

let line_re = Netstring_pcre.regexp "\r?\n"

let parse_features s =
  let lines = Netstring_pcre.split line_re s in
  List.flatten
    (List.map
       (fun line ->
	  match Netstring_pcre.string_match feature_line_re line 0 with
	    | None -> []
	    | Some m ->
		let label = Netstring_pcre.matched_group m 1 line in
		let param = 
		  try Some(Netstring_pcre.matched_group m 2 line)
		  with Not_found -> None in
		[ label, param ]
       )
       lines)

type reply = int * string
    (* Reply code plus text *)


let init_state s =
  let _s_name =
    match Unix.getsockname s with
      | Unix.ADDR_INET(addr,_) ->
	  Unix.string_of_inet_addr addr
      | _ ->
	  failwith "Not an internet socket"
  in
  { cmd_state = `Init;
    ftp_connected = true;
    ftp_data_conn = false;
    ftp_user = None;
    ftp_password = None;
    ftp_account = None;
    ftp_logged_in = false;
    ftp_port = `Unspecified;
    ftp_repr = `ASCII None;
    ftp_structure = `File_structure;
    ftp_trans = `Stream_mode;
    ftp_dir = [];
    ftp_features = None;
    ftp_options = [];
  }


let start_reply_re = Netstring_pcre.regexp "^[0-9][0-9][0-9]-"
let end_reply_re = Netstring_pcre.regexp "^[0-9][0-9][0-9] "


let is_active state =
  match state.ftp_port with
    | `Active _ -> true
    | _ -> false

let is_passive state =
  match state.ftp_port with
    | `Passive _ -> true
    | _ -> false


class work_engine e =
object
  method is_working =
    match e # state with
      | `Working _ -> true
      | _ -> false
  method abort() =
    match e # state with
      | `Working _ -> e # abort()
      | _ -> ()
end


exception Dummy


class ftp_client_pi
        ?(event_system = Unixqueue.create_unix_event_system())
        ?(onempty = fun _ -> ())
        ?(onclose = fun () -> ())
        ?(onerrorstate = fun _ -> ())
        ?(onusererror = fun _ -> ())
        sock =
  let ctrl_input_buffer = Netbuffer.create 500 in
  let ctrl_input, ctrl_input_shutdown = 
    Netchannels.create_input_netbuffer ctrl_input_buffer in
object(self)

  val queue = Queue.create()

  val mutable state = `Working 0

  val mutable ftp_state = init_state sock

  val mutable data_engine = None
  val mutable work_engines = ( [] : work_engine list)

  val ctrl = new telnet_session

  val reply_text = Buffer.create 200
  val mutable reply_code = (-1)
  val mutable reply_callback = (fun _ _ -> ())

  val mutable interaction_state = 
    ( `Waiting `Connect 
	: [ `Ready 
          | `Connecting_pasv of cmd * Uq_engines.connect_status Uq_engines.engine
	  | `Listening_actv of cmd * Unixqueue.event Uq_engines.engine
          | `Transfer of cmd
	  | `Transfer_replied of cmd * int * string
	  | `Waiting of cmd
	  ] )
      (* `Ready: another command can be sent now
       * `Connecting_pasv: In passive mode, we are connecting to the
       *    remote port (done by the argument engine). This state is
       *    skipped when we are still connected.
       * `Listening_actv: In active mode, we are listening for the
       *    connect (done by the argument engine). This state is
       *    skipped when we are still connected.
       * `Transfer: a data transfer is in progress
       * `Transfer_replied: while the rest of the transfer is not yet
       *    done, the server already sent a reply
       * `Waiting: it is waited for the reply
       *)



  initializer (
    ctrl # set_connection (Telnet_socket sock);
    ctrl # set_event_system event_system;
    ctrl # set_callback self#receive_ctrl_reply;
    ctrl # set_exception_handler self#catch_telnet_exception;
    ctrl # attach();
  )

  method ftp_state = ftp_state

  method is_empty = Queue.is_empty queue

  method state = (state : unit Uq_engines.engine_state)

  method event_system = event_system

  method abort() =
    ctrl # reset();
    self # close_connection();
    self # set_state `Aborted

  method private catch_telnet_exception e =
    ctrl # reset();
    self # close_connection();
    self # set_state (`Error(Telnet_protocol e))

  method private set_state s =
    state <- s;
    match s with
      | `Error e -> onerrorstate e
      | _ -> ()

  method private set_error e =
    ctrl # reset();
    self # close_connection();
    self # set_state (`Error e)

  method private protect f =
    (* Run [f ()] and catch exceptions *)
    try
      f()
    with
	err ->
	  ctrl # reset();
	  self # close_connection();
	  self # set_state (`Error err)
      

  method private clean_work_engines() =
    work_engines <- List.filter (fun e -> e # is_working) work_engines


  method private receive_ctrl_reply got_synch =
    while not (Queue.is_empty ctrl#input_queue) do
      let tc = Queue.take ctrl#input_queue in
      match tc with
	| Telnet_data data ->
	    Netbuffer.add_string ctrl_input_buffer data;
	    self # protect (self # parse_ctrl_reply)

	| Telnet_nop ->
	    ()

	| Telnet_will _
	| Telnet_wont _
	| Telnet_do _
	| Telnet_dont _ ->
	    ctrl # process_option_command tc

	| Telnet_sb _ 
	| Telnet_se ->
	    ()    (* Ignore subnegotiation *)

	| Telnet_eof ->
	    ctrl_input_shutdown();
	    self # protect (self # parse_ctrl_reply)

	| Telnet_timeout ->
	    ()  (* TODO *)

	| _ ->
	    (* Unexpected telnet command *)
	    self # protect (fun () ->
			      proto_viol "Unexpected command on Telnet level")
    done

  method private parse_ctrl_reply() =
    try
      while true do
	let line = ctrl_input # input_line() in  (* or exception! *)
	if verbose then prerr_endline ("< " ^ line);
	if Netstring_pcre.string_match start_reply_re line 0 <> None then (
	  let code = int_of_string (String.sub line 0 3) in
	  if reply_code <> (-1) && reply_code <> code then 
	    proto_viol "Parse error of control message";
	  reply_code <- code;
	  Buffer.add_string reply_text line;
	  Buffer.add_string reply_text "\n";
	)
	else
	  if Netstring_pcre.string_match end_reply_re line 0 <> None then (
	    let code = int_of_string (String.sub line 0 3) in
	    if reply_code <> (-1) && reply_code <> code then
	      proto_viol "Parse error of control message";
	    Buffer.add_string reply_text line;
	    Buffer.add_string reply_text "\n";
	    let text = Buffer.contents reply_text in
	    reply_code <- (-1);
	    Buffer.clear reply_text;
	    self # interpret_ctrl_reply code text;
	  )
	  else (
	    if reply_code = (-1) then
	      proto_viol "Parse error of control message";
	    Buffer.add_string reply_text line;
	    Buffer.add_string reply_text "\n";
	  )
      done
    with
      | Netchannels.Buffer_underrun ->
	  (* No complete line yet *)
	  ()
      | End_of_file ->
	  onclose();
	  self # set_state (`Done ())

  method private interpret_ctrl_reply code text =
    (* This method is called whenever a reply has been completely received.
     * This may happen in a number of situations:
     * - As greeting message
     * - As regular response to a sent FTP command
     * - When the data transfer is over. Note that the control response
     *   may be received before the end of the transfer is seen by the
     *   client.
     * - Within the data transfer
     * - At any other point in time, but this is regarded as protocol error
     *)
    let reply st cmd_state =
      let st' = { st with cmd_state = cmd_state } in
      ftp_state <- st';
      try
	reply_callback st' (code,text)
      with
	  err ->
	    onusererror err
    in
    let ready() =
      interaction_state <- `Ready in
    ( match interaction_state with
	| `Ready ->
	    proto_viol "Spontaneous control message"
	| `Waiting `Connect ->
	    (match code with
	       | 120 -> reply ftp_state `Preliminary
	       | 220 -> ready(); reply ftp_state `Success
	       | n when n >= 400 && n <= 499 ->
		   ready(); reply ftp_state `Temp_failure
	       | n when n >= 500 && n <= 599 ->
		   ready(); reply ftp_state `Perm_failure
	       | _   -> proto_viol "Unexpected control message"
	    )
	| `Waiting `Dummy ->
	    ready(); reply ftp_state `Success
	| `Waiting (`USER s) ->
	    ( match code with
		| 230 -> 
		    ready(); 
                    reply { ftp_state with 
			      ftp_user = Some s;
			      ftp_password = None;
			      ftp_account = None;
			      ftp_logged_in = true } `Success
		| 530 -> 
		    ready();
		    reply { ftp_state with
			      ftp_logged_in = false } `Perm_failure
		| 331 -> 
		    ready(); 
		    reply { ftp_state with 
			      ftp_user = Some s;
			      ftp_password = None;
			      ftp_account = None;
			      ftp_logged_in = false } `User_pass_seq
		| 332 -> 
		    ready();
		    reply { ftp_state with 
			      ftp_user = Some s;
			      ftp_password = None;
			      ftp_account = None;
			      ftp_logged_in = false } `User_acct_seq
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`PASS s) ->
	    ( match code with
		| 202 | 230 -> 
		    ready(); 
		    reply { ftp_state with
			      ftp_password = Some s;
			      ftp_account = None;
			      ftp_logged_in = true } `Success
		| 530 ->
		    ready();
		    reply { ftp_state with
			      ftp_logged_in = false } `Perm_failure
		| 332 ->  
		    ready();
		    reply { ftp_state with 
			      ftp_password = Some s;
			      ftp_account = None;
			      ftp_logged_in = false } `Pass_acct_seq
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`ACCT s) ->
	    ( match code with
		| 202 | 230 -> 
		    ready(); 
		    reply { ftp_state with
			      ftp_account = Some s;
			      ftp_logged_in = true } `Success
		| 530 -> 
		    ready();
		    reply { ftp_state with
			      ftp_logged_in = false } `Perm_failure
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`CWD s) ->
	    ( match code with
		| 200 | 250 -> 
		    ready(); 
		    let ftp_state' =
		      { ftp_state with ftp_dir = s :: ftp_state.ftp_dir } in
		    reply ftp_state' `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting `CDUP ->
	    ( match code with
		| 200 | 250 -> 
		    ready();
		    let ftp_state' =
		      match ftp_state.ftp_dir with
			| [] -> ftp_state
			| _ :: dir ->
			    { ftp_state with ftp_dir = dir } in
		    reply ftp_state' `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting `REIN ->
	    ( match code with
		| 120 -> 
		    reply ftp_state `Preliminary
		| 220 -> 
		    ready(); reply (init_state sock) `Success
		    (* CHECK: Close data connection? *)
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting `PASV ->
	    ( match code with
		| 227 ->
		    let (addr,port) = extract_port text in
		    ready();
		    self # close_connection();
		    reply (set_ftp_port ftp_state (`Passive(addr,port))) `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`TYPE t) ->
	    ( match code with
		| n when n >= 200 && n <= 299 ->
		    ready();
		    reply { ftp_state with ftp_repr = t } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`MODE m) ->
	    ( match code with
		| n when n >= 200 && n <= 299 ->
		    ready();
		    reply { ftp_state with ftp_trans = m } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`STRU s) ->
	    ( match code with
		| n when n >= 200 && n <= 299 ->
		    ready();
		    reply { ftp_state with ftp_structure = s } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`REST _) ->
	    ( match code with
		| 350 ->
		    ready();
		    reply ftp_state `Restart_seq
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`RNFR _) ->
	    ( match code with
		| 350 ->
		    ready(); reply ftp_state `Rename_seq
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting `FEAT ->
	    ( match code with
		| 211 ->
		    let l = parse_features text in
		    ready(); 
		    reply { ftp_state with ftp_features = Some l } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`OPTS(cmd,param_opt)) ->
	    ( match code with
		| n when n >= 200 && n <= 299 ->
		    let l =
		      (cmd, param_opt) ::
			(List.filter
			   (fun (cmd',_) -> 
			      String.lowercase cmd <> String.lowercase cmd')
			   ftp_state.ftp_options) in
		    ready(); 
		    reply { ftp_state with ftp_options = l } `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	| `Waiting (`SMNT _)
	| `Waiting `QUIT
	| `Waiting `PORT
	| `Waiting (`ALLO _)
	| `Waiting (`RNTO _)
	| `Waiting (`DELE _)
	| `Waiting (`RMD _)
	| `Waiting (`MKD _)
	| `Waiting `PWD 
 	| `Waiting `SYST
 	| `Waiting (`STAT _)
	| `Waiting (`HELP _)
	| `Waiting (`SITE _)
	| `Waiting (`MDTM _)
            (* See http://www.ietf.org/internet-drafts/draft-ietf-ftpext-mlst-16.txt *)
	| `Waiting `NOOP ->
	    ( match code with
		| n when n >= 100 && n <= 199 ->
		    reply ftp_state `Preliminary
		| n when n >= 200 && n <= 299 ->
		    ready(); reply ftp_state `Success
		| n when n >= 400 && n <= 499 ->
		    ready(); reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    ready(); reply ftp_state `Perm_failure
		| _   -> 
		    proto_viol "Unexpected control message"
	    )
	(* `STOR |`STOU | `APPE left out for now *)

	| `Connecting_pasv(_, conn_engine) ->
	    (* Somewhat unexpected reply! *)
	    ( match code with
		| 125 | 150 ->
		    reply ftp_state `Preliminary
		| n when n >= 400 && n <= 499 ->
		    conn_engine # abort();
		    ready();
		    reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    conn_engine # abort();
		    ready();
		    reply ftp_state `Perm_failure
		| _ ->
		    conn_engine # abort();
		    proto_viol "Unexpected control message"
	    )
	| `Listening_actv(_, acc_engine) ->
	    (* Somewhat unexpected reply! *)
	    ( match code with
		| 125 | 150 ->
		    reply ftp_state `Preliminary
		| n when n >= 400 && n <= 499 ->
		    acc_engine # abort();
		    ready();
		    reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    acc_engine # abort();
		    ready();
		    reply ftp_state `Perm_failure
		| _ ->
		    acc_engine # abort();
		    proto_viol "Unexpected control message"
	    )
	| `Transfer cmd ->
	    (* The transfer probably ends in the near future, just record
             * the reply, and wait for the end of the transfer.
             *)
	    ( match code with
		| 125 | 150 ->
		    reply ftp_state `Preliminary
		| n when n >= 400 && n <= 499 ->
		    self # close_connection();
		    ready();
		    reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    self # close_connection();
		    ready();
		    reply ftp_state `Perm_failure
		| _ ->
		    interaction_state <- `Transfer_replied(cmd,code,text)
	    )
	| `Transfer_replied cmd ->
	    (* Another reply! This is an error. *)
	    proto_viol "Unexpected control message"
	| `Waiting(`RETR(_,f))
	| `Waiting(`LIST(_,f))
	| `Waiting(`NLST(_,f)) ->
	    (* This state is only possible when the transfer has already
             * been completed.
             *)
	    ( match data_engine with
		| None -> ()  (* strange *)
		| Some e -> 
		    if e # descr_state <> `Clean then self # close_connection()
	    );
	    ( match code with
		| 125 | 150 ->
		    reply ftp_state `Preliminary
		| 226 ->
		    self # close_connection();
 		    ready();
		    reply ftp_state `Success
		| 250 ->
 		    ready();
		    reply ftp_state `Success
		| n when n >= 400 && n <= 499 ->
		    self # close_connection();
		    reply ftp_state `Temp_failure
		| n when n >= 500 && n <= 599 ->
		    self # close_connection();
		    reply ftp_state `Perm_failure
		| _ ->
		    proto_viol "Unexpected control message"
	    )
	| _ -> assert false

    );
    self # send_command_when_ready()


  method private send_command_when_ready() =
    if interaction_state = `Ready then (
      try
	assert(reply_code = (-1));
	let (cmd, onreply) = Queue.take queue in  (* or Queue.Empty *)
	interaction_state <- `Waiting cmd;
	( match cmd with
	    | `RETR(_,f)
	    | `LIST(_,f)
	    | `NLST(_,f) ->
		( match ftp_state.ftp_port with
		    | `Passive(_,_) ->
			(* In passive mode, connect now: *)
			self # setup_passive_receiver cmd f
			
		    | `Active(_,_,_) ->
			(* In active mode, accept the connection now *)
			self # setup_active_receiver cmd f

		    | `Unspecified ->
			failwith "FTP client: Usage error, one must send `PORT or `PASV before the transfer"
		)
	    | _ -> ()
	);
	let line = 
	  match cmd with
	    | `PORT ->
		let addr =  (* of control connection *)
		  match Unix.getsockname sock with
		    | Unix.ADDR_INET(addr,_) -> addr
		    | _ -> assert false in
		let addr_str = Unix.string_of_inet_addr addr in
		let dom = Netsys.domain_of_inet_addr addr in
		let server_sock = Unix.socket dom Unix.SOCK_STREAM 0 in
		Unix.bind server_sock (Unix.ADDR_INET(addr,0));
		Unix.listen server_sock 1;
		let port =
		  match Unix.getsockname server_sock with
		    | Unix.ADDR_INET(_,port) -> port
		    | _ -> assert false in
		ftp_state <- set_ftp_port ftp_state (`Active(addr_str,port,server_sock));
		let port_str = format_port (addr_str,port) in
		"PORT " ^ port_str ^ "\r\n"
	    | _ -> string_of_cmd cmd in
	reply_callback <- onreply;
	if line <> "" then (
	  if verbose then prerr_endline ("> " ^ line);
	  Queue.push (Telnet_data line) ctrl#output_queue;
	  ctrl # update();
	) else (
	  if verbose then prerr_endline "> DUMMY";
	  raise Dummy
	)
      with
	| Queue.Empty ->
	    onempty ftp_state
	| Dummy ->
	    self # interpret_ctrl_reply 200 "200 DUMMY"
    )


  method private close_connection() =
    (* Terminates any transfer immediately and closes the connection *)
    ( match data_engine with
	| None -> ()
	| Some e -> 
	    ( match e # state with
		| `Working _ -> e # abort()
		| _ -> ()
	    );
	    let data_sock = e # descr in
	    Unix.close data_sock;
	    data_engine <- None;
    );
    ftp_state <- set_ftp_port ftp_state `Unspecified;
    List.iter (fun e -> e # abort()) work_engines;
    work_engines <- []


  method private setup_passive_receiver cmd f =
    assert(is_passive ftp_state);
    ( match data_engine with
	| Some e ->
	    (* Continue with the old connection *)
	    if e # descr_state <> `Clean then
	      proto_viol "Data connection not clean";
	    (* Create a new engine taking the connection over *)
	    let data_sock = e # descr in
	    self # setup_receiver 
	      ~is_done:(fun () ->
			  match interaction_state with
			    | `Transfer_replied(_,c,t) ->
				interaction_state <-
				  `Waiting cmd;
				self # interpret_ctrl_reply c t
			    | `Transfer cmd ->
				interaction_state <-
				  `Waiting cmd
			    | _ -> assert false
		       )
	      ~is_error:(fun err ->
			   self # set_error (FTP_error err))
	      f data_sock;
	    interaction_state <- `Transfer cmd
	| None ->
	    (* Indicates that a connection is to be opened *)
	    let addr,port =
	      match ftp_state.ftp_port with
		| `Passive(a,p) -> a,p | _ -> assert false in
	    let conn_engine =
	      Uq_engines.connector
		(`Socket(
		   `Sock_inet(Unix.SOCK_STREAM,
			      Unix.inet_addr_of_string addr,
			      port),
		   Uq_engines.default_connect_options))
		event_system in
	    Uq_engines.when_state
	      ~is_done:(function
			  | `Socket(data_sock,_) ->
			      self # setup_receiver 
				~is_done:(fun () ->
					    match interaction_state with
					      | `Transfer_replied(_,c,t) ->
						  interaction_state <-
						    `Waiting cmd;
						  self # interpret_ctrl_reply c t
					      | `Transfer cmd ->
						  interaction_state <-
						    `Waiting cmd
					      | _ -> assert false
					 )
				~is_error:(fun err ->
					     self # set_error (FTP_error err))
				f data_sock;
			      interaction_state <- `Transfer cmd;
			      self # clean_work_engines();
			  | _ -> assert false
		       )
	      ~is_error:(fun err -> 
			   self # clean_work_engines();
			   self # set_error (FTP_error err))
	      conn_engine;
	    work_engines <- new work_engine conn_engine :: work_engines;
	    interaction_state <- `Connecting_pasv(cmd,conn_engine);
    )

  method private setup_active_receiver cmd f =
    assert(is_active ftp_state);
    ( match data_engine with
	| Some e ->
	    (* Continue with the old connection *)
	    if e # descr_state <> `Clean then
	      proto_viol "Data connection not clean";
	    (* Create a new engine taking the connection over *)
	    let data_sock = e # descr in
	    self # setup_receiver 
	      ~is_done:(fun () ->
			  match interaction_state with
			    | `Transfer_replied(_,c,t) ->
				interaction_state <-
				  `Waiting cmd;
				self # interpret_ctrl_reply c t
			    | `Transfer cmd ->
				interaction_state <-
				  `Waiting cmd
			    | _ -> assert false
		       )
	      ~is_error:(fun err ->
			   self # set_error (FTP_error err))
	      f data_sock;
	    interaction_state <- `Transfer cmd
	| None ->
	    (* Indicates that a connection is to be opened *)
	    let addr,port,server_sock =
	      match ftp_state.ftp_port with
		| `Active(a,p,fd) -> a,p,fd | _ -> assert false in
	    let acc_engine =
	      new Uq_engines.poll_engine 
		[ (Unixqueue.Wait_in server_sock), (-1.0) ] event_system in
	    Uq_engines.when_state
	      ~is_done:(function
			  | Unixqueue.Input_arrived(_,_) ->
			      let data_sock, _ = Unix.accept server_sock in
			      self # setup_receiver 
				~is_done:(fun () ->
					    match interaction_state with
					      | `Transfer_replied(_,c,t) ->
						  interaction_state <-
						    `Waiting cmd;
						  self # interpret_ctrl_reply c t
					      | `Transfer cmd ->
						  interaction_state <-
						    `Waiting cmd
					      | _ -> assert false
					 )
				~is_error:(fun err ->
					     self # set_error (FTP_error err))
				f data_sock;
			      interaction_state <- `Transfer cmd;
			      self # clean_work_engines()
			  | _ ->
			      assert false
		       )
	      ~is_error:(fun err -> 
			   self # clean_work_engines();
			   self # set_error (FTP_error err))
	      acc_engine;
            let acc_engine = (acc_engine :> Unixqueue.event Uq_engines.engine) in
	    work_engines <- new work_engine acc_engine :: work_engines;
	    interaction_state <- `Listening_actv(cmd,acc_engine);
    )


  method private setup_receiver ~is_done ~is_error f data_sock =
    let local = f ftp_state in
    let e = 
      new ftp_data_receiver
	~esys:event_system
	~mode:ftp_state.ftp_trans
	~local_receiver:local
	~descr:data_sock () in
    data_engine <- Some (e :> ftp_data_engine);
    Uq_engines.when_state 
      ~is_done
      ~is_error
      e

	

  method add_cmd ?(onreply = fun _ _ -> ()) (cmd : cmd) =
    match state with
      | `Working _ ->
	  Queue.push (cmd, onreply) queue;
	  self # protect (self # send_command_when_ready)
      | _ ->
	  failwith "Ftp_client.ftp_client_pi: Connection already terminated"


  method send_abort () = ()

  method run () = Unixqueue.run event_system

end


module Action : sig
  type plan
  type action = plan -> unit

  val ftp_state : plan -> ftp_state

  val empty : action
  val command : cmd -> action
  val dyn_command : (unit -> cmd) -> action
  val seq2 : action -> action -> action
  val full_seq2 : action -> (reply -> action) -> action
  val seq : action list -> action
  val expect : cmd_state -> action -> action
  val seq2_case : action -> (cmd_state * action) list -> action
  val execute : onreply:(ftp_state -> reply -> unit) -> 
                onerror:(ftp_state -> reply -> unit) -> 
                ftp_client_pi -> 
                action ->
                  unit

end = struct
  type plan = 
      { pi : ftp_client_pi;            (* where we are *)
	ftp_state : ftp_state;
	onreply : ftp_state -> reply -> unit; 
                    (* what to do next *)
	onerror : ftp_state -> reply -> unit
		    (* what to do if all else fails *)
      }

  type action =
      plan -> unit

  let ftp_state p = p.ftp_state

  let execute ~onreply ~onerror pi act =
    act
      { pi = pi; 
	ftp_state = pi#ftp_state; 
	onreply = onreply; 
	onerror = onerror
      }

  let empty p =
    p.pi # add_cmd 
      ~onreply:p.onreply
      `Dummy

  let command cmd p =
    p.pi # add_cmd
      ~onreply:(fun ftp_state r ->
		  if ftp_state.cmd_state <> `Preliminary then
		    p.onreply ftp_state r)
      cmd

  let dyn_command f_cmd p =
    command (f_cmd()) p

  let seq2 act1 act2 p =
    act1
      { p with
	  onreply = (fun ftp_state r ->
		       act2
			 { p with
			     ftp_state = ftp_state
			 }
		    )
      }

  let full_seq2 act1 f_act2 p =
    act1
      { p with
	  onreply = (fun ftp_state r ->
		       f_act2
			 r
			 { p with
			     ftp_state = ftp_state
			 }
		    )
      }

  let rec seq act_list p =
    match act_list with
      | [] ->
	  empty p
      | [act] ->
	  act p
      | act :: act_list' ->
	  act
	    { p with
		onreply = (fun ftp_state r ->
			     seq act_list' { p with ftp_state = ftp_state }
			  )
	    }

  let expect s act p =
    act
      { p with
	  onreply = (fun ftp_state r ->
		       if ftp_state.cmd_state = s then
			 p.onreply ftp_state r
		       else
			 p.onerror ftp_state r
		    )
      }

  let seq2_case act cases p =
    act 
      { p with
	  onreply = (fun ftp_state r ->
		       match
			 try
			   Some(List.assoc ftp_state.cmd_state cases)
			 with
			     Not_found -> None
		       with
			 | Some act' ->
			     act' { p with ftp_state = ftp_state }
			 | None ->
			     p.onerror ftp_state r
		    )
      }

end


class type ftp_method =
object
  method connect : (string * int) option
  method execute : Action.action
end


exception FTP_method_temp_failure of int * string
exception FTP_method_perm_failure of int * string
exception FTP_method_unexpected_reply of int * string


class connect_method ~host ?(port = 21) () : ftp_method =
object(self)
  method connect = Some(host,port)
  method execute = Action.empty
end


class login_method ~user ~get_password ~get_account () : ftp_method =
object(self)
  method connect = None
  method execute =
    Action.seq2_case
      (Action.command (`USER user))
      [ `Success, Action.empty;
	`User_pass_seq, ( Action.seq2_case
			    (Action.dyn_command
			       (fun () -> `PASS (get_password())))
			    [ `Success, Action.empty;
			      `Pass_acct_seq, ( Action.expect `Success 
						  (Action.dyn_command 
						     (fun () ->
							`ACCT (get_account())))
					      );
			    ]
			);
	`User_acct_seq, ( Action.expect `Success 
			    (Action.dyn_command 
			       (fun () -> `ACCT (get_account())))
			);
      ]
end


let slash_re = Netstring_pcre.regexp "/+";;


let rec basename l =
  match l with
    | [] -> failwith "Bad filename"
    | [name] -> name
    | _ :: l' -> basename l'


let rec dirname l =
  match l with
    | [] -> []
    | [name] -> []
    | dir :: l' -> dir :: dirname l'


let rec is_prefix l1 l2 =
  match (l1, l2) with
    | (x1::l1'), (x2::l2') ->
	x1 = x2 && is_prefix l1' l2' 
    | [], _ ->
	true
    | (_::_), [] ->
	false


let rec without_prefix l1 l2 =
  match (l1, l2) with
    | (x1::l1'), (x2::l2') ->
	if x1 = x2 then without_prefix l1' l2' else failwith "without_prefix"
    | [], _ ->
	l2
    | (_::_), [] ->
	failwith "without_prefix"


class walk_method (destination : [ `File of string | `Dir of string | `Stay ] )
                  : ftp_method =
object(self)
  method connect = None
    
  method execute =
    let rec walk_to_directory path p =
      let ftp_state = Action.ftp_state p in
      let cur_dir = List.rev ftp_state.ftp_dir in
      if is_prefix cur_dir path then
	match without_prefix cur_dir path with
	  | [] ->
	      Action.empty p
	  | dir :: _  ->
	      Action.seq2
		(Action.expect
		   `Success
		   (Action.command (`CWD dir)))
		(walk_to_directory path)
		p
      else
	Action.seq2
	  (Action.expect
	     `Success
	     (Action.command `CDUP))
	  (walk_to_directory path)
	  p
    in

    match destination with
      | `File name ->
	  let rpath = List.rev (Netstring_pcre.split slash_re name) in
	  ( match rpath with
	      | _ :: dir -> walk_to_directory (List.rev dir)
	      | [] -> failwith "Bad filename"
	  )
      | `Dir name ->
	  let path = Netstring_pcre.split slash_re name in
	  walk_to_directory path
      | `Stay ->
	  Action.empty
end



type filename =
    [ `NVFS of string
    | `Verbatim of string
    ]


let destination_of_file = 
  function
    | `NVFS name -> `File name
    | `Verbatim _ -> `Stay

let destination_of_dir = 
  function
    | `NVFS name -> `Dir name
    | `Verbatim _ -> `Stay

let ftp_filename =
  function
    | `NVFS name -> basename (Netstring_pcre.split slash_re name)
    | `Verbatim name -> name


class file_method ~(perform : string -> ftp_method) 
                  (file : filename) : ftp_method =
  (* Internal class, not exported *)
  let walk = new walk_method (destination_of_file file) in
  let filename = ftp_filename file in
object(self)
  method connect = walk # connect

  method execute =
    Action.seq2
      walk#execute
      ((perform filename) # execute)
end


class dir_method ~(perform : string option -> ftp_method) 
                 (dir : filename) : ftp_method =
  (* Internal class, not exported *)
  let walk = new walk_method (destination_of_dir dir) in
  let filename_opt = 
    match dir with
      | `NVFS name -> None
      | `Verbatim name -> Some name in
object(self)
  method connect = walk # connect

  method execute =
    Action.seq2
      walk#execute
      ((perform filename_opt) # execute)

end


class invoke_method ~command 
                    ~(process_result : ftp_state -> (int * string) -> unit)
                    () : ftp_method =
object(self)
  method connect = None

  method execute =
    Action.full_seq2
      (Action.expect `Success
	 (Action.command command))
      (fun reply p ->
	 let fs = Action.ftp_state p in
	 process_result fs reply)
end    


class set_structure_method structure =
  invoke_method 
    ~command:(`STRU structure) 
    ~process_result:(fun _ _ -> ())
    ()


class set_mode_method mode =
  invoke_method 
    ~command:(`MODE mode) 
    ~process_result:(fun _ _ -> ())
    ()


class mkdir_method file =
  file_method
    ~perform:(fun filename ->
		new invoke_method 
		  ~command:(`MKD filename)
		  ~process_result:(fun _ _ -> ())
                  () )
    file


class rmdir_method file =
  file_method
    ~perform:(fun filename ->
		new invoke_method 
		  ~command:(`RMD filename)
		  ~process_result:(fun _ _ -> ())
                  () )
    file


class delete_method file =
  file_method
    ~perform:(fun filename ->
		new invoke_method 
		  ~command:(`DELE filename)
		  ~process_result:(fun _ _ -> ()) 
                   () )
    file


(* MDTM response is YYYYMMDDHHMMSS[.s+] (GMT) *)

let time_re = Netstring_pcre.regexp ".*[^0-9](\\d{4})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(\\d{2})(?:\\.(\\d+))?"

let extract_time s =
  match Netstring_pcre.string_match time_re s 0 with
  | None ->
      failwith ("Cannot parse time-val: " ^ s)
  | Some m ->
      let fraction =
        try if (Netstring_pcre.matched_group m 7 s).[0] < '5' then 0 else 1
        with Not_found -> 0
      in
      let get_int i = int_of_string (Netstring_pcre.matched_group m i s) in
      Netdate.since_epoch
        { Netdate.year = get_int 1;
          Netdate.month = get_int 2;
          Netdate.day = get_int 3;
          Netdate.hour = get_int 4;
          Netdate.minute = get_int 5;
          Netdate.second = get_int 6 + fraction;
          Netdate.zone = 0;  (* GMT *)
          Netdate.week_day = -1 }


class mdtm_method ~file ~process_result () =
  file_method
    ~perform:(fun filename ->
		new invoke_method 
		  ~command:(`MDTM filename)
		  ~process_result:(fun _ (code,text) -> 
				     let t = extract_time text in
				     process_result t)
		  () )
    file


class rename_method' filename_from filename_to : ftp_method =
object(self)
  method connect = None

  method execute =
    Action.seq2_case
      (Action.command (`RNFR filename_from))
      [ `Rename_seq, (Action.expect `Success 
			(Action.command (`RNTO filename_to))) ]
end


class rename_method ~file_from ~(file_to : filename) () =
  (* Check arguments: *)
  let filename_to =
    match (file_from, file_to) with
      | (`NVFS p1), (`NVFS p2) ->
	  (* p1 and p2 must point to the same directory. Return basename of p2 *)
	  let p1' = Netstring_pcre.split slash_re p1 in
	  let p2' = Netstring_pcre.split slash_re p2 in
	  let d1 = dirname p1' in
	  let d2 = dirname p2' in
	  if d1 <> d2 then invalid_arg "Ftp_client.rename_method";
	  basename p2'
      | (`Verbatim _), (`Verbatim s) -> s
      | _ -> invalid_arg "Ftp_client.rename_method"
  in
  file_method
    ~perform:(fun filename_from ->
		new rename_method' filename_from filename_to
	     )
    file_from



class retrieve_method ~command ~representation () : ftp_method =
object(self)
  method connect = None

  method execute =
    Action.seq
      [Action.expect `Success (Action.command (`TYPE representation));
       Action.seq2_case
	 (Action.command `PASV)
	 [ `Success, Action.empty;
	   `Perm_failure, (Action.expect `Success
			     (Action.command `PORT))
	 ];
       Action.expect `Success (Action.command command)
      ]
end


class get_method ~file ~representation ~store () : ftp_method =
  file_method
    ~perform:(fun filename ->
		new retrieve_method 
		  ~command:(`RETR(filename,store))
		  ~representation ())
    file


class list_method ~dir ~representation ~store () : ftp_method =
  dir_method
    ~perform:(fun filename_opt ->
		new retrieve_method 
		  ~command:(`LIST(filename_opt,store))
		  ~representation ())
    dir


class nlst_method ~dir ~representation ~store () : ftp_method =
  dir_method
    ~perform:(fun filename_opt ->
		new retrieve_method 
		  ~command:(`NLST(filename_opt,store))
		  ~representation ())
    dir


class ftp_client 
        ?(event_system = Unixqueue.create_unix_event_system())
        ?(onempty = fun () -> ())
        () =
object(self)
  val mutable pi_opt = (None : ftp_client_pi option)
  val mutable queue = Queue.create()
  val mutable current = None
  val mutable work_engines = ( [] : work_engine list)

  val mutable state = `Working 0
  val mutable notify_list = []
  val mutable notify_list_new = []

  method state = (state : unit Uq_engines.engine_state)
  method event_system = event_system

  method abort() =
    ( match pi_opt with
	| None -> ()
	| Some pi -> pi # abort()
    );
    List.iter (fun e -> e # abort()) work_engines;
    work_engines <- [];
    self # set_state `Aborted

  method private clean_work_engines() =
    work_engines <- List.filter (fun e -> e # is_working) work_engines

  method request_notification f =
    notify_list_new <- f :: notify_list_new

  method private notify() =
    notify_list <- notify_list @ notify_list_new;
    notify_list_new <- [];
    notify_list <- List.filter (fun f -> f()) notify_list

  method private set_state s =
    if s <> state then (
      state <- s;
      self # notify();
    )

  method private set_error e =
    self # abort();
    self # set_state (`Error e)

  method private still_running =
    match state with
      | `Working _ -> true
      | _ -> false

  method private protect : 'a . ('a -> unit) -> 'a -> unit =
    fun f arg ->
      try
	f arg
      with
	  err ->
	    self # set_error err

  method private next() =
    try
      let (ftpm, onsuccess, onerror) = Queue.take queue in (* or Queue.Empty *)
      current <- Some(ftpm,onsuccess,onerror);
      (* First check whether we have to connect again: *)
      ( match ftpm # connect with
	  | None ->
	      ( match pi_opt with
		  | None -> failwith "Missing connection"
		  | Some pi -> self # exec pi ftpm onsuccess onerror
	      )
	  | Some (host,port) ->
	      (* First stop the current connection *)
	      ( match pi_opt with
		  | None -> ()
		  | Some pi ->
		      pi # add_cmd `QUIT;
		      (* TODO: fallback method if QUIT fails *)
		      self # clean_work_engines();
		      work_engines <- new work_engine pi :: work_engines;
		      pi_opt <- None
	      );
	      let conn_engine =
		Uq_engines.connector
		  (`Socket(
		     `Sock_inet(Unix.SOCK_STREAM,
				Unix.inet_addr_of_string host,
				port),
		     Uq_engines.default_connect_options))
		  event_system in
	      Uq_engines.when_state
		~is_done:(function
			    | `Socket(sock,_) ->
				let pi = 
				  new ftp_client_pi
				    ~event_system
				    ~onerrorstate:self#set_error
				    sock in
				pi_opt <- Some pi;
				self # clean_work_engines();
				self # exec pi ftpm onsuccess onerror
			    | _ -> assert false
			 )
		~is_error:(fun err -> 
			     self # clean_work_engines();
			     self # set_error (FTP_error err))
		~is_aborted:(fun _ -> self # clean_work_engines())
		conn_engine;
	      work_engines <- new work_engine conn_engine :: work_engines
      )
    with
	Queue.Empty ->
	  onempty();
	  ( match pi_opt with
	      | None -> ()
	      | Some pi ->
		  pi # add_cmd `QUIT;
		  (* TODO: fallback method if QUIT fails *)
		  self # clean_work_engines();
		  work_engines <- new work_engine pi :: work_engines;
		  pi_opt <- None;
	  );

	  

  method private exec pi ftpm onsuccess onerror =
    let onreply fs r = 
      self # protect onsuccess ();
      current <- None;
      if self # still_running then self # next()
    in
    let onerror fs (code,text) = 
      let e =
	match fs.cmd_state with
	  | `Temp_failure -> FTP_method_temp_failure (code,text)
	  | `Perm_failure -> FTP_method_perm_failure (code,text)
	  | _             -> FTP_method_unexpected_reply (code,text) in
      self # protect onerror e;
      current <- None;
      if self # still_running then self # next();
    in
    let _ftp_state = pi # ftp_state in
    self # protect (
      fun () ->
	let action = ftpm # execute in
	Action.execute ~onreply ~onerror pi action) ();

(*
  method private pi_got_empty ftp_state =
    current <- None;
    self # next()
 *)
    
  method add ?(onsuccess = fun () -> ()) 
             ?(onerror = fun e -> (raise e : unit)) 
             (ftpm : ftp_method) =
    if not self#still_running then
      failwith "Ftp_client.ftp_client: Engine has already finished";
    Queue.add (ftpm, onsuccess, onerror) queue;
    if current = None then self # next();


  method run () = Unixqueue.run event_system

end
