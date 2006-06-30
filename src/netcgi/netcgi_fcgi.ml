(* netcgi_fcgi.ml

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)


(* FIXME: how do we handle SIGTERM ? *)

(* FastCGI protocol (version 1) according to the specification found
   at http://www.fastcgi.com/devkit/doc/fcgi-spec.html
*)
(** There is a bug in mod_fastcgi with the following symptom:

    "If the answer to a request to the server has more than 8192 Bytes
    and less then about 16000 Bytes of payload, the HTTP header is not
    counted, the request will fail with an idle timeout.  But the FCGI
    server has delivered the complete response to the apache server in
    time."

    A fix is to close the socket at the end of the request -- in
    addition the spec. sec. 3.5 says "A simple application gets a
    significant performance boost by closing the transport connection
    when it has finished writing its response."

    This of course disallows the multiplexing of requests.  As moreover
    this library will deal with 1 request at a time, there is no much
    point in queuing the packets for the other requests, better
    DISALLOW MULTIPLEXING.

    TODO: If the need is felt, a multithreaded connector running
    requests in separated threads makes sense.
*)


(* Monomorphic version for speed *)
let min (i:int) j = if i <= j then i else j


open Netcgi_common

let fcgi_version = '\001'
let fcgi_listensock = Unix.stdin

(* FCGI record types *)
type fcgi_type = char
let fcgi_begin_request = '\001'
let fcgi_abort_request = '\002'
let fcgi_end_request   = '\003'
let fcgi_params        = '\004'
let fcgi_stdin         = '\005'
let fcgi_stdout        = '\006'
let fcgi_stderr        = '\007'
let fcgi_data          = '\008'
let fcgi_get_values        = '\009'
let fcgi_get_values_result = '\010'
let fcgi_unknown_type      = '\011'

type role = [`Responder | `Authorizer | `Filter]

exception Abort of int
  (* [Abort id] is raised when a FCGI_ABORT_REQUEST record is
     received. *)

let fcgi_keep_conn = 0x1
  (* Mask for flags component of FCGI_BeginRequestBody *)

(* Get the list of valid IP addresses for the Web server or [] if not set. *)
let fcgi_web_server_addrs =
  try
    let is_comma c = (c = ',') in
    let addrs = rev_split is_comma (Unix.getenv "FCGI_WEB_SERVER_ADDRS") in
    List.map Unix.inet_addr_of_string addrs
  with
    Not_found | Failure _ -> []


(************************************************************************)
(** Raw Input functions *)

(* FCGI dialog takes the form of records (spec. section 3.3).  The
   incoming ones will be decoded to the following structure.  *)
type record = {
  version : int;	(* at present = 1 *)
  ty : char;		(* type, see the constants above *)
  id : int;		(* FastCGI request id; 0 = management *)
  length : int;
  (* length (<= 65535) is the number of bytes of data.  The data
     itself is not part of this record, input functions will take a
     buffer [data] and return the record.  [data.[0 .. length-1]] will
     be the actual data read.  This scheme has been chosen in order to
     be able to reuse a given string as buffer. *)
}

let rec really_read fd buf ofs len =
  let r = Unix.read fd buf ofs len in
  if r < len then really_read fd buf (ofs + r) (len - r)

(* Padding is at most 255 bytes long and we do not care about thread
   safety (since we read garbage anyway), so we hoist the buffer
   outside the function. *)
let padding_buffer = String.create 0xFF

let rec read_padding fd len =
  let r = Unix.read fd padding_buffer 0 len in
  if r < len then read_padding fd (len - r)

(** [input_record fd buf] reads the next record from the socket [fd],
    returns the information as a record [r] and puts the data into
    [buf.[0 .. r.length-1]].  It is ASSUMED that [String.length buf >=
    ofs + 0xFFFF] to be able to handle any kind of incoming record. *)
let input_record fd buf ofs =
  let header = String.create 8 in
  really_read fd header 0 8;
  let version = Char.code(header.[0])
  and id =  Char.code(header.[2]) lsl 8 + Char.code(header.[3])
  and len = Char.code(header.[4]) lsl 8 + Char.code(header.[5])
  and padding = Char.code(header.[6]) in
  really_read fd buf ofs len;
  read_padding fd padding;
  { version = version;
    ty = header.[1];
    id = id;
    length = len }


(* [get_length data ofs] returns the [(l, o)] where [l] is length at
   offset [ofs] encoded according to the Name-Value Pairs specs
   (section 3.4) and [o] is the next offset.
   It is assumed that [0 <= ofs < String.length s].
   @raise Failure if the spec is not respected.  *)
let get_length data ofs =
  let b = Char.code(data.[ofs]) in
  if b lsr 7 = 0 then
    (b, ofs + 1)
  else begin
    if ofs + 3 >= String.length data then
      failwith "Netcgi_fcgi.update_props_inheader";
    let b2 = Char.code(data.[ofs + 1])
    and b1 = Char.code(data.[ofs + 2])
    and b0 = Char.code(data.[ofs + 3]) in
    (((b land 0x7F) lsl 24) + (b2 lsl 16) + (b1 lsl 8) + b0, ofs + 4)
  end

(** [update_props_inheader data datalen props_inheader] adds to
    [props_inheader] the key-value pairs contained in [data].  The
    keys are uppercased (CGI/1.1 says they must be treated case
    insensitively).

    @raise Failure if the key or val lengths exceed the length of the
    string [data]. *)
let update_props_inheader =
  let rec add data ofs datalen props_inheader =
    if ofs < datalen then begin
      let namelen, ofs = get_length data ofs in
      if ofs >= datalen then failwith "Netcgi_fcgi.update_props_inheader";
      let valuelen, ofs = get_length data ofs in
      let ofs_value = ofs + namelen in
      let ofs_next = ofs_value + valuelen in
      if  ofs_next > datalen then failwith "Netcgi_fcgi.update_props_inheader";
      let name = String.uppercase(String.sub data ofs namelen)
      and value = String.sub data ofs_value valuelen in
      let props_inheader =
	Netcgi_common.update_props_inheader (name, value) props_inheader in
      add data ofs_next datalen props_inheader
    end
    else props_inheader in
  fun data datalen props_inheader -> add data 0 datalen props_inheader



(************************************************************************)
(** Raw Output functions *)

(* [unsafe_output fd ty id s ofs len] send on [fd] for a request [id],
   a record of type [ty] whose data is the substring [s.[ofs .. ofs +
   len - 1]] where [len <= 65535].  *)
let unsafe_output fd ty id s ofs len =
  assert(len <= 0xFFFF);
  let padding_len = let r = len mod 8 in if r = 0 then 0 else 8 - r in
  (* We keep total size a multiple of 8 bytes so the web server can
     easily align the data for efficency. *)
  let r = String.create 8 in
  r.[0] <- fcgi_version;
  r.[1] <- ty;
  r.[2] <- Char.chr(id lsr 8);
  r.[3] <- Char.chr(id land 0xFF);  (* requestId *)
  r.[4] <- Char.chr(len lsr 8);
  r.[5] <- Char.chr(len land 0xFF); (* contentLength *)
  r.[6] <- Char.chr(padding_len);  (* paddingLength *)
  if Unix.write fd r 0 8 < 8 then 0 else (
    let w = Unix.write fd s ofs len in
    ignore(Unix.write fd r 0 padding_len); (* Padding (garbage) *)
    w
  )

(* [unsafe_really_output ty fd id s ofs len] does the same as
   [unsafe_output] except that empty strings are not outputted (they
   would close the stream) and strings longer than 65535 bytes are
   sent in several records. *)
let rec unsafe_really_output fd ty id  s ofs len =
  if len <= 0 then () else (
    let w = unsafe_output fd ty id s ofs (min len 0xFFFF) in
    if w = 0 then raise Netchannels.Closed_channel;
    unsafe_really_output fd ty id  s (ofs + w) (len - w)
  )

(* [output_string fd ty id s] sends on [fd] for a request [id],
   a record of type [ty] whose data is [s]. *)
let output_string fd ty id s =
  unsafe_really_output fd ty id  s 0 (String.length s)


(* Values for protocolStatus component of FCGI_EndRequestBody *)
type protocol_status =
  | REQUEST_COMPLETE
  | CANT_MPX_CONN
  | OVERLOADED
  | UNKNOWN_ROLE

(* End the request, either because the handling script ended or
   because it was rejected.  [exit_code] is an application-level
   status code (the meaning depends on the role).  [status] is a
   protocol-level status code. *)
let send_end_request fd id exit_code status =
  let r = String.create 16 in
  r.[0] <- fcgi_version;
  r.[1] <- fcgi_end_request;
  r.[2] <- Char.chr(id lsr 8);
  r.[3] <- Char.chr(id land 0xFF); (* requestId *)
  r.[4] <- '\000';
  r.[5] <- '\008'; (* contentLength = 8 *)
  r.[6] <- '\000'; (* no padding *)
  r.[11] <- Char.chr(exit_code land 0xFF); (* appStatus (4 bytes) *)
  let exit_code = exit_code lsr 8 in
  r.[10] <- Char.chr(exit_code land 0xFF);
  let exit_code = exit_code lsr 8 in
  r.[9] <- Char.chr(exit_code land 0xFF);
  r.[8] <- Char.chr(exit_code lsr 8);
  r.[12] <- Char.unsafe_chr(match status with
			    | REQUEST_COMPLETE -> 0
			    | CANT_MPX_CONN ->    1
			    | OVERLOADED ->       2
			    | UNKNOWN_ROLE ->     3); (* protocolStatus *)
  ignore(Unix.write fd r 0 16)


(* Date and executable name already provided by FCGI handler. *)
let log_error fd id msg =
  output_string fd fcgi_stderr id msg



(************************************************************************)
(** Management records *)

let set_length4 s ofs n =
  (* 4 bytes encoding of the length [n] in the string [s] from
     position [ofs]. *)
  s.[ofs+3] <- Char.unsafe_chr(n land 0xFF);
  let n = n lsr 8 in
  s.[ofs+2] <- Char.unsafe_chr(n land 0xFF);
  let n = n lsr 8 in
  s.[ofs+1] <- Char.unsafe_chr(n land 0xFF);
  s.[ofs] <- Char.chr((n lsr 8) lor 0x80)

(* [lengths_of_key_val k v] returns a string encoding the lengths
   of the key-value pair [(k,v)] according to fcgi spec: 3.4
   Name-Value Pairs. *)
let lengths_of_key_val k v =
  let klen = String.length k
  and vlen = String.length v in
  if klen < 128 then
    if vlen < 128 then begin
      let s = String.create 2 in
      s.[0] <- Char.chr klen;
      s.[1] <- Char.chr vlen;
      s
    end
    else begin
      let s = String.create 5 in
      s.[0] <- Char.chr klen;
      set_length4 s 1 vlen;
      s
    end
  else
    if vlen < 128 then begin
      let s = String.create 5 in
      set_length4 s 0 klen;
      s.[4] <- Char.chr vlen;
      s
    end
    else begin
      let s = String.create 8 in
      set_length4 s 0 klen;
      set_length4 s 4 vlen;
      s
    end

(* Add the key-value pair [k], [v] to the buffer [buf]. *)
let add_key_val buf k v =
  Buffer.add_string buf (lengths_of_key_val k v);
  Buffer.add_string buf k;
  Buffer.add_string buf v

(* [get_values_result fd data datalen ~max_conns] send back (on [fd])
   an appropriate FCGI_GET_VALUES_RESULT response for a
   FCGI_GET_VALUES record [r]. *)
let get_values_result fd data datalen ~max_conns =
  let props = (try fst(update_props_inheader data datalen ([], []))
	       with Failure _ -> []) in
  let buf = Buffer.create 64 in
  if List.mem_assoc "FCGI_MAX_CONNS" props then
    add_key_val buf "FCGI_MAX_CONNS" (string_of_int max_conns);
  if List.mem_assoc "FCGI_MAX_REQS" props then
    add_key_val buf "FCGI_MAX_REQS" "1"; (* no multiplexing! *)
  if List.mem_assoc "FCGI_MPXS_CONNS" props then
    add_key_val buf "FCGI_MPXS_CONNS" "0"; (* no multiplexing! *)
  output_string fd fcgi_get_values_result 0 (Buffer.contents buf)

(* Response to a managment record of type [t] that this library does
   not understand. *)
let send_unknown_type fd t =
  let r = String.create 16 in
  r.[0] <- fcgi_version;
  r.[1] <- fcgi_unknown_type;
  r.[2] <- '\000';
  r.[3] <- '\000'; (* requestId = 0 *)
  r.[4] <- '\000';
  r.[5] <- '\008'; (* contentLength = 8 *)
  r.[6] <- '\000'; (* no padding *)
  r.[8] <- t; (* type *)
  ignore(Unix.write fd r 0 16)


(************************************************************************)
(** Application records *)

(* This function returns the next application record.  Management
   records (of id=0) are dealt with automatically.  (This is very
   useful to get them out of the way for several functions below.) *)
let rec input_app_record fd buf ~max_conns =
  let r = input_record fd buf 0 in
  if r.id = 0 then (
    (* null request ID => management record *)
    if r.ty = fcgi_get_values then (
      get_values_result fd buf r.length ~max_conns;
      input_app_record fd buf ~max_conns
    )
    else (
      send_unknown_type fd r.ty;
      input_app_record fd buf ~max_conns
    )
  )
  else r


(* Get the next input record of type [ty] and id [id], put the data
   into [buf] and return it.  It is ASSUMED that [buf] as length at
   least 0xFFFF to be able to contain any record data.

   @raise Abort if th server send a FCGI_ABORT_REQUEST for the current
   request id.

   This function is useful because we do not allow multiplexing and
   because the protocol is nearly sequential.

   BEWARE: you should not call this function after receiving an empty
   record (indicating that the stream is closed), otherwise it will
   loop indefinitely waiting for a record that will never come.
*)
let rec input_stream_record fd ty id buf ~max_conns =
  let r = input_app_record fd buf ~max_conns in
  if r.id <> id then (
    (* Another id -- and not a management record.  Close the new
       request and try again. *)
    send_end_request fd r.id 0 CANT_MPX_CONN;
    input_stream_record fd ty id buf ~max_conns
  )
  else if r.ty = fcgi_abort_request then
    raise(Abort id)
  else if r.ty <> ty then
    (* Not the expected type; ignore the record (who knows, it may be
       some filter data that we never read). *)
    input_stream_record fd ty id buf ~max_conns
  else
    r (* Record of the desired type *)


(* [input_begin_request fd buf ~max_conns] handles management records and
   skip the other ones till a FCGI_BEGIN_REQUEST is received, in which
   case [(id, role, flags)] is returned. *)
let rec input_begin_request_loop fd buf ~max_conns : (int * role * int) =
  let r = input_app_record fd buf ~max_conns in
  if r.ty = fcgi_begin_request && r.length = 8 then (
    let role = Char.code(buf.[0]) lsl 8 + Char.code(buf.[1])
    and flags = Char.code(buf.[2]) in
    match role with
    | 1 -> (r.id, `Responder, flags)
    | 2 -> (r.id, `Authorizer, flags)
    | 3 -> (r.id, `Filter, flags)
    | _ ->
	(* Rejecting this request that has an unknown role and waiting
	   for the next one. *)
	send_end_request fd r.id 0 UNKNOWN_ROLE;
	input_begin_request_loop fd buf ~max_conns
  )
  else
    input_begin_request_loop fd buf ~max_conns

let input_begin_request fd buf ~max_conns =
  assert(String.length buf >= 0xFFFF);
  input_begin_request_loop fd buf ~max_conns


(* Accumulate the stream of params into a list of properties and
   input_header. *)
let rec input_props_inheader_loop fd id ~max_conns buf props_inheader =
  let r = input_stream_record fd fcgi_params id buf ~max_conns in
  if r.length = 0 then
    (* End of stream *)
    props_inheader
  else
    let props_inheader = update_props_inheader buf r.length props_inheader in
    input_props_inheader_loop fd id ~max_conns buf props_inheader


let input_props_inheader fd buf id ~max_conns =
  assert(String.length buf >= 0xFFFF);
  input_props_inheader_loop fd id ~max_conns buf ([],[])


(************************************************************************)
(** Input object -- FCGI_STDIN *)

(* When we will be reading the data on FCGI_STDIN or FCGI_DATA, this
   object will have to deal with all records coming on FCGI_STDIN (in
   addition to the expected data).  Response will be sent on the file
   descriptor [fd].  There is no problem of concurrent access since we
   do not accept multiplexing. *)
class in_obj (fd:Unix.file_descr) buffer (ty:fcgi_type) id ~max_conns =
object(self)
  inherit in_obj_of_descr ~buffer fd
  val ty = ty
  val id = id
  val max_conns = max_conns

  (* @override *)
  method private fill_in_buf() =
    if in0 >= in1 then (
      in0 <- 0;
      let r = input_stream_record fd ty id in_buf ~max_conns in
      in1 <- r.length;
      if in1 = 0 then (
	in1 <- -1; (* close the channel to avoid calling again this
		      function and waiting for more records that will
		      never come. *)
	raise End_of_file;
      )
    )
end


(* When no input of FCGI_DATA is required, we need a dummy object to
   play the input channel role.  *)
class closed_in_obj : Netchannels.in_obj_channel =
object
  method input (_:string) (_:int) (_:int) = raise Netchannels.Closed_channel
  method close_in () = raise Netchannels.Closed_channel
  method pos_in = raise Netchannels.Closed_channel
  method really_input (_:string) (_:int) (_:int) =
    raise Netchannels.Closed_channel
  method input_char () = raise Netchannels.Closed_channel
  method input_byte () = raise Netchannels.Closed_channel
  method input_line () = raise Netchannels.Closed_channel
end


(************************************************************************)
(** Output object *)

class out_obj (fd:Unix.file_descr) (id:int) : Netchannels.out_obj_channel =
object(self)
  inherit out_obj_of_descr ~buffer:(String.create 0xFFFF) fd
  val id = id

  (* @override *)
  method private write buf ofs len =
    assert(len <= 0xFFFF); (* should be true as only used for [out_buf] *)
    if len > 0 then unsafe_output fd fcgi_stdout id buf ofs len
    else 0

  (* @override *)
  method close_out () =
    if out1 < 0 then raise Netchannels.Closed_channel else begin
      (* Do not close the file descriptor [fd]; this is the job of the
         accept() loop -- and it may be reused for subsequent connections. *)
      (try
         self#flush();
         (* Close the stream FCGI_STDOUT by sending an empty record *)
         ignore(unsafe_output fd fcgi_stdout id "" 0 0);
         out1 <- -1
       with e -> out1 <- -1; raise e);
    end
end



(************************************************************************)
(** CGI abstraction *)

(* Creates the environment including the output channel *)
class fcgi_env ~config ~properties ~input_header fd id
  : Netcgi_common.cgi_environment =
  let out_obj = new out_obj fd id in
object
  inherit cgi_environment ~config ~properties ~input_header out_obj

  (* Override to use the correct channel *)
  method log_error msg = log_error fd id msg
end

class type cgi =
object
  inherit Netcgi.cgi
  method role : role
  method data : Netchannels.in_obj_channel
  method data_length : int
  method data_mtime : float
end

class fcgi (role:role) ~max_conns fd id  env op request_method args : cgi =
object
  inherit Netcgi_common.cgi env op request_method args

  val data_stream =
    if role = `Filter then
      let buf = String.create 0x10000 in
      (new in_obj fd buf fcgi_data id ~max_conns :> Netchannels.in_obj_channel)
    else new closed_in_obj
  val role = role
  val data_length =
    try int_of_string(env#cgi_property "FCGI_DATA_LENGTH") with _ -> 0
  val data_mtime =
    try float_of_string(env#cgi_property "FCGI_DATA_LAST_MOD") with _ -> 0.

  method role = role
  method data = data_stream
  method data_length = data_length
  method data_mtime = data_mtime
end


(* [handle_connection fd .. f] handle an accept()ed connection,
   reading incoming records on the file descriptor [fd] and running
   [f] for each incoming request. *)
let rec handle_connection fd ~max_conns ~external_server ~config
    output_type arg_store exn_handler f =
  let in_buf = String.create 0x10000 in
  let close_conn =
    try
      let (id, role, flags) = input_begin_request fd in_buf ~max_conns in

      let (properties, input_header) =
        input_props_inheader fd in_buf id ~max_conns in
      let env = new fcgi_env ~config ~properties ~input_header fd id in

      (* Now that one knows the environment, one can deal with exn. *)
      exn_handler_default env ~exn_handler
        (fun () ->
           try
	     let in_obj = (new in_obj fd in_buf fcgi_stdin id ~max_conns
                           :> Netchannels.in_obj_channel) in
	     let cgi = (cgi_with_args (new fcgi role fd id ~max_conns)
	                  env output_type in_obj arg_store) in
	     (try
	        f cgi;
	        cgi#finalize()
	      with e when config.default_exn_handler ->
                cgi#finalize(); raise e);
             None
           with Abort _ as e -> Some e
        )
        ~finally:
        (fun () ->
           try
             env#out_channel#close_out(); (* => flush buffer *)
	     ignore(unsafe_output fd fcgi_stderr id "" 0 0); (* close stderr *)
	     send_end_request fd id 0 REQUEST_COMPLETE;
           with _ -> ()
        );
      flags land fcgi_keep_conn = 0
    with Abort id ->
      (* FCGI_ABORT_REQUEST received.  The exit_status should come from
	 the application the spec says.  However, in general, the
	 application will not have yet started, so just return 0.
	 (Since we so not allow multiplexed requests, it is more likely
	 that the web server just closes the connection but we handle it
	 anyway.) *)
      true
  in
  (* FIXME: because of the bug explained in the beginning of this
     file, we would like to close the file descriptor when the request
     is complete.

     If we do nothing, one sees the entire output but get an "idle
     timeout".  If we close [fd], one sometimes get "Broken pipe:
     ... write failed"!  So we just shutdown.  Do we risk fd
     shortage???  *)
  if external_server then
    if close_conn then  (
      try
	Unix.shutdown fd Unix.SHUTDOWN_ALL;
	Unix.close fd
      with _ -> ())
    else
      (* The server is supposed to take care of closing [fd].  (Tail
	 recursiveness is important as many requests may be handled by
	 this fun.)  *)
      handle_connection fd ~max_conns ~external_server
	~config output_type arg_store exn_handler f
  else (
    Unix.shutdown fd Unix.SHUTDOWN_ALL;
    (* Unix.close fd *)
  )


let default_allow server =
  match server with
  | Unix.ADDR_UNIX _ -> true
  | Unix.ADDR_INET(addr,_) ->
      fcgi_web_server_addrs = [] || List.mem addr fcgi_web_server_addrs


let run ?(config=Netcgi.default_config)
    ?(allow=default_allow)
    ?(output_type=(`Direct "":Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic))
    ?(exn_handler=(fun _ f -> f()))
    ?sockaddr
    f =
  (* FIXME: Under M$win, the web server communicates with a FCGI script
     that it launches by means of a named pipe [fd] (contrarily to the
     spec).  The requests are all sent through that pipe.  Thus there is
     a single connection. *)

  let sock = match sockaddr with
    | None ->
	(* FastCGI launched by the web server *)
	fcgi_listensock
    | Some sockaddr ->
	(* FastCGI on a distant machine, listen on the given socket. *)
	let sock =
	  Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
	Unix.setsockopt sock Unix.SO_REUSEADDR true;
	Unix.bind sock sockaddr;
	Unix.listen sock 5;
	sock
  in
  let max_conns = 1 (* single process/thread *) in
  while true do
    let (fd, server) = Unix.accept sock in
    try
      if allow server then (
        let external_server = (match server with
                               | Unix.ADDR_UNIX _ -> false
                               | Unix.ADDR_INET _ -> true) in
	handle_connection fd ~max_conns ~external_server
	  ~config output_type arg_store exn_handler f
      );
      (try Unix.close fd with _ -> ())
    with
    | Unix.Unix_error(Unix.EPIPE, _, _) ->
	(* This error is perfectly normal: the sever or ourselves may
	   have closed the connection (Netcgi_common ignore
	   SIGPIPE).  Just wait for the next connection. *)
	(try Unix.close fd with _ -> ())
    | e when config.default_exn_handler ->
	(* Log the error and wait for the next conection. *)
	(try
	   log_error fd 0 (Printexc.to_string e);
	   Unix.close fd
	 with _ -> ())
  done


(* To ckeck:

   http://mapserver.gis.umn.edu/cgi-bin/wiki.pl?FastCGIOnWin32
 *)
