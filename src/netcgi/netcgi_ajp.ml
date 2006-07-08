(* netcgi_ajp.ml

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

(* Implementation of the Apache JServ Protocol version 1.3 (ajp13) as
   described at
   http://tomcat.apache.org/connectors-doc/common/ajpv13a.html
*)

open Netcgi_common
open Printf

(* Specialize [min] to integers for performance reasons (> 150% faster). *)
let min x y = if (x:int) <= y then x else y

let log_error msg =
  let zone = Netdate.localzone (* log local time *) in
  let date = Netdate.format "%c" (Netdate.create ~zone (Unix.gettimeofday())) in
  prerr_endline ("[" ^ date ^ "] [Netcgi_ajp] " ^ msg)


(* Property file
 ***********************************************************************)

let rm_htspace =
  Netcgi_common.rm_htspace (fun c -> c = ' ' || c = '\t')


let props_of_file fname =
  let props = ref [] in
  let fh = open_in fname in
  try
    while true do
      let line = input_line fh in
      (* Index of comment sign, to remove it *)
      let comment = (try String.index line '#'
	             with Not_found -> String.length line) in
      try
	let i = String.index line '=' in
	let name = rm_htspace line 0 i
	and value = rm_htspace line (i + 1) comment in
	props := (name,value) :: !props
      with Not_found ->
	() (* No '=' sign; ignore silently *)
    done;
    assert false
  with
  | End_of_file -> close_in fh; List.rev !props
  | err -> close_in fh;	raise err


let arg_parse speclist anonf usage =
  let specs =
    Arg.align (("-classpath", Arg.String(fun _ -> ()),
	       " The option [-classpath <path>] is ignored")
	        :: speclist) in
  let f_call = ref 0 in
  let property_file = ref "" in
  let f s =
    incr f_call;
    if !f_call = 1 then () (* ignore *)
    else if !f_call = 2 then property_file := s
    else anonf s in
  Arg.parse specs f usage;
  (* Parse the property file *)
  try props_of_file !property_file
  with Invalid_argument _ ->
    Arg.usage specs usage;
    failwith "Netcgi_ajp.arg_parse: property file \
      (second anonymous argument) cannot be read."


(* Output
 ***********************************************************************)

(* Max packet size according the the spec. *)
let max_packet_size = 8192 (* 8Kb *)

(* The buffer size has exactly the size of a packet minus the 7 first
   bytes that are added by [write]. *)
class out_obj (fd: Unix.file_descr) =
  let buffer = String.create(max_packet_size - 7) in
object(self)
  inherit out_obj_of_descr ~buffer fd  as super

  (* @override *)
  (* Write an AJP13_SEND_BODY_CHUNK on [fd].  Since [len] will be less
     than the buffer size, there will be no problem to write it on two
     bytes.

     Although it is not required by the protocol (but said to be the
     way tomcat5 expects it), we terminate the chunk by '\000'.  *)
  method private write buf ofs len =
    if len <= 0 then 0
    else begin
        let chunk_len = min len 8184 (* 8Kb - 8 *) in
        let payload_len = chunk_len + 4 (* prefix_code + chunk_len + \0 *) in
        let s = String.create 8 in
        s.[0] <- 'A';
        s.[1] <- 'B';
        s.[2] <- Char.unsafe_chr(payload_len lsr 8);
        s.[3] <- Char.unsafe_chr(payload_len land 0xFF);
        s.[4] <- '\x03'; (* prefix_code *)
        s.[5] <- Char.unsafe_chr(chunk_len lsr 8);
        s.[6] <- Char.unsafe_chr(chunk_len land 0xFF);
        s.[7] <- '\000'; (* to terminate the chunk *)
        ignore(super#write s 0 7);
        let w = super#write buf ofs chunk_len in
        ignore(super#write s 7 1);
        w
      end
end

(* AJP13_END_RESPONSE.  Signals on [fd] the end of the
   request-handling cycle.  We specify that [fd] can be reused to
   handle new incoming requests.  *)
let send_end_response fd =
  (* (packet prefix "AB") (length=2) (prefix_code=5) (reuse=true) *)
  ignore(Unix.write fd "AB\x00\x02\x05\x01" 0 6)

(* Ask for more data from the request body.  Ask for the maximum of
   8186 bytes (8Kb - 6).  *)
let send_get_body_chunk fd =
  (* (packet prefix "AB") (length=3) (prefix_code=6) (size=0x1FFA) *)
  ignore(Unix.write fd "AB\x00\x03\x06\x1F\xFA" 0 7)

(* Send a CPong reply on [fd]. *)
let send_cpong fd =
  (* (packet prefix "AB") (payload length=1) (prefix_code=9) *)
  ignore(Unix.write fd "AB\x00\x01\x09" 0 5)



let buffer_add_int b n =
  assert(0 <= n && n <= 0xFFFF);
  let sn = String.create 2 in
  sn.[0] <- Char.unsafe_chr(n lsr 8);
  sn.[1] <- Char.unsafe_chr(n land 0xFF);
  Netbuffer.add_string b sn

let buffer_add_string b s =
  buffer_add_int b (String.length s);
  Netbuffer.add_string b s;
  Netbuffer.add_string b "\x00"


(* [buffer_add_header b 0 fields] add the header [fields] to the
   netbuffer [b] encoded according to the AJP spec.  It returns the
   number of fields that were added.  If the number of headers or the
   buffer becomes too large, we discard the remaining fields. *)
let rec buffer_add_header b num_headers = function
  | [] -> num_headers
  | (f,v) :: tl ->
      if num_headers >= 0xFFFF then
        num_headers (* too many header fields; stop *)
      else
        (* Ok, can add one more field (2 bytes to code the number of
           headers). *)
        match String.lowercase f with
        | "content-type" ->     add_code_value b num_headers 0xA001 v tl
        | "content-language" -> add_code_value b num_headers 0xA002 v tl
        | "content-length" ->   add_code_value b num_headers 0xA003 v tl
        | "date" ->             add_code_value b num_headers 0xA004 v tl
        | "last-modified" ->    add_code_value b num_headers 0xA005 v tl
        | "location" ->         add_code_value b num_headers 0xA006 v tl
        | "set-cookie" ->       add_code_value b num_headers 0xA007 v tl
        | "set-cookie2" ->      add_code_value b num_headers 0xA008 v tl
        | "servlet-engine" ->   add_code_value b num_headers 0xA009 v tl
        | "status" ->           add_code_value b num_headers 0xA00A v tl
        | "www-authenticate" -> add_code_value b num_headers 0xA00B v tl
        | _ ->
            let f_len = String.length f
            and v_len = String.length v in
            if 6 + f_len + v_len + Netbuffer.length b > max_packet_size
            then num_headers
            else begin
                (* Skip (unlikely) too long names or values *)
                if f_len < 0xA000 && v_len <= 0xFFFF then (
                    buffer_add_string b f;
                    buffer_add_string b v;
                    buffer_add_header b (num_headers + 1) tl
                  )
                else buffer_add_header b num_headers tl
              end

and add_code_value b num_headers code value tl =
  let len = String.length value in
  if 5 + len + Netbuffer.length b > max_packet_size then num_headers
  else begin
      (* skip if [value] is too long *)
      if len <= 0xFFFF then (
          buffer_add_int b code;
          (* encode string [value] *)
          buffer_add_int b len;
          Netbuffer.add_string b value;
          Netbuffer.add_string b "\x00";
          buffer_add_header b (num_headers + 1) tl
        )
      else buffer_add_header b num_headers tl
    end

(* AJP13_SEND_HEADERS *)
let send_headers fd (header: Netmime.mime_header) =
  let b = Netbuffer.create 0x1000 in
  Netbuffer.add_string b "AB??\x04"; (* "AB" length[to fill] prefix_code *)
  (* Status *)
  let st = try int_of_string(header#field "Status") with _ -> 200 in
  buffer_add_int b st;
  let st_msg = Nethttp.string_of_http_status(Nethttp.http_status_of_int st) in
  buffer_add_string b st_msg;
  (* Headers *)
  let pos = Netbuffer.length b in
  Netbuffer.add_string b "??"; (* # of headers, to set *)
  let num_headers = buffer_add_header b 0 header#fields in
  (* Set the length of the payload and the number of headers and output *)
  let buf = Netbuffer.unsafe_buffer b
  and len = Netbuffer.length b in
  let payload_len = len - 4 in
  buf.[2] <- Char.unsafe_chr(payload_len lsr 8);
  buf.[3] <- Char.unsafe_chr(payload_len land 0xFF);
  buf.[pos] <- Char.unsafe_chr(num_headers lsr 8);
  buf.[pos+1] <- Char.unsafe_chr(num_headers land 0xFF);
  ignore(Unix.write fd buf 0 len)



class cgi_environment ~config ~properties ~input_header fd =
object(self)
  inherit Netcgi_common.cgi_environment ~config ~properties ~input_header
    (new out_obj fd :> Netchannels.out_obj_channel)
  val fd = fd

  (* @override *)
  (* AJP has a special packet type (different from the output one) to
     send the headers. *)
  method send_output_header () =
    if header_not_sent then begin
        send_headers fd self#output_header;
        header_not_sent <- false (* One output header per request *)
      end
end



(* Input object
 ***********************************************************************)

exception Invalid of string
  (** Exception raised if the protocol is not respected.  The string
      is an explanation to be logged.  *)

let rec read fd buf ofs len =
  try
    let r = Unix.read fd buf ofs len in
    if r = 0 then raise End_of_file;
    r
  with
  | Unix.Unix_error(Unix.EINTR,_,_) -> read fd buf ofs len
  | Unix.Unix_error(Unix.EAGAIN, _, _)
  | Unix.Unix_error(Unix.EWOULDBLOCK, _, _) -> 0

let rec really_read fd buf ofs len =
  if len > 0 then begin
      let r = read fd buf ofs len in
      really_read fd buf (ofs + r) (len - r)
    end

(* Input a whole packet data into [buf].  Return the length of the
   payload.  Thus the data is in [buf.[0 .. length-1]].
   @raise Shutdown if the packet is larger than the buffer. *)
let input_packet fd buf =
  really_read fd buf 0 4;
  if String.unsafe_get buf 0 <> '\x12' || String.unsafe_get buf 1 <> '\x34' then
    raise(Invalid "Packets must start with 0x1234");
  let len = (Char.code(buf.[2]) lsl 8) lor (Char.code(buf.[3])) in
  if len > String.length buf then
    raise(Invalid(sprintf "Packet data length = %i bytes > allowed = %i."
                     len (String.length buf)));
  really_read fd buf 0 len;
  len


(* The input object is only used for the input data stream (if any). *)
class in_obj (fd: Unix.file_descr) buffer =
object(self)
  inherit in_obj_of_descr ~buffer fd
    (* Use a buffer size a bit bigger than 8K to be able to hold most
       requests entirely (as the spec says they are with 8K of data). *)

  val mutable first_filling = true

  (* @override *)
  method private fill_in_buf () =
    if in0 >= in1 then begin
        (* We need to ask for the next data packet. *)
        if first_filling then first_filling <- false
        else send_get_body_chunk fd;
        let payload_len = input_packet fd in_buf in
        if payload_len < 2 then raise(Invalid "Data packet payload too small");
        in0 <- 2; (* The first 2 bytes of the payload are the length of
                     the data.  Because it is not excluded that the
                     payload has some padding, use it. *)
        let len = (Char.code(in_buf.[0]) lsl 8) lor (Char.code(in_buf.[1])) in
        in1 <- 2 + len;
        if in1 > String.length in_buf then
          raise(Invalid "Length of data is too large")
      end
end


(* AJP13_FORWARD_REQUEST
 ***********************************************************************)

exception Shutdown
  (** Special exception raised if the connector is requested to
      shutdown.  *)

(* Get the boolean at position [ofs].  It is assumed that [buf],
   [ofs], [len] determine a valid substring. *)
let get_bool buf ofs len =
  if len <= 0 then raise(Invalid "Cannot read boolean");
  (String.unsafe_get buf ofs <> '\x00', ofs + 1, len - 1)

let get_byte buf ofs len =
  if len <= 0 then raise(Invalid "Cannot read byte");
  (Char.code(String.unsafe_get buf ofs), ofs + 1, len - 1)

let get_int buf ofs len =
  if len <= 1 then raise(Invalid "Cannot read integer");
  ((Char.code(String.unsafe_get buf ofs) lsl 8)
   lor (Char.code(String.unsafe_get buf (ofs+1))),
   ofs + 2, len - 2)

let get_string buf ofs len =
  if len <= 1 then raise(Invalid "Cannot read length of string");
  let l =
    (Char.code(String.unsafe_get buf ofs) lsl 8)
    lor (Char.code(String.unsafe_get buf (ofs+1))) in
  if l = 0xFFFF then
    (* BLOODY HELL: A length of 0xFFFF means "null" and there is no
       terminating '\000'.  This is not documented in the spec! *)
    ("", ofs + 2, len - 2)
  else
    let total_len = 3 + l (* length + actual string + '\000' *) in
    if total_len > len then raise(Invalid "String too long");
    (String.sub buf (ofs+2) l, ofs + total_len, len - total_len)


let http_headers =
  [| "accept"; "accept-charset"; "accept-encoding"; "accept-language";
     "authorization"; "connection"; "content-type"; "content-length";
     "cookie"; "cookie2"; "host"; "pragma"; "referer"; "user-agent" |]

(** Input all (name, value) request header pairs. *)
let rec get_request_headers buf ofs0 len0 num headers =
  if num = 0 then (headers, ofs0, len0)
  else
    let b1, ofs, len = get_byte buf ofs0 len0 in
    let b0, ofs, len = get_byte buf ofs len in
    let name, ofs, len =
      if b1 = 0xA0 && 1 <= b0 && b0 <= 0x0E then
        (* The header name is encoded as 0xA0?? *)
        (Array.unsafe_get http_headers (b0 - 1), ofs, len)
      else
        (* The two bytes read form the length of the string *)
        get_string buf ofs0 len0 in
    let value, ofs, len = get_string buf ofs len in
    get_request_headers buf ofs len (num - 1) ((name, value) :: headers)


let attributes =
  [| "context"; "servlet_path"; "remote_user"; "auth_type";
     "QUERY_STRING"; "jvm_route"; "ssl_cert"; "ssl_cipher";
     "ssl_session"; "req_attribute"; "ssl_key_size" |]

(** Input all attributes and add them to the list [attr]. *)
let rec get_attributes buf ofs len attr =
  let b, ofs, len = get_byte buf ofs len in
  if 0x01 <= b && b <= 0x0B then
    let name, ofs, len =
      if b = 0x0A then get_string buf ofs len (* req_attribute *)
      else Array.unsafe_get attributes (b - 1), ofs, len in
    let value, ofs, len = get_string buf ofs len in
    get_attributes buf ofs len ((name, value) :: attr)
  else
    attr (* ends *)


let request_methods =
  [| "OPTIONS"; "GET"; "HEAD"; "POST"; "PUT"; "DELETE"; "TRACE";
     "PROPFIND"; "PROPPATCH"; "MKCOL"; "COPY"; "MOVE"; "LOCK"; "UNLOCK";
     "ACL"; "REPORT"; "VERSION-CONTROL"; "CHECKIN"; "CHECKOUT";
     "UNCHECKOUT"; "SEARCH"; "MKWORKSPACE"; "UPDATE"; "LABEL";
     "MERGE"; "BASELINE_CONTROL"; "MKACTIVITY" |]

(** Supposing that [buf.[ofs ..]] is the payload of an
    AJP13_FORWARD_REQUEST except its first byte, reads it and return
    [(props, inheader)].  The properties are already sorted so we do
    not need to use {!Netcgi_common.update_props_inheader}.
*)
let update_props_inheader ?script_name buf ofs len =
  let m, ofs, len = get_byte buf ofs len in
  let req_method =
    if 1 <= m && m <= Array.length request_methods then
      Array.unsafe_get request_methods (m - 1)
    else
      (* Do not raise an exc because the spec explicitely says
         additional methods will be transported. *)
      "Unknown method " ^ string_of_int m in
  let protocol, ofs, len = get_string buf ofs len in
  let req_uri, ofs, len =  get_string buf ofs len in
  let remote_addr, ofs, len = get_string buf ofs len in
  let remote_host, ofs, len = get_string buf ofs len in
  let server_name, ofs, len = get_string buf ofs len in
  let server_port, ofs, len = get_int buf ofs len in
  let is_ssl, ofs, len = get_bool buf ofs len in
  let num_headers, ofs, len = get_int buf ofs len in
  (* Fix: AJP does not transmit the compulsory CGI properties
     PATH_INFO and SCRIPT_NAME.  Unfortunately there is not enough
     information to deduce them from [req_uri] (but it is important to
     get [req_uri] by concatenating the two).  That's why we use the
     optional [script_name].  *)
  let script_name, path_info =
    match script_name with
    | None ->
        begin
          (* Cut at the first '/' not at the beginning of the string. *)
          try
            let i = String.index_from req_uri 1 '/' in
            (String.sub req_uri 0 i,
            String.sub req_uri i (String.length req_uri - i))
          with Not_found | Invalid_argument _ ->
            (req_uri, "") (* fallback *)
        end
    | Some s ->
        let s_len = String.length s in
        let is_script_name = is_prefix s req_uri (* => req_uri longer *)
          && (s_len = String.length req_uri
              || String.unsafe_get req_uri s_len = '/') in
        if is_script_name then
          (s, String.sub req_uri s_len (String.length req_uri - s_len))
        else (
          log_error(sprintf "The given script_name=%S is not a prefix \
		of the REQUEST_URI=%S" s req_uri);
          (req_uri, "")
        ) in
  let props =
    [ ("REQUEST_METHOD", req_method);
      ("SERVER_PROTOCOL", protocol);
      ("REQUEST_URI", req_uri);
      ("REMOTE_ADDR", remote_addr);
      ("REMOTE_HOST", remote_host);
      ("SERVER_NAME", server_name);
      ("SERVER_PORT", string_of_int server_port);
      ("HTTPS", if is_ssl then "on" else "off");
      ("PATH_INFO", path_info);
      ("SCRIPT_NAME", script_name);
    ] in
  (* HTTP headers *)
  let inheader, ofs, len = get_request_headers buf ofs len num_headers [] in
  (* Attributes *)
  let props = get_attributes buf ofs len props in
  (* @return *)
  (props, inheader)


(** [input_forward_request fd] loops, reading packets on [fd], until
    it finds a AJP13_FORWARD_REQUEST in which case it returns the
    corresponding pair [(props, inheader)].  Other type of requests
    sent are handled automatically.

    This function will potentially run a great number of times so must
    be tail rec. *)
let rec input_forward_request ?script_name fd buf =
  let payload_len = input_packet fd buf in
  if payload_len = 0 then begin
      log_error "Packet with empty payload!";
      input_forward_request ?script_name fd buf
    end
  else begin
      match Char.code(String.unsafe_get buf 0) with
      | 7 -> raise Shutdown
      | 8 ->
          (* Ping: the web server asks the container to take control
             (secure login phase). *)
          (* FIXME: ignore it -- do not know how to respond!! *)
          log_error "Packet \"Ping\" received.  IGNORING.";
          input_forward_request ?script_name fd buf
      | 10 ->
          (* CPing: the web server asks the container to respond quickly
             with a CPong. *)
          send_cpong fd;
          input_forward_request ?script_name fd buf
      | 2 ->
          (* Forward Request: Begin the request-processing cycle with the
             following data *)
          update_props_inheader ?script_name buf 1 (payload_len - 1)
      | b ->
          (* Unknown, skip packet *)
          log_error("Unknown packet code " ^ string_of_int b ^ ".  Skipped.");
          input_forward_request ?script_name fd buf
    end


(************************************************************************)


let rec handle_connection fd ~config ?script_name output_type arg_store
    exn_handler f =
  (* The channel objects will be reused for all incoming requests.
     WARNING: Make sure it is big enough to contain an entire packet. *)
  let buf = String.create 0x2000 (* 8K *) in
  try
    while true do
      (* FIXME: Although the debug info says the connection is
         recycled, apache does not close the sockect and tries to
         reconnect.  That leaves unused processes running.  Until I
         understand what is going on, I close the connection handler
         if I have to wait more and 1 sec for the next request. *)
      let in_socks, _, _ =  Unix.select [fd] [] [] 1. in
      if in_socks = [] then (
        (* log_error "Timeout waiting for the next connection.  Closing."; *)
        raise Shutdown
      );

      let (properties, input_header) =
        input_forward_request ?script_name fd buf in
      let env = new cgi_environment ~config ~properties ~input_header fd in

      (* Now that one knows the environment, one can warn about exceptions *)
      exn_handler_default env ~exn_handler
        (fun () ->
           try
             let cgi = cgi_with_args (new cgi) env output_type
               (new in_obj fd buf :> Netchannels.in_obj_channel) arg_store in
             (try
                f(cgi: Netcgi.cgi);
                cgi#out_channel#commit_work();
                cgi#finalize()
              with e when config.default_exn_handler ->
                cgi#finalize(); raise e);
             None
           with Shutdown -> Some Shutdown
        )
        ~finally:(fun () ->
                    try
                      env#out_channel#close_out(); (* => flush buffer *)
                      send_end_response fd;
                    with _ -> ()
                 );
    done
  with
  | Shutdown -> () (* We do not really shutdown, just close the connection. *)
  | Invalid msg -> log_error("PROTOCOL ERROR: " ^ msg)


let run
    ?props
    ?(config=Netcgi.default_config)
    ?script_name
    ?(allow=fun _ -> true)
    ?(output_type=(`Direct "": Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic))
    ?(exn_handler=(fun _ f -> f()))
    ?(port=8009)
    f =
  (* Socket to listen to *)
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any, port) in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 5;
  while true do
    let (fd, server) = Unix.accept sock in
    try
      if allow server then
        handle_connection fd ~config ?script_name output_type arg_store
          exn_handler f;
      Unix.close fd
    with
    | End_of_file
    | Unix.Unix_error(Unix.EPIPE, _, _)
    | Unix.Unix_error(Unix.ECONNRESET, _, _) ->
	(* These exceptions do not imply there is an error: the sever
	   or ourselves may have closed the connection (Netcgi_common
	   ignores SIGPIPE).  Just wait for the next connection. *)
	(try Unix.close fd with _ -> ())
    | e when config.default_exn_handler ->
	(* Log the error and wait for the next connection. *)
	(try
	   log_error(Printexc.to_string e);
           Unix.close fd
	 with _ -> ())
  done

