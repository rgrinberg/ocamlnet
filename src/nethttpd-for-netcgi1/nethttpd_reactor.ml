(* $Id$
 *
 *)

(*
 * Copyright 2005 Baretta s.r.l. and Gerd Stolpmann
 *
 * This file is part of Nethttpd.
 *
 * Nethttpd is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Nethttpd is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with WDialog; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Nethttp
open Nethttp.Header
open Nethttpd_types
open Nethttpd_kernel
open Netchannels

class type http_processor_config =
object
  inherit Nethttpd_kernel.http_protocol_config
  method config_timeout_next_request : float
  method config_timeout : float
  method config_cgi : Netcgi1_compat.Netcgi_env.cgi_config
  method config_error_response : int -> string
  method config_log_error : 
    Unix.sockaddr option -> Unix.sockaddr option -> http_method option ->
    http_header option -> string -> unit
  method config_log_access :
    Unix.sockaddr -> Unix.sockaddr -> http_method ->
    http_header -> int64 -> (string * string) list -> bool -> 
    int -> http_header -> int64 -> unit
end

class type http_reactor_config =
object
  inherit http_processor_config
  method config_reactor_synch : [ `Connection | `Close | `Flush | `Write ]
end


class type internal_environment =
object
  inherit extended_environment

  method unlock : unit -> unit
  method req_method : http_method
  method response : http_response
  method log_access : unit -> unit

end


class type http_reactive_request =
object
  method environment : extended_environment
  method accept_body : unit -> unit
  method reject_body : unit -> unit
  method finish : unit -> unit
  method finish_request : unit -> unit
end


let get_this_host addr =
  match addr with
    | Unix.ADDR_UNIX path ->
	("", None)   (* questionable *)
    | Unix.ADDR_INET(ia,port) ->
	(Unix.string_of_inet_addr ia, Some port)



class http_environment (proc_config : #http_processor_config)
                       req_meth req_uri req_version req_hdr 
                       fd_addr peer_addr
                       in_ch in_cnt out_ch resp close_after_send_file
		       reqrej
                      : internal_environment =

  (* Decode important input header fields: *)
  let (in_host, in_port_opt) =
    (* Host and port of the [Host] header *)
    try get_host req_hdr
    with 
      | Not_found -> 
	  (* For HTTP/1.1 and later this is not allowed. For earlier protocols, we
	   * just fill in the IP address that accepted the request. 
	   *)
	  ( match req_version with
	      | `Http((1,n),_) when n>= 1 ->
		  raise(Standard_response(`Bad_request, 
				        None,
				        Some "Nethttpd: Bad request: [Host] header is missing"))
	      | _ ->
		  get_this_host fd_addr
	  )
      | Bad_header_field _ ->
	  raise(Standard_response(`Bad_request,
                                   None,
			        Some "Nethttpd: Bad request: Cannot decode [Host] header")) in

  let (script_name, query_string) = decode_query req_uri in

(*
  let full_uri =
    "http://" ^ in_host ^ 
    (match in_port with Some n -> ":" ^ string_of_int n | None -> "") ^ 
    req_uri
 *)

object(self)
  inherit empty_environment

  val mutable locked = true

  val mutable logged_props = []

  initializer (
    config <- proc_config # config_cgi;
    in_state <- `Received_header;
    out_state <- `Start;
    in_header <- req_hdr;
    in_channel <- in_ch;
    out_channel <- out_ch;
    protocol <- req_version;
    properties <- [ "GATEWAY_INTERFACE", "Nethttpd/0.0";
		  "SERVER_SOFTWARE",   "Nethttpd/0.0";
		  "SERVER_NAME",       in_host;
		  "SERVER_PROTOCOL",   string_of_protocol req_version;
		  "REQUEST_METHOD",    req_meth;
		  "SCRIPT_NAME",       script_name;
		  (* "PATH_INFO",         ""; *)
		  (* "PATH_TRANSLATED",   ""; *)
		  "QUERY_STRING",      query_string;
		  (* "REMOTE_HOST",       ""; *)
		  "REMOTE_ADDR",       fst(get_this_host peer_addr);
		  (* "AUTH_TYPE",         ""; *)
		  (* "REMOTE_USER",       ""; *)
		  (* "REMOTE_IDENT",      ""; *)
		  "HTTPS",             "off";
		  "REQUEST_URI",       req_uri;
		  ] @
                  ( match in_port_opt with
		      | Some p -> [ "SERVER_PORT", string_of_int p ]
		      | None   -> [] );
    logged_props <- properties
  )

  method unlock() =
    locked <- false

  method server_socket_addr = fd_addr
  method remote_socket_addr = peer_addr

  method response = resp
  method req_method = (req_meth, req_uri)

  val mutable sent_status = 0
  val mutable sent_resp_hdr = new Netmime.basic_mime_header []

  method send_output_header() =
    if locked then failwith "Nethttpd_reactor: channel is locked";
    if out_state <> `Start then
      failwith "send_output_header";
    (* The response status is encoded in the [Status] pseudo header *)
    let (code, phrase) = status_of_cgi_header out_header in
    resp # send (`Resp_status_line(code, phrase));
    (* Create a copy of the header without [Status]: *)
    let h = new Netmime.basic_mime_header out_header#fields in
    h # delete_field "Status";
    sent_status <- code;   (* remember what has been sent for access logging *)
    sent_resp_hdr <- h;
    resp # send (`Resp_header h);
    out_state <- `Sent_header

  method send_file fd length =
    if locked then failwith "Nethttpd_reactor: channel is locked";
    if out_state <> `Start then
      failwith "send_file";
    (* The response status is encoded in the [Status] pseudo header *)
    let (code, phrase) = status_of_cgi_header out_header in
    let status = http_status_of_int code in
     (* Create a copy of the header without [Status]: *)
    let h = new Netmime.basic_mime_header out_header#fields in
    h # delete_field "Status";
    sent_status <- code;   (* remember what has been sent for access logging *)
    sent_resp_hdr <- h;
    send_file_response resp status (Some h) fd length;
    out_state <- `Sending_body;  (* best approximation *)
    close_after_send_file()

  method log_error s =
    proc_config # config_log_error 
      (Some fd_addr) (Some peer_addr) (Some(req_meth,req_uri)) (Some req_hdr) s


  method log_props l =
    logged_props <- l


  val mutable access_logged = false

  method log_access () =
    (* Called when the whole response is written. Do now access logging *)
    if not access_logged then (
      proc_config # config_log_access
	fd_addr
	peer_addr
	(req_meth, req_uri)
	req_hdr
	!in_cnt
	logged_props
	!reqrej
	sent_status
	sent_resp_hdr
	resp#body_size;
      access_logged <- true
    )

end


class http_reactor_input next_token in_cnt =
  (* an extension of rec_in_channel *)
object(self)
  val mutable current_chunk = None
  val mutable eof = false
  val mutable closed = false
  val mutable locked = true

  method private refill() =
    match next_token() with
      | `Req_body(s,pos,len) ->
	  assert(len > 0);
	  in_cnt := Int64.add !in_cnt (Int64.of_int len);
	  current_chunk <- Some(s,pos,len)
      | `Req_trailer _ ->
	  self # refill ()   (* ignore *)
      | `Req_end ->
	  current_chunk <- None;
	  eof <- true;
	  raise End_of_file;
      | _ ->
	  (* Something else... Handle this as `Req_end! *)
	  current_chunk <- None;
	  eof <- true;
	  raise End_of_file;
	  

  method input s spos slen =
    if closed then raise Closed_channel;
    if locked then failwith "Nethttpd_reactor: channel is locked";
    if eof then raise End_of_file;
    if current_chunk = None then self#refill();
    match current_chunk with
      | Some(u,upos,ulen) ->
	  (* We have [ulen] data, copy that to [s] *)
	  let len = min slen ulen in
	  String.blit u upos s spos len;
	  let ulen' = ulen - len in
	  if ulen' = 0 then
	    current_chunk <- None
	  else
	    current_chunk <- Some(u,upos+len,ulen');
	  len
      | None ->
	  (* After [refill] this is not possible *)
	  assert false

  method close_in() =
    if closed then raise Closed_channel;
    if locked then failwith "Nethttpd_reactor: channel is locked";
    (* It is no problem to ignore further arriving tokens. These will be "eaten" by
     * [finish_request] later. (Of course, we could call [finish_request] here,
     * but that would probably defer the generation of responses.)
     *)
    closed <- true;

  method unlock() =
    locked <- false;

  method drop() =
    locked <- false;
    eof <- true

end


class http_reactor_output config resp synch (f_access : unit -> unit) =   
  (* an extension of rec_in_channel *)
object
  val mutable closed = false
  val mutable locked = true

  method output s spos slen =
    if closed then raise Closed_channel;
    if locked then failwith "Nethttpd_reactor: channel is locked";
    let u = String.sub s spos slen in
    resp # send (`Resp_body(u, 0, String.length u));
    ( match config#config_reactor_synch with
	| `Write ->
	    synch()
	| _ ->
	    ()
    );
    slen

  method flush() =
    if closed then raise Closed_channel;
    if locked then failwith "Nethttpd_reactor: channel is locked";
    match config#config_reactor_synch with
      | `Write
      | `Flush ->
	  synch()
      | _ ->
	  ()

  method close_out() =
    if closed then raise Closed_channel;
    if locked then failwith "Nethttpd_reactor: channel is locked";
    closed <- true;
    resp # send `Resp_end;
    ( match config#config_reactor_synch with
	| `Write
	| `Flush
	| `Close ->
	    synch()
	| _ ->
	    ()
    );
    (* This is the right time for writing the access log entry: *)
    f_access()
      


  method close_after_send_file() =
    closed <- true;
    ( match config#config_reactor_synch with
	| `Write
	| `Flush
	| `Close ->
	    synch()
	| _ ->
	    ()
    );
    (* This is the right time for writing the access log entry: *)
    f_access()

  method unlock() =
    locked <- false

end


class http_reactive_request_impl config env inch outch resp expect_100_continue
                                 finish_request reqrej
                                 : http_reactive_request =
object(self)
  method environment = 
    (env : internal_environment :> extended_environment)

  method accept_body() =
    if expect_100_continue then
      resp # send resp_100_continue;
    (* We need not to synch here! The attempt to read the body will synchronize
     * implicitly.
     * We should keep in mind, however, that when the existing body isn't read
     * the "100 Continue" might be transmitted very late. This is no disadvantage,
     * I think.
     *)
    inch # unlock();
    outch # unlock();
    env # unlock()

  method reject_body() =
    inch # drop();
    outch # unlock();
    env # unlock();
    reqrej := true    (* for access logging only *)

  val mutable fin_req = false

  method finish_request() =
    if not fin_req then (    (* Do this only once *)
      fin_req <- true;
      inch # drop();
      outch # unlock();
      env # unlock();
      finish_request();   (* Read the rest of the request until `Req_end *)
    )

  method finish() =
    self # finish_request();
    match env # output_state with
      | `Start ->
	  (* The whole response is missing! Generate a "Server Error": *)
	  output_std_response config env `Internal_server_error None 
	    (Some "Nethttpd: Missing response, replying 'Server Error'");
	  env # set_output_state `End;
      | `Sent_header
      | `Sending_body ->
	  (* The response body is probably incomplete or missing. Try to close
	   * the channel.
	   *)
	  ( try env # output_ch # close_out() with Closed_channel -> () );
	  env # set_output_state `End;
      | `Sent_body 
      | `End ->
	  (* Everything ok, just to be sure... *)
	  ( try env # output_ch # close_out() with Closed_channel -> () );
	  env # set_output_state `End;
      | _ ->
	  (* These states must not happen! *)
	  assert false

end



class http_reactor (config : #http_reactor_config) fd =
object(self)
  val proto = new http_protocol config fd
  val fd_addr = Unix.getsockname fd
  val peer_addr = Unix.getpeername fd

  method private cycle() =
    let block = 
      if proto # waiting_for_next_message then 
	config#config_timeout_next_request
      else
	config#config_timeout in
    proto # cycle ~block ();

  method private next_token() =
    if proto # recv_queue_len = 0 then (
      self # cycle();
      self # next_token()
    )
    else
      proto # receive() 

  method private peek_token() =
    if proto # recv_queue_len = 0 then (
      self # cycle();
      self # peek_token()
    )
    else
      proto # peek_recv() 

  method private finish_request() =
    (* Read the rest of the previous request, ignoring it *)
    match self # peek_token() with
      | `Req_header _
      | `Eof
      | `Fatal_error _
      | `Bad_request_error _
      | `Timeout ->
	  (* Everything ok, do nothing *)
	  ()
      | `Req_end ->
	  (* Just drop this token, the next token starts the new request *)
	  ignore(proto # receive ())
      | `Req_expect_100_continue
      | `Req_body _
      | `Req_trailer _ ->
	  (* Continue to read this request until its end *)
	  while
	    match self # peek_token () with
	      | `Req_header _
	      | `Eof 
	      | `Fatal_error _
	      | `Bad_request_error _ 
	      | `Timeout ->
		  false   (* Do not read further *)
	      | _ ->
		  ignore(self # next_token());
		  true    (* Continue *)
	  do
	    ()
	  done


  method private synch() =
    (* Ensure that all written data are actually transmitted: *)
    while proto # do_output do
      self # cycle();
    done;
    (* CHECK: Maybe we have to throw away the remaining tokens of the current request! *)


  method next_request () =
    let tok = self # next_token() in
    match tok with
      | `Req_header (req, req_hdr, resp) ->
	  (* Ok, we have a new request. Initialize the new environment processing
	   * it
	   *)
	  let expect_100_continue =
	    try
	      proto # peek_recv() = `Req_expect_100_continue
	    with
		Recv_queue_empty -> false in
	  if expect_100_continue then
	    ignore(proto # receive());

	  let ((req_meth, req_uri), req_version) = req in

	  let f_access = ref (fun () -> ()) in  (* set below *)
	  let in_cnt = ref 0L in
	  let input_ch = new http_reactor_input self#next_token in_cnt in
	  let output_ch = 
	    new http_reactor_output config resp self#synch 
	      (fun () -> !f_access()) in
	  let lifted_input_ch = 
	    lift_in ~buffered:false (`Rec (input_ch :> rec_in_channel)) in
	  let lifted_output_ch = 
	    lift_out (`Rec (output_ch :> rec_out_channel)) in
	  (* The input channel needs no additional buffer here. The httpd kernel
	   * already implements a good buffer.
	   *
	   * The output channel profits from a buffer. The effect is that the
	   * kernel is driven with output chunks of uniform size. Furthermore,
	   * `Write synchronization is only performed after every of these chunks,
	   * and not after every output method invocation.
	   *)
	  
	  ( try
	      let reqrej = ref false in
	      let env = new http_environment 
			      config 
                              req_meth req_uri req_version req_hdr 
                              fd_addr peer_addr
		              lifted_input_ch in_cnt lifted_output_ch 
			      resp output_ch#close_after_send_file reqrej
	      in
	      f_access := env#log_access;
	      let req_obj = new http_reactive_request_impl 
			      config env input_ch output_ch resp expect_100_continue 
			      self#finish_request reqrej
	      in
	      Some req_obj
	    with
		Standard_response(status, hdr_opt, msg_opt) ->
		  (* Probably a problem when decoding a header field! *)
		  ( match msg_opt with
		      | Some msg ->
			  config # config_log_error
			    (Some fd_addr) (Some peer_addr) (Some(req_meth,req_uri)) 
			    (Some req_hdr) msg
		      | None -> ()
		  );
		  (* CHECK: Also log to access log? *)
		  let code = int_of_http_status status in
		  let body = config # config_error_response code in
		  Nethttpd_kernel.send_static_response resp status hdr_opt body;
		  
		  self # synch();
		  !f_access();  (* do access logging if env could be created *)
		  self # finish_request();
		  self # next_request()
	  )

      | `Eof ->
	  self # synch();
	  None
	  
      | `Fatal_error e ->
	  (* The connection is already down. Just log the incident: *)
	  let msg = Nethttpd_kernel.string_of_fatal_error e in
	  config # config_log_error 
	    (Some fd_addr) (Some peer_addr) None None msg;
	  None

      | `Bad_request_error (e, resp) ->
	  (* Log the incident, and reply with a 400 response: *)
	  let msg = string_of_bad_request_error e in
	  let status = status_of_bad_request_error e in
	  config # config_log_error
	    (Some fd_addr) (Some peer_addr) None None msg;
	  let body = config # config_error_response (int_of_http_status status) in
	  Nethttpd_kernel.send_static_response resp status None body;
	  self # next_request()

      | `Timeout ->
	  (* Just ignore. The next token will be `Eof *)
	  self # next_request()

      | _ ->
	  (* Everything else means that we lost synchronization, and this is a
	   * fatal error!
	   *)
	  config # config_log_error 
	    (Some fd_addr) (Some peer_addr) None None 
	    "Nethttpd: Reactor out of synchronization";
	  proto # abort `Server_error;
	  self # next_request()

  method close () =
    ( try
	self # synch();
      with
	| err -> Unix.close fd; raise err
    );
    if proto # need_linger then (
      let lc = new Nethttpd_kernel.lingering_close fd in
      while lc # lingering do
	lc # cycle ~block:true ()
      done
    )
    else
      Unix.close fd

end


exception Redirect_response_legal of string * http_header

type x_reaction = 
    [ http_service_reaction
    | `Redirect_request of string * http_header
    ]


let process_connection config fd (stage1 : 'a http_service) =

  let _fd_addr = Unix.getsockname fd in
  let _peer_addr = Unix.getpeername fd in

  let protect env f arg =
    try
      f arg
    with
      | Redirect_response_legal(_,_) as e -> raise e

      | Standard_response(status, hdr_opt, errmsg_opt) when env#output_state = `Start ->
	  output_std_response config env status hdr_opt errmsg_opt;
 
      | err when env#output_state = `Start ->
	  output_std_response config env `Internal_server_error None 
	    (Some("Nethttpd: Uncaught exception: " ^ Printexc.to_string err));
  in

  let do_stage3 env stage3 =
    try
      stage3 # generate_response env
    with
      | Redirect_request(_,_) ->
	  failwith "Caught Redirect_request in stage 3, but it is only allowed in stage 1"
      | Redirect_response(uri,hdr) ->
	  if env#output_state <> `Start then
	    failwith "Caught Redirect_response, but it is too late for redirections";
	  raise (Redirect_response_legal(uri,hdr))
  in

  let do_stage2 req env stage2 =
    let stage3 = 
      try
	stage2 # process_body env 
      with
	| Redirect_request(_,_) ->
	    failwith "Caught Redirect_request in stage 2, but it is only allowed in stage 1"
	| Redirect_response(_,_) ->
	    failwith "Caught Redirect_response in stage 2, but it is only allowed in stage 3"
    in
    req # finish_request();
    do_stage3 env stage3
  in

  let rec process_request req redir_env redir_count =
    (* [redir_env]: The environment of the request, possibly rewritten by redirects.
     * [redir_count]: The number of already performed redirections
     * [req]: Contains always the original environment
     *)
    if redir_count > 10 then
      failwith "Too many redirections";
    let reaction = 
      try (stage1 # process_header redir_env :> x_reaction)
      with 
	| Redirect_request(new_uri, new_hdr) ->
	    `Redirect_request(new_uri, new_hdr)
	| Redirect_response(_,_) ->
	    failwith "Caught Redirect_response in stage 1, but it is only allowed in stage 3"
    in
    ( try
	( match reaction with
	    | `Accept_body stage2 ->
		req # accept_body();
		protect redir_env (do_stage2 req redir_env) stage2
	    | `Reject_body stage3 ->
		req # reject_body();
		protect redir_env (do_stage3 redir_env) stage3
	    | `Static(status, resp_hdr_opt, resp_str) ->
		req # reject_body();
		output_static_response redir_env status resp_hdr_opt resp_str
	    | `File(status, resp_hdr_opt, resp_filename, pos, length) ->
		req # accept_body();
		protect
		  redir_env 
		  (output_file_response redir_env status resp_hdr_opt resp_filename pos) 
		  length
	    | `Std_response(status, resp_hdr_opt, errlog_opt) ->
		req # reject_body();
		output_std_response config redir_env status resp_hdr_opt errlog_opt
	    | `Redirect_request(new_uri, new_hdr) ->
		let (new_script_name, new_query_string) = decode_query new_uri in
		new_hdr # update_multiple_field 
		  "Content-length" (redir_env # multiple_input_header_field "Content-length");
		let new_properties =
		  update_alist 
		    [ "REQUEST_URI", new_uri;
		      "SCRIPT_NAME", new_script_name;
		      "QUERY_STRING", new_query_string ] 
		    redir_env#cgi_properties in
		let new_env =
		  new redirected_environment 
		    ~properties:new_properties
		    ~in_header:new_hdr
		    ~in_channel:(redir_env # input_ch) redir_env in
		process_request req new_env (redir_count+1)
	)
      with
	| Redirect_response_legal(new_uri, new_hdr) ->
	    if redir_env # output_state <> `Start then
	      failwith "Redirect_response is not allowed after output has started";
	    let (new_script_name, new_query_string) = decode_query new_uri in
	    new_hdr # update_field "Content-length" "0";
	    let new_properties =
	      update_alist 
		[ "REQUEST_URI", new_uri;
		  "SCRIPT_NAME", new_script_name;
		  "QUERY_STRING", new_query_string;
		  "REQUEST_METHOD", "GET"
		] 
		redir_env#cgi_properties in
	    let new_env =
	      new redirected_environment 
		~properties:new_properties
		~in_header:new_hdr
		redir_env in
	    process_request req new_env (redir_count+1)
	    
    );
    req # finish()
  in

  let rec fetch_requests reactor =
    match reactor # next_request() with
      | None ->
	  ()
      | Some req ->
	  process_request req req#environment 0;
	  fetch_requests reactor
  in
  
  let reactor = 
    try
      new http_reactor config fd 
    with
	err ->
	  (* An exception means here that getsockname or getpeername failed.
             We can only close the descriptor!
           *)
	  Unix.close fd;
	  raise err
  in
  ( try
      fetch_requests reactor
    with
	err ->
	  config # config_log_error None None None None
	              ("Nethttpd: Uncaught exception: " ^ Printexc.to_string err);
  );
  ( try
      reactor # close()
    with
	err ->
	  config # config_log_error None None None None
	              ("Nethttpd: Uncaught exception: " ^ Printexc.to_string err);
  )
;;
