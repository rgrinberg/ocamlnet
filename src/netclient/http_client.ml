(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* Reference documents:
 * RFC 2068, 2616:      HTTP 1.1
 * RFC 2069, 2617:      Digest Authentication
 *)


exception Bad_message of string;;
exception Http_error of (int * string);;
exception Http_protocol of exn;;
exception No_reply;;
exception Too_many_redirections;;
exception Name_resolution_error of string
exception URL_syntax_error of string

let () =
  Netexn.register_printer
    (Http_protocol Not_found)
    (fun e ->
       match e with
	 | Http_protocol e' ->
	     "Http_client.Http_protocol(" ^ Netexn.to_string e' ^ ")"
	 | _ ->
	     assert false
    )

let() =
  Netsys_signal.init()


type status =
  [ `Unserved
  | `Http_protocol_error of exn
  | `Successful
  | `Redirection
  | `Client_error
  | `Server_error
  ]

type response_body_storage =
    [ `Memory
    | `File of unit -> string
    | `Body of unit -> Netmime.mime_body
    ]

type 'message_class how_to_reconnect =
    Send_again
  | Request_fails
  | Inquire of ('message_class -> bool)
  | Send_again_if_idem
;;

type 'message_class how_to_redirect =
    Redirect
  | Do_not_redirect
  | Redirect_inquire of ('message_class -> bool)
  | Redirect_if_idem
;;

type resolver =
    Unixqueue.unix_event_system -> 
    string -> 
    (Unix.inet_addr option -> unit) -> 
      unit
;;

type counters =
    { mutable new_connections : int;
      mutable timed_out_connections : int;
      mutable crashed_connections: int;
      mutable server_eof_connections : int;
      mutable successful_connections : int;
      mutable failed_connections : int;
    }


let better_unix_error f arg =
  try
    f arg
  with
    Unix.Unix_error (e,syscall,param) ->
      let error = Unix.error_message e in
      if param = "" then
	failwith error
      else
	failwith (param ^ ": " ^ error)


let rec syscall f =
  (* Invoke system call, and handle EINTR *)
  try
    f()
  with
      Unix.Unix_error(Unix.EINTR,_,_) ->
	(* "interrupted system call": A signal happened while the system
	 * blocked.
	 * Simply restart the call.
	 *)
	syscall f
;;


let hex_digits = [| '0'; '1'; '2'; '3'; '4'; '5'; '6'; '7';
		    '8'; '9'; 'a'; 'b'; 'c'; 'd'; 'e'; 'f' |];;

let encode_hex s =
  (* encode with lowercase hex digits *)
  let l = String.length s in
  let t = String.make (2*l) ' ' in
  for n = 0 to l - 1 do
    let x = Char.code s.[n] in
    t.[2*n]   <- hex_digits.( x lsr 4 );
    t.[2*n+1] <- hex_digits.( x land 15 );
  done;
  t
;;


type synchronization =
  | Sync
  | Pipeline of int
;;


let max_pipeline = 8 ;;

let pipeline_blacklist =
  (* Stolen from Mozilla *)
  [ Netstring_pcre.regexp "Microsoft-IIS/";
    Netstring_pcre.regexp "Netscape-Enterprise/3.";
  ]
;;


type http_options = 
    { synchronization : synchronization;
      maximum_connection_failures : int;
      maximum_message_errors : int;
      inhibit_persistency : bool;
      connection_timeout : float;
      number_of_parallel_connections : int;
      maximum_redirections : int;
      handshake_timeout : float;
      resolver : resolver;
      configure_socket : Unix.file_descr -> unit;
      verbose_status : bool;
      verbose_request_header : bool;
      verbose_response_header : bool;
      verbose_request_contents : bool;
      verbose_response_contents : bool;
      verbose_connection : bool;
    }
;;

type header_kind = [ `Base | `Effective ]

let http_re =
  Netstring_pcre.regexp
    "^http://(([^/:@]+)(:([^/:@]+))?@)?([^/:@]+)(:([0-9]+))?(/.*)?$"

let parse_http_url url =
  match Netstring_pcre.string_match http_re url 0 with
    | None ->
	raise Not_found
    | Some m ->
	let user =
	  try Some(Netstring_pcre.matched_group m 2 url) 
	  with Not_found -> None in
	let password =
	  try Some(Netstring_pcre.matched_group m 4 url)
	  with Not_found -> None in
	let host =
	  Netstring_pcre.matched_group m 5 url in
	let port =
	  try int_of_string(Netstring_pcre.matched_group m 7 url)
	  with Not_found -> 80 in
	let path =
	  try Netstring_pcre.matched_group m 8 url with Not_found -> "" in
	(user,password,host,port,path)
;;


let comma_re = Netstring_pcre.regexp "[ \t\n\r]*,[ \t\n\r]*" ;;

let split_words_by_commas s =
  Netstring_pcre.split comma_re s ;;

let space_re = Netstring_pcre.regexp "[ \t\n\r]+" ;;

let split_words s =
  Netstring_pcre.split space_re s ;;


let sync_resolver esys name reply =
  let addr =
    try
      Some (Unix.inet_addr_of_string name)
    with
	Failure _ ->
	  try
	    let h = Unix.gethostbyname name in
	    Some h.Unix.h_addr_list.(0)
	  with Not_found ->
	    None in
  reply addr
;;


(**********************************************************************)
(***                          BUFFERS                               ***)
(**********************************************************************)

(* Similar to Buffer, but may be accessed in a more efficient way. *)

module B = Netbuffer;;

type buffer_type = B.t;;


module Q = 
struct
  (* This queue allows one to insert elements. *)

  type 'a t = 'a Queue.t

  exception Empty = Queue.Empty

  let create = Queue.create

  let add = Queue.add

  let push = add 

  let add_at n x q =
    (* Add x behind the n-the last element of q, i.e.
     * add_at 0 x q = Add behind the last element = add x q
     * add_at 1 x q = Add before the last element
     * ...
     * This algorithm is usually called for n ~ length, and is quite
     * efficient in this case.
     *)
    let l = Queue.length q in
    let q' = Queue.create() in
    for k = 1 to l-n do
      Queue.add (Queue.take q) q'
    done;
    Queue.add x q';
    Queue.transfer q q';
    Queue.transfer q' q

  let take = Queue.take
     
  let pop = take

  let peek = Queue.peek

  let top = peek

  let clear = Queue.clear

  let copy = Queue.copy

  let is_empty = Queue.is_empty

  let length = Queue.length

  let iter = Queue.iter

  let transfer = Queue.transfer

  (* fold: omitted *)

end



(**********************************************************************)
(***                  THE HTTP CALL CONTAINER                       ***)
(**********************************************************************)


let dump_header prefix h =
  List.iter
    (fun (n,v) ->
       prerr_endline (prefix ^ n ^ ": " ^ v))
    h



type 'session auth_state =  (* 'session = auth_session, defined below *)
    [ `None
         (* Authentication has not yet been tried *)
    | `In_advance of 'session
         (* This session had been tried before a 401 response was seen *)
    | `In_reply of 'session
         (* This session was tried after a 401 response was seen *)
    ]

class type http_call =
object
  method is_served : bool
  method status : status
  method request_method : string
  method request_uri : string
  method set_request_uri : string -> unit
  method request_header : header_kind -> Netmime.mime_header
  method set_request_header : Netmime.mime_header -> unit
  method effective_request_uri : string
  method request_body : Netmime.mime_body
  method set_request_body : Netmime.mime_body -> unit
  method response_status_code : int
  method response_status_text : string
  method response_status : Nethttp.http_status
  method response_protocol : string
  method response_header : Netmime.mime_header
  method response_body : Netmime.mime_body
  method response_body_storage : response_body_storage
  method set_response_body_storage : response_body_storage -> unit
  method get_reconnect_mode : http_call how_to_reconnect
  method set_reconnect_mode : http_call how_to_reconnect -> unit
  method get_redirect_mode : http_call how_to_redirect
  method set_redirect_mode : http_call how_to_redirect -> unit
  method proxy_enabled : bool
  method set_proxy_enabled : bool -> unit
  method no_proxy : unit -> unit
  method is_proxy_allowed : unit -> bool
  method empty_path_replacement : string
  method is_idempotent : bool
  method has_req_body : bool
  method has_resp_body : bool
  method same_call : unit -> http_call
  method get_req_method : unit -> string
  method get_host : unit -> string
  method get_port : unit -> int
  method get_path : unit -> string
  method get_uri : unit -> string
  method get_req_body : unit -> string
  method get_req_header : unit -> (string * string) list
  method assoc_req_header : string -> string
  method assoc_multi_req_header : string -> string list
  method set_req_header : string -> string -> unit
  method get_resp_header : unit -> (string * string) list
  method assoc_resp_header : string -> string
  method assoc_multi_resp_header : string -> string list
  method get_resp_body : unit -> string
  method dest_status : unit -> (string * int * string)
  method private_api : private_api
end

and private_api =
object
  method get_error_counter : int
  method set_error_counter : int -> unit
  method set_error_exception : exn -> unit

  method get_redir_counter : int
  method set_redir_counter : int -> unit

  method continue : bool
  method set_continue : unit -> unit

  method auth_state : auth_session auth_state
  method set_auth_state : auth_session auth_state -> unit

  method set_effective_request_uri : string -> unit

  method prepare_transmission : unit -> unit
    (* Initializes the `Effective request header, and creates the response
     * objects. Sets the status to [`Unserved]. The error and redirection
     * counters are not changed.
     *)
  method set_response_status : int -> string -> string -> unit
    (* code, text, proto *)
  method set_response_header : Netmime.mime_header -> unit
    (* Sets the response header *)
  method response_body_open_wr : unit -> Netchannels.out_obj_channel
    (* Opens the response body for writing *)
  method finish : unit -> unit
    (* The call is finished. The [status] is set to [`Successful], 
     * [`Redirection], [`Client_error], or [`Server_error] depending on
     * the response.
     *)

  method response_code : int
  method response_proto : string
  method response_header : Netmime.mime_header
  method dump_status : unit -> unit
  method dump_response_header : unit -> unit
  method dump_response_body : unit -> unit
end

and auth_session =
object
  method auth_scheme : string
  method auth_domain : string list
  method auth_realm : string
  method auth_user : string
  method auth_in_advance : bool
  method authenticate : http_call -> (string * string) list
  method invalidate : http_call -> bool
end


class type virtual gen_call =
object
  inherit http_call
  method private virtual fixup_request : unit -> unit
  method private virtual def_request_method : string
  method private virtual def_empty_path_replacement : string
  method private virtual def_is_idempotent : bool
  method private virtual def_has_req_body : bool
  method private virtual def_has_resp_body : bool
end


class virtual generic_call : gen_call =
object(self)
  val mutable status = (`Unserved : status)

  val mutable req_uri = ""
  val mutable req_host = ""   (* derived from req_uri *)
  val mutable req_port = 80   (* derived from req_uri *)
  val mutable req_path = ""   (* derived from req_uri *)
  val mutable req_base_header = new Netmime.basic_mime_header []
  val mutable req_work_header = new Netmime.basic_mime_header []
  val mutable req_body = new Netmime.memory_mime_body ""

  val mutable eff_req_uri = ""

  val mutable resp_code = 0
  val mutable resp_text = ""
  val mutable resp_proto = ""
  val mutable resp_header = new Netmime.basic_mime_header []
  val mutable resp_body = new Netmime.memory_mime_body ""

  val mutable resp_body_storage = (`Memory : response_body_storage)
  val mutable reconn_mode = Send_again_if_idem
  val mutable redir_mode = Redirect_if_idem
  val mutable proxy_enabled = true

  val mutable private_api = None

  val mutable continue = false   (* Whether 100-Continue has been seen *)    
  val mutable error_counter = 0
  val mutable redir_counter = 0
  val mutable resp_ch = None
  val mutable auth_state = `None


  method private def_private_api fixup_request =
    ( object(pself) 

	val self = false
	  (* We cannot call methods of [self] due to a bug in O'Caml 3.08
           * (present until 3.08.3, fixed in 3.08.4 and 3.09)
           * PR#3576, PR#3678
           *)

	method private close_resp_ch() =
	  match resp_ch with
	    | None -> ()
	    | Some ch -> ch # close_out(); resp_ch <- None

	method get_error_counter = error_counter
	method set_error_counter n = error_counter <- n
	method get_redir_counter = redir_counter
	method set_redir_counter n = redir_counter <- n

	method continue = continue
	method set_continue() = continue <- true

	method auth_state = auth_state
	method set_auth_state s = auth_state <- s

	method set_effective_request_uri s = eff_req_uri <- s

	method prepare_transmission () =
	  pself # close_resp_ch();
	  status <- `Unserved;
	  req_work_header <- new Netmime.basic_mime_header 
	                             req_base_header#fields;
	  resp_code <- 0;
	  resp_text <- "";
	  resp_proto <- "";
	  resp_header <- new Netmime.basic_mime_header [];
	  resp_body <- ( match resp_body_storage with
			   | `Memory ->
			       new Netmime.memory_mime_body ""
			   | `File f ->
			       let name = f() in
			       new Netmime.file_mime_body name
			   | `Body f ->
			       f ()
		       );
	  fixup_request();
	  ( try ignore(req_work_header # field "Date")
	    with Not_found ->
	      Nethttp.Header.set_date req_work_header (Unix.time())
	  );
	  ( try ignore(req_work_header # field "User-agent")
	    with Not_found ->
	      req_work_header # update_field "User-agent" "Netclient"
	  );

	method set_response_status code text proto =
	  resp_code <- code;
	  resp_text <- text;
	  resp_proto <- proto

	method set_response_header h =
	  resp_header <- h

	method response_body_open_wr() =
	  pself # close_resp_ch();
	  let ch = resp_body # open_value_wr() in
	  resp_ch <- Some ch;
	  ch

	method response_code = resp_code
	method response_proto = resp_proto
	method response_header = resp_header

	method finish() =
	  assert(resp_code <> 0);
	  pself # close_resp_ch();
	  status <- (if resp_code >= 200 && resp_code <= 299 then
		       `Successful
		     else if resp_code >= 300 && resp_code <= 399 then
		       `Redirection
		     else if resp_code >= 400 && resp_code <= 499 then
		       `Client_error
		     else
		       `Server_error);

	method set_error_exception x =
	  pself # close_resp_ch();
	  match x with
	      Http_error(_,_) -> assert false   (* not allowed *)
	    | _ ->
		let do_increment =
		  match status with
		    | `Http_protocol_error _ -> false
		    | _ -> true in
		if do_increment then
		  error_counter <- error_counter + 1;
		status <- (`Http_protocol_error x);

	method dump_status () =
	  prerr_endline ("HTTP response code: " ^ string_of_int resp_code);
	  prerr_endline ("HTTP response code: " ^ resp_text);
	  prerr_endline ("HTTP response protocol: "  ^ resp_proto);

	method dump_response_header () =
	  dump_header "HTTP response " (resp_header # fields)

	method dump_response_body () =
	  prerr_endline ("HTTP Response:\n");
	  prerr_endline resp_body#value

      end
    )


  (* Call state *)

  method is_served = status <> `Unserved
  method status = status

  (* Accessing the request message (new style) *)

  method request_method = self # def_request_method
  method request_uri = req_uri
  method set_request_uri uri = 
    try
      let u, pw, h, pt, ph = parse_http_url uri in
      if u <> None || pw <> None then
	failwith "Http_client, set_request_uri: URL must not contain user or password";
      req_uri <- uri;
      req_host <- h;
      req_port <- pt;
      req_path <- ph
    with
	Not_found ->
	  failwith "Http_client: bad URL"


  method request_header (k:header_kind) =
    match k with
      | `Base -> req_base_header
      | `Effective -> req_work_header
  method set_request_header h =
    req_base_header <- h
  method request_body = req_body
  method set_request_body b = req_body <- b

  method effective_request_uri = eff_req_uri

  (* Accessing the response message (new style) *)

  method private check_response() =
    match status with
      | `Unserved -> 
	  failwith "Http_client: HTTP call is unserved, no response yet"
      | `Http_protocol_error e -> 
	  raise (Http_protocol e)
      | _ -> ()

  method response_status_code = self#check_response(); resp_code
  method response_status_text = self#check_response(); resp_text
  method response_status = 
    self#check_response(); Nethttp.http_status_of_int resp_code
  method response_protocol = self#check_response(); resp_proto
  method response_header = self#check_response(); resp_header
  method response_body = self#check_response(); resp_body

  (* Options *)

  method response_body_storage = resp_body_storage
  method set_response_body_storage s = resp_body_storage <- s
  method get_reconnect_mode = reconn_mode
  method set_reconnect_mode m = reconn_mode <- m
  method get_redirect_mode = redir_mode
  method set_redirect_mode m = redir_mode <- m
  method proxy_enabled = proxy_enabled
  method set_proxy_enabled e = proxy_enabled <- e
  method no_proxy() = proxy_enabled <- false
  method is_proxy_allowed() = proxy_enabled

  (* Method characteristics *)

  method empty_path_replacement = self # def_empty_path_replacement
  method is_idempotent = self # def_is_idempotent
  method has_req_body = self # def_has_req_body
  method has_resp_body = self # def_has_resp_body

  (* Repeating calls *)
    
  method same_call() =
    let same =
      {< status = `Unserved;
	 resp_header = new Netmime.basic_mime_header [];
	 resp_body = new Netmime.memory_mime_body "";
	 eff_req_uri = "";
	 private_api = None;
	 error_counter = 0;
	 redir_counter = 0;
	 continue = false;
	 resp_ch = None;
	 auth_state = `None
      >} in
    (same : #http_call :> http_call)

  (* Old style access methods *)

  method get_req_method() = self # def_request_method
  method get_host() = req_host
  method get_port() = req_port
  method get_path() = req_path
  method get_uri() = req_uri
  method get_req_body() = req_body # value
  method get_req_header () =
    List.map (fun (n,v) -> (String.lowercase n, v)) req_base_header#fields
  method assoc_req_header n =
    req_base_header # field n
  method assoc_multi_req_header n =
    req_base_header # multiple_field n
  method set_req_header n v =
    req_base_header # update_field n v
  method get_resp_header() =
    self#check_response(); 
    List.map (fun (n,v) -> (String.lowercase n, v)) resp_header#fields
  method assoc_resp_header n =
    self#check_response(); 
    resp_header # field n
  method assoc_multi_resp_header n =
    self#check_response(); 
    resp_header # multiple_field n
  method get_resp_body() =
    self#check_response();
    if resp_code >= 200 && resp_code <= 299 then
      resp_body # value
    else
      raise(Http_error(resp_code, resp_body#value))
  method dest_status() =
    self#check_response();
    (resp_proto, resp_code, resp_text)

  (* Private *)

  method private_api = 
    match private_api with
      | None ->
	  let api = self # def_private_api self#fixup_request in
	  private_api <- Some api;
	  api
      | Some api ->
	  api

  (* Virtual methods *)

  method virtual private fixup_request : unit -> unit
  method virtual private def_request_method : string
  method virtual private def_empty_path_replacement : string
  method virtual private def_is_idempotent : bool
  method virtual private def_has_req_body : bool
  method virtual private def_has_resp_body : bool
end

(******** SUBCLASSES IMPLEMENTING HTTP METHODS ************************)

class get_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "GET"
  method private def_is_idempotent = true
  method private def_has_req_body = false
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class head_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "HEAD"
  method private def_is_idempotent = true
  method private def_has_req_body = false
  method private def_has_resp_body = false
  method private def_empty_path_replacement = "/"
end

class trace_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "TRACE"
  method private def_is_idempotent = false
  method private def_has_req_body = false
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class options_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "OPTIONS"
  method private def_is_idempotent = false
  method private def_has_req_body = true
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "*"
end

class post_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "POST"
  method private def_is_idempotent = false
  method private def_has_req_body = true
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class put_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "PUT"
  method private def_is_idempotent = false
  method private def_has_req_body = true
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end

class delete_call =
object(self)
  inherit generic_call
  method private fixup_request() = ()
  method private def_request_method = "DELETE"
  method private def_is_idempotent = true
  method private def_has_req_body = false
  method private def_has_resp_body = true
  method private def_empty_path_replacement = "/"
end


class get the_query =
  object (self)
    inherit get_call
    initializer
      self # set_request_uri the_query
  end


class trace the_query max_hops =
  object (self)
    inherit trace_call
    initializer
      self # set_request_uri the_query
    method private fixup_request() =
      (self # request_header `Effective) # update_field 
	                                "max-forwards" (string_of_int max_hops)
  end


class options the_query =
  object (self)
    inherit options_call
    initializer
      self # set_request_uri the_query
  end


class head the_query =
  object (self)
    inherit head_call
    initializer
      self # set_request_uri the_query
  end


class post query params =
  object (self)
    inherit post_call
    initializer
      self # set_request_uri query
    method private fixup_request() =
      let rh = self # request_header `Effective in
      rh # update_field "Content-type" "application/x-www-form-urlencoded";
      let l = List.map (fun (n,v) -> n ^ "=" ^ Netencoding.Url.encode v) params in
      let s = String.concat "&" l in
      rh # update_field "Content-length" (string_of_int (String.length s));
      self # request_body # set_value s
  end
;;


class post_raw the_query s =
  object (self)
    inherit post_call
    initializer
      self # set_request_uri the_query
    method private fixup_request() =
      let rh = self # request_header `Effective in
      self # request_body # set_value s;
      rh # update_field "Content-length" (string_of_int (String.length s));
  end
;;


class put the_query s =
  object (self)
    inherit put_call
    initializer
      self # set_request_uri the_query
    method private fixup_request() =
      let rh = self # request_header `Effective in
      self # request_body # set_value s;
      rh # update_field "Content-length" (string_of_int (String.length s));
  end
;;


class delete the_query =
  object (self)
    inherit delete_call
    initializer
      self # set_request_uri the_query
  end
;;


(**********************************************************************)
(***                  AUTHENTICATION METHODS                        ***)
(**********************************************************************)

class type key =
object
  method user : string
  method password : string
  method realm : string
  method domain : string list
end


class type key_handler =
object
  method inquire_key :
            domain:string list -> realms:string list -> auth:string -> key
  method invalidate_key : key -> unit
end


class key_ring ?uplink () =
object(self)
  val mutable keys = (Hashtbl.create 10 : 
			(string list * string, key * bool) Hashtbl.t)
    (* Maps (domain, realm) to (key, from_uplink) *)

  method inquire_key ~domain ~realms ~(auth:string) =
    let l =
      List.flatten
	(List.map
	   (fun realm ->
	      try
		[ Hashtbl.find keys (domain, realm) ]
	      with
		  Not_found -> [])
	   realms) in
    match l with
      | (key,_) :: _ ->
	  key
      | [] ->
	  ( match uplink with
	      | None -> raise Not_found
	      | Some h ->
		  let key = h # inquire_key ~domain ~realms ~auth in
		  (* or Not_found *)
		  Hashtbl.replace keys (key#domain, key#realm) (key,true);
		  key
	  )

  method invalidate_key (key : key) =
    let domain = key # domain in
    let realm = key # realm in
    try
      let (_, from_uplink) = Hashtbl.find keys (domain, realm) in
      Hashtbl.remove keys (domain, realm);
      if from_uplink then
	( match uplink with
	    | None -> assert false
	    | Some h -> h # invalidate_key key
	)
    with
	Not_found -> ()

  method clear () =
    Hashtbl.clear keys

  method add_key key =
    let domain = key # domain in
    let realm = key # realm in
    Hashtbl.replace keys (domain, realm) (key, false)

  method keys =
    Hashtbl.fold
      (fun _ (key,_) acc -> key :: acc)
      keys
      []

end



class type auth_handler =
object
  method create_session : http_call -> auth_session option
end


exception Not_applicable

let get_domain_uri call =
  let h = call # get_host() in
  let p = call # get_port() in
  "http://" ^ h ^ ":" ^ string_of_int p ^ "/"


class basic_auth_session enable_auth_in_advance 
                         key_handler init_call
                         : auth_session =
  let domain = [ get_domain_uri init_call ] in
  let basic_realms =
    (* Return all "Basic" realms in www-authenticate, or raise Not_applicable *)
    let auth_list = 
      try
	Nethttp.Header.get_www_authenticate init_call#response_header
      with
	| Not_found -> raise Not_applicable
	| Nethttp.Bad_header_field _ -> raise Not_applicable in
    let basic_auth_list =
      List.filter 
	(fun (scheme,_) -> String.lowercase scheme = "basic") auth_list in
    let basic_auth_realm_list =
      List.flatten
	(List.map 
	   (fun (_,params) ->
	      try
		let (_,realm) =
		  List.find (fun (pname,_) -> 
			       String.lowercase pname = "realm") params in
		[realm]
	      with 
		  Not_found -> [])
	   basic_auth_list) in
    if basic_auth_realm_list = [] then
      raise Not_applicable
    else
      basic_auth_realm_list
  in
  let key =
    (* Return the selected key, or raise Not_applicable *)
    try
      key_handler # inquire_key ~domain ~realms:basic_realms ~auth:"basic"
    with
	Not_found -> raise Not_applicable
  in
  (* Check the key: *)
  let () =
    if not (List.mem key#realm basic_realms) then raise Not_applicable;
    if key#domain <> domain then raise Not_applicable;
  in
object(self)
  method auth_scheme = "basic"
  method auth_domain = domain
  method auth_realm = key # realm
  method auth_user = key # user
  method auth_in_advance = enable_auth_in_advance
  method authenticate call =
    let basic_cookie = 
      Netencoding.Base64.encode 
	(key#user ^ ":" ^ key#password) in
    let cred = "Basic " ^ basic_cookie in
    [ "Authorization", cred ]
  method invalidate call =
    key_handler # invalidate_key key;
    false
end


class basic_auth_handler ?(enable_auth_in_advance=false) 
                         (key_handler : #key_handler)
                         : auth_handler =
object(self)
  method create_session call =
    try
      Some(new basic_auth_session enable_auth_in_advance key_handler call)
    with
	Not_applicable ->
	  None
end



let contains_auth v =
  List.mem "auth" (split_words v)


class digest_auth_session enable_auth_in_advance 
                          key_handler init_call
                          : auth_session =
  let normalize_domain s =
    if s <> "" && s.[0] = '/' then
      let host = init_call#get_host() in
      let port = init_call#get_port() in
      "http://" ^ host ^ ":" ^ string_of_int port ^ s
    else
      ( try
	  let (_,_,host,port,path) = parse_http_url s in
	  "http://" ^ host ^ ":" ^ string_of_int port ^ path
	with
	    Not_found -> s
      )
  in
  let digest_request =
    (* Return the "Digest" params in www-authenticate, or raise Not_applicable *)
    let auth_list = 
      try
	Nethttp.Header.get_www_authenticate init_call#response_header
      with
	| Not_found -> raise Not_applicable
	| Nethttp.Bad_header_field _ -> raise Not_applicable in
    let digest_auth_list =
      List.filter 
	(fun (scheme,params) -> 
	   String.lowercase scheme = "digest"
	    && List.mem_assoc "realm" params
	    && (try List.mem (List.assoc "algorithm" params) ["MD5";"MD5-sess"]
		with Not_found -> true)
	    && (try contains_auth (List.assoc "qop" params)
		with Not_found -> true)
	    && List.mem_assoc "nonce" params
	) 
	auth_list in
    match  digest_auth_list with
      | [] ->
	  raise Not_applicable
      | (_,params) :: _ ->
	  (* Restriction: only the first request can be processed *)
	  params
  in
  let domain =
    try 
      List.map
	normalize_domain
	(split_words (List.assoc "domain" digest_request))
    with
	Not_found -> [ get_domain_uri init_call ] in
  let realm =
    try List.assoc "realm" digest_request
    with Not_found -> assert false in
  let key =
    (* Return the selected key, or raise Not_applicable *)
    try
      key_handler # inquire_key ~domain ~realms:[realm] ~auth:"digest"
    with
	Not_found -> raise Not_applicable
  in
  (* Check the key: *)
  let () =
    if key#realm <> realm then raise Not_applicable;
    if key#domain <> domain then raise Not_applicable;
  in
  let algorithm =
    try List.assoc "algorithm" digest_request
    with Not_found -> "MD5" in
  let qop =
    if List.mem_assoc "qop" digest_request then "auth" else "" in
    (* "" = RFC 2069 mode *)
  let nonce =
    try List.assoc "nonce" digest_request
    with Not_found -> assert false in
object(self)
  val mutable cnonce_init = string_of_float (Unix.time())
  val mutable cnonce_incr = 0
  val mutable nc = 0
  val mutable opaque = None
  val mutable a1 = None

  method private first_cnonce =
    Digest.to_hex
      (Digest.string (cnonce_init ^ ":0"))

  method private next_cnonce() =
    let cnonce =
      Digest.to_hex
	(Digest.string (cnonce_init ^ ":" ^ string_of_int cnonce_incr)) in
    cnonce_incr <- cnonce_incr + 1;
    cnonce

  method private next_nc() =
    let r = nc in
    nc <- nc + 1;
    r

  method private fn_h data =
    encode_hex (Digest.string data)

  method private fn_kd secret data =
    encode_hex (Digest.string (secret ^ ":" ^ data))

  method private a1 =
    match a1 with
      | Some v -> v
      | None ->
	  let v =
	    match algorithm with
	      | "MD5" ->
		  key#user ^ ":" ^ realm ^ ":" ^ key#password
	      | "MD5-sess" ->
		  (self # fn_h
		     (key#user ^ ":" ^ realm ^ ":" ^ key#password)) ^ 
		  ":" ^ nonce ^ ":" ^ self#first_cnonce
	      | _ ->
		  assert false
	  in
	  a1 <- Some v;
	  v

  method private a2 call =
    let meth = call # request_method in
    let uri = call # effective_request_uri in
    meth ^ ":" ^ uri

  method authenticate call =
    let cnonce = self#next_cnonce() in
    let nc = self # next_nc() in
    let digest =
      match qop with
	| "auth" ->
	    self#fn_kd
	      (self#fn_h self#a1)
	      (nonce ^ ":" ^ (Printf.sprintf "%08x" nc) ^ ":" ^ cnonce ^ ":" ^ 
		 "auth:" ^ (self#fn_h (self#a2 call)))
	| "" ->
	    self#fn_kd
	      (self#fn_h self#a1)
	      (nonce ^ ":" ^ (self#fn_h (self#a2 call))) 
	| _ ->
	    assert false  (* such digests are not accepted *)
    in
    let creds =
      Printf.sprintf
	"Digest username=\"%s\",realm=\"%s\",nonce=\"%s\",uri=\"%s\",response=\"%s\",algorithm=%s,cnonce=\"%s\",%s%snc=%08d"
	key#user
	realm
	nonce
	call#effective_request_uri
	digest
	algorithm
	cnonce
	(match opaque with
	   | None -> ""
	   | Some s -> "opaque=\"" ^ s ^ "\",")
	(match qop with
	   | "" -> ""
	   | "auth" -> "qop=auth,"
	   | _ -> assert false)
	nc in
    [ "Authorization", creds ]

  method auth_scheme = "digest"
  method auth_domain = domain
  method auth_realm = key # realm
  method auth_user = key # user
  method auth_in_advance = enable_auth_in_advance

  method invalidate call =
    (* Check if the [stale] flag is set for our nonce: *)
    let is_stale =
      try
	let auth_list = 
	  Nethttp.Header.get_www_authenticate call#response_header in
	List.exists
	  (fun (scheme,params) -> 
	     String.lowercase scheme = "digest"
	      && (try List.assoc "realm" params = realm
		  with Not_found -> false) 
	      && (try List.assoc "nonce" params = nonce
		  with Not_found -> false)
	      && (try String.lowercase (List.assoc "stale" params) = "true"
		  with Not_found -> false)
	  ) 
	  auth_list
      with
	| Not_found -> false  (* No www-authenticate header *)
	| Nethttp.Bad_header_field _ -> false in
    is_stale || (
      key_handler # invalidate_key key;
      false
    )
end


class digest_auth_handler ?(enable_auth_in_advance=false) 
                         (key_handler : #key_handler)
                         : auth_handler =
object(self)
  method create_session call =
    try
      Some(new digest_auth_session enable_auth_in_advance key_handler call)
    with
	Not_applicable ->
	  None
end


let only_http =
  let http_syntax = Hashtbl.find Neturl.common_url_syntax "http" in
  let schemes = Hashtbl.create 1 in
  Hashtbl.add schemes "http" http_syntax;
  schemes

let parse_http_neturl s =
  (* Parses the http URL s *)
  Neturl.parse_url
    ~schemes:only_http
    ~accept_8bits:true
    ~enable_fragment:true
    s

let norm_neturl neturl =
  (* Returns the neturl as normalized string (esp. normalized % sequences) *)
  let neturl' =
    Neturl.make_url 
      ~encoded:false
      ~scheme:(Neturl.url_scheme neturl)
      ~host:(Neturl.url_host neturl)
      ~port:(try Neturl.url_port neturl with Not_found -> 80)
      ~path:(try Neturl.url_path neturl with Not_found -> [])
      ?query:(try Some(Neturl.url_query neturl) with Not_found -> None)
      ?fragment:(try Some(Neturl.url_fragment neturl) with Not_found -> None)
      (Neturl.url_syntax_of_url neturl) in
  Neturl.string_of_url neturl'

let prefixes_of_neturl s_url =
  (* Returns a list of all legal prefixes of the absolute URI s.
   * The prefixes are in Neturl format.
   *)
  let rec rev_path_prefixes rev_path =
    match rev_path with
      | [] -> []
      | [ "" ] -> [ rev_path; [] ]
      | [ ""; "" ] -> assert false
      | [ _; "" ] -> rev_path :: rev_path_prefixes [ "" ]
      | "" :: rev_path' ->
	  if rev_path' = [ ""; "" ] then
	    rev_path :: rev_path_prefixes [ "" ]
	  else
	    rev_path :: rev_path_prefixes rev_path'
      | _ :: rev_path' ->
	  rev_path :: (rev_path_prefixes ("" :: rev_path'))
  in
  let path_prefixes path =
    List.map List.rev (rev_path_prefixes (List.rev path)) in
  let s_nofrag_url = Neturl.remove_from_url ~fragment:true s_url in
  let s_noquery_url = Neturl.remove_from_url ~query:true s_nofrag_url in
  let path = Neturl.url_path s_noquery_url in
  s_url :: s_nofrag_url ::
    (List.map
       (fun prefix -> Neturl.modify_url ~path:prefix s_noquery_url
       )
       (path_prefixes path))



class auth_cache =
object(self)
  val mutable auth_handlers = []
  val sessions = Hashtbl.create 10
    (* Only sessions that can be used for authentication in advance. 
     * The hash table maps domain URIs to sessions.
     *)

  method add_auth_handler (h : auth_handler) =
    auth_handlers <- auth_handlers @ [h]

  method create_session (call : http_call) =
    (* Create a new session after a 401 reply *)
    let rec find l =
      match l with
	| [] -> None
	| h :: l' ->
	    ( match h # create_session call with
		| None ->
		    find l'
		| Some s ->
		    Some s
	    )
    in
    find auth_handlers

  method tell_successful_session (sess : auth_session) =
    (* Called by [postprocess_complete_message] when authentication was
     * successful. If enabled, [sess] can be used for authentication
     * in advance.
     *)
    if sess # auth_in_advance then (
      List.iter
	(fun dom_uri ->
	   try
	     let dom_uri' = parse_http_neturl dom_uri in
	     let dom_uri'' = norm_neturl dom_uri' in
	     Hashtbl.replace sessions dom_uri'' sess
	   with
	     | Neturl.Malformed_URL -> ()
	)
	sess#auth_domain
    )

  method tell_failed_session (sess : auth_session) =
    (* Called by [postprocess_complete_message] when authentication 
     * failed
     *)
    List.iter
      (fun dom_uri ->
	 try
	   let dom_uri' = parse_http_neturl dom_uri in
	   let dom_uri'' = norm_neturl dom_uri' in
	   Hashtbl.remove sessions dom_uri''
	 with
	   | Neturl.Malformed_URL -> ()
      )
      sess#auth_domain;


  method find_session_in_advance (call : http_call) =
    (* Find a session suitable for authentication in advance *)
    let uri = call # request_uri in
    (* We are not only looking for [uri], but also for all prefixes of [uri] *)
    try
      let uri' = parse_http_neturl uri in
      let prefixes = prefixes_of_neturl uri' in
      let prefix =
	List.find (* or Not_found *)
	  (fun prefix ->
	     let s = norm_neturl prefix in
	     Hashtbl.mem sessions s
	  )
	  prefixes in
      Hashtbl.find sessions (norm_neturl prefix)
    with
      | Neturl.Malformed_URL ->
	  raise Not_found
end


(* Backwards compatibility: *)

class key_backing_store =
object(self)
  val db = (Hashtbl.create 10 : (string, (string*string)) Hashtbl.t)
  method set_realm realm user password =
    Hashtbl.replace db realm (user,password)
  method inquire_key ~domain ~realms ~(auth:string) =
    let realm = List.find (fun realm -> Hashtbl.mem db realm) realms in
    let (user, password) = Hashtbl.find db realm in
    ( object
	method user = user
	method password = password
	method realm = realm
	method domain = (domain : string list)
      end
    )
  method invalidate_key (_ : key) = ()
end


class auth_method name (mk_auth_handler : key_ring -> auth_handler) =
  let key_bs =
    new key_backing_store in
  let key_ring = 
    new key_ring ~uplink:key_bs () in
  let auth_handler = 
    mk_auth_handler key_ring in
object(self)
  method name = (name : string)
  method set_realm realm user password =
    key_bs # set_realm realm user password
  method as_auth_handler =
    auth_handler
end


class basic_auth_method =
  auth_method 
    "basic"
    (fun kr -> 
       new basic_auth_handler ~enable_auth_in_advance:true kr)

class digest_auth_method =
  auth_method
    "digest"
    (fun kr -> 
       new digest_auth_handler ~enable_auth_in_advance:true kr)


(**********************************************************************)
(***                 THE CONNECTION CACHE                           ***)
(**********************************************************************)

type conn_state = [ `Inactive | `Active of < > ]
  (** A TCP connection may be either [`Inactive], i.e. it is not used
    * by any pipeline, or [`Active obj], i.e. it is in use by the pipeline
    * [obj].
   *)

class type connection_cache =
object
  method get_connection_state : Unix.file_descr -> conn_state
    (** Returns the state of the file descriptor *)
  method set_connection_state : Unix.file_descr -> conn_state -> unit
    (** Sets the state of the file descriptor. It is allowed that
      * inactive descriptors are simply closed and forgotten.
     *)
  method find_inactive_connection : Unix.sockaddr -> Unix.file_descr
    (** Returns an inactive connection to the passed peer, or raise
      * [Not_found].
     *)
  method find_my_connections : < > -> Unix.file_descr list
    (** Returns all active connections owned by the object *)
  method forget_connection : Unix.file_descr -> unit
    (** Deletes the connection from the cache (it has been shut down
      * and is going to be closed) *)
  method close_all : unit -> unit
    (** Closes all descriptors known to the cache *)
end


let close_connection_cache conn_cache =
  conn_cache # close_all()


class restrictive_cache : connection_cache =
object(self)
  val mutable active_conns = Hashtbl.create 10
  val mutable rev_active_conns = Hashtbl.create 10

  method get_connection_state fd =
    `Active(Hashtbl.find active_conns fd)

  method set_connection_state fd state =
    match state with
      | `Active owner ->
	  Hashtbl.replace active_conns fd owner;
	  let fd_list = 
	    try Hashtbl.find rev_active_conns owner with Not_found -> [] in
	  if not (List.mem fd fd_list) then
	    Hashtbl.replace rev_active_conns owner (fd :: fd_list)
      | `Inactive ->
	  self # forget_connection fd;
	  Unix.close fd;

  method find_inactive_connection _ = raise Not_found

  method find_my_connections owner =
    try
      Hashtbl.find rev_active_conns owner
    with
	Not_found -> []

  method forget_connection fd =
    ( try
	let owner = Hashtbl.find active_conns fd in
	let fd_list = 
	  try Hashtbl.find rev_active_conns owner with Not_found -> [] in
	let fd_list' =
	  List.filter (fun fd' -> fd' <> fd) fd_list in
	Hashtbl.replace rev_active_conns owner fd_list'
      with
	  Not_found -> ()
    );
    Hashtbl.remove active_conns fd;

  method close_all () =
    Hashtbl.iter
      (fun fd _ ->
	 Unix.close fd)
      active_conns;
    Hashtbl.clear active_conns;
    Hashtbl.clear rev_active_conns
	  
end


let create_restrictive_cache() = new restrictive_cache


class aggressive_cache : connection_cache =
object(self)
  val mutable active_conns = Hashtbl.create 10
    (* maps file_descr to owner *)
  val mutable rev_active_conns = Hashtbl.create 10
    (* maps owner to file_descr list *)
  val mutable inactive_conns = Hashtbl.create 10
    (* maps file_descr to sockaddr *)
  val mutable rev_inactive_conns = Hashtbl.create 10
    (* maps sockaddr to file_descr list *)

  method get_connection_state fd =
    try
      `Active(Hashtbl.find active_conns fd)
    with
	Not_found ->
	  if Hashtbl.mem inactive_conns fd then
	    `Inactive
	  else
	    raise Not_found

  method set_connection_state fd state =
    match state with
      | `Active owner ->
	  self # forget_inactive_connection fd;
	  Hashtbl.replace active_conns fd owner;
	  let fd_list = 
	    try Hashtbl.find rev_active_conns owner with Not_found -> [] in
	  if not (List.mem fd fd_list) then
	    Hashtbl.replace rev_active_conns owner (fd :: fd_list)
      | `Inactive ->
	  ( try
	      let peer = Unix.getpeername fd in
	      self # forget_active_connection fd;
	      Hashtbl.replace inactive_conns fd peer;
	      let fd_list =
		try Hashtbl.find rev_inactive_conns peer with Not_found -> [] in
	      if not (List.mem fd fd_list) then
		Hashtbl.replace rev_inactive_conns peer (fd :: fd_list)
	    with
	      | Unix.Unix_error(Unix.ENOTCONN,_,_) ->
		  self # forget_connection fd
	  )

  method find_inactive_connection peer =
    match Hashtbl.find rev_inactive_conns peer with
      | [] -> raise Not_found
      | fd :: _ -> fd

  method find_my_connections owner =
    try
      Hashtbl.find rev_active_conns owner
    with
	Not_found -> []

  method private forget_active_connection fd =
    ( try
	let owner = Hashtbl.find active_conns fd in
	let fd_list = 
	  try Hashtbl.find rev_active_conns owner with Not_found -> [] in
	let fd_list' =
	  List.filter (fun fd' -> fd' <> fd) fd_list in
	if fd_list' <> [] then 
	  Hashtbl.replace rev_active_conns owner fd_list'
	else
	  Hashtbl.remove rev_active_conns owner
      with
	  Not_found -> ()
    );
    Hashtbl.remove active_conns fd;
   

  method private forget_inactive_connection fd =
    try
      let peer = Hashtbl.find inactive_conns fd in
      (* Do not use getpeername! fd might be disconnected in the meantime! *)
      let fd_list = 
	try Hashtbl.find rev_inactive_conns peer with Not_found -> [] in
      let fd_list' =
	List.filter (fun fd' -> fd' <> fd) fd_list in
      if fd_list' <> [] then 
	Hashtbl.replace rev_inactive_conns peer fd_list'
      else
	Hashtbl.remove rev_inactive_conns peer;
      Hashtbl.remove inactive_conns fd;
    with
      | Not_found ->
	  ()


  method forget_connection fd =
    self # forget_active_connection fd;
    self # forget_inactive_connection fd


  method close_all () =
    Hashtbl.iter
      (fun fd _ ->
	 Unix.close fd)
      active_conns;
    Hashtbl.clear active_conns;
    Hashtbl.clear rev_active_conns;
    Hashtbl.iter
      (fun fd _ ->
	 Unix.close fd)
      inactive_conns;
    Hashtbl.clear inactive_conns;
    Hashtbl.clear rev_inactive_conns
end


let create_aggressive_cache() = new aggressive_cache


(**********************************************************************)
(***                       THE I/O BUFFER                           ***)
(**********************************************************************)

(* io_buffer performs the socket I/O.
 *
 * TODO: COMMENT OUT OF DATE
 * The input side is a queue of octets which can be filled by
 * a Unix.read call at its end, and from which octets can be removed
 * at its beginning ("consuming octets").
 * There is also functionality to remove 1XX responses at the beginning
 * of the buffer, and to interpret the beginning of the buffer as HTTP
 * status line.
 * The idea of the buffer is that octets can be added at the end of the
 * buffer while the beginning of the buffer is interpreted as the beginning
 * of the next message. Once enough octets have been added that the message
 * is complete, it is removed (consumed) from the buffer, and the possibly
 * remaining octets are the beginning of the following message.
 *)

exception Garbage_received of string
  (* This exception is raised by [parse_response] when a protocol error
   * occurred before the response status line has been completely received.
   * Such errors are not transferred to the http_call.
   *)


let line_end_re = Netstring_pcre.regexp "[^\\x00\r\n]+\r?\n";;

let line_end2_re = Netstring_pcre.regexp "([^\\x00\r\n]+\r?\n)*\r?\n";;

let status_re = Netstring_pcre.regexp "^([^ \t]+)[ \t]+([0-9][0-9][0-9])([ \t]+([^\r\n]*))?\r?\n$" ;;

let chunk_re = Netstring_pcre.regexp "[ \t]*([0-9a-fA-F]+)[ \t]*(;[^\r\n\\x00]*)?\r?\n" ;;

let crlf_re = Netstring_pcre.regexp "\r?\n";;

type sockstate =
    Down
  | Up_rw 
  | Up_r
;;


class io_buffer options conn_cache fd fd_state =
  object (self)

    (****************************** SOCKET ********************************)

    val mutable socket_state = fd_state

    method socket_state = socket_state

    method socket = 
      match socket_state with
	| Down -> failwith "Socket is down"
	| _ -> fd

    method close_out() =
      match socket_state with
	| Down  -> ()
	| Up_rw -> 
	    if options.verbose_connection then 
	      prerr_endline "HTTP connection: Sending EOF!";
	    Unix.shutdown fd Unix.SHUTDOWN_SEND; 
	    socket_state <- Up_r
	| Up_r  -> 
	    ()

    method close() =
      match socket_state with
	| Down  -> ()
	| _     -> 
	    if options.verbose_connection then 
	      prerr_endline "HTTP connection: Closing socket";
	    socket_state <- Down;
	    conn_cache # forget_connection fd;
	    Unix.close fd; 

    method release() =
      (* Give socket back to cache: *)
      match socket_state with
	| Down  -> ()
	| Up_r  -> self # close()
	| Up_rw -> 
	    conn_cache # set_connection_state fd `Inactive;
	    socket_state <- Down  (* our view *)


    (****************************** INPUT ********************************)

    val mutable in_buf = B.create 8192
    val mutable in_eof = false
    val mutable in_eof_parsed = false (* whether eof has been seen by parser *)
    val mutable in_pos = 0            (* parse position *)
    val mutable status_seen = false
    val mutable timeout = false
    val mutable restart_parser = (fun _ -> assert false)

    initializer
      restart_parser <- self # parse_status_line;

      (* Ensure the 0-byte invariant holds (see comment in unix_read): *)
      let b = B.unsafe_buffer in_buf in
      if B.length in_buf < String.length b then
	b.[B.length in_buf] <- '\000'

    method unix_read () =
      if not in_eof && not timeout then (
	let n = 
	  B.add_inplace 
	    in_buf
	    (fun io_buf pos len ->
	       syscall (fun() -> Unix.recv fd io_buf pos len []))
	in
	if n = 0 then
	  in_eof <- true;
	(* None of the regexps matches the null byte. So this byte can be
         * used as guard that indicates the end of the buffer. We only
         * have to make sure it is always there.
         *)
	let b = B.unsafe_buffer in_buf in
	if B.length in_buf < String.length b then
	  b.[B.length in_buf] <- '\000';
      )
	    
    method timeout =
      timeout

    method set_timeout =
      timeout <- true

    method has_unparsed_data =
      (* whether data has been received that must go through the parser *)
      (in_eof && not in_eof_parsed) || (in_pos < B.length in_buf)

    method has_unparsed_bytes =
      (* whether there are unparsed data consisting of at least one byte *)
      in_pos < B.length in_buf

    method status_seen =
      (* Whether at least one status line (incl. status 1XX) has been seen *)
      status_seen

    method in_eof = 
      (* end of stream indicator *)
      in_eof

    method parse_response (call : http_call) =
      (* Parses the [in_buf] buffer and puts the parsed data into
       * [call].
       *
       * This function must only be called if at least one additional
       * byte has been received. The function returns when
       * - not enough bytes are available, or
       * - the response has been completely parsed. This latter case
       *   can be recognized because the [call] is finished then.
       *)
      try
	restart_parser call;
	assert(in_pos >= 0 && in_pos <= B.length in_buf);
	   (* Many versions of Netbuffer do not check arguments of [delete]
            * correctly.
            *)
	B.delete in_buf 0 in_pos;
	in_pos <- 0;
	in_eof_parsed <- in_eof;
	(* Ensure the 0-byte invariant holds (see comment in unix_read): *)
	let b = B.unsafe_buffer in_buf in
	if B.length in_buf < String.length b then
	  b.[B.length in_buf] <- '\000'
      with
	| Bad_message _ as err -> 
	    call # private_api # set_error_exception err;
	    assert(status_seen)  (* otherwise cleanup code will not work *)
        (* Garbage_received not caught! *)
      

    method private parse_status_line call =
      (* Parses the status line. If 1XX: do XXX *)
      restart_parser <- self # parse_status_line;
      let b = B.unsafe_buffer in_buf in
      match Netstring_pcre.string_match line_end_re b in_pos with
	| None ->
	    if B.length in_buf - in_pos > 500 then 
	      raise (Garbage_received "Status line too long");
	    if in_eof then 
	      raise (Garbage_received "EOF where status line expected")
	| Some m ->
	    in_pos <- Netstring_pcre.match_end m;
	    assert(in_pos <= B.length in_buf);
	    let s = Netstring_pcre.matched_string m b in
	    ( match Netstring_pcre.string_match status_re s 0 with
		| None ->
		    raise (Garbage_received "Bad status line")
		| Some m ->
		    let proto = Netstring_pcre.matched_group m 1 s in
		    let code_str = Netstring_pcre.matched_group m 2 s in
		    let code = int_of_string code_str in
		    let text =
		      try Netstring_pcre.matched_group m 4 s 
		      with Not_found -> "" in
		    if code < 100 || code > 599 then 
		      raise (Garbage_received "Bad status code");
		    status_seen <- code >= 200;
		    call # private_api # set_response_status code text proto;
		    self # parse_header code call
	    )

    method private parse_header code call =
      (* Parses the HTTP header following the status line *)
      restart_parser <- self # parse_header code;
      let b = B.unsafe_buffer in_buf in
      match Netstring_pcre.string_match line_end2_re b in_pos with
	| None ->
	    if B.length in_buf - in_pos > 100000 then (
	      if code < 200 then
		raise (Garbage_received "Response header too long")
	      else
		raise (Bad_message "Response header too long")
	    );
	    if in_eof then (
	      if code < 200 then
		raise (Garbage_received "EOF where response header expected")
	      else
		raise (Bad_message "EOF where response header expected")
	    )
	| Some m ->
	    let start = in_pos in
	    in_pos <- Netstring_pcre.match_end m;
	    assert(in_pos <= B.length in_buf);
	    let _ch =
	      new Netchannels.input_string ~pos:start ~len:(in_pos-start) b in
	    let header_l, real_end_pos =
	      try
		Mimestring.scan_header
	          ~downcase:false ~unfold:true ~strip:true b 
		  ~start_pos:start ~end_pos:in_pos 
	      with
		| Failure _ ->
		    if code < 200 then
		      raise (Garbage_received "Bad response header")
		    else
		      raise (Bad_message "Bad response header")
	    in
	    assert(real_end_pos = in_pos);
	    let header = new Netmime.basic_mime_header header_l in
	    if code >= 100 && code <= 199 then (
	      call # private_api # set_continue();
	      self # parse_status_line call
	    )
	    else (
	      call # private_api # set_response_header header;
	      self # parse_body code header call
	    )

    method private parse_body code header call =
      (* Parses the whole HTTP body *)
      restart_parser <- self # parse_body code header;
      (* First determine whether a body is expected: *)
      let have_body =
	call # has_resp_body && code <> 204 && code <> 304 in
      if have_body then (
	let ch = call # private_api # response_body_open_wr() in
	(* Check if we have chunked encoding: *)
	let is_chunked =
	  try header # field "Transfer-encoding" <> "identity"
	  with Not_found -> false in
	if is_chunked then
	  self # parse_chunked_body code header ch call
	else (
	  let length_opt =
	    try 
	      let l = Int64.of_string (header # field "Content-Length") in
	      if l < 0L then raise (Bad_message "Bad Content-Length field");
	      Some l
	    with
	      | Failure _ -> raise (Bad_message "Bad Content-Length field")
	      | Not_found -> None in
	  self # parse_plain_body code header ch length_opt call
	)
      )
      else
	self # parse_end call

    method private parse_plain_body code header ch length_opt call =
      (* Parses a non-chunked HTTP body. If length_opt=None, the message
       * is terminated by EOF. If length_opt=Some len, the message has
       * this length.
       *)
      restart_parser <- self # parse_plain_body code header ch length_opt;
      let av_len = B.length in_buf - in_pos in
      match length_opt with
	| None ->
	    ch # really_output (B.unsafe_buffer in_buf) in_pos av_len;
	    in_pos <- in_pos + av_len;
	    assert(in_pos <= B.length in_buf);
	    if in_eof then self # parse_end call

	| Some len ->
	    let l = Int64.to_int (min (Int64.of_int av_len) len) in
	    ch # really_output (B.unsafe_buffer in_buf) in_pos l;
	    in_pos <- in_pos + l;
	    assert(in_pos <= B.length in_buf);
	    let len' = Int64.sub len (Int64.of_int l) in
	    if len' > 0L then (
	      if in_eof then 
		raise (Bad_message "Response body too short");
	      restart_parser <- 
		self # parse_plain_body code header ch (Some len');
	    ) else (
	      self # parse_end call
	    )

    method private parse_chunked_body code header ch call =
      (* Parses a chunked HTTP body *)
      restart_parser <- self # parse_chunked_body code header ch;
      let b = B.unsafe_buffer in_buf in
      match Netstring_pcre.string_match chunk_re b in_pos with
	| None ->
	    if B.length in_buf - in_pos > 5000 then 
	      raise (Bad_message "Cannot parse chunk of response body");
	    if in_eof then 
	      raise (Bad_message "EOF where next response chunk expected")
	| Some m ->
	    in_pos <- Netstring_pcre.match_end m;
	    assert(in_pos <= B.length in_buf);
	    let hex_len = Netstring_pcre.matched_group m 1 b in
	    let len =
	      try Int64.of_string ("0x" ^ hex_len)
	      with Failure _ -> 
		raise (Bad_message "Chunk too large") in
	    if len = 0L then
	      self # parse_trailer code header ch call
	    else
	      self # parse_chunk_data code header ch len call

    method private parse_chunk_data code header ch len call =
      (* Parses the chunk data following the chunk size field *)
      restart_parser <- self # parse_chunk_data code header ch len;
      let av_len = B.length in_buf - in_pos in
      let l = Int64.to_int (min (Int64.of_int av_len) len) in
      ch # really_output (B.unsafe_buffer in_buf) in_pos l;
      in_pos <- in_pos + l;
      assert(in_pos <= B.length in_buf);
      let len' = Int64.sub len (Int64.of_int l) in
      if len' > 0L then (
	if in_eof then 
	  raise (Bad_message "Repsonse chunk terminated by EOF");
	restart_parser <- 
	  self # parse_chunk_data code header ch len'
      ) else
	self # parse_chunk_end code header ch call

    method private parse_chunk_end code header ch call =
      (* Parses the CRLF after the chunk, and the next chunks *)
      restart_parser <- self # parse_chunk_end code header ch;
      let b = B.unsafe_buffer in_buf in
      match Netstring_pcre.string_match crlf_re b in_pos with
	| None ->
	    if B.length in_buf - in_pos > 2 then 
	      raise (Bad_message "CR/LF after response chunk is missing");
	    if in_eof then 
	      raise (Bad_message "EOF where next response chunk expected")
	| Some m ->
	    in_pos <- Netstring_pcre.match_end m;
	    assert(in_pos <= B.length in_buf);
	    self # parse_chunked_body code header ch call

    method private parse_trailer code header ch call =
      (* Parses the trailer *)
      restart_parser <- self # parse_trailer code header ch;
      let b = B.unsafe_buffer in_buf in
      match Netstring_pcre.string_match line_end2_re b in_pos with
	| None ->
	    if B.length in_buf - in_pos > 10000 then 
	      raise (Bad_message "Response trailer too large");
	    if in_eof then 
	      raise (Bad_message "EOF where response trailer expected")
	| Some m ->
	    let start = in_pos in
	    in_pos <- Netstring_pcre.match_end m;
	    assert(in_pos <= B.length in_buf);
	    let _ch =
	      new Netchannels.input_string ~pos:start ~len:(in_pos-start) b in
	    let trailer_l, real_end_pos = 
	      try
		Mimestring.scan_header
	          ~downcase:false ~unfold:true ~strip:true b 
		  ~start_pos:start ~end_pos:in_pos 
	      with
		| Failure _ -> raise(Bad_message "Bad trailer") in
	    assert(real_end_pos = in_pos);
	    (* The trailer is simply added to the header: *)
	    let new_header =
	      new Netmime.basic_mime_header (header#fields @ trailer_l) in
	    call # private_api # set_response_header new_header;
	    self # parse_end call
	      
    method private parse_end call =
      (* The message ends here! *)
      restart_parser <- self # parse_status_line;  (* for the next message *)
      call # private_api # finish()


    (****************************** OUTPUT ********************************)

    val mutable string_to_send = ""
    val mutable send_buffer = String.create 8192
    val mutable send_position = 0
    val mutable send_length = 0

    method send_this_string s =
      string_to_send <- s;
      send_position <- 0;
      send_length <- String.length s

    method send_by_buffer f =
      string_to_send <- send_buffer;
      send_position <- 0;
      send_length <- 0;
      let m = String.length send_buffer in
      let n = f send_buffer 0 m in
      send_length <- n;
      n

    method nothing_to_send =
      send_position = send_length

    method unix_write() =
      let n_to_send = send_length - send_position in
      let n = 
	syscall (fun () -> 
		   Unix.send fd string_to_send send_position n_to_send [])
      in
      send_position <- send_position + n;

    method dump_send_buffer () =
      prerr_endline ("HTTP request body fragment:\n" ^ 
		       String.sub send_buffer 0 send_length ^ "\n"
		    )

  end
;;

(**********************************************************************)
(***                PROTOCOL STATE OF A SINGLE MESSAGE              ***)
(**********************************************************************)

(* The class 'transmitter' is a container for the message and the
 * associated protocol state, and defines the methods to send
 * the request, and to receive the reply.
 * The protocol state stored here mainly controls the send and receive
 * buffers (e.g. how many octets have already been sent? Are the octets
 * received so far already a complete message or not?)
 *)


type message_state =
    Unprocessed     (* Not even a byte sent or received *)
  | Sending_hdr     (* Sending currently the header *)
  | Handshake       (* The header has been sent, now waiting for status 100 *)
  | Sending_body    (* The header has been sent, now sending the body *)
  | Sent_request    (* The body has been sent; the reply is being received *)
  | Complete        (* The whole reply has been received *)
  | Complete_broken (* The whole reply has been received, but the connection
                     * is in a state that does not permit further requests
                     *)
  | Broken          (* Garbage was responded *)
;;

(* Transitions:
 *
 * (1) Normal transitions:
 * 
 * Unprocessed -> Sending_hdr: Always done (a)
 * Sending_hdr -> Handshake: Only if we are handshaking (a)
 * Handshake -> Sending_body: When the 100 Continue status arrives (b)
 * Sending_hdr -> Sending_body: usually (a)
 * Sending_body -> Sent_request: usually (a)
 * Sending_hdr -> Sent_request: if no body is sent (a)
 * Sent_request -> Complete: when the response has arrived (b)
 * Handshake -> Complete_broken:  when the response arrives instead of 100 Continue (b)
 *
 * (2) Unusual transitions:
 *
 * (Unprocessed | Sending_hdr | Sending_body) -> Complete_broken: 
 *    The response arrives while/before sending the header/body (b)
 *    or: Parsing error in the response (b)
 *
 * (Unprocessed | Sending_hdr | Sending_body) -> Broken
 *    Garbage arrives (b)
 *
 *      (a) transition is done by the sending side of the transmitter
 *      (b) transition is done by the receiving side of the transmitter
 *)



class transmitter
  peer_is_proxy
  (m : http_call) 
  (f_done : http_call -> unit)
  options
  =
  object (self) 
    val mutable state = Unprocessed
    val indicate_done = f_done
    val msg = m

    val mutable auth_headers = []
      (* Additional header for _proxy_ authentication *)

    val mutable body = new Netchannels.input_string ""
    val mutable body_length = None   (* Set after [init] *)

    method state = state

    method f_done = indicate_done

    method init() =
      (* Prepare for (re)transmission:
       * - Set the `Effective request header
       * - Reset the status info of the http_call
       * - Initialize transmission state
       *)
      msg # private_api # prepare_transmission();
      (* Set the effective URI. This must happen before authentication. *)
      let eff_uri =
	if peer_is_proxy then
	   msg # request_uri
	else
	  let path = msg # get_path() in
	  if path = "" then (
	    msg # empty_path_replacement
	  )
	  else path
      in
      msg # private_api # set_effective_request_uri eff_uri;
      let ah = 
	match msg # private_api # auth_state with
	  | `None -> []
	  | `In_advance session 
	  | `In_reply session ->
	      session # authenticate msg in
      let rh = msg # request_header `Effective in
      List.iter
	(fun (n,v) ->
	   rh # update_field n v
	)
	(auth_headers @ ah);
      state <- Unprocessed;
      body_length <- ( try
			 let s = rh # field "Content-length" in
			 Some(Int64.of_string s)
		       with Not_found -> None
		     );
      self # close_body();
      let have_body =
	msg # has_req_body &&
	  (body_length = None || body_length <> Some 0L) in
      if not have_body then (
	(* Remove certain headers *)
	rh # delete_field "Expect";
      );

    method add_auth_header n v =
      auth_headers <- (n,v) :: auth_headers

    method private open_body() =
      body <- msg # request_body # open_value_rd()

    method close_body() =
      try body # close_in() with _ -> ()
      (* also called by [clear_write_queue] *)

    method send (io : io_buffer) do_handshake =
      (* do_handshake: If true, only the header is transmitted, and the
       * state transitions to [Handshake]. This flag is ignored if we have
       * already [Sending_body].
       *)

      assert 
	(state = Unprocessed || state = Sending_hdr || state = Sending_body);

      (* First check whether we have to refill [string_to_send]: *)

      ( match state with
	  | Unprocessed ->
	      (* Fill [string_to_send] with the request line and the header. *)
	
	      let buf = Buffer.create 1000 in
	      let req_meth = msg # request_method in
	      
	      Buffer.add_string buf req_meth;
	      Buffer.add_string buf " ";
	      Buffer.add_string buf msg#effective_request_uri;
	      let rh = msg # request_header `Effective in
	      if not peer_is_proxy then (
		let host = msg # get_host() in
		let port = msg # get_port() in
		let host_str = host ^ (if port = 80 then "" 
				       else ":" ^ string_of_int port) in
		rh # update_field "Host" host_str;
	      );
	      Buffer.add_string buf " HTTP/1.1";
	      
	      if options.verbose_status then
		prerr_endline ("HTTP request: " ^ Buffer.contents buf);
	      
	      Buffer.add_string buf "\r\n";
	      
	      let ch = new Netchannels.output_buffer buf in
	      Mimestring.write_header ch rh#fields;
	      ch # close_out();
	      
	      if options.verbose_request_header then
		dump_header "HTTP request " rh#fields;
	      
	      io # send_this_string (Buffer.contents buf);
	      state <- Sending_hdr;

	  | Sending_body when io # nothing_to_send ->
	      ( try 
		  let m =
		    match body_length with
		      | None -> 
			  max_int
		      | Some l -> 
			  Int64.to_int
			    (min (Int64.of_int max_int) l) in
		  if m = 0 then raise End_of_file;
		  let n = io # send_by_buffer body#input in
		  ( match body_length with
		      | None -> ()
		      | Some l ->
			  body_length <- Some(Int64.sub l (Int64.of_int n))
		  );
		  if options.verbose_request_contents then
		    io # dump_send_buffer()
		with
		    End_of_file ->
		      self # close_body();
		      if body_length = None then (
			io # close_out();
		      );
		      state <- Sent_request
	      )

	  | _ ->
	      ()
      );

      (* Send: *)

      if state = Sending_hdr || state = Sending_body then
	io # unix_write();
      
      (* Update state: *)

      if io # nothing_to_send then (
	match state with
	  | Sending_hdr ->
	      let have_body =
		msg # has_req_body &&
		(body_length = None || body_length <> Some 0L) in

	      if have_body then (
		if do_handshake then
		  state <- Handshake
		else
		  state <- Sending_body;
		self # open_body();
	      )
	      else
		state <- Sent_request

	  | _ ->
	      ()
      )

    method receive (io : io_buffer) =
      (* This method is invoked if some additional octets have been received
       * that 
       * - may begin this reply
       * - or continue this reply
       * - or make this reply complete
       * - or make this reply complete and begin another reply
       * It is checked if the reply is complete, and if so, it is recorded
       * and the octets forming the reply are removed from the input buffer.
       * Raises Bad_message if the message is malformed.
       *)
      try
	io # parse_response msg;
	match msg # status with
	  | `Unserved -> 
	      if state = Handshake && msg # private_api # continue then
		state <- Sending_body
	  | `Http_protocol_error e ->
	      state <- Complete_broken;
	      let e_msg =
		match e with
		  | Bad_message s -> ": Bad message: " ^ s
		  | _ -> ": " ^ Netexn.to_string e in
	      if options.verbose_status then
		prerr_endline ("HTTP protocol error" ^ e_msg)
	  | _ -> 
	      if state = Sent_request then 
		state <- Complete 
	      else (
		if options.verbose_status then
		  prerr_endline "HTTP status: Got response before request was completely sent";
		state <- Complete_broken;
		io # close_out()
	      );
	      if options.verbose_status then
		msg # private_api # dump_status();
	      if options.verbose_response_header then
		msg # private_api # dump_response_header();
	      if options.verbose_response_contents then
		msg # private_api # dump_response_body();
      with
	| Garbage_received s ->
	    state <- Broken;
	    if options.verbose_status then
	      prerr_endline ("HTTP protocol error: Garbage received: " ^ s)

    method handshake_timeout() =
      if state = Handshake then
	state <- Sending_body

    method indicate_pipelining =
      (* Return 'true' iff the reply is HTTP/1.1 compliant and does not
       * contain the 'connection: close' header.
       *)
      let resp_header = msg # private_api # response_header in
      let proto_str = msg # private_api # response_proto in
      let b1 =
	try
	  let proto = Nethttp.protocol_of_string proto_str in
	  match proto with
	    | `Http((1,n),_) when n >= 1 ->  (* HTTP/1.1 <= proto < HTTP/2.0 *)
		let conn_list = 
		  try Nethttp.Header.get_connection resp_header 
		  with _ (* incl. syntax error *) -> [] in
		not (List.mem "close" conn_list)
	    | _ ->
		false
	with _ -> false in
      b1 && (
	try
	  let server = resp_header # field "Server" in
	  not (List.exists 
		 (fun re -> 
		    Netstring_pcre.string_match re server 0 <> None
		 ) 
		 pipeline_blacklist)
	with
	  | Not_found -> true  (* Nothing known ... Assume the best! *)
      )
	
    method indicate_sequential_persistency =
      (* Returns 'true' if persistency without pipelining
       * is possible.
       *)
      let resp_header = msg # private_api # response_header in
      let proto_str = msg # private_api # response_proto in
      let is_http_11 =
	try
	  let proto = Nethttp.protocol_of_string proto_str in
	  match proto with
	    | `Http((1,n),_) when n >= 1 ->  (* HTTP/1.1 <= proto < HTTP/2.0 *)
		true
	    | _ ->
		false
	with _ -> false in
      let proxy_connection =
	List.map String.lowercase
	  (resp_header # multiple_field "proxy-connection") in
      let connection = 
	try Nethttp.Header.get_connection resp_header 
	with _ (* incl. syntax error *) -> [] in
      let normal_persistency =
	not peer_is_proxy && 
	  (not (List.mem "close" connection)) &&
	  (is_http_11 || List.mem "keep-alive" connection) in
      let proxy_persistency =
	peer_is_proxy && List.mem "keep-alive" proxy_connection in
      normal_persistency || proxy_persistency
	

    method postprocess =
      indicate_done msg;

    method message = msg
  
  end
;;


(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(***                                                                ***)
(***           THE PROTOCOL STATE OF THE CONNECTION                 ***)
(***                                                                ***)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

(* This is the core of this HTTP implementation: The object controlling
 * the state of the connection to a server (or a proxy).
 *)

class connection the_esys 
                 (peer_host,peer_port)
                 peer_is_proxy
                 (proxy_user,proxy_password) 
                 auth_cache
                 conn_cache
		 conn_owner   (* i.e. the pipeline coerced to < > *)
		 counters     (* we count here only when connections close *)
		 the_options
  =
  object (self)
    val mutable esys = the_esys
    val mutable group = None

    (* timeout_groups: Unixqueue groups that must be deleted when the 
     * connection is closed. These groups represent timeout conditions.
     *)
    val mutable timeout_groups = []

    val mutable io = 
      new io_buffer the_options conn_cache Unix.stdin Down  (* dummy *)

    val mutable write_queue = Q.create()
    val mutable read_queue = Q.create()
      (* Invariant: write_queue is a suffix of read_queue *)

    (* polling_wr is 'true' iff the write side of the socket is currently
     * polled. (The read side is always polled.)
     *)

    val mutable polling_wr = false

    (* 'connecting' is 'true' if the 'connect' system call cannot connect
     * immediately, and continues to connect in the background.
     *)

    val mutable connecting = None

    (* 'connect_pause': seconds to wait until the next connection is tried.
     * There seems to be a problem with some operating systems (namely
     * Linux 2.2) which do not like immediate reconnects if the previous
     * connection did not end in a sane state.
     * A value of 0.5 seems to be sufficient for reconnects (see below).
     *)			      

    val mutable connect_pause = 0.0

    (* The following two variables control whether pipelining is enabled or
     * not. The problem is that it is unclear how old servers react if we
     * send to them several requests at once. The solution is that the first
     * "round" of requests and replies is done in a compatibility mode: After
     * the first request has been sent sending stops, and the client waits
     * until the reply is received. If the reply indicates HTTP/1.1 and does
     * not contain a "connection: close" header, all further requests and 
     * replies will be performed in pipelining mode, i.e. the requests are
     * sent independent of whether the replies of the previous requests have
     * been received or not.
     *
     * sending_first_message: 'true' means that the first request has not yet
     *    been completely sent to the server. 
     * done_first_message: 'true' means that the reply of the first request
     *    has been arrived.
     *)

    val mutable sending_first_message = true
    val mutable done_first_message = false

    (* 'inhibit_pipelining_byserver': becomes true if the server is able
     * to keep persistent connections but is not able to use pipelining.
     * (HTTP/1.0 "keep alive" connections)
     *)

    val mutable inhibit_pipelining_byserver = false
 
    (* 'connect_started': when the last 'connect' operation started.
     * 'connect_time': how many seconds the last 'connect' lasted. 
     *)

    val mutable connect_started = 0.0
    val mutable connect_time = 0.0

    (* If a connection does not lead to any type of response, the client 
     * simply re-opens a new connection and tries again. The following 
     * variables limit the number of attempts until the client gives up.
     *
     * 'totally_failed_connections': counts the number of failed connections
     *      without any response from the server.
     *)

    val mutable totally_failed_connections = 0

    (* 'close_connection' indicates that a HTTP/1.0 response was received or 
     * that a response contained a 'connection: close' header.
     *)

    val mutable close_connection = false

    (* Proxy authorization: If 'proxy_user' is non-empty, the variables
     * 'proxy_user' and 'proxy_password' are interpreted as user and
     * password for proxy authentication. More precisely, once the proxy
     * responds with status code 407, 'proxy_credentials_required' becomes
     * true, and all following requests will include the credentials identifying
     * the user (with the "basic" authentication method).
     * If the proxy responds again with code 407, this reaction will not
     * be handled again but will be visible to the outside.
     *)

    val mutable proxy_credentials_required = false
    val mutable proxy_cookie = ""


    (* 'critical_section': true if further additions to the queues and
     * every change of the internal state must be avoided. In this case,
     * additions are put into 'deferred_additions' and actually added later.
     *)

    val mutable deferred_additions = Q.create()
    val mutable critical_section = false

    val mutable options = the_options

    method length =
      (* Returns the number of open requests (requests without response) *)
      Q.length read_queue + Q.length deferred_additions


    method active =
      (* Whether something is to be done *)
      self # length > 0


    method add (m : http_call) f_done =
      (* add 'm' to the read and write queues *)
      ignore(self # add_msg false m f_done)


    method private add_msg ?(critical=critical_section) urgent m f_done =
      (* In contrast to 'add', the new message transporter is returned. *)

      (* Create the transport container for the message and add it to the
       * queues:
       *)
      let trans = new transmitter peer_is_proxy m f_done options in

      (* If proxy authentication is enabled, and it is already known that
       * the proxy demands authentication, add the necessary header fields: 
       * (See also the code and the comments in method 
       * 'postprocess_complete_message')
       *)
      if proxy_credentials_required  &&  peer_is_proxy then begin
	(* If the cookie is not yet computed, do this now: *)
	if proxy_cookie = "" then
	  proxy_cookie <- Netencoding.Base64.encode
	                     (proxy_user ^ ":" ^ proxy_password);
	(* Add the "proxy-authorization" header: *)
	trans # add_auth_header "proxy-authorization" ("Basic " ^ proxy_cookie)
      end;

      (* Check whether we can authenticate in advance: *)
      if m # private_api # auth_state = `None then (
	try
	  let sess = auth_cache # find_session_in_advance m in
	  m # private_api # set_auth_state (`In_advance sess)
	with
	    Not_found -> ()
      );

      (* Initialize [trans] for transmission: (Must be after in-advance
       * authentication)
       *)
      trans # init();

      if critical then begin
	(* This 'add_msg' invocation was done in a callback. This may interfer
	 * with other queue modifications.
	 *)
	Q.add trans deferred_additions;
      end
      else begin
	let n = Queue.length write_queue in
	if urgent && n > 0 then (
	  (* Insert [trans] at the ealierst possible place *)
	  Q.add_at (n-1) trans write_queue;
	  Q.add_at (n-1) trans read_queue;
	)
	else (
	  Q.add trans write_queue;
	  Q.add trans read_queue;
	);

        (* If there is currently no event group, we are detached from the
         * event system (i.e. not receiving events). Attach again.
         *)
	if group = None then self # attach_to_esys;

        (* Update the polling state. *)
	self # maintain_polling;
      end;

      trans


    method leave_critical_section : unit =
      (* Move the entries from 'deferred_additions' to the real queues. *)

      Q.iter
	(fun trans ->
	   Q.add trans write_queue;
	   Q.add trans read_queue;
	)
	deferred_additions;

      if Q.length deferred_additions > 0 then begin
	Q.clear deferred_additions;

        (* If there is currently no event group, we are detached from the
	 * event system (i.e. not receiving events). Attach again.
	 *)
	if group = None then self # attach_to_esys;

        (* Update the polling state. *)
	self # maintain_polling;

      end
 

    method private add_again m_trans =
      (* add 'm_trans' again to the read and write queues (as a consequence
       * of authorization)
       *)
      let m = m_trans # message in
      let f_done = m_trans # f_done in
      self # add_msg ~critical:true true m f_done


    method set_options p =
      options <- p


    method private attach_to_esys =
      assert (group = None);
      assert (io#socket_state = Down);

      let g = Unixqueue.new_group esys in
      group <- Some g;

      connecting <- None;
      sending_first_message <- true;
      done_first_message <- false;
      close_connection <- false;
      polling_wr <- false;
      critical_section <- false;

      (*
      inhibit_pipelining_byserver <- false;
       -- never reset once it is [true]! Reason: Failed connections are often
          caused by missing pipelining capabilities of the server (e.g. IIS)
       *)

      (* Now check how to get a socket connection: *)
      
      let timeout_value = options.connection_timeout in
      
      options.resolver
	esys
	peer_host
	(function 
	   | None ->
	       (* Very early error! *)
	       ( match group with
		     Some g ->
		       Unixqueue.clear esys g;         (* clean up esys *)
		       group <- None
		   | None -> ()
	       );
	       let err = Name_resolution_error peer_host in
	       self # cleanup_on_eof ~force_total_failure:true err;
	       counters.crashed_connections <-
		 counters.crashed_connections + 1;
	       self # leave_critical_section

	   | Some addr ->
	       ( let peer = Unix.ADDR_INET(addr, peer_port) in
		 try
		   let fd = conn_cache # find_inactive_connection peer in
		   connect_time <- 0.0;
		   conn_cache # set_connection_state fd (`Active conn_owner);
		   io <- new io_buffer options conn_cache fd Up_rw;
		   Unixqueue.add_resource esys g (Unixqueue.Wait_in fd, 
						  timeout_value);
		   Unixqueue.add_handler esys g (self # handler);
		 with
		     Not_found ->
		       (* No inactive connection found. Connect: *)
		       Unixqueue.once 
			 esys
			 g
			 connect_pause
			 (fun () -> 
			    let g1 = Unixqueue.new_group esys in
			    connect_pause <- 0.0;

			    if options.verbose_connection then
			      prerr_endline ("HTTP connection: Connecting to " ^ 
					       peer_host);
			    let eng =
			      Uq_engines.connector 
				(`Socket(`Sock_inet(Unix.SOCK_STREAM,
						    addr,
						    peer_port),
					 Uq_engines.default_connect_options))
				esys in

			    connect_started <- Unix.gettimeofday();
			    connecting <- Some eng;

			    Uq_engines.when_state
			      ~is_done:(function
					  | `Socket(s,_) ->
					      Unixqueue.clear esys g1;
					      self # connected g s
					  | _ -> assert false
				       )
			      ~is_error:(function
					   | err ->
					       Unixqueue.clear esys g1;
					       self # connect_error g err
					)
			      ~is_aborted:(fun () -> 
					     Unixqueue.clear esys g1;
					     connecting <- None
					  )
			      eng;
			    Unixqueue.once esys g1 timeout_value eng#abort
			 )
	       )
	)


    method private connected g s =
      let t1 = Unix.gettimeofday() in
      connect_time <- t1 -. connect_started;
      connecting <- None;

      if options.verbose_connection then
	prerr_endline "HTTP connection: Connected!";

      options.configure_socket s;

      conn_cache # set_connection_state s (`Active conn_owner);
      io <- new io_buffer options conn_cache s Up_rw;

      let timeout_value = options.connection_timeout in
      Unixqueue.add_resource esys g (Unixqueue.Wait_in s, timeout_value);
      Unixqueue.add_handler esys g (self # handler);
      self # maintain_polling;


    method private connect_error g err =
      connecting <- None;
      (* We cannot call abort_connection, because the
       * state is not fully initialized. So clean up everything
       * manually.
       *)
      if options.verbose_connection then (
	match err with
	  | Unix.Unix_error(e,_,_) ->
	      prerr_endline("HTTP connection: Cannot connect: Unix error " ^
			      Unix.error_message e)
	  | _ ->
	      prerr_endline("HTTP connection: Cannot connect: Exception " ^ 
			      Netexn.to_string err)
      );
      Unixqueue.clear esys g;         (* clean up esys *)
      group <- None;
      (* Continue with the regular error path: *)
      counters.crashed_connections <- 
	counters.crashed_connections + 1;
      self # cleanup_on_eof ~force_total_failure:true err;
      self # leave_critical_section
	

    method private maintain_polling =

      (* If one of the following conditions is true, we need not to poll
       * the write side of the socket:
       * - The write_queue is empty but the read_queue not
       * - The difference between the read and the write queue is too big
       * - We wait for the reply of the first request send to a server
       * - The write side of the socket is closed
       *)

      if group <> None && io # socket_state <> Down then (
	let timeout_value = options.connection_timeout in
	
	let actual_max_drift =
	  if inhibit_pipelining_byserver then 0 else
	    match options.synchronization with
		Pipeline drift -> min drift max_pipeline
	      | _              -> 0
		  (* 0: Allow no drift if pipelining is not allowed *)
	in
	
	let have_requests =
	  Q.length read_queue - Q.length write_queue <= actual_max_drift
	  && Q.length write_queue > 0 
	  && (done_first_message || sending_first_message) in

	(* Note: sending_first_message is true while the first request is sent.
         * done_first_message becomes true when the response arrived. In the
         * meantime both variables are false, and nothing is sent.
         *)
	
	let do_close_output =
	  Q.length read_queue = 0 && Q.length write_queue = 0 in
	(* If the socket is still up, we must send EOF. Normally, this 
         * should have happened already, just to be sure we check this here.
	 *)

	let waiting_for_handshake =
	  Q.length read_queue > 0 && 
	    (Q.peek read_queue) # state = Handshake in

	let do_poll_wr =
	  io#socket_state = Up_rw 
	  && ( ( have_requests && not waiting_for_handshake) ||
	       do_close_output ) in
	
	if do_poll_wr && not polling_wr then (
	  let g = match group with
	      Some x -> x
	    | None -> assert false
	  in
	  Unixqueue.add_resource esys g (Unixqueue.Wait_out io#socket, 
					 timeout_value
					);
	);
	
	if not do_poll_wr && polling_wr then (
	  let g = match group with
	      Some x -> x
	    | None -> assert false
	  in
	  Unixqueue.remove_resource esys g (Unixqueue.Wait_out io#socket);
	);
	
	polling_wr <- do_poll_wr;
      )

      (* On the other hand, all of the following conditions must be true
       * to enable polling again:
       * - The write_queue is not empty, or
       *   both the write_queue and the read_queue are empty !!!CHECK!!!
       * - The difference between the read and the write queue is small enough
       * - We send the first request to a server, or do pipelining
       * - The write side of the socket is open
       * - pe_waiting_for_status is false.
       * - waiting_for_status100 is not `Waiting
       *)



    method private clear_timeout g =
      Unixqueue.clear esys g;
      timeout_groups <- List.filter (fun x -> x <> g) timeout_groups;


    method private abort_connection =
      (* This method is called when the connection is in an errorneous
       * state, and the protocol handler decides to give it up.
       *)
      ( match connecting with
	  | None -> ()
	  | Some eng -> eng # abort()
      );
      if io # socket_state <> Down then (
	match group with
	  | Some g -> 
	       if options.verbose_connection then 
		 prerr_endline "HTTP connection: Shutdown!";
	      begin match io#socket_state with
		  Down -> ()
		| Up_r -> 
		    io # close()
		| Up_rw ->
		    io # release()    (* hope this is right *)
	      end;
	      Unixqueue.clear esys g;
	      polling_wr <- false;
	      group <- None;
	      List.iter (Unixqueue.clear esys) timeout_groups;
	      timeout_groups <- []
          | None -> 
	      ()
      )


    method private handler _ _ ev =
      let g = match group with
	  Some x -> x
	| None -> 
	    (* This is possible while shutting down the socket *)
	    raise Equeue.Reject
      in
      match ev with
	  Unixqueue.Input_arrived (g0,fd0) ->
	    if g0 = g then self # handle_input else raise Equeue.Reject
	| Unixqueue.Output_readiness (g0,fd0) ->
	    if g0 = g then self # handle_output else raise Equeue.Reject
	| Unixqueue.Timeout (g0, _) ->
	    if g0 = g then self # handle_timeout else raise Equeue.Reject
	| _ ->
	    raise Equeue.Reject

    (**********************************************************************)
    (***                    THE TIMEOUT HANDLER                         ***)
    (**********************************************************************)

    method private handle_timeout =
      (* No network packet arrived for a period of time.
       * May only happen when a connection is already established
       *)
      io # set_timeout;
      self # handle_input;   (* timeout is similar to EOF *)


    (**********************************************************************)
    (***                     THE INPUT HANDLER                          ***)
    (**********************************************************************)

    method private handle_input =
      (* Data have arrived on the 'socket'. First we receive as much as we
       * can; then the data are interpreted as sequence of messages.
       * This method is also called when the connection times out. In
       * this case [io # timeout] is set.
       *)
      
      (* Ignore this event if the socket is Down (this may happen
       * if the input side is closed while there are still input
       * events in the queue):
       *)
      if io#socket_state = Down then
	raise Equeue.Reject;

      if options.verbose_connection then 
	prerr_endline "HTTP connection: Input event!";

      let _g = match group with
	  Some x -> x
	| None -> assert false
      in

      let end_of_queueing = ref false in
      (* 'end_of_queueing': stores whether there was an EOF or not *)


      (************ ACCEPT THE RECEIVED OCTETS ************)

      if not io # timeout then
	begin try
	  io # unix_read();
	with
	    Unix.Unix_error(Unix.EAGAIN,_,_) ->
	      ();  (* Ignore! *)
	  | Unix.Unix_error(Unix.ECONNRESET,_,_) ->
	      if options.verbose_connection then
		prerr_endline("HTTP connection: Connection reset by peer");
	      self # abort_connection;
	      end_of_queueing := true;
	      counters.crashed_connections <-
		counters.crashed_connections + 1;
	  | Unix.Unix_error(e,a,b) as err ->
	      if options.verbose_connection then
		prerr_endline("HTTP connection: Unix error " ^
			      Unix.error_message e);
	      counters.crashed_connections <- 
		counters.crashed_connections + 1;
	      self # abort_connection;
	      end_of_queueing := true;
	      (* This exception is reported to the message currently being read
               * only.
               *)
	      if Q.length read_queue > 0 then (
		let t = Q.peek read_queue in
		t # message # private_api # set_error_exception err
	      )
	end;

      if io # in_eof || io # timeout then begin
	(* shutdown the connection, and clean up the event system: *)
	if io # in_eof then (
	  if options.verbose_connection	then
	    prerr_endline "HTTP connection: Got EOF!";
	  counters.server_eof_connections <- 
	    counters.server_eof_connections + 1
	);
	if io # timeout then (
	  if options.verbose_connection then 
	    prerr_endline "HTTP connection: Connection timeout!";
	  counters.timed_out_connections <- 
	    counters.timed_out_connections + 1;
	);
	self # abort_connection;
	end_of_queueing := true
      end;


      (************ TRY TO INTERPRET THE OCTETS AS MESSAGES **************)

      (* The [read_loop] parses the data in [io] and fills the 
       * [read_queue]. It may raise the excepions:
       * - Q.Empty: No message is expected, but there is data to
       *   parse (or the connection is at EOF)
       *)

      let rec read_loop() =
	if io # has_unparsed_data then begin
	  let this = Q.peek read_queue in    (* may raise Q.Empty *)

	  (*** Process 'this' ***)

	  this # receive io;
	  (* Parse received data for this message. Set this#state. *)

	  ( match this # state with
	      | Unprocessed ->
		  (* We get response data before we even tried to send
                   * the request. We allow this for pragmatic reasons.
                   *)
		  if options.verbose_connection then (
		    if io # has_unparsed_bytes then
		      prerr_endline "HTTP connection: Got data of spontaneous response";
		    if io # in_eof then
		      prerr_endline "HTTP connection: premature EOF";
		  );
		  ()

	      | Sending_hdr ->
		  (* We get response data before we finished
                   * the request. We allow this for pragmatic reasons.
                   *)
		  assert (try Q.peek write_queue == this
			  with Q.Empty -> false);
		  if options.verbose_connection then (
		    if io # has_unparsed_bytes then
		      prerr_endline "HTTP connection: Got data of premature response";
		    if io # in_eof then
		      prerr_endline "HTTP connection: premature EOF"
		  );
		  ()

	      | Handshake 
	      | Sending_body ->
		  (* This is perfectly legal: We have sent the request header,
                   * and the server may directly reply with whatever.
                   *)
		  assert (try Q.peek write_queue == this
			  with Q.Empty -> false);
		  ()

	      | Sent_request ->
		  (* Somewhere in the middle of the response... *)
		  ()

	      | Complete ->
		  (* The response has been received, and the connection
                   * is still usable
                   *)
		  ignore (Q.take read_queue);
		  if Q.length write_queue >= 1 &&
		    Q.peek write_queue == this then
		      ignore (Q.take write_queue);

		  (* Initialize for next request/response: *)

		  let able_to_pipeline = 
		    this # indicate_pipelining in
		  (* able_to_pipeline: is true if we assume that the server
		   * is HTTP/1.1-compliant and thus is able to manage pipelined
		   * connections.
		   * Update first 'close_connection': This variable becomes
		   * true if the connection is not assumed to be pipelined
		   * which forces that the CLIENT closes the connection 
		   * immediately (see the code in the output handler).
		   *)

		  let only_sequential_persistency =
		    not able_to_pipeline && 
		    this # indicate_sequential_persistency in
		  (* only_sequential_persistency: is true if the connection is
		   * HTTP/1.0, and the server indicated a persistent connection.
		   * In this case, pipelining is disabled.
		   *)

		  if only_sequential_persistency then begin
		    (* 'close_connection': not set.
		     *)
		    if options.verbose_connection then 
		      prerr_endline "HTTP connection: using HTTP/1.0 style persistent connection";
		    inhibit_pipelining_byserver <- true;
		  end
		  else
		    close_connection  <- close_connection  || not able_to_pipeline;

		  if close_connection || options.inhibit_persistency then (
		    self # abort_connection;
		    end_of_queueing := true;
		    (* We do not count this event - it is regular! *)
		  );

		  (* Remember that the first request/reply round is over: *)
		  done_first_message <- true;

		  (* postprocess 'this' (may raise exceptions! (callbacks)) *)
		  self # postprocess_complete_message this;

		  read_loop()

	      | Complete_broken ->
		  (* The response has been received, but the connection
                   * is in a problematic state, and must be aborted.
                   *)

		  if options.verbose_connection then
		    prerr_endline "HTTP connection: Aborting the invalidated connection";

		  self # abort_connection;
		  end_of_queueing := true;
		  counters.crashed_connections <-
		    counters.crashed_connections + 1;

		  (* If the response has a proper status, we can remove it
                   * from the queue. Otherwise leave it on the queue, so
                   * it can be scheduled again.
                   *)
		  ( match this # message # status with
		      | `Unserved
		      | `Http_protocol_error _ ->
			  ()    (* leave it *)
		      | _ ->
			  (* Remove the message from the queues: We must 
                           * also check write_queue, because we have the
                           * invariant that write_queue is a suffix of
                           * read_queue.
			   *)
			  ignore (Q.take read_queue);
			  if Q.length write_queue >= 1 &&
			     Q.peek write_queue == this then
			       ignore (Q.take write_queue);
			  (* postprocess 'this' (Exceptions! (callbacks)) *)
			  self # postprocess_complete_message this;
		  );

		  (* Do not continue [read_loop] *)
	      | Broken ->
		  (* Simply stop here. *)
		  if options.verbose_connection then
		    prerr_endline "HTTP connection: Aborting the errorneuos connection";
		  self # abort_connection;
		  end_of_queueing := true;
		  counters.crashed_connections <-
		    counters.crashed_connections + 1;
	  );
	end
      in         (* of "let rec read_loop() = " *)

      begin try
	(* Start the interpretation formulated in 'read_loop' and catch
	 * exceptions.
	 *)
	read_loop();
      with
	| Q.Empty ->
	    (* Either we hit EOF, or there are additional bytes but no
             * message is expected:
             *)
	    if io # has_unparsed_bytes then begin
	      (* No more responses expected, but still octets to interpret.
	       * This is a protocol error, too.
	       *)
	      if options.verbose_connection then
		prerr_endline "HTTP connection: Extra octets -- aborting connection";
	      self # abort_connection;
	      end_of_queueing := true;
	      counters.crashed_connections <-
		counters.crashed_connections + 1;
	    end
      end;

      (************** CLOSE THE CONNECTION IF NECESSARY, ****************)
      (************** AND PREPARE RECONNECTION           ****************)

      if !end_of_queueing then begin
	assert (group = None);
	self # cleanup_on_eof (Bad_message "Incomplete or missing response")
      end;

      (*************** UPDATE THE POLLING STATE **************)

      (* If there were 'add' invocations from callbacks, move these additions
       * to the real queues now.
       *)
      self # leave_critical_section; 

      (* Update polling state: *)
      self # maintain_polling;


    (************** CLOSE THE CONNECTION IF NECESSARY, ****************)
    (************** AND PREPARE RECONNECTION           ****************)

    method private cleanup_on_eof ?(force_total_failure=false) err : unit =
      assert (group = None);

      (* If the socket is closed, it is necessary to check whether all 
       * requests sent so far have got their replies. 
       * Cases:
       * - write_queue and read_queue are empty: all is done.
       * - write_queue and read_queue have the same number of elements:
       *   reconnect
       * - else: some replies are missing. The requests are NOT tried again
       *   by default because the operations might not be idempotent. 
       *   The messages carry a flag with them indicating whether reconnection
       *   is allowed or not.
       * It is not possible that the write_queue is longer than the read_queue.
       *)

      (* First check if the connection was a total failure, i.e. if not
       * even a status line was received. In this case
       * increase the counter for totally failed connections. If the
       * counter exceeeds a limit, all messages on the queues are discarded.
       *)
	
      if force_total_failure || not io#status_seen then begin
	(* It was a total failure. *)
	totally_failed_connections <- totally_failed_connections + 1;
	if options.verbose_connection then
	  prerr_endline "HTTP connection: total failure";
	
	if totally_failed_connections >= options.maximum_connection_failures then
	  begin
	    (* Set the error exception of all remaining messages, and
	     * clear the queues.
	     *)
	    self # clear_read_queue err;
	    self # clear_write_queue ();
	    
	    (* Reset state variables *)

	    totally_failed_connections <- 0;

	    (* Simply continue with the following piece of code, which will
	     * no nothing.
	     *)
	  end
	  
      end
      else (
	(* This connection breaks the series of total failures (if there 
	 * was such a series.
	 *)
	totally_failed_connections <- 0;

	(* Turn pipelining off to be on the safe side: *)
	inhibit_pipelining_byserver <- true;
      );

      (* Now examine the queues, and decide what to do. *)
	
      let n_read  = Q.length read_queue in
      let n_write = Q.length write_queue in
      if n_read > 0 || n_write > 0 then begin
	assert (n_read >= n_write);
	assert (group = None);
	
	connect_pause <- 1.0;
	
	(* ASSERTION:
	 *     read_queue  = q1 . q2
	 *     write_queue =      q2
	 * i.e. the extra elements of the read queue are at the beginning
	 * of the read queue.
	 * 
	 * PLAN: Make that
	 *     read_queue  = q2 . q1'
	 *     write_queue = q2 . q1'
	 * where q1' are the elements of q1 for which a reconnection is
	 * allowed. Reset the error exception for these elements.
	 * For the other elements (q1 \ q1') leave the error
	 * exception as it is, but change every No_exception into 
	 * No_reply.
         *
         * Note: q1 = empty is possible.
	 *)
	for i = 1 to n_read - n_write do
	  let m_trans = Q.take read_queue in
	  let m = m_trans # message in
	  (* Increase error counter *)
	  let e = m # private_api # get_error_counter in
	  m # private_api # set_error_counter (e+1);
	  (* Test: Are reconnections allowed? *)
	  if e+1 <= options.maximum_message_errors then begin
	    let do_reconnect =
	      match m # get_reconnect_mode with
		| Send_again -> true
		| Request_fails -> false
		| Send_again_if_idem -> m # is_idempotent
		| Inquire f ->
		    (* Ask the function 'f' whether to reconnect or not: *)
		    begin 
		      try f m    (* returns true or false *)
		      with
			  (* The invocation of 'f' may raise an exception.
			   * It is printed to stderr (there is no other
			   * way to report it).
			   *)
			  x ->
			    prerr_string "Exception caught in Http_client: ";
			    prerr_endline (Netexn.to_string x);
			    false
		    end
	    in
	    if do_reconnect then begin
	      (* Ok, this request is tried again. *)
	      m_trans # init();               (* Reinit call *)
	      Q.add m_trans write_queue;  (* ... add it to the queue of open *)
	      Q.add m_trans read_queue;   (* ...to the unfinished requests. *)
	    end
	    else begin
	      (* Drop this message because reconnection is not allowed *)
	      (* If status of the message is unset, change it into No_reply. 
	       *)
	      ( match m # status with
		  | `Unserved ->
		      m # private_api # set_error_exception No_reply;
		  | _ -> 
		      ()
	      );
	      (* We do not reconnect, so postprocess now. *)
	      self # critical_postprocessing m_trans;
	    end
	  end
	  else (
	    (* drop this message because of too many errors *)
	    (* We do not reconnect, so postprocess now. *)
	    ( match m # status with
		| `Unserved ->
		    m # private_api # set_error_exception No_reply;
		| _ -> 
		    ()
	    );
	    self # critical_postprocessing m_trans;
	  )
	done;

	let n_read  = Q.length read_queue in
	let n_write = Q.length write_queue in
	assert (n_read = n_write);
	  
	(* It is now possible that n_read = n_write = 0, in which case
	 * no more is to do, or that there are remaining requests.
	 *)

	if n_write > 0 then begin
	  assert (Q.peek read_queue == Q.peek write_queue);
	  (* Force reinitialisation of all queue elements: *)
	  Q.iter (fun m -> m#init()) read_queue;
	  (* Process the queues: *)
	  self # attach_to_esys;
	end
	else (
	  if options.verbose_connection then
	    prerr_endline "HTTP connection: Nothing left to do";
	  counters.failed_connections <- counters.failed_connections + 1
	)

      end else (
	(* n_read = 0 && n_write = 0 *)
	counters.successful_connections <- counters.successful_connections + 1
      )
      

    method clear_read_queue err =
      let q' = Q.create() in
      Q.transfer read_queue q';
      Q.iter 
	(fun m ->
	   ( match m # message # status with
	       | `Unserved ->
		   m # message # private_api # set_error_exception err;
	       | _ ->
		   ()
	   );
	)
	q';

      let do_callbacks() =
	Q.iter
	  (fun m ->
	     self # critical_postprocessing m;     (* because m is dropped *)
	  )
	  q'
      in

      if critical_section then
	let g = Unixqueue.new_group esys in
	Unixqueue.once esys g 0.0 do_callbacks
      else
	do_callbacks()


    method clear_write_queue() =
      Q.iter (fun m -> m # close_body()) write_queue;
      Q.clear write_queue


    method critical_postprocessing m =
      critical_section <- true;
      try
	m # postprocess;
	critical_section <- false
      with
	  any ->
	    critical_section <- false;
	    raise any

    (**********************************************************************)
    (***                     THE OUTPUT HANDLER                         ***)
    (**********************************************************************)

    method private handle_output =

      (* Ignore this event if the socket is not Up_rw (this may happen
       * if the output side is closed while there are still output
       * events in the queue):
       *)
      if io#socket_state <> Up_rw then
	raise Equeue.Reject;

      if options.verbose_connection then 
	prerr_endline "HTTP connection: Output event!";

      let _g = match group with
	  Some x -> x
	| None -> assert false
      in

      (* Leave the write_loop by exceptions:
       * - Q.Empty: No more request to write
       *)

      let rec write_loop () =
	let this = Q.peek write_queue in  (* or Q.Empty *)
	begin match this # state with
	    (Unprocessed | Sending_hdr | Sending_body) ->

	      let rh = this # message # request_header `Effective in

	      (* if no_persistency, set 'connection: close' *)

	      if options.inhibit_persistency && this # state = Unprocessed then
		rh # update_field "connection" "close";

	      (* If a "100 Continue" handshake is requested,
	       * transmit only the header of the request, and set
	       * waiting_for_status100. Furthermore, add a timeout
	       * handler that resets this variable after some time.
	       *)

	      let do_handshake =
		try
		  (* Proper parsing not required, because [Expect] is
                   * set by the user.
                   * [continue]: Already seen status 100
                   *)
		  not (this # message # private_api # continue) &&
		  String.lowercase(rh # field "expect") = "100-continue"
		with
		    Not_found -> false 
	      in

	      ( try
		  this # send io do_handshake;

		  (* If a handshake is requested: set the variable and the
		     * timer.
		   *)
		  if do_handshake  &&  this # state = Handshake
		  then (
		    let timeout = options.handshake_timeout in
		    let tm = Unixqueue.new_group esys in
		    timeout_groups <- tm :: timeout_groups;
		    Unixqueue.once
		      esys tm timeout
		      (fun () ->
			 ( try
			     let this' = Q.peek write_queue in
			     let still_waiting =
			       this == this' && this # state = Handshake in
			     if still_waiting then (
			       this # handshake_timeout();
			       self # maintain_polling;
			     );
			   with Q.Empty -> ()
			 );
			 self # clear_timeout tm;
		      );
		    if options.verbose_connection then
		      prerr_endline "HTTP connection: waiting for 100 CONTINUE";
		  );
		  
		with
		    Unix.Unix_error(Unix.EPIPE,_,_) ->
		      (* Broken pipe: This can happen if the server decides
		       * to close the connection in the same moment when the
		       * client wants to send another request after the 
		       * connection has been idle for a period of time.
		       * Reaction: Close our side of the connection, too,
		       * and open a new connection. The current request will
		       * be silently resent because it is sure that the
		       * request was not received completely; it does not
		       * matter whether the request is idempotent or not.
		       *
		       * Broken pipes are very unlikely because this must 
		       * happen between the 'select' and 'write' system calls.
                       * The [read] syscall will get ECONNRESET, so we do
                       * nothing here.
		       *)
		      if options.verbose_connection then
			prerr_endline "HTTP connection: broken pipe";
		      ()

		  | Unix.Unix_error(Unix.EAGAIN,_,_) ->
		      ()
		      
		  | Unix.Unix_error(e,a,b) ->
		      if options.verbose_connection then
			prerr_endline("HTTP connection: Unix error " ^
				      Unix.error_message e);
		      (* Hope the input handler will do the right thing *)
		      ()
	      );
	      if sending_first_message && this # state = Sent_request then
		sending_first_message <- false;
	      if this # state = Sent_request then
		ignore (Q.take write_queue);
	      (* Do not call write_loop: Otherwise other handlers would
               * not have any chance to run
               *)
	  | Handshake ->
	      (* Should normally not happen. *)
	      ()
	  | (Sent_request | Complete | Complete_broken) ->
	      (* If Complete_broken, we also have Up_r. *)
	      ignore (Q.take write_queue);
	      if io # socket_state = Up_rw then
		write_loop()        (* continue with the next message *)
	  | Broken ->
	      (* This case is fully handled by the input handler *)
	      ()
	end
      in

      begin try
	write_loop()
      with
	  Q.Empty -> ()
      end;

      (* Release the connection if the queues have become empty *)

      if (Q.length write_queue = 0 && Q.length read_queue = 0) then (
	self # abort_connection;
	counters.successful_connections <- counters.successful_connections + 1
      );

      self # maintain_polling;


    (**********************************************************************)
    (***                     AUTHENTICATION                             ***)
    (**********************************************************************)

    method private postprocess_complete_message msg_trans =
      (* This method is invoked for every complete reply. The following
       * cases are handled at this stage of processing:
       *
       * - Status code 407: The proxy demands authorization. If the request
       *   already contains credentials for the proxy, this status code
       *   isn't handled here. Otherwise, the request is added again onto
       *   the queue, and a flag ('proxy_credentials_required') is set which 
       *   forces that the proxy credentials must be added for every new 
       *   request.
       *   Note: The necessary authentication header fields are added in
       *   the 'add' method.
       *
       * - Status code 401: XXX
       *
       * All other status codes are not handled here. Note that it is not
       * possible to react on the redirection codes because this object
       * represents the connection to exactly one server.
       * As default behaviour, the method 'postprocess' of the 
       * transmitter object is invoked; this method incorporates
       * all the intelligence not coded here.
       *)

      let default_action() =
	self # critical_postprocessing msg_trans;
      in

      let msg = msg_trans # message in
      let code = msg # private_api # response_code in
      let req_hdr = msg # request_header `Effective in
      let _resp_hdr = msg # private_api # response_header in
      match code with
	| 407 ->
	    (* --------- Proxy authorization required: ---------- *)
	    if
	      try 
		let _ = req_hdr # field "proxy-authorization" in
		if options.verbose_status then
		  prerr_endline "HTTP auth: proxy authentication required again";
		false
	      with Not_found -> true
	    then begin
	      (* The request did not contain the "proxy-authorization" header.
	       * Enable proxy authentication if there is a user/password pair.
	       * Otherwise, do the default action.
	       *)
	      if peer_is_proxy then begin
		if options.verbose_status then
		  prerr_endline "HTTP auth: proxy authentication required";
		if proxy_user <> "" then begin
		  (* We have a user/password pair: Enable proxy authentication
		   * and add 'msg' again to the queue of messages to be
		   * processed.
		   * Note: Following the HTTP protocol standard, the header
		   * of the response contains a 'proxy-authenticate' field
		   * with the authentication method and the realm. This is
		   * not supported; the method is always "basic" and realms
		   * are not distinguished.
		   *)
		  if not proxy_credentials_required then begin
		    proxy_credentials_required <- true;
		    proxy_cookie <- "";
		  end;
		  if options.verbose_status then
		    prerr_endline "HTTP auth: proxy credentials added";
		  ignore (self # add_again msg_trans);
		end
		else (
		  (* No user/password pair: We cannot authorize ourselves. *)
		  if options.verbose_status then
		    prerr_endline "HTTP auth: user/password missing";
		  default_action()
		)
	      end
	      else (
		(* The server was not contacted as a proxy, but it demanded
		 * proxy authorization. Regard this as an intrusion.
		 *)
		if options.verbose_status then
		  prerr_endline "HTTP auth: intrusion by proxy authentication";
		default_action()
	      )
	    end
	    else 
	      (* The request did already contain "proxy-authenticate". *)
	      default_action()
	      
	| 401 ->
	    (* -------- Content server authorization required: ---------- *)
	    (* Unless a previous authentication attempt failed, just create
             * a new session, and repeat the request.
             *)
	    let try_again =
	      match msg # private_api # auth_state with
		| `None
		| `In_advance _ -> 
		    true
		| `In_reply sess ->
		    (* A previous attempt failed. *)
		    let continue = sess # invalidate msg in
		    if not continue then auth_cache # tell_failed_session sess;
		    continue
	    in
	    if try_again then (
	      match auth_cache # create_session msg with
		| None ->
		    (* Authentication failed immediately *)
		    default_action()
		| Some sess ->
		    (* Remember the new session: *)
		    msg # private_api # set_auth_state (`In_reply sess);
		    ignore(self # add_again msg_trans)
	    )
	    else
	      default_action()
	| n when n >= 200 && n < 400 ->
	    (* Check whether authentication was successful *)
	    ( match msg # private_api # auth_state with
		| `None -> ()
		| `In_advance _ -> ()
		| `In_reply session ->
		    auth_cache # tell_successful_session session
	    );
	    default_action()
	| _ ->
	    default_action()


    (**********************************************************************)
    (***                   RESET COMPLETELY                             ***)
    (**********************************************************************)

    (* [reset] is called by the pipeline object to shutdown any processing *)

    method reset =
      (* Close the socket; clear the Unixqueue *)
      self # abort_connection;

      (* Discard all messages on the queues. *)
      self # clear_read_queue No_reply;
      self # clear_write_queue();

      (* Reset state variables *)

      totally_failed_connections <- 0;

      (* If there were 'add' invocations from callbacks, delete these additions
       * now
       *)
      let q = Queue.create() in
      Queue.iter
	(fun trans ->
	   Queue.push trans q;
	   let m = trans # message in
	   match m # status with
	     | `Unserved ->
		 m # private_api # set_error_exception No_reply;
	     | _ -> 
		 ()
	)
	deferred_additions;
      Queue.clear deferred_additions;

      (* Because [reset] might be called from a critical section, defer
       * callbacks (see also [clear_read_queue])
       *)
      let g = Unixqueue.new_group esys in
      Unixqueue.once
	esys g 0.0
	(fun () ->
	   Queue.iter
	     (fun trans ->
		self # critical_postprocessing trans
	     )
	     q
	)

  end
;;


(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(***                                                                ***)
(***                 THE PIPELINE INTERFACE                         ***)
(***                                                                ***)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

(* The following class, 'pipeline' defines the interface for the outside
 * world.
 *)

class pipeline =
  object (self)
    val mutable esys = Unixqueue.create_unix_event_system()

    val mutable proxy = ""
    val mutable proxy_port = 80
    val mutable proxy_auth = false
    val mutable proxy_user = ""
    val mutable proxy_password = ""

    val mutable no_proxy_for = []

    val mutable connections = Hashtbl.create 10

    val mutable open_messages = 0

    val mutable open_connections = 0

    val mutable options =
	    { (* Default values: *)
	      synchronization = Pipeline 5;
	      maximum_connection_failures = 2;
	      maximum_message_errors = 2;
	      inhibit_persistency = false;
	      connection_timeout = 300.0;
	      number_of_parallel_connections = 2;
	      maximum_redirections = 10;
	      handshake_timeout = 1.0;
	      resolver = sync_resolver;
	      configure_socket = (fun _ -> ());
	      verbose_status = false;
	      verbose_request_header = false;
	      verbose_response_header = false;
	      verbose_request_contents = false;
	      verbose_response_contents = false;
	      verbose_connection = false;
	    }

    val auth_cache = new auth_cache

    val mutable conn_cache = create_restrictive_cache()

    val counters =
      { new_connections = 0;
	timed_out_connections = 0;
	crashed_connections = 0;
	server_eof_connections = 0;
	successful_connections = 0;
	failed_connections = 0;
      }

    method event_system = esys

    method set_event_system new_esys =
      esys <- new_esys;
      Hashtbl.clear connections;

    method connection_cache = conn_cache

    method set_connection_cache cc = conn_cache <- cc

    method add_authentication_method ( m : auth_method ) =
      self # add_auth_handler (m # as_auth_handler)

    method add_auth_handler (h : auth_handler) =
      auth_cache # add_auth_handler h

    method set_proxy the_proxy the_port =
      (* proxy="": disables proxy *)
      proxy       <- the_proxy;
      proxy_port  <- the_port;
      ()

    method set_proxy_auth user passwd =
      (* sets 'user' and 'password' if demanded by a proxy *)
      proxy_auth     <- user <> "";
      proxy_user     <- user;
      proxy_password <- passwd


    method avoid_proxy_for l =
      (* l: List of hosts or domains *)
      no_proxy_for <- l


    method set_proxy_from_environment() =
      (* Is the environment variable "http_proxy" set? *)
      let http_proxy =
	try Sys.getenv "http_proxy" with Not_found -> "" in
      begin try
	let (user,password,host,port,path) = parse_http_url http_proxy in
	self # set_proxy (Netencoding.Url.decode host) port;
	match user with
	  Some user_s ->
	    begin match password with
	      Some password_s ->
		self # set_proxy_auth (Netencoding.Url.decode user_s) (Netencoding.Url.decode password_s)
	    | None -> ()
	    end
	| None -> ()
      with
	Not_found -> ()
      end;

      (* Is the environment variable "no_proxy" set? *)
      let no_proxy =
	try Sys.getenv "no_proxy" with Not_found -> "" in
      let no_proxy_list =
	split_words_by_commas no_proxy in
      self # avoid_proxy_for no_proxy_list;


    method reset () =
      (* deletes all pending requests; closes connection *)

      (* Reset all connections: *)
      Hashtbl.iter
	(fun _ cl ->
	   List.iter
	     (fun c ->
		c # reset)
	     !cl)
	connections;

(*
   - well, this _should_ do nothing
      List.iter
	(fun fd -> 
	   conn_cache # forget_connection fd;
	   Unix.close fd;
	)
	(conn_cache # find_my_connections (self :> < >));
 *)

      self # reset_counters()
      

    method private add_with_callback_no_redirection (request : http_call) f_done =

      let host = request # get_host() in
      let port = request # get_port() in

      let use_proxy = 
	proxy <> "" &&
	request # proxy_enabled &&
	not
          (List.exists
             (fun dom ->
                if dom <> "" &
                   dom.[0] = '.' &
		   String.length host > String.length dom
                then
                  let ld = String.length dom in
                  String.lowercase(String.sub 
                                     host 
                                     (String.length host - ld) 
                                     ld)
                  = String.lowercase dom
                else
                  dom = host)
             no_proxy_for)
      in

      (* find out the effective peer: *)
      let peer, peer's_port =
	if use_proxy then
	  proxy, proxy_port
	else
	  host, port
      in
      
      (* Find out if there is already a connection to this peer: *)

      let conn = 
	let connlist = 
	  try
	    Hashtbl.find connections (peer, peer's_port, use_proxy) 
	  with
	      Not_found ->
		let new_connlist = ref [] in
		Hashtbl.add connections (peer, peer's_port, use_proxy) new_connlist;
		new_connlist
	in
	if List.length !connlist < options.number_of_parallel_connections 
	  then begin
	    let new_conn = new connection
	                     esys
	                     (peer, peer's_port)
			     use_proxy
	                     (proxy_user, proxy_password) 
			     auth_cache
			     conn_cache
			     (self :> < >)
			     counters
			     options in
	    open_connections <- open_connections + 1;
	    counters.new_connections <- counters.new_connections + 1;
	    connlist := new_conn :: !connlist;
	    new_conn
	  end 
	  else begin
	    (* Find the connection with the lowest number of queue entries: *)
	    List.fold_left
	      (fun best_conn a_conn ->
		 if a_conn # length < best_conn # length then
		   a_conn
		 else
		   best_conn)
	      (List.hd !connlist)
	      (List.tl !connlist)
	  end
      in
      
      (* Add the request to the queue of this connection: *)

      conn # add request 
	(fun m ->
	   (* Update 'open_connections', 'connections', and 'open_messages' *)
	   if not conn#active then begin
	     (* Check whether the connection is still in the [connections]
              * hash. It is possible that it is already deleted here.
              *)

	     let connlist =
	       try
		 Hashtbl.find connections (peer, peer's_port, use_proxy);
	       with
		   Not_found -> ref []
	     in
	     if List.exists (fun c -> c == conn) !connlist then (
	       open_connections <- open_connections - 1;
	       connlist := List.filter (fun c -> c != conn) !connlist;
	       if !connlist = [] then
		 Hashtbl.remove connections (peer, peer's_port, use_proxy);
	     )
	   end;
	   self # update_open_messages;
	   (* Do user action: *)
	   f_done m;
	);

      open_messages <- open_messages + 1;

    method private update_open_messages =
      open_messages <- 0;
      Hashtbl.iter
	(fun _ cl ->
	   List.iter
	     (fun c ->
		if c # active then 
		  open_messages <- open_messages + (c # length))
	     !cl)
	connections;


    method add_with_callback (request : http_call) f_done =
      self # add_with_callback_no_redirection
	request
	(fun m ->
	   try
	     let (_,code,_) = m # dest_status() in
	     match code with
		 (301|302) ->
		   (* Simply repeat the request with a different URI *)
		   let do_redirection =
		     match m # get_redirect_mode with
			 Redirect -> true
		       | Do_not_redirect -> false
		       | Redirect_if_idem -> m # is_idempotent
		       | Redirect_inquire f ->
			   (* Ask the function 'f' whether to redirect: *)
			   begin 
			     try f m    (* returns true or false *)
			     with
			     (* The invocation of 'f' may raise an exception.
			      * It is printed to stderr (there is no other
			      * way to report it).
			      *)
				 x ->
				   prerr_string 
				     "Exception caught in Http_client: ";
				   prerr_endline (Netexn.to_string x);
				   false
			   end
		   in

		   if do_redirection then begin
		     (* Maybe the redirection limit is exceeded: *)
		     let rc = m # private_api # get_redir_counter in
		     if rc >= options.maximum_redirections
		     then (
		       m # private_api # set_error_exception Too_many_redirections;
		       f_done m
		     )
		     else (
		       let location = m # assoc_resp_header "location" in
		       (* or raise Not_found *)
		       let location' =
			 if location <> "" && location.[0] = '/' then
			   (* Problem: "Location" header must be absolute due
			    * to RFC specs. Now it is relative (with full path).
			    * Workaround: Interpret relative to old server
			    *)
			   let host = m # get_host() in
			   let port = m # get_port() in
			   let prefix =
			     "http://" ^ host ^ 
			       (if port = 80 then "" else ":" ^ string_of_int port)
			   in
			   prefix ^ location
			 else
			   location in
		       let ok =
			 try
			   m # set_request_uri location';
			   true
			 with
			   | _ ->
			       (* Bad URL! *)
			       let e = URL_syntax_error location' in
			       m # private_api # set_error_exception e;
			       false
		       in
		       if ok then (
			 m # private_api # set_redir_counter (rc+1);
			 m # private_api # set_error_counter 0;
			 self # add_with_callback m f_done
		       )
		       else f_done m
		     )
		   end
		     else f_done m

	       | _ -> 
		   f_done m
	     with
		 (Http_protocol _ | Not_found) -> 
		   f_done m
	)


    method add request =
      self # add_with_callback request (fun _ -> ())

    method run () =
      (* Runs through the requests in the pipeline. If a request can be
       * fulfilled, i.e. the server sends a response, the status of the
       * request is set and the request is removed from the pipeline.
       * If a request cannot be fulfilled (no response, bad response,
       * network error), an exception is raised and the request remains in
       * the pipeline (and is even the head of the pipeline).
       *
       * Exception Broken_connection:
       *  - The server has closed the connection before the full request
       *    could be sent. It is unclear if something happened or not.
       *    The application should figure out the current state and
       *    retry the request.
       *  - Also raised if only parts of the response have been received
       *    and the server closed the connection. This is the same problem.
       *    Note that this can only be detected if a "content-length" has
       *    been sent or "chunked encoding" was chosen. Should normally
       *    work for persistent connections.
       *  - NOT raised if the server forces a "broken pipe" (normally
       *    indicates a serious server problem). The intention of
       *    Broken_connection is that retrying the request will probably
       *    succeed.
       *)

	 Unixqueue.run esys

    method get_options = options

    method set_options p =
      options <- p;
      Hashtbl.iter
	(fun _ cl ->
	   List.iter
	     (fun c ->
		c # set_options p)
	     !cl)
	connections

    method number_of_open_messages = open_messages

    method number_of_open_connections = open_connections

    method connections =
      let l = ref [] in
      Hashtbl.iter
	(fun (peer, port, _) conns ->
	   List.iter
	     (fun conn ->
		l := (peer, port, conn#length) :: !l
	     )
	     !conns
	)
	connections;
      !l

    method cnt_new_connections = counters.new_connections

    method cnt_timed_out_connections = counters.timed_out_connections

    method cnt_crashed_connections = counters.crashed_connections

    method cnt_server_eof_connections = counters.server_eof_connections

    method cnt_successful_connections = counters.successful_connections

    method cnt_failed_connections = counters.failed_connections

    method reset_counters() =
      counters.new_connections <- 0;
      counters.timed_out_connections <- 0;
      counters.crashed_connections <- 0;
      counters.server_eof_connections <- 0;
      counters.successful_connections <- 0;
      counters.failed_connections <- 0;


  end
;;

(**********************************************************************)
(**********************************************************************)
(**********************************************************************)
(***                                                                ***)
(***                 THE CONVENIENCE MODULE                         ***)
(***                                                                ***)
(**********************************************************************)
(**********************************************************************)
(**********************************************************************)

(* This module is intended for beginners and for simple applications
 * of this HTTP implementation.
 *)

let omtp = !Netsys_oothr.provider
let mutex = omtp # create_mutex()

let serialize f arg =
  mutex # lock();
  try
    let r = f arg in
    mutex # unlock();
    r
  with
      err -> mutex # unlock(); raise err
;;
	

module Convenience =
  struct

    let http_trials = ref 3
    let http_user = ref ""
    let http_password = ref ""

    let this_user = ref ""
    let this_password = ref ""

    let conv_verbose = ref false

    class simple_key_handler : key_handler =
    object
      method inquire_key ~domain ~realms ~auth =
	if !this_user <> "" then
	  ( object
	      method user = !this_user
	      method password = !this_password
	      method realm = List.hd realms 
	      method domain = domain
	    end )
	else
	  if !http_user <> "" then
	    ( object
		method user = !http_user
		method password = !http_password
		method realm = List.hd realms 
		method domain = domain
	      end )
	  else
	    raise Not_found
      method invalidate_key (_ : key) = ()
    end


    let auth_basic =
      new basic_auth_handler 
	~enable_auth_in_advance:true (new simple_key_handler)

    let auth_digest =
      new basic_auth_handler 
	~enable_auth_in_advance:true (new simple_key_handler)

    let get_default_pipe() =

      let p = new pipeline in

      p # set_proxy_from_environment();

      (* Add authentication methods: *)
      p # add_auth_handler auth_basic;
      p # add_auth_handler auth_digest;

      (* That's it: *)
      p


    let pipe = lazy (get_default_pipe())
    let pipe_empty = ref true

    let request m =
      serialize
	(fun trials ->
	   let p = Lazy.force pipe in
	   if not !pipe_empty then
	     p # reset();
	   p # add_with_callback m (fun _ -> pipe_empty := true);
	   pipe_empty := false;
	   p # run()
	)

    let prepare_url =
      serialize
	(fun url ->
	   try
	     this_user := "";
    	     let (user,password,host,port,path) = parse_http_url url in
	     begin match user with
		 Some user_s ->
		   this_user := Netencoding.Url.decode user_s;
		   this_password := "";
		   begin match password with
		       Some password_s ->
			 this_password := Netencoding.Url.decode password_s
		     | None -> ()
		   end
	       | None -> ()
	     end;
	     "http://" ^ host ^ ":" ^ string_of_int port ^ path
	   with
	       Not_found -> 
		 url
	)

    let http_get_message url =
      let m = new get (prepare_url url) in
      request m !http_trials;
      m

    let http_get url = (http_get_message url) # get_resp_body()

    let http_head_message url =
      let m = new head (prepare_url url) in
      request m !http_trials;
      m

    let http_post_message url params =
      let m = new post (prepare_url url) params in
      request m 1;
      m

    let http_post url params = (http_post_message url params) # get_resp_body()

    let http_put_message url content =
      let m = new put (prepare_url url) content in
      request m !http_trials;
      m

    let http_put url content = (http_put_message url content) # get_resp_body()

    let http_delete_message url =
      let m = new delete (prepare_url url) in
      request m 1;
      m

    let http_delete url = (http_delete_message url) # get_resp_body()


    let http_verbose =
      serialize
	(fun () ->
	   let p = Lazy.force pipe in
	   let opt = p # get_options in
	   p # set_options
	     { opt with verbose_status = true;
	         verbose_request_header = true;
		 verbose_response_header = true;
		 verbose_request_contents = true;
		 verbose_response_contents = true;
		 verbose_connection = true 
             };
	   conv_verbose := true;
	)
  end
