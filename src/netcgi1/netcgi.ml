(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Netcgi_env ;;
open Netcgi_types ;;
open Netchannels ;;
open Nethttp ;;

let status_line (s:http_status) =
  let code = int_of_http_status s in
  let text = string_of_http_status s in
  string_of_int code ^ " " ^ text


class simple_argument ?(ro=false) name value : cgi_argument =
object(self)
  inherit Netmime.memory_mime_body ~ro value

  val name = name

  method name = 
    name
  method content_type = 
    "text/plain"
  method content_type_params =
    []
  method charset =
    ""
  method filename = 
    None
  method representation = 
    `Simple (self :> simple_message)
end ;;


class mime_argument ?(work_around_backslash_bug = true) 
                    (n : string) (m : Netmime.mime_message) =
  let (m_hdr, `Body m_body) = m in
object (self)
  val name = n
  val hdr = m_hdr
  val body = m_body

  (* The name could also be extracted from the MIME header; it is the "name"
   * parameter of the "content-disposition" field. However, we don't do it
   * here.
   *)

  method name = name

  method value = m_body#value
  method open_value_rd = m_body#open_value_rd
  method ro = m_body#ro
  method store = m_body#store
  method set_value = m_body#set_value
  method open_value_wr = m_body#open_value_wr
  method finalize = m_body#finalize

  method content_type = 
    try fst(m_hdr # content_type()) with Not_found -> "text/plain"

  method content_type_params =
    try snd(m_hdr # content_type()) with Not_found -> []

  method charset =
    let params = self # content_type_params in
    try Mimestring.param_value(List.assoc "charset" params) with Not_found -> ""

  method filename = 
    try
      let s = m_hdr # field "content-disposition" in
      let options = 
	if work_around_backslash_bug then
	  [ Mimestring.No_backslash_escaping ] 
	else
	  []
      in
      let tok, params = Mimestring.scan_value_with_parameters_ep s options in
      Some(Mimestring.param_value(List.assoc "filename" params))
    with
	Not_found ->
	  None

  method representation = 
    (`MIME (m_hdr, `Body m_body) : representation)

end ;;


type operating_type = 
  [ `Direct of string 
  | `Transactional of (cgi_config -> out_obj_channel -> trans_out_obj_channel)
  ]


class no_trans_channel sep 
                       (f_after: unit -> unit)
                       (ch : out_obj_channel) : trans_out_obj_channel =
object
  (* Maybe we should better define a 'delegating_out_obj_channel' and
   * inherit from this class.
   *)
  val sep = sep
  val f_after = f_after
  val ch = ch
  method output =         ch # output
  method really_output =  ch # really_output
  method output_char =    ch # output_char
  method output_string =  ch # output_string
  method output_byte =    ch # output_byte
  method output_buffer =  ch # output_buffer
  method output_channel = ch # output_channel
  method flush =          ch # flush
  method close_out =      ch # close_out
  method pos_out =        ch # pos_out

  method commit_work ()  = ch # flush(); f_after()
  method rollback_work() = ch # output_string sep
end ;;


class on_commit_channel (f_before : unit -> unit) 
                        (f_after: unit -> unit)
                        (ch : trans_out_obj_channel) : trans_out_obj_channel =
object
  (* Maybe we should better define a 'delegating_trans_out_obj_channel' and
   * inherit from this class.
   *)
  val f_before = f_before
  val f_after = f_after
  val ch = ch
  method output =         ch # output
  method really_output =  ch # really_output
  method output_char =    ch # output_char
  method output_string =  ch # output_string
  method output_byte =    ch # output_byte
  method output_buffer =  ch # output_buffer
  method output_channel = ch # output_channel
  method flush =          ch # flush
  method close_out =      ch # close_out
  method pos_out =        ch # pos_out
  method rollback_work =  ch # rollback_work

  method commit_work () = f_before(); ch # commit_work(); f_after()
end ;;


class discarding_channel (f_before : unit -> unit) 
                         (f_after: unit -> unit) () : trans_out_obj_channel =
object
  inherit output_null() as super
  val mutable pos_commit = 0  (* position of last "commit" *)
  val mutable pos_delta = 0   (* how much to substract from position *)
  method rollback_work() =
    (* "rollback" can be emulated by increasing the pos_delta value, such
     * that pos_out will return the position of the last commit operation
     *)
    pos_delta <- pos_delta + (super # pos_out - pos_commit)
  method commit_work() =
    f_before();
    pos_commit <- super # pos_out;
    f_after()
  method pos_out =
    super # pos_out - pos_delta
end ;;


type argument_processing = 
  [ `Memory | `File | `Automatic ] ;;


class uninitialized_activation
        ?env
	?(operating_type = (`Direct "" : operating_type))
	() =
let _env = 
  match env with
      Some e -> e
    | None   -> ( try new std_environment() 
		  with Std_environment_not_found ->
		    new test_environment() 
		) in
object (self)
  (* Improve: Currently, the arguments are stored as alists. This is inefficient
   * for a bigger number of arguments. Better use hashtables or maps.
   *)

  val environ = _env
  val op = operating_type
  val mutable out_ch = 
    new buffered_trans_channel (new output_buffer (Buffer.create 1))
    (* A dummy value. This variable is initialized right below *)

  val mutable req_meth = (`GET : request_method)   
                             (* must be initialized by heir *)

  val mutable initial_args = ([] : (string * cgi_argument) list) 
                             (* must be initialized by heir *)
  val mutable current_args = ([] : (string * cgi_argument) list) 
                             (* must be initialized by heir *)

  method private init_out_ch () =
    let env_ch = environ # output_ch in
    out_ch <-
      match op with
	  `Direct sep -> 
	    ( match req_meth with
		  `HEAD -> 
		    new discarding_channel 
		      (fun () -> ()) (self # after_commit) ()
		| _ ->
		    new no_trans_channel 
	              sep 
	              (self # after_commit) 
	              env_ch
	    )
	| `Transactional f ->
	    ( match req_meth with
		  `HEAD ->
		    new discarding_channel 
		      (self # before_commit) (self # after_commit) ()
		| _ ->
		    new on_commit_channel 
	              (self # before_commit) 
	              (self # after_commit)
	              (f (environ # config) env_ch)
	    )

  method environment = environ

  method request_method = req_meth

  method initial_arguments = initial_args
  method initial_argument name = List.assoc name initial_args
  method initial_argument_value ?(default="") name =
    try (List.assoc name initial_args) # value with Not_found -> default
  method initial_multiple_argument name =
    List.map snd (List.filter(fun (n,a) -> n = name) initial_args)

  method arguments = current_args
  method argument name = List.assoc name current_args
  method argument_value ?(default="") name =
    try (List.assoc name current_args) # value with Not_found -> default
  method multiple_argument name =
    List.map snd (List.filter(fun (n,a) -> n = name) current_args)

  method set_arguments ?(fin = true) l =
    (* Improve: Currently N^2 time *)
    if fin then begin
      List.iter
	(fun (n,a) ->
	   if not(List.mem a l) then a # finalize())
	current_args
	(* Note: List.mem works because arguments are objects, and List.mem
	 * compares the OIDs in this case.
	 *)
    end;
    current_args <- List.map (fun a -> (a # name, a)) l

  method update_argument ?(fin = true) arg =
    let name = arg # name in
    current_args <- (name, arg) ::
                    (List.filter
		       (fun (n,a) -> 
			  if fin && n = name && a <> arg then 
			    ( a # finalize(); false )
			  else
			    n <> name) 
		       current_args)

  method update_multiple_argument ?(fin = true) arglist =
    match arglist with
	[] -> ()
      | arg :: arglist' ->
	  let name = arg # name in
	  if List.exists (fun a -> a # name <> name) arglist' then
	    invalid_arg "update_multiple_argument";
	  current_args <- (List.map (fun a -> (name,a)) arglist) @
                          (List.filter
			     (fun (n,a) -> 
				if fin && n = name && not (List.mem a arglist) 
				then 
				  ( a # finalize(); false )
				else
				  n <> name) 
			     current_args)

  method delete_argument ?(fin = true) name =
    current_args <- (List.filter
		       (fun (n,a) -> 
			  if fin && n = name then
			    ( a # finalize(); false )
			  else
			    n <> name) 
		       current_args)

  method url ?(protocol = environ # protocol)
             ?(with_authority = (`Env : other_url_spec))
	     ?(with_script_name = (`Env : other_url_spec))
	     ?(with_path_info = (`Env : other_url_spec))
             ?(with_query_string = (`None : query_string_spec))
	     () =
    let p_scheme, p_port =
      match protocol with
	  `Http(v,atts) ->
	    if List.mem `Secure_https atts then
	      "https", 443
	    else
	      "http", 80
	| `Other ->
	    failwith "url: cannot cope with this protocol"
    in
    ( match with_authority with
	  `Env ->
	    begin p_scheme ^ "://" ^
	      environ # cgi_server_name ^ 
	      match environ # cgi_server_port with
		  None -> ""
		| Some port -> 
		    if port = p_port then "" else ":" ^ string_of_int port
	    end
	| `This s -> s
	| `None -> ""
    ) ^ 
    ( match with_script_name with
	  `Env -> environ # cgi_script_name
	| `This s -> s
	| `None -> ""
    ) ^ 
    ( match with_path_info with
	  `Env -> environ # cgi_path_info
	| `This s -> s
	| `None -> ""
    ) ^ 
    ( let args =
	match with_query_string with
	    `None -> []
	  | `Initial -> List.map snd initial_args
	  | `Current -> List.map snd current_args
	  | `Args al -> al
      in
      let args' =   (* consider only `Memory args *)
	List.filter
	  (fun a -> match a # store with `Memory -> true | _ -> false)
	  args
      in
      let s =
	String.concat "&"
	  (List.map
	     (fun a ->
		let n = a # name in
		let v = a # value in
		Netencoding.Url.encode n ^ "=" ^ Netencoding.Url.encode v
	     )
	     args'
	  )
      in
      if s = "" then
	""
      else
	"?" ^ s
    )

  method output = out_ch

  method private before_commit () =
    (* Called just before the transactional channel is committed *)
    (* DISCUSS: Set the content length header field here? *)
    match environ # output_state with
	`Start       -> environ # send_output_header();
	                environ # set_output_state `Sending_body
      | `Sent_header -> environ # set_output_state `Sending_body
      | `Sent_body   -> ()
	  (* May happen if data is committed several times *)
      | _ ->
	  failwith "Netcgi.std_activation: unexpected output state"


  method private after_commit () =
    environ # set_output_state `Sent_body
      (* This is actually not true. It would be better to set the output
       * state to `Sent_body only when the whole body has been sent.
       * Of course, we do not know this here.
       *)

  method set_header
           ?status
	   ?(content_type = "text/html")
           ?(cache = (`Unspecified : cache_control)) 
           ?(filename = "")
           ?(language = "")
           ?(script_type = "")
           ?(style_type = "")
           ?(set_cookie = [])
           ?(fields = [])
	   () =
    environ # set_output_header_fields [];
    environ # set_output_header_field "Content-type" content_type;
    (match status with
	 None -> ()
       | Some s -> 
	   environ # set_output_header_field "Status" (status_line s)
    );
    (match cache with
	 `Unspecified -> ()
       | `No_cache ->
	   let over = Unix.time() -. 1.0 in
	   environ # set_output_header_field "Cache-control" "no-cache";
	   environ # set_output_header_field "Pragma" "no-cache";
	   environ # set_output_header_field "Expires" 
	                                     (Netdate.mk_mail_date over);
       | `Max_age n ->
	   let exp = Unix.time() +. float_of_int n in
	   environ # set_multiple_output_header_field 
	     "Cache-control" 
	     [ "max-age=" ^ string_of_int n; "must-revalidate" ];	       
	   environ # set_output_header_field "Expires" 
	                                     (Netdate.mk_mail_date exp);
    );
    if filename <> "" then begin
      (* We need to quote quotation marks and backslashes: *)
      let qfn = Pcre.substitute 
		  ~rex:(Pcre.regexp "[\"\\\\]")
		  ~subst:(fun s -> "\\" ^ s)
		  filename
      in
      environ # set_output_header_field 
	"Content-disposition"
	("attachment; filename=\"" ^ qfn ^ "\"");
    end;
    if language <> "" then
      environ # set_output_header_field "Content-language" language;
    if script_type <> "" then
      environ # set_output_header_field "Content-script-type" script_type;
    if style_type <> "" then
      environ # set_output_header_field "Content-style-type" style_type;
    Nethttp.Header.set_set_cookie environ#output_header set_cookie;
    List.iter
      (fun (n,vlist) ->
	 environ # set_multiple_output_header_field n vlist
      )
      fields;
    (* If in direct mode, send the header immediately: *)
    match op with
	`Direct _ -> environ # send_output_header();
	             environ # set_output_state `Sending_body;
      | _         -> ()

  method set_redirection_header loc =
    environ # set_output_header_fields [];
    environ # set_output_header_field "Location" loc;
    (* If in direct mode, send the header immediately: *)
    match op with
	`Direct _ -> environ # send_output_header()
      | _         -> ()

  method finalize() =
    List.iter
      (fun (_,a) -> a # finalize())
      initial_args;
    List.iter
      (fun (_,a) -> a # finalize())
      current_args;
    initial_args <- [];
    current_args <- []

end ;;


class std_activation 
        ?env         
        ?(processing = (fun _ _ -> `Automatic : 
			    string -> Netmime.mime_header -> argument_processing))
	?operating_type 
        () : cgi_activation =
object (self)
  inherit uninitialized_activation ?env ?operating_type ()

  initializer
    (* It is an error if the environment is in the wrong state: *)
    if environ # input_state <> `Received_header then
      failwith "Netcgi.std_activation: environment indicates the wrong input state";
    if environ # output_state <> `Start then
      failwith "Netcgi.std_activation: environment indicates the wrong output state";

    (* Check request method: *)
    let perm_methods = environ # config.permitted_http_methods in
    if not (List.mem (environ # cgi_request_method) perm_methods) then
      failwith ("Netcgi.std_activation: Request method: " ^ 
		(environ # cgi_request_method) ^ 
		" is not permitted");

    let content_length = try environ#input_content_length with Not_found -> 0 in

    (* Check content type (if necessary): *)
    if content_length > 0 then begin
      let content_main_type, _ = environ # input_content_type in
      let perm_main_types = environ # config.permitted_input_content_types in
      if perm_main_types <> [] && 
	not (List.mem content_main_type perm_main_types) then
	  failwith ("Netcgi.std_activation: Content type: " ^ content_main_type ^
		    " is not permitted");
    end;

    if content_length > 0 then
      environ # set_input_state `Receiving_body;

    let m =
      match environ # cgi_request_method with
	  "GET"    -> `GET
	| "HEAD"   -> `HEAD
	| "POST"   -> `POST
	| "DELETE" -> `DELETE
	| "PUT"    -> `PUT (new simple_argument "" "")   (* replaced later *)
	| ms ->
	    failwith ("cgi_activation: unknown request method: " ^ ms)
    in
    req_meth <- m;

    self # init_out_ch();

    let set_url_encoded_args s =
      let args = Netencoding.Url.dest_url_encoded_parameters s in
      initial_args <- List.map (fun (n,v) ->
				  (n, new simple_argument ~ro:true n v)) args;
      current_args <- List.map (fun (n,v) ->
				  (n, new simple_argument ~ro:false n v)) args;
    in

    let work_around_backslash_bug = 
      List.mem `Work_around_backslash_bug environ#config.workarounds in

    let counter = ref 0 in

    let now = Unix.gettimeofday() in
    let millis = truncate((now -. floor now) *. 1000.0) in
    let tmp_directory = environ#config.tmp_directory in
    let tmp_prefix_p = environ#config.tmp_prefix ^ 
		       Netdate.format "-%Y%m%d%H%M%S-" (Netdate.create now) ^ 
		       string_of_int millis ^ "-" in

    let temp_file() =
      let tmp_prefix = tmp_prefix_p ^ string_of_int !counter in
      let (f_n,f_in,f_out) = 
	Netchannels.make_temporary_file ~tmp_directory ~tmp_prefix () in
      incr counter;
      close_in f_in;
      close_out f_out;
      f_n
    in

    let form_arg stream =
      let arg_hdr = Netmime.read_mime_header ~ro:false stream in
      let arg_hdr_ro = new Netmime.basic_mime_header ~ro:true arg_hdr#fields in
      let decoder = Netmime.decode_mime_body arg_hdr in
      let disposition, disp_params = arg_hdr # content_disposition() in
      if disposition <> "form-data" then 
	failwith ("Netcgi.std_activation: Unknown Content-disposition: " ^ disposition);
      let name = 
	try Mimestring.param_value(List.assoc "name" disp_params)
	with Not_found -> failwith "Netcgi.std_activation: missing arg name" in
      let store = 
	match processing name arg_hdr with
	    `Memory    -> `Memory
	  | `File      -> `File (temp_file())
	  | `Automatic -> 
	      let has_filename = List.mem_assoc "filename" disp_params in
	      if has_filename then `File (temp_file()) else `Memory
      in
      let ro = store <> `Memory in
      let body, body_ch = Netmime.storage ~ro ~fin:true store in
      with_out_obj_channel 
        (decoder body_ch)
        (fun body_ch' ->
           body_ch' # output_channel (stream :> in_obj_channel));
      let body_ro = 
	if ro then body 
	else new Netmime.memory_mime_body ~ro:true body#value in
      let ibody = (arg_hdr_ro, `Body body_ro) in
      let cbody = (arg_hdr,    `Body body) in
      let iarg = new mime_argument ~work_around_backslash_bug name ibody in
      let carg = new mime_argument ~work_around_backslash_bug name cbody in
      ((name,iarg),(name,carg))
    in

    begin match m with
	`GET 
      | `HEAD
      | `DELETE	->
	  (* Read the arguments from QUERY_STRING *)
	  set_url_encoded_args environ#cgi_query_string
      | (`POST | `PUT _ as m') ->
	  (* Read the arguments from environ#input_ch *)
	  let n = 
	    try environ # input_content_length 
	    with Not_found -> failwith "Netcgi.std_activation: missing Content-length"
	  in
	  if n > environ#config.input_content_length_limit then
	    raise Resources_exceeded;
	  begin match m' with
	      `POST ->
		(* Read several arguments from input_ch *)
		begin match environ # input_content_type with
		    "application/x-www-form-urlencoded", _ ->
		      let buf = String.create n in
		      environ # input_ch # really_input buf 0 n;
		      set_url_encoded_args buf
		  | "multipart/form-data", params ->
		      let boundary = 
			try Mimestring.param_value(List.assoc "boundary" params)
			with Not_found -> failwith "Netcgi.std_activation: missing boundary"
		      in
		      let stream = 
			new Netstream.input_stream ~len:n environ#input_ch in
		      let icargs =
			Mimestring.read_multipart_body form_arg boundary stream in
		      let (iargs,cargs) = List.split icargs in
		      initial_args <- iargs;
		      current_args <- cargs
		  | _ ->
		      (* Unknown argument type. There are a number of such
                       * types in use, e.g.:
                       * - application/vnd.fdf: Acrobat Reader
                       * - multipart/related, application/xml: XForms 1.0
                       * - application/x-www-form+xml, text/plain: WHATWG 
                       *   Web Forms 2.0 
                       * We leave that totally to the application, and only
                       * generate one argument "BODY" containing the
                       * data. Note that these argument types must also be
                       * allowed in cgi_config.
                       *)
		      let hdr_fields = environ#input_header#fields in
		      let arg_hdr = 
			new Netmime.basic_mime_header ~ro:false hdr_fields in
		      let arg_hdr_ro = 
			new Netmime.basic_mime_header ~ro:true hdr_fields in
		      let stream = 
			new Netstream.input_stream ~len:n environ#input_ch in
		      let decoder = Netmime.decode_mime_body arg_hdr in
		      let name = "BODY" in
		      let store = 
			match processing name arg_hdr with
			    `Memory    -> `Memory
			  | `File
			  | `Automatic -> `File (temp_file()) in
		      let ro = store <> `Memory in
		      let body, body_ch = 
			Netmime.storage ~ro ~fin:true store in
		      with_out_obj_channel 
			(decoder body_ch)
			(fun body_ch' ->
			   body_ch' # output_channel (stream :>in_obj_channel));
		      let body_ro = 
			if ro then body 
			else new Netmime.memory_mime_body ~ro:true body#value in
		      let ibody = (arg_hdr_ro, `Body body_ro) in
		      let cbody = (arg_hdr,    `Body body) in
		      let iarg = 
			new mime_argument ~work_around_backslash_bug 
			  name ibody in
		      let carg = 
			new mime_argument ~work_around_backslash_bug
			  name cbody in
		      initial_args <- [ name, iarg ];
		      current_args <- [ name, carg ];
		end
	    | `PUT _ ->
		(* Read the normal arguments from QUERY_STRING *)
		set_url_encoded_args environ#cgi_query_string;
		(* Read the special PUT argument from input_ch *)
		let hdr = environ#input_header in 
		let stream = 
		  new Netstream.input_stream ~len:n environ#input_ch in
		let decoder = Netmime.decode_mime_body hdr in
		let name = "BODY" in
		let store = 
		  match processing name hdr with
		      `Memory    -> `Memory
		    | `File
		    | `Automatic -> `File (temp_file()) in
		let body, body_ch = Netmime.storage ~ro:true ~fin:true store in
		with_out_obj_channel 
		  (decoder body_ch)
		  (fun body_ch' ->
		     body_ch' # output_channel (stream :> in_obj_channel));
		let msg = (hdr, `Body body) in
		let arg = 
		  new mime_argument ~work_around_backslash_bug name msg in
		req_meth <- `PUT arg
	  end
    end;
    environ # set_input_state `Received_body;

    (* Note: initial_args and current_args are physically separated (they
     * have their own argument objects). However, the value strings are
     * shared.
     *)

end
;;

class custom_activation 
      ?env ?(args=[]) ?(meth=`GET) ?operating_type () : cgi_activation =
object (self)
  inherit uninitialized_activation ?env ?operating_type ()

  initializer
    req_meth <- meth;
    initial_args <- List.map (fun a -> a#name, a) args;
    current_args <- List.map (fun a -> a#name, a) args;
    self # init_out_ch()
end
;;


let buffered_transactional_optype =
  `Transactional (fun config ch -> new buffered_trans_channel ch)
;;


let tempfile_transactional_optype =
  `Transactional
    (fun config ch ->
       let tmp_directory = config.tmp_directory in
       let tmp_prefix = config.tmp_prefix in
       new tempfile_trans_channel ~tmp_directory ~tmp_prefix ch
    )
;;

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.16  2005/12/17 18:26:03  stolpmann
 * Enhancement: standard activation can now process non-standard POST
 * data
 *
 * Revision 1.15  2005/07/25 22:36:40  stolpmann
 * Merge of nethttpd.
 * Adding the "overview" section to Nethttpd_types.
 * Cosmetic fixes.
 * Release updates.
 *
 * Revision 1.14.2.1  2005/04/30 16:31:16  stolpmann
 * Integration of the new Nethttpd component into Ocamlnet.
 *
 * Revision 1.14  2005/02/02 21:19:47  gremlin43820
 * do the same for content type, and content disposition
 *
 * Revision 1.12  2004/07/01 12:56:20  stolpmann
 * 	Removed dependency on Cgi.
 *
 * Revision 1.11  2004/05/30 21:27:14  stolpmann
 * 	Supporting the standard for class-based I/O. This also means
 * the semantics of [input] and [output] methods have changed (return
 * value 0, End_of_file).
 * 	Removed _rep_in, _rep_out.
 * 	Removed Netstream.Deprecated.
 *
 * Revision 1.10  2002/10/24 23:47:48  stolpmann
 * 	Support for the HEAD method.
 * 	Workaround for a bug in MSIE: Empty cookies are represented
 * in the wrong way
 *
 * Revision 1.9  2002/02/03 21:27:50  stolpmann
 * 	Added: custom_activation.
 *
 * Revision 1.8  2002/01/14 01:22:43  stolpmann
 * 	[See also rev. 1.6 of netcgi.mli]
 * 	The class mime_argument bases on the Netmime types.
 * 	The argument parser no longer depends on the deprecated Cgi
 * module. A new argument parser has been written using Mimestring.read_
 * mime_body and Netmime.
 * 	The request methods PUT and DELETE are now supported.
 *
 * Revision 1.7  2001/12/22 09:18:43  pdoane
 * 	Updated for Netchannel modifications
 *
 * Revision 1.6  2001/11/17 23:27:07  stolpmann
 * 	Changed options for [url] method: The with_XXX options
 * have the values [ `Env | `This of string | `None ] instead of
 * just bool.
 *
 * Revision 1.5  2001/10/04 01:04:58  stolpmann
 * 	Moved from directory /src/netstring to /src/cgi.
 *
 * Revision 1.4  2001/10/04 00:54:45  stolpmann
 * 	~ro: CGI argument can now be read-only. Mainly for the
 * set of initial arguments that should be immutable.
 * 	Fixed method [url].
 *
 * Revision 1.3  2001/09/30 00:10:12  stolpmann
 *         The class mime_argument has now a workaround option.
 *         Changed the [operating_type], the config is now passed
 * to the function creating the transactional channel.
 *         New functions [buffered_transactional_optype] and
 * [tempfile_transactional_optype].
 * 	The class [std_activation] has been completed: The configuration
 * is now respected; arbitrary in_obj_channels are now accepted and not
 * only channels using a real in_channel; the input/output states of the
 * environment are checked and the state transitions are performed.
 *
 * Revision 1.2  2001/09/28 21:23:11  stolpmann
 * 	Improved MIME arguments.
 *
 * Revision 1.1  2001/09/27 21:59:00  stolpmann
 * 	Initial revision
 *
 * 
 *)
