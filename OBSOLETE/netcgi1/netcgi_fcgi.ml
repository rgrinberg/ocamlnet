(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* This code is copyright 2003 Eric Stokes, and may be used under
either, the GNU GPL, or the same license as ocamlnet *)

open Netcgi_fcgi_10
open Netcgi_env
open Netcgi_types
open Netcgi
open Netchannels
open Pcre
open Unix

(* classes which I must impliment
class type raw_out_channel = object
  method output : string -> int -> int -> int
  method close_out : unit -> unit
  method pos_out : int             (* number of written characters *)
  method flush : unit -> unit
end
*)

class fcgi_out_channel req : out_obj_channel = 
object
  inherit augment_raw_out_channel
  val req = req
  val mutable lenout = 0
  method output buf pos len =     
    (* Split the request up so that we never exceed the maximum record size *)
    let rec split_buf buf pos len =
      let s = Netcgi_fcgi_10.max_rec_size in
	if len > s then (String.sub buf pos s) :: (split_buf buf (pos + s) (len - s))
	else [String.sub buf pos len]
    in
      List.iter
	(fun b -> 
	   fcgi_write_stdout req b; 
	   let len = String.length b in
	     lenout <- lenout + len)
	(split_buf buf pos len);
      len
  method close_out () = 
    fcgi_write_stdout req "";
    fcgi_write_stderr req "";
    fcgi_write_end_request req {astatus=0;pstatus=0};
    fcgi_destroy req
  method pos_out = lenout
  method flush () = ()
end;;

class fcgi_in_channel req : in_obj_channel =
object
  inherit augment_raw_in_channel
  val req = req
  val mutable pos = 0
  method input buf offset len =
    try
      String.blit req.stdin pos buf offset len;
      pos <- pos + len;
      len
    with Invalid_argument(_) -> (* send back the last of the buffer *)
      let stdinlen = String.length req.stdin in
	if pos < stdinlen then (
	  String.blit req.stdin pos buf offset (stdinlen - pos);
	  let written = stdinlen - pos in
	    pos <- stdinlen;
	    written)
	else
	  raise End_of_file
  method pos_in = pos
  method close_in () = ()
end;;

(* TODO 
-build an input channel subclass
which can fetch the stdin params
-set request method to the proper
value based on the actual request*)

(* usefull examples
build the cgi request object need more
  data, see netcgi_jserv_app.ml:179*)
(* build the environment need to
  look into this a whole lot more see netcgi_jserv_ajp12.ml:204 *)

(* build the cgi environment, including initializing the
output, and input channels to the correcto operating type *)

(* regexes used in the setup_env function, they are here for performance reasons *)
let underscore = regexp ~study:true "_"
let http = regexp ~study:true "^HTTP-"

(* build a table of environment variables that we set explicitly. 
   Fastcgi does not distinguish between environment variables and http headers,
   so when we set_input_header_fields (the method of passing on the http headers
   to the higher level) we need to filter out the environment variables. The reason
   we build a hashtbl outside the function is performance. We init the hash table at
   application startup, and only do lookups thoughout the app life cycle (fast cgi apps 
   are usually daemons). List.mem is O(n), Hashtbl.mem is nearly always O(1) *)
let evn_already_set =
  let tbl = Hashtbl.create 15 in
    (List.iter (fun var -> Hashtbl.add tbl var true)
       ["REQUEST_METHOD";"QUERY_STRING";"SERVER_SOFTWARE";
	"SERVER_NAME";"SERVER_PROTOCOL";"SERVER_PORT"; 
	"PATH_INFO";"SCRIPT_NAME";"REMOTE_HOST";
	"REMOTE_ADDR";"AUTH_TYPE";"REMOTE_USER"]);
    tbl

let setup_env config req =
    let env = new Netcgi_env.custom_environment ?config () in
    let evn_already_set = Hashtbl.create 15 in
    let getparam p = try (List.assoc p (req.params)) with Not_found -> "" in
      env#set_cgi (*set up cgi paramaters. extract them from the low level req struct*)
	~request_method: (getparam "REQUEST_METHOD") 
	~query_string: (getparam "QUERY_STRING")
	~server_software: (getparam "SERVER_SOFTWARE")
	~server_name: (getparam "SERVER_NAME")
	~server_protocol: (getparam "SERVER_PROTOCOL")
	~server_port: (try 
			 Some(int_of_string (getparam "SERVER_PORT")) 
		       with Failure(_) -> None)
	~path_info: (getparam "PATH_INFO")
	~script_name: (getparam "SCRIPT_NAME")
	~remote_host: (getparam "REMOTE_HOST")
	~remote_addr: (getparam "REMOTE_ADDR")
	~auth_type: (getparam "AUTH_TYPE")
	~remote_user: (getparam "REMOTE_USER")
	~https: (if (getparam "HTTPS") = "" then false else true)
	();
      env#set_input_header_fields (* set all the input headers *)
	(List.rev_map
	   (fun (name, valu) -> 
	      (* mod_fastcgi seems to append HTTP_ to some header fields *)
	      (* wdialog wants - as a word seperator, fcgi sends _ *)
	      (replace ~templ:"" ~rex:http 
		  (replace ~templ:"-" ~rex:underscore name), valu))
	      (List.filter (* filter out environment variable we've already set *)
	      (fun (name, valu) -> 
		 not (Hashtbl.mem evn_already_set name))
	      req.params));
      env#set_input_state `Received_header;
      env#set_output_ch (new fcgi_out_channel req);
      env#set_input_ch (new fcgi_in_channel req);
      env#set_error_log (fun s -> fcgi_write_stderr req (s ^ "\n"));
      env#setup_finished ();
      env


let get_fcgi_env ?(config:cgi_config option) () = 
  ((setup_env config (fcgi_accept ())) :> Netcgi_env.cgi_environment)

let get_fcgi_activation ?(config:cgi_config option) optype =
  ((new std_activation ~env:(get_fcgi_env ?config ())
      ~operating_type:optype ()) :> cgi_activation)

(* start looking here! This contains the high level
   steps necessary to create a properly formed activation
   object, and send it to the (user defined) handler, and
   since we are a daemon, just like any other network service
   we do it over and over again forever.
*)
let serv ?(config:cgi_config option)
         (handler:(cgi_activation -> unit)) (optype:operating_type) =
  while true
  do
      let cgi = get_fcgi_activation ?config optype in
	try
	  handler cgi;
	  cgi#output#close_out () (*close down this connection*)
	with FCGI_error (_, Unix_error (EPIPE, _, _)) -> ()
	  (* clients who close the connection early will result in a
	     sigpipe (which is ignored by default when using
	     mod_fastcgi and returned via a Unix_error). This is
	     normal behavior, and should NOT kill the application, 
	     nor should each application writer be required to handle 
	     it. We ensure two things with this handler.
	     
	     1. We will properly handle the sigpipe condition
	     2. We will ONLY handle it if it comes from fastcgi,
	        we should not mask bugs in the application. This is
	        the greater part of the rational for creating the
	        FCGI_error exception. *)
  done;;
