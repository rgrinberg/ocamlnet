(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* This code is copyright 2003 Eric Stokes, and may be used under
either, the GNU GPL, or the same license as ocamlnet *)

open Unix

exception FCGI_error of string * exn

(* tuning paramaters *)
let bufsize = 100;;

(* maximum stdout/stderr record size *)
let max_rec_size = 65535

(* protocol defines *)
let fcgi_begin_request     = 1;;
let fcgi_abort_request     = 2;;
let fcgi_end_request       = 3;;
let fcgi_params            = 4;;
let fcgi_stdin             = 5;;
let fcgi_stdout            = 6;;
let fcgi_stderr            = 7;;
let fcgi_data              = 8;;
let fcgi_get_values        = 9;;
let fcgi_get_values_result = 10;;
let fcgi_unknown_type      = 11;;
let fcgi_maxtype           = fcgi_unknown_type;;
let fcgi_null_request_id   = 0;;
let fcgi_keep_conn         = 1;;
let fcgi_responder         = 1;;
let fcgi_authorizer        = 2;;
let fcgi_filter            = 3;;
let fcgi_request_complete  = 0;;
let fcgi_cant_mpx_conn     = 1;;
let fcgi_overloaded        = 2;;
let fcgi_unknown_role      = 3;;


(* protocal header *)
type fcgiHeader = {version: int; rtype: int; requestid: int; contentlen: int; padlen: int}

(* begin request *)
type fcgiBeginRequestBody = {role: int; flags: int}

(* end request *)
type fcgiEndRequestBody = {astatus: int; pstatus: int}

(* fcgi params will return an asociation list *)
(* fcgi stdin will return a string *)

(* a full request record for the responder roll *)
type fcgiRequest = {id: int; 
		    app_type: int;
		    params: (string * string) list; 
		    stdin: string;
		    data: string;
		    con: Unix.file_descr}


(* debug print header *)
let print_header hd =
  print_string "ver:";print_int hd.version;print_endline "";
  print_string "typ:";print_int hd.rtype;print_endline "";
  print_string "id:";print_int hd.requestid;print_endline "";
  print_string "clen:";print_int hd.contentlen;print_endline "";
  print_string "plen:";print_int hd.padlen;print_endline ""

let print_packet (hd, data) =
  print_header hd; print_endline data

let print_request rq =
  let print_params prms =
    List.iter (fun x -> print_string (fst x);print_string ": ";print_endline (snd x)) prms 
  in
  print_string "requestid: ";print_int rq.id;print_endline "";
  print_string "role: ";print_int rq.app_type;print_endline "";
  print_endline "params"; print_params rq.params;
  print_endline "stdin"; print_endline rq.stdin;
  print_endline "data"; print_endline rq.data


(*********************************************
  general encodeing, and decodeing functions 
**********************************************)
let byte_from_int1 i = (* one byte integer encoding *)
  if i > 255 then failwith "Netcgi_fcgi_10.byte_from_int1: out of range";
  let buf = String.create 1 in
    String.set buf 0 (Char.chr i); buf

let int_from_byte1 s off = (* one byte integer decoding *)
  Char.code (String.get s off)

let byte_from_int2 i = (* two byte integer encoding *)
  if i > 65535 then failwith "Netcgi_fcgi_10.byte_from_int2: out of range";
  let buf = String.create 2 in
    String.set buf 0 (Char.chr (i lsr 8));
    String.set buf 1 (Char.chr (i land 255)); buf
      
let int_from_byte2 s off = (* two byte integer decodeing *)
  ((int_from_byte1 s off) lsl 8) + int_from_byte1 s (off + 1)

let byte_from_int4 i = (* four byte integer encoding *)
  let buf = String.create 4 in
    String.set buf 0 (Char.chr (i lsr 24));
    String.set buf 1 (Char.chr ((i lsr 16) land 255));
    String.set buf 2 (Char.chr ((i lsr 8) land 255));
    String.set buf 3 (Char.chr (i land 255)); buf

let int_from_byte4 data off = (* four byte integer decoding *)
      let byte0 = (Char.code (String.get data off)) in
      let byte1 = (Char.code (String.get data (off + 1))) in
      let byte2 = (Char.code (String.get data (off + 2))) in
      let byte3 = (Char.code (String.get data (off + 3))) in
	((+) ((+) ((+) ((lsl) byte0 24) ((lsl) byte1 16)) ((lsl) byte2 8)) byte3)

(*************************************************** 
 fastcgi structure encodeing and decodeing functions 
****************************************************)
let encode_fcgi_header hdr = (* encoding of fcgi header *)
  (byte_from_int1 hdr.version)    ^
  (byte_from_int1 hdr.rtype)      ^
  (byte_from_int2 hdr.requestid)  ^ 
  (byte_from_int2 hdr.contentlen) ^
  (byte_from_int1 hdr.padlen)     ^
  (String.create 1)

let decode_fcgi_header buf off =
  let version   = int_from_byte1 buf off in
  let rectype   = int_from_byte1 buf (off + 1) in
  let requestid = int_from_byte2 buf (off + 2) in
  let datalen   = int_from_byte2 buf (off + 4) in
  let padlen    = int_from_byte1 buf (off + 6) in
    {version=version;
     rtype=rectype;
     requestid=requestid;
     contentlen=datalen;
     padlen=padlen}

let decode_fcgi_begin_request data off =
  {role=int_from_byte2 data off; flags=int_from_byte1 data (off + 2)}

let decode_fcgi_param data off =
  let get_data data off nmlen vlen = 
    ((String.sub data off nmlen), (String.sub data (off + nmlen) vlen), (off + nmlen + vlen))
  in
    (* check flags for 4 byte encoding, or 1 byte encoding of
       the length of the datum. This is silly on modern networks. 
       All this work to save 3 bytes, but there we have it :P *)
    if ((lsr) (Char.code (String.get data off)) 7) = 1 then
      (let nmlen = int_from_byte4 data off in
	 if ((lsr) (Char.code (String.get data (off + 4))) 7) = 1 then
	   get_data data (off + 8) nmlen ((int_from_byte4 data (off + 4)) land 0x7fffffff)
	 else
	   get_data data (off + 5) nmlen (int_from_byte1 data (off + 4)))
    else
      (let nmlen = int_from_byte1 data off in
	 if ((lsr) (Char.code (String.get data (off + 1))) 7) = 1 then
	   get_data data (off + 5) nmlen ((int_from_byte4 data (off + 1)) land 0x7fffffff)
	 else
	   get_data data (off + 2) nmlen (int_from_byte1 data (off + 1)))

let encode_fcgi_data data id d_type = (* encode stdout, or stderr data for requestid id *)
  let buf = Buffer.create bufsize in
    Buffer.add_string buf
      (encode_fcgi_header {version=1;
			   rtype=d_type;
			   requestid=id;
			   contentlen=(String.length data);
			   padlen=0 (* might explore padding for perormance *)});
    Buffer.add_string buf data;
    Buffer.contents buf

let encode_fcgi_end_request req id =
  let {astatus=astat; pstatus=pstat} = req in
  let buf = Buffer.create bufsize in    
    Buffer.add_string buf 
      (encode_fcgi_header {version=1;
			   rtype=fcgi_end_request;
			   requestid=id;
			   contentlen=8;
			   padlen=0});
    Buffer.add_string buf (byte_from_int4 astat);
    Buffer.add_string buf (byte_from_int1 pstat);
    Buffer.add_string buf (String.create 3);
    Buffer.contents buf
  
(***************************************************
  functions which read or write fcgi structures
***************************************************)
let fcgi_read_header con = (* read the packet header *)
  try
    let buf = String.create 8 in
    let read = read con buf 0 8 in
      decode_fcgi_header buf 0
  with Unix_error (e, f, i) -> 
    raise (FCGI_error ("fcgi_read_header", Unix_error (e, f, i)))
	
let fcgi_drop_padding con len =
  try
    if len > 0 then
      let buf = String.make len 'c' in
      let read = read con buf 0 len in
	read
    else
      0
  with Unix_error (e, f, i) -> 
    raise (FCGI_error ("fcgi_drop_padding", Unix_error (e, f, i)))

let fcgi_read_packet con = (* read one full packet *)  
  try
    let header = fcgi_read_header con in
    let buf = String.create header.contentlen in
    let read = read con buf 0 header.contentlen in
    let pad = fcgi_drop_padding con header.padlen in
      (header, buf)
  with Unix_error (e, f, i) -> 
    raise (FCGI_error ("fcgi_read_packet", Unix_error (e, f, i)))

let fcgi_read_begin_request con =
    let (header, data) = fcgi_read_packet con in
      match header with
	  {rtype=fcgi_begin_request;contentlen=0} -> failwith "Protocal Error"
	| {rtype=fcgi_begin_request;requestid=id;contentlen=8} ->
	    (id, (decode_fcgi_begin_request data 0))
	| {rtype=_} -> failwith "Invalid FCGI Structure"

(* each packet contains a param. The last one will have content
   length of 0. keep reading params until there are no more *)
let fcgi_read_params con =
  let rec fcgi_read_params con params =
    let (header, data) = fcgi_read_packet con in
      match header with
	  {rtype=v;contentlen=0} when v = fcgi_params -> params
	| {rtype=v} when v = fcgi_params -> 
	    let rec fcgi_parse_block params data header off =
	      let (n, pv, n_off) = decode_fcgi_param data off in
		if n_off < header.contentlen then
	          fcgi_parse_block ((n, pv) :: params) data header n_off
		else
	          (n, pv) :: params
	    in
	      fcgi_read_params con (fcgi_parse_block params data header 0)
	| {rtype=_} -> failwith "Invalid FCGI Structure"
  in
    try fcgi_read_params con []
    with Unix_error (e, f, i) -> 
      raise (FCGI_error ("fcgi_read_params", Unix_error (e, f, i)))

(* each stream packet contains part of the stream data 
   keep reading them and concatinate them all together until
   we see the end of stream packed (one with content length 0)*)
let fcgi_read_stream type_t con =
  let rec read_stream type_t con data =
    let (header, payload) = fcgi_read_packet con in
      match header with
	  {rtype=t;contentlen=0} when t = type_t -> 
	    Buffer.contents data
	| {rtype=t} when t = type_t -> 
	    read_stream type_t con (Buffer.add_string data payload;data)
	| {rtype=_} -> failwith "Invalid FCGI Structure"
  in
    try read_stream type_t con (Buffer.create bufsize)
    with Unix_error (e, f, i) -> 
      raise (FCGI_error ("fcgi_read_stream", Unix_error (e, f, i)))
      
let fcgi_read_stdin con =
  try fcgi_read_stream fcgi_stdin con
  with Unix_error (e, f, i) -> 
    raise (FCGI_error ("fcgi_read_stdin", Unix_error (e, f, i)))

let fcgi_read_data con =
  try fcgi_read_stream fcgi_data con
  with Unix_error (e, f, i) -> 
    raise (FCGI_error ("fcgi_read_data", Unix_error (e, f, i)))

(* read a full responder request *)
let fcgi_read_responder_request con id =
  let params = fcgi_read_params con in
  let stdin = fcgi_read_stdin con in 
    {id=id;
     app_type=fcgi_responder;
     params=params;
     stdin=stdin;
     data="";
     con=con}

(* read full authorizer request (note, this is the same as the responder)*)
let fcgi_read_authorizer_request con id =
  let params  = fcgi_read_params con in
  let stdin = fcgi_read_stdin con in 
    {id=id;
     app_type=fcgi_authorizer;
     params=params;
     stdin=stdin;
     data="";
     con=con}

let fcgi_read_filter_request con id =
  let params = fcgi_read_params con in
  let stdin = fcgi_read_stdin con in
  let data = fcgi_read_data con in
    {id=id;
     app_type=fcgi_filter;
     params=params;
     stdin=stdin;
     data=data;
     con=con}

(* read a request *)
let fcgi_read_request con = 
  let (id, beginreq) = fcgi_read_begin_request con in
    match beginreq with
	{role=v} when v = fcgi_responder  -> fcgi_read_responder_request con id
      | {role=v} when v = fcgi_authorizer -> fcgi_read_authorizer_request con id
      | {role=v} when v = fcgi_filter     -> fcgi_read_filter_request con id
      | {role=_}                          -> failwith "FCGI Role Not Supported"

let fcgi_write_end_request req endreq =
  try ignore (write req.con (encode_fcgi_end_request endreq req.id) 0 16)
  with Unix_error (e, f, i) -> 
    raise (FCGI_error ("fcgi_write_end_request", Unix_error (e, f, i)))


let fcgi_write_stdout req data =
  if String.length data > max_rec_size then
    raise (FCGI_error ("fcgi_write_stdout", Failure "maximum record size exceeded"))
  else
    try 
      ignore 
	(write 
	   req.con 
	   (encode_fcgi_data data req.id fcgi_stdout) 0 
	   (8 + (String.length data)))
    with Unix_error (e, f, i) -> 
      raise (FCGI_error ("fcgi_write_stdout", Unix_error (e, f, i)))

let fcgi_write_stderr req data = 
  if String.length data > max_rec_size then
    raise (FCGI_error ("fcgi_write_sterr", Failure "maximum record size exceeded"))
  else
    try 
      ignore
	(write 
	   req.con 
	   (encode_fcgi_data data req.id fcgi_stderr) 0 
	   (8 + (String.length data)))
    with Unix_error (e, f, i) -> 
      raise (FCGI_error ("fcgi_write_stderr", Unix_error (e, f, i)))

(* accept a conn from fastcgi *)
let fcgi_accept unit =
  try
    let (con, addr) = accept Unix.stdin in
      fcgi_read_request con
  with exn -> raise (FCGI_error ("fcgi_accept", exn));;

(* for toplevel debugging. In the case of apache mod_fastcgi, use the
   FastCGIExternalServer directive, open up the socket in the
   toplevel, and you'll be able to talk fastcgi protocol with the web
   server inside your toplevel *)
let fcgi_debug_accept s = 
  let (con, addr) = accept s in
    fcgi_read_request con

(* destroy a connection when we're done with it *)
let fcgi_destroy req = close req.con

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.9  2006/03/02 22:24:49  stolpmann
 * Bugfix for 64 bit archs: Long header were improperly decoded.
 *
 * Revision 1.8  2005/12/02 20:42:33  gremlin43820
 * fixed a bug in the handling of very large pages and fastcgi. netchannel expects to transfer the page all at once (which is fine), however fastcgi can only handle 64Kbytes of data at a time. We handle this by splitting things up into 64Kbyte chunks in the fcgi_out_channel. Thanks to Francois Rouaix for reporting this issue.
 *
 * Revision 1.7  2005/02/03 19:16:53  gremlin43820
 * open unix in netcgi_fcgi.ml to fix compiler error. Make naming clearer in netcgi_fcgi_10.ml, in read_params, pluralize the list of params "param" is a confusing name for a list.
 *
 *)
