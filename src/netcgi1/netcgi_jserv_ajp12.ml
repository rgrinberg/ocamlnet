(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


open Netcgi_jserv

(* Implementation of the JSERV protocol 1.2 *)

let readstring inch =
  let buf = String.create 2 in
  inch#really_input buf 0 2;
  let n = ((Char.code buf.[0]) lsl 8) lor (Char.code buf.[1]) in
  if n = 0xffff then
    None
  else
    let str = String.create n in
    inch#really_input str 0 n;
    Some str
;;


let readmark inch =
  let buf = String.create 1 in
  inch#really_input buf 0 1;
  Char.code buf.[0]
;;


let include_if name s_opt =
  match s_opt with
      None -> []
    | Some x -> [name,x]
;;


let close_in ch =
  try ch # close_in() 
  with Netchannels.Closed_channel -> ()
;;


let close_out ch =
  try ch # close_out() 
  with Netchannels.Closed_channel -> ()
;;


let find_substring s1 s2 =
  (* Searches for the first occurrence of s2 in s1, and returns the position,
   * or raises Not_found 
   *)
  let l1 = String.length s1 in
  let l2 = String.length s2 in
  let rec search p =
    if p > l1-l2 then raise Not_found;
    if s1.[p] = s2.[0] && String.sub s1 p l2 = s2 then p else search (p+1)
  in
  if l2 = 0 then
    0  (* questionable *)
  else
    search 0
;;


let jk_fixups jk_servletSubString uri servlet path_info script_name =
  match servlet with
      Some _ -> 
	(* Do not fix anything! *)
	(servlet,path_info,script_name)

    | None ->
	(* Check whether the substring occurs in uri *)
	begin try
	  let substring = 
	    match jk_servletSubString with
		None -> "/servlets/"       (* default value *)
	      | Some s -> 
		  if s = "" then raise Not_found (* disable *) else s
	  in
	  let p = find_substring uri substring in  (* or Not_found *)
	  (* Yes, the substring occurs at position p *)
	  let p' = p + String.length substring in
	  let p'' = 
	    try String.index_from uri p' '/' 
	    with Not_found -> String.length uri in
	  let servlet = Some (String.sub uri p' (p''-p')) in
	  let path_info = Some (String.sub uri p'' (String.length uri - p'')) in
	  let script_name = Some (String.sub uri 0 p'') in
	  (servlet,path_info,script_name)
	with
	    Not_found ->
	      (* Fallback solution *)
	      (Some uri, Some "", Some uri)
	end
;;


let serve_connection ?config ?(https=false) ?jk_servletSubString 
                     cgi_handler authopt inch outch =
  try
    begin match authopt with
	None -> ()  (* no authentication *)
      | Some auth ->
	  (* authenticate *)
	  (* (1) Send size of challenge string *)
	  let l = auth.auth_challenge_length in
	  if l >= 65536 then 
	    failwith "Challenge string too long";
	  let c1 = Char.chr (l lsr 8) in
	  let c0 = Char.chr (l land 0xff) in
	  let s = String.make 4 '\000' in
	  s.[2] <- c1;
	  s.[3] <- c0;
	  outch # output_string s;
	  outch # flush();
	  (* (2) Generate challenge string, and send it *)
	  let challenge = String.create l in
	  for i = 0 to l -1 do
	    challenge.[i] <- Char.chr (random_8bits())
	      (* TODO: Thread safety *)
	  done;
	  outch # output_string challenge;
	  outch # flush();
	  (* (3) Compute hash value: *)
	  let hv = Digest.string (challenge ^ auth.auth_secret) in
	  (* (4) Receive hash value from client: *)
	  let hv' = String.create 16 in
	  inch # really_input hv' 0 16;
	  (* (5) Compare hash values: *)
	  if hv <> hv' then raise End_of_file;
    end;
    (* Read the request from inch: *)
    match readmark inch with
	1 ->
	  (* Normal request: *)
	  let request_method = readstring inch in
	  let req_zone = readstring inch in
	  let req_servlet = readstring inch in         (* ??? *)
	  let req_hostname = readstring inch in
	  let req_docroot = readstring inch in
	  let path_info = readstring inch in
	  let path_translated = readstring inch in
	  let query_string = readstring inch in          (* ??? *)
	  let remote_addr = readstring inch in
	  let remote_host = readstring inch in
	  let remote_user = readstring inch in
	  let auth_type = readstring inch in
	  let req_server_port = readstring inch in
	  let req_method_again = readstring inch in
	  let req_uri = readstring inch in           (* scriptname + pathinfo *)
	  let req_filename = readstring inch in
	  let script_name = readstring inch in
	  let server_name = readstring inch in
	  let req_server_port_again = readstring inch in
	  let server_protocol = readstring inch in
	  let server_signature = readstring inch in
	  let server_software = readstring inch in
	  let jserv_route = readstring inch in
	  let _ = readstring inch in
	  let _ = readstring inch in
	  let mark = ref (readmark inch) in
	  let props = ref [] in
	  while !mark = 5 do
	    let key = readstring inch in
	    let value = readstring inch in
	    begin match key,value with
		Some k, Some v -> props := (k,v) :: !props;
	      | _ -> ()
	    end;
	    mark := readmark inch
	  done;
	  let headers = ref [] in
	  while !mark = 3 do
	    let name = readstring inch in
	    let value = readstring inch in
	    begin match name,value with
		Some n, Some v -> headers := (n,v) :: !headers;
	      | _ -> ()
	    end;
	    mark := readmark inch
	  done;
	  if !mark <> 4 then raise End_of_file;
	  (* Fixups for mod_jk: *)
	  let req_servlet, path_info, script_name =
	    let uri = match req_uri with None -> "" | Some s -> s in
	    jk_fixups jk_servletSubString uri req_servlet path_info script_name in
	  (* Create a custom environment: *)
	  let server_port =
	    match req_server_port with
		None -> None
	      | Some p -> ( try Some(int_of_string p) with _ -> None )
	  in
	  let other_props =
	    include_if "DOCUMENT_ROOT" req_docroot @
	    include_if "REQUEST_URI" req_uri @
	    include_if "SCRIPT_FILENAME" req_filename @
	    include_if "JSERV_ZONE" req_zone @
	    include_if "JSERV_SERVLET" req_servlet @
	    include_if "JSERV_ROUTE" jserv_route @
	    include_if "HOSTNAME" req_hostname
	  in
	  let env = new Netcgi_env.custom_environment ?config () in
	  env # set_cgi
	    ~gateway_interface:"JSERV/1.2"
	    ?server_software
	    ?server_name
	    ?server_protocol
	    ~server_port
	    ?request_method
	    ?path_info
	    ?path_translated
	    ?script_name
	    ?query_string
	    ?remote_host
	    ?remote_addr
	    ?auth_type
	    ?remote_user
	    ~https
	    ();
	  List.iter
	    (fun p ->
	       env # set_cgi ~property:p ())
	    (List.rev !props @ other_props);
	  env # set_input_header_fields (List.rev !headers);
	  env # set_input_state `Received_header;
	  env # set_input_ch inch;
	  env # set_output_ch outch;
	  env # setup_finished();
	  begin try
	    cgi_handler req_zone req_servlet (env :> Netcgi_env.cgi_environment);
	    (* Check the env state *)
	    begin match env # output_state with
		`Start -> 
		  failwith "Handler returned but did not output anything"
	      | `Sent_header ->
		  (* Ok, but will result in an empty reply *)
		  ()
	      | `Sent_body ->
		  (* Ok *)
		  ()
	      | `End ->
		  (* Ok *)
		  ()
	      | _ ->
		  failwith "Strange output state"
	    end;
	    close_out outch;
	    close_in  inch;
	  with
	      err ->
		(* Try to output error: *)
		begin match env # output_state with
		    `Start ->
		      env # set_output_header_fields
			[ "Status", "500 Internal Server Error";
			  "Servlet-Error", (String.escaped (Printexc.to_string
							      err));
			];
		      env # send_output_header();
		      close_out outch;
		      close_in  inch;
		  | _ ->
		      (* It is not possible to forward the error *)
		      ()
		end
	  end    

      | 254 ->
	  (* Signal request: *)
	  let signal = readmark inch in
	  begin match signal with
	      1 -> (* restart *)
		close_in inch;
		close_out outch;
		raise Signal_restart
	    | 15 -> (* termination *)
		close_in inch;
		close_out outch;
		raise Signal_shutdown
	    | 0 -> (* ping *)
		(* Send one byte as reply *)
		outch # output_byte 255;
		close_out outch;
		close_in inch;
	    | _ -> (* unknown *)
		raise End_of_file
	  end
      | _ ->
	  (* Unknown request: *)
	  raise End_of_file
  with
      End_of_file ->
	close_in inch;
	close_out outch;
    | any ->
	close_in inch;
	close_out outch;
	raise any
;;
