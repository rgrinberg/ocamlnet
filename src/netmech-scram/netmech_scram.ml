(* $Id$ *)

(* Steps:

   client               <->               server
   ----------------------------------------------------------------------
   username, nonce       ->
                         <-               salt, i, nonce'
   clientproof, nonce'   ->
     (=algo(password, salt, i))
                         <-               serversignature
 *)

open Printf

type ptype = [ `GSSAPI ]

type mechanism = [ `SHA_1 ]

type profile =
    { ptype : ptype;
      mechanism : mechanism;
      return_unknown_user : bool;
      iteration_count_limit : int;
    }


type client_first =  (* actually client_first_bare *)
    { c1_username : string;  (* "=xx" encoding not yet applied *)
      c1_nonce : string;     (* anything but comma *)
      c1_extensions : (string * string) list
    }

type server_first =
    { s1_nonce : string;     (* anything but comma *)
      s1_salt : string;      (* decoded *)
      s1_iteration_count : int;
      s1_extensions : (string * string) list
    }

type client_final =
    { cf_nonce : string;     (* anything but comma *)
      cf_extensions : (string * string) list;
      cf_proof : string option;   (* decoded *)
    }

type server_error =
    [ `Invalid_encoding
    | `Extensions_not_supported
    | `Invalid_proof
    | `Channel_bindings_dont_match
    | `Server_does_support_channel_binding
    | `Channel_binding_not_supported
    | `Unsupported_channel_binding_type
    | `Unknown_user
    | `Invalid_username_encoding
    | `No_resources
    | `Other_error
    | `Extension of string
    ]

type server_error_or_verifier =
    [ `Error of server_error
    | `Verifier of string
    ]

type server_final =
    { sf_error_or_verifier : server_error_or_verifier;
      sf_extensions : (string * string) list;
    }

type client_session =
    { cs_profile : profile;
      mutable cs_state :
                 [ `Start | `C1 | `S1 | `CF | `SF | `Connected | `Error ];
      mutable cs_c1 : client_first option;
      mutable cs_s1 : server_first option;
      mutable cs_s1_raw : string;
      mutable cs_cf : client_final option;
      mutable cs_sf : server_final option;
      mutable cs_salted_pw : string;
      mutable cs_auth_message : string;
      cs_username : string;
      cs_password : string;
    }


type server_session =
    { ss_profile : profile;
      mutable ss_state :
                 [ `Start | `C1 | `S1 | `CF | `SF | `Connected | `Error ];
      mutable ss_c1 : client_first option;
      mutable ss_c1_raw : string;
      mutable ss_s1 : server_first option;
      mutable ss_s1_raw : string;
      mutable ss_cf : client_final option;
      mutable ss_cf_raw : string;
      mutable ss_sf : server_final option;
      mutable ss_spw: string option;
      mutable ss_err : server_error option;
      ss_authenticate : string -> (string * string * int);
    }

(* Exported: *)
exception Invalid_encoding of string * string
exception Invalid_username_encoding of string * string
exception Extensions_not_supported of string * string
exception Protocol_error of string
exception Invalid_server_signature
exception Server_error of server_error

(* Not exported: *)
exception Invalid_proof of string


module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netmech_scram" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netmech_scram" Debug.enable

let () =
  Netlog.Debug.register_module "Netmech_scram" Debug.enable



let profile ?(return_unknown_user=false) ?(iteration_count_limit=100000) pt =
  { ptype = pt;
    mechanism = `SHA_1;
    return_unknown_user = return_unknown_user;
    iteration_count_limit = iteration_count_limit
  }


let saslprep s =
  (* We do not want to implement SASLprep here. It's brainf*ck, because the
     ambiguities it resolves do not occur in practice (people are not as
     dumb as the Unicode guys think). The RFC says we have to limit the
     strings then to US-ASCII.
   *)
  for k = 0 to String.length s - 1 do
    let c = s.[k] in
    if c < '\x20' || c >= '\x7f' then
      raise(Invalid_encoding("Netmech_scram.saslprep: restricted to US-ASCII",
			     s));
  done;
  s


let username_saslprep s =
  try
    saslprep s
  with
    | Invalid_encoding(s1,s2) ->
	raise(Invalid_username_encoding(s1,s2))


let comma_re = Netstring_pcre.regexp ","

let comma_split s =
  Netstring_pcre.split_delim comma_re s

let n_value_re = Netstring_pcre.regexp "([a-zA-Z])=(.*)"

let n_value_split s =
  match Netstring_pcre.string_match n_value_re s 0 with
    | None -> raise (Invalid_encoding("n_value_split", s))
    | Some r ->
	(Netstring_pcre.matched_group r 1 s,
	 Netstring_pcre.matched_group r 2 s)

let check_value_safe_chars s =
  let enc =
    `Enc_subset(`Enc_utf8,
	    fun i -> i <> 0 && i <> 0x2c && i <> 0x3d) in
  try
    Netconversion.verify enc s
  with _ -> raise(Invalid_encoding("check_value_safe_chars",s))

let check_value_chars s =
  let enc =
    `Enc_subset(`Enc_utf8,
		fun i -> i <> 0 && i <> 0x2c) in
  try
    Netconversion.verify enc s
  with _ -> raise(Invalid_encoding("check_value_chars",s))

let check_printable s =
  for i = 0 to String.length s - 1 do
    match s.[i] with
      | '\x21'..'\x2b' -> ()
      | '\x2d'..'\x7e' -> ()
      | _ -> raise(Invalid_encoding("check_printable",s))
  done

let pos_re = Netstring_pcre.regexp "[1-9][0-9]+$"

let check_positive_number s =
  match Netstring_pcre.string_match pos_re s 0 with
    | None -> raise(Invalid_encoding("check_positive_number",s))
    | Some _ -> ()

let comma_slash_re = Netstring_pcre.regexp "[,/]"

let rev_comma_slash_re = Netstring_pcre.regexp "(=2C|=3D|=|,)"

let encode_saslname s =
  ( try
      Netconversion.verify `Enc_utf8 s
    with _ -> raise(Invalid_username_encoding("encode_saslname",s))
  );
  Netstring_pcre.global_substitute
    comma_slash_re
    (fun r s ->
       match Netstring_pcre.matched_string r s with
	 | "," -> "=2C"
	 | "/" -> "=3D"
	 | _ -> assert false
    )
    s

let decode_saslname s =
  let s' =
    Netstring_pcre.global_substitute
      rev_comma_slash_re
      (fun r s ->
	 match Netstring_pcre.matched_string r s with
	   | "=2C" -> ","
	   | "=3D" -> "/"
	   | "=" | "," -> raise(Invalid_username_encoding("decode_saslname",s))
	   | _ -> assert false
      )
      s in
  ( try
      Netconversion.verify `Enc_utf8 s'
    with _ -> raise(Invalid_username_encoding("decode_saslname",s))
  );
  s'


let encode_c1_message c1 =
  (* No gs2-header in GSS-API *)
  "n=" ^ encode_saslname(username_saslprep c1.c1_username) ^ 
  ",r=" ^ c1.c1_nonce ^ 
  (if c1.c1_extensions <> [] then 
      "," ^ 
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) c1.c1_extensions)
   else ""
  )  


let decode_c1_message s =
  let l = List.map n_value_split (comma_split s) in
  match l with
    | [] ->
	raise(Invalid_encoding("decode_c1_mesage: empty", s))
    | ("m",_) :: _ ->
	raise(Extensions_not_supported("decode_c1_mesage: unsupported", s))
    | ("n", username_raw) :: ("r", nonce) :: l' ->
	let username = decode_saslname username_raw in
	let username' = username_saslprep username in
	if username <> username' then
	  raise(Invalid_username_encoding("Netmech_scram.decode_c1_message",
					  s));
	{ c1_username = username;
	  c1_nonce = nonce;
	  c1_extensions = l'
	}
    | _ ->
	raise(Invalid_encoding("decode_c1_mesage", s))


let encode_s1_message s1 =
  "r=" ^ s1.s1_nonce ^
  ",s=" ^ Netencoding.Base64.encode s1.s1_salt ^ 
  ",i=" ^ string_of_int s1.s1_iteration_count ^ 
  ( if s1.s1_extensions <> [] then
      "," ^ 
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) s1.s1_extensions)
   else ""
  )  


let decode_s1_message s =
  let l = List.map n_value_split (comma_split s) in
  match l with
    | [] ->
	raise(Invalid_encoding("decode_s1_mesage: empty", s))
    | ("m",_) :: _ ->
	raise(Extensions_not_supported("decode_s1_mesage: unsupported", s))
    | ("r",nonce) :: ("s",salt_b64) :: ("i",icount_raw) :: l' ->
	let salt =
	  try Netencoding.Base64.decode salt_b64
	  with _ ->
	    raise(Invalid_encoding("decode_s1_message: invalid s", s)) in
	check_positive_number icount_raw;
	let icount = 
	  try int_of_string icount_raw 
	  with _ -> 
	    raise(Invalid_encoding("decode_s1_message: invalid i", s)) in
	{ s1_nonce = nonce;
	  s1_salt = salt;
	  s1_iteration_count = icount;
	  s1_extensions = l'
	}
    | _ ->
	raise(Invalid_encoding("decode_s1_mesage", s))

	
let encode_cf_message cf =
  (* No channel binding in GSS-API *)
  "r=" ^ cf.cf_nonce ^ 
  ( if cf.cf_extensions <> [] then
      "," ^ 
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) cf.cf_extensions)
   else ""
  ) ^ 
  ( match cf.cf_proof with
      | None -> ""
      | Some p ->
	  ",p=" ^ Netencoding.Base64.encode p
  )


let decode_cf_message expect_proof s =
  let l = List.map n_value_split (comma_split s) in
  match l with
    | [] ->
	raise(Invalid_encoding("decode_cf_mesage: empty", s))
    | ("r",nonce) :: l' ->
	let p, l'' =
	  if expect_proof then
	    match List.rev l' with
	      | ("p", proof_b64) :: l''_rev ->
		  let p = 
		    try Netencoding.Base64.decode proof_b64 
		    with _ ->
		      raise(Invalid_encoding("decode_cf_mesage: invalid p",
					     s)) in
		  (Some p, List.rev l''_rev)
	      | _ ->
		  raise(Invalid_encoding("decode_cf_mesage: proof not found",
					 s))
	  else
	    None, l' in
	{ cf_nonce = nonce;
	  cf_extensions = l'';
	  cf_proof = p
	}
    | _ ->
	raise(Invalid_encoding("decode_cf_mesage", s))

let strip_cf_proof s =
  let l = List.rev (List.map n_value_split (comma_split s)) in
  match l with
    | ("p",_) :: l' ->
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) (List.rev l'))
    | _ ->
	assert false


let string_of_server_error =
  function
    | `Invalid_encoding -> "invalid-encoding"
    | `Extensions_not_supported -> "extensions-not-supported"
    | `Invalid_proof -> "invalid-proof"
    | `Channel_bindings_dont_match -> "channel-bindings-dont-match"
    | `Server_does_support_channel_binding -> 
	"server-does-support-channel-binding"
    | `Channel_binding_not_supported -> "channel-binding-not-supported"
    | `Unsupported_channel_binding_type -> "unsupported-channel-binding-type"
    | `Unknown_user -> "unknown-user"
    | `Invalid_username_encoding -> "invalid-username-encoding"
    | `No_resources -> "no-resources"
    | `Other_error -> "other-error"
    | `Extension s -> s

let server_error_of_string =
  function
    | "invalid-encoding" -> `Invalid_encoding
    | "extensions-not-supported" -> `Extensions_not_supported
    | "invalid-proof" -> `Invalid_proof
    | "channel-bindings-dont-match" -> `Channel_bindings_dont_match
    | "server-does-support-channel-binding" ->
	`Server_does_support_channel_binding
    | "channel-binding-not-supported" -> `Channel_binding_not_supported
    | "unsupported-channel-binding-type" -> `Unsupported_channel_binding_type
    | "unknown-user" -> `Unknown_user
    | "invalid-username-encoding" -> `Invalid_username_encoding
    | "no-resources" -> `No_resources
    | "other-error" -> `Other_error
    | s -> `Extension s


let () =
  Netexn.register_printer
    (Server_error `Invalid_encoding)
    (fun e ->
       match e with
	 | Server_error token ->
	     sprintf "Server_error(%s)" (string_of_server_error token)
	 | _ -> assert false
    )


let encode_sf_message sf =
  ( match sf.sf_error_or_verifier with
      | `Error e ->
	  "e=" ^ string_of_server_error e
      | `Verifier v ->
	  "v=" ^ Netencoding.Base64.encode v
  ) ^
  ( if sf.sf_extensions <> [] then
      "," ^ 
	String.concat "," (List.map (fun (n,v) -> n ^ "=" ^ v) sf.sf_extensions)
   else ""
  )


let decode_sf_message s =
  let l = List.map n_value_split (comma_split s) in
  match l with
    | [] ->
	raise(Invalid_encoding("decode_cf_mesage: empty", s))
    | ("v",verf_raw) :: l' ->
	let verf =
	  try Netencoding.Base64.decode verf_raw 
	  with _ -> 
	    raise(Invalid_encoding("decode_sf_message: invalid v", s)) in
	{ sf_error_or_verifier = `Verifier verf;
	  sf_extensions = l'
	}
    | ("e",error_s) :: l' ->
	let error = server_error_of_string error_s in
	{ sf_error_or_verifier = `Error error;
	  sf_extensions = l'
	}
    | _ ->
	raise(Invalid_encoding("decode_sf_mesage", s))


let sha1 s =
  Cryptokit.hash_string (Cryptokit.Hash.sha1()) s

let hmac key str =
  Netauth.hmac
    ~h:sha1
    ~b:64
    ~l:20
    ~k:key
    ~message:str

let int_s i =
  let s = String.make 4 '\000' in
  s.[0] <- Char.chr ((i lsr 24) land 0xff);
  s.[1] <- Char.chr ((i lsr 16) land 0xff);
  s.[2] <- Char.chr ((i lsr 8) land 0xff);
  s.[3] <- Char.chr (i land 0xff);
  s

let hi str salt i =
  let rec uk k =
    if k=1 then
      let u = hmac str (salt ^ int_s i) in
      let h = u in
      (u,h)
    else (
      let (u_pred, h_pred) = uk (k-1) in
      let u = hmac str u_pred in
      let h = Netauth.xor_s u h_pred in
      (u,h)
    ) in
  snd (uk i)


let create_nonce() =
  let s = String.make 16 ' ' in
  Netsys_rng.fill_random s;
  Digest.to_hex s


let create_salt = create_nonce


let create_client_session profile username password =
  ignore(saslprep username);
  ignore(saslprep password);  (* Check for errors *)
  { cs_profile = profile;
    cs_state = `Start;
    cs_c1 = None;
    cs_s1 = None;
    cs_s1_raw = "";
    cs_cf = None;
    cs_sf = None;
    cs_auth_message = "";
    cs_salted_pw = "";
    cs_username = username;
    cs_password = password
  }
 

let client_emit_flag cs =
  match cs.cs_state with
    | `Start | `S1 -> true
    | _ -> false


let client_recv_flag cs =
  match cs.cs_state with
    | `C1 | `CF -> true
    | _ -> false


let client_finish_flag cs =
  cs.cs_state = `Connected


let client_error_flag cs =
  cs.cs_state = `Error


let catch_error cs f arg =
  try
    f arg
  with
    | error ->
	dlog (sprintf "Client caught error: %s"
		(Netexn.to_string error));
	cs.cs_state <- `Error;
	raise error


let salt_password password salt iteration_count =
  let sp = hi (saslprep password) salt iteration_count in
  (* eprintf "salt_password(%S,%S,%d) = %S\n" password salt iteration_count sp; *)
  sp


let client_emit_message cs =
  catch_error cs
    (fun () ->
       match cs.cs_state with
	 | `Start ->
	     let c1 =
	       { c1_username = cs.cs_username;
		 c1_nonce = create_nonce();
		 c1_extensions = []
	       } in
	     cs.cs_c1 <- Some c1;
	     cs.cs_state <- `C1;
	     let m = encode_c1_message c1 in
	     dlog (sprintf "Client state `Start emitting message: %s" m);
	     m
	       
	 | `S1 ->
	     let c1 =
	       match cs.cs_c1 with None -> assert false | Some c1 -> c1 in
	     let s1 =
	       match cs.cs_s1 with None -> assert false | Some s1 -> s1 in
	     let salted_pw = 
	       salt_password cs.cs_password s1.s1_salt s1.s1_iteration_count in
	     let client_key = hmac salted_pw "Client Key" in
	     let stored_key = sha1 client_key in
	     let cf_no_proof =
	       encode_cf_message { cf_nonce = s1.s1_nonce;
				   cf_extensions = [];
				   cf_proof = None
				 } in
	     let auth_message =
	       encode_c1_message c1 ^ "," ^ 
		 cs.cs_s1_raw ^ "," ^ 
		 cf_no_proof in
	     let client_signature = hmac stored_key auth_message in
	     let p = Netauth.xor_s client_key client_signature in
	     let cf =
	       { cf_nonce = s1.s1_nonce;
		 cf_extensions = [];
		 cf_proof = Some p;
	       } in
	     cs.cs_cf <- Some cf;
	     cs.cs_state <- `CF;
	     cs.cs_auth_message <- auth_message;
	     cs.cs_salted_pw <- salted_pw;
	     let m = encode_cf_message cf in
	     dlog (sprintf "Client state `S1 emitting message: %s" m);
	     m
	       
	 | _ ->
	     failwith "Netmech_scram.client_emit_message"
    )
    ()


let client_recv_message cs message =
  catch_error cs
    (fun () ->
       match cs.cs_state with
	 | `C1 ->
	     dlog (sprintf "Client state `C1 receiving message: %s" message);
	     let s1 = decode_s1_message message in
	     let c1 =
	       match cs.cs_c1 with None -> assert false | Some c1 -> c1 in
	     if String.length s1.s1_nonce < String.length c1.c1_nonce then
	       raise (Protocol_error
			"client_recv_message: Nonce from the server is too short");
	     if String.sub s1.s1_nonce 0 (String.length c1.c1_nonce) <> c1.c1_nonce
	     then
	       raise (Protocol_error
			"client_recv_message: bad nonce from the server");
	     if s1.s1_iteration_count > cs.cs_profile.iteration_count_limit then
	       raise (Protocol_error
			"client_recv_message: iteration count too high");
	     cs.cs_state <- `S1;
	     cs.cs_s1 <- Some s1;
	     cs.cs_s1_raw <- message
	       
	 | `CF ->
	     dlog (sprintf "Client state `CF receiving message: %s" message);
	     let sf = decode_sf_message message in
	     ( match sf.sf_error_or_verifier with
		 | `Verifier v ->
		     let salted_pw = cs.cs_salted_pw in
		     let server_key =
		       hmac salted_pw "Server Key" in
		     let server_signature =
		       hmac server_key cs.cs_auth_message in
		     if v <> server_signature then
		       raise Invalid_server_signature;
		     cs.cs_state <- `Connected;
		     dlog "Client is authenticated"
		 | `Error e ->
		     cs.cs_state <- `Error;
		     dlog (sprintf "Client got error token from server: %s"
			     (string_of_server_error e));
		     raise(Server_error e)
	     )
	       
	 | _ ->
	     failwith "Netmech_scram.client_recv_message"
    )
    ()


let create_server_session profile auth =
  (* auth: called as: let (salted_pw, salt, i) = auth username *)
  { ss_profile = profile;
    ss_state = `Start;
    ss_c1 = None;
    ss_c1_raw = "";
    ss_s1 = None;
    ss_s1_raw = "";
    ss_cf = None;
    ss_cf_raw = "";
    ss_sf = None;
    ss_authenticate = auth;
    ss_spw = None;
    ss_err = None;
  }


let server_emit_flag ss =
  match ss.ss_state with
    | `C1 | `CF -> true
    | _ -> false

let server_recv_flag ss =
  match ss.ss_state with
    | `Start | `S1 -> true
    | _ -> false

let server_finish_flag ss =
  ss.ss_state = `Connected

let server_error_flag ss =
  ss.ss_state = `Error

let catch_condition ss f arg =
  let debug e =
    dlog (sprintf "Server caught error: %s"
	    (Netexn.to_string e)) in
  try
    f arg
  with
    (* After such an error the protocol will continue, but the final
       server message will return the condition
     *)
    | Invalid_encoding(_,_) as e ->
	debug e;
	if ss.ss_err = None then
	  ss.ss_err <- Some `Invalid_encoding
    | Invalid_username_encoding _ as e ->
	debug e;
	if ss.ss_err = None then
	  ss.ss_err <- Some `Invalid_username_encoding
    | Extensions_not_supported(_,_) as e ->
	debug e;
	if ss.ss_err = None then
	  ss.ss_err <- Some `Extensions_not_supported
    | Invalid_proof _ as e ->
	debug e;
	if ss.ss_err = None then
	  ss.ss_err <- Some `Invalid_proof
	

exception Skip_proto


let server_emit_message ss =
  match ss.ss_state with
    | `C1 ->
	let m =
	  try
	    let c1 = 
	      match ss.ss_c1 with
		| None -> raise Skip_proto | Some c1 -> c1 in
	    let (spw, salt, i) = ss.ss_authenticate c1.c1_username in
	    let s1 =
	      { s1_nonce = c1.c1_nonce ^ create_nonce();
		s1_salt = salt;
		s1_iteration_count = i;
		s1_extensions = []
	      } in
	    ss.ss_state <- `S1;
	    ss.ss_s1 <- Some s1;
	    ss.ss_spw <- Some spw;
	    let s1 = encode_s1_message s1 in
	    ss.ss_s1_raw <- s1;
	    s1
	  with Not_found | Skip_proto ->
	    (* continue with a dummy auth *)
	    dlog "Server does not know this user";
	    let c1_nonce =
	      match ss.ss_c1 with
		| None -> create_nonce() | Some c1 -> c1.c1_nonce in
	    let s1 =
	      { s1_nonce = c1_nonce ^ create_nonce();
		s1_salt = create_nonce();
		s1_iteration_count = 4096;
		s1_extensions = []
	      } in
	    ss.ss_state <- `S1;
	    ss.ss_s1 <- Some s1;
	    if ss.ss_err = None then
	      ss.ss_err <- Some (if ss.ss_profile.return_unknown_user then
				   `Unknown_user
				 else
				   `Invalid_proof);
	    (* This will keep the client off being successful *)
	    let s1 = encode_s1_message s1 in
	    ss.ss_s1_raw <- s1;
	    s1
	in
	dlog (sprintf "Server state `C1 emitting message: %s" m);
	m
	  
    | `CF ->
	( match ss.ss_err with
	    | Some err ->
		let sf =
		  { sf_error_or_verifier = `Error err;
		    sf_extensions = []
		  } in
		ss.ss_sf <- Some sf;
		ss.ss_state <- `Error;
		let m = encode_sf_message sf in
		dlog (sprintf "Server state `CF[Err] emitting message: %s" m);
		m
		  
	    | None ->
		let spw =
		  match ss.ss_spw with
		    | None -> assert false | Some spw -> spw in
		let cf_no_proof = strip_cf_proof ss.ss_cf_raw in
		let auth_message =
		  ss.ss_c1_raw ^ "," ^ 
		    ss.ss_s1_raw ^ "," ^ 
		    cf_no_proof in
		let server_key =
		  hmac spw "Server Key" in
		let server_signature =
		  hmac server_key auth_message in
		let sf =
		  { sf_error_or_verifier = `Verifier server_signature;
		    sf_extensions = []
		  } in
		ss.ss_sf <- Some sf;
		ss.ss_state <- `Connected;
		let m = encode_sf_message sf in
		dlog (sprintf "Server state `CF emitting message: %s" m);
		m
	)
	  
    | _ ->
	failwith "Netmech_scram.server_emit_message"


let server_recv_message ss message =
  match ss.ss_state with
    | `Start ->
	dlog (sprintf "Server state `Start receiving message: %s" message);

	catch_condition ss
	  (fun () ->
	     let c1 = decode_c1_message message in
	     ss.ss_c1 <- Some c1;
	  ) ();
	ss.ss_c1_raw <- message;
	ss.ss_state <- `C1
	  (* Username is checked later *)
    | `S1 ->
	dlog (sprintf "Server state `S1 receiving message: %s" message);

	catch_condition ss
	  (fun () ->
	     try
	       let s1 =
		 match ss.ss_s1 with
		   | None -> raise Skip_proto | Some s1 -> s1 in
	       let salted_pw =
		 match ss.ss_spw with
		   | None -> raise Skip_proto | Some spw -> spw in
	       let cf = decode_cf_message true message in
	       if s1.s1_nonce <> cf.cf_nonce then
		 raise (Invalid_proof "nonce mismatch");
	       let client_key = hmac salted_pw "Client Key" in
	       let stored_key = sha1 client_key in
	       let cf_no_proof = strip_cf_proof message in
	       let auth_message =
		 ss.ss_c1_raw ^ "," ^ 
		   ss.ss_s1_raw ^ "," ^ 
		   cf_no_proof in
	       let client_signature = hmac stored_key auth_message in
	       let p = Netauth.xor_s client_key client_signature in
	       if Some p <> cf.cf_proof then
		 raise (Invalid_proof "bad client signature");
	     with
	       | Skip_proto -> ()
	  ) ();
	ss.ss_cf_raw <- message;
	ss.ss_state <- `CF
    | _ ->
	failwith "Netmech_scram.server_recv_message"
