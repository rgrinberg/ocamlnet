#use "topfind";;
#require "netmech-scram";;

open Printf;;

let manipulate_rng() =
  Netsys_rng.set_rng
    (fun s ->
       for k = 0 to String.length s - 1 do
	 s.[k] <- Char.chr (k land 0xff)
       done
    );;

let () = manipulate_rng();;

let client_key_ring =
object
  method password_of_user_name =
    function
      | "gerd" -> "xyz"
      | _ -> raise Not_found
  method default_user_name = Some "gerd"
end

let salt = "16dc2b4137e71d6470fdd5f0dfff6659"
let iteration_count = 4096

let server_key_verifier =
object
  method scram_credentials =
    function
      | "gerd" ->
	  let salted_pw = 
	    Netmech_scram.salt_password "xyz" salt  iteration_count in
	  (salted_pw, salt, iteration_count)
      | _ -> raise Not_found
end

let profile =
  Netmech_scram.profile `GSSAPI

let c =
  new Netmech_scram_gssapi.scram_gss_api
    ~client_key_ring
    profile

let s =
  new Netmech_scram_gssapi.scram_gss_api
    ~server_key_verifier
    profile

(* Simulate context creation: *)

let tok1, c_ctx =
  c # init_sec_context
    ~initiator_cred:c#no_credential
    ~context:None
    ~target_name:c#no_name
    ~mech_type:Netmech_scram_gssapi.scram_mech
    ~req_flags:[]
    ~time_rec:None
    ~chan_bindings:None
    ~input_token:None
    ~out:(fun ~actual_mech_type ~output_context
	       ~output_token ~ret_flags ~time_rec
	      ~minor_status ~major_status () ->
		let (ce,re,flags) = major_status in
		if ce <> `None || re <> `None then
		  failwith("tok1: " ^ 
			     Netgssapi.string_of_major_status major_status);
		if not(List.mem `Continue_needed flags) then
		  failwith("tok1: no continue");
		let ctx =
		  match output_context with
		    | None -> failwith "tok1: no context"
		    | Some ctx -> ctx in
		(output_token, ctx)
	 )
    ()

let tok2, s_ctx =
  s # accept_sec_context
    ~context:None
    ~acceptor_cred:s#no_credential
    ~input_token:tok1
    ~chan_bindings:None
    ~out:(fun ~src_name ~mech_type ~output_context ~output_token
	      ~ret_flags ~time_rec ~delegated_cred ~minor_status
	      ~major_status () ->
		let (ce,re,flags) = major_status in
		if ce <> `None || re <> `None then
		  failwith("tok2: " ^ 
			     Netgssapi.string_of_major_status major_status);
		if not(List.mem `Continue_needed flags) then
		  failwith("tok2: no continue");
		let ctx =
		  match output_context with
		    | None -> failwith "tok2: no context"
		    | Some ctx -> ctx in
		(output_token, ctx)
	 )
    ()

let tok3 =
  c # init_sec_context
    ~initiator_cred:c#no_credential
    ~context:(Some c_ctx)
    ~target_name:c#no_name
    ~mech_type:Netmech_scram_gssapi.scram_mech
    ~req_flags:[]
    ~time_rec:None
    ~chan_bindings:None
    ~input_token:(Some tok2)
    ~out:(fun ~actual_mech_type ~output_context
	      ~output_token ~ret_flags ~time_rec
	      ~minor_status ~major_status () ->
		let (ce,re,flags) = major_status in
		if ce <> `None || re <> `None then
		  failwith("tok3: " ^ 
			     Netgssapi.string_of_major_status major_status);
		if not (List.mem `Continue_needed flags) then
		  failwith("tok3: no continue");
		output_token
	 )
    ()

let tok4 =
  s # accept_sec_context
    ~context:(Some s_ctx)
    ~acceptor_cred:s#no_credential
    ~input_token:tok3
    ~chan_bindings:None
    ~out:(fun ~src_name ~mech_type ~output_context ~output_token
	      ~ret_flags ~time_rec ~delegated_cred ~minor_status
	      ~major_status () ->
		let (ce,re,flags) = major_status in
		if ce <> `None || re <> `None then
		  failwith("tok4: " ^ 
			     Netgssapi.string_of_major_status major_status);
		if (List.mem `Continue_needed flags) then
		  failwith("tok4: no continue");
		output_token
	 )
    ()

let () =
  c # init_sec_context
    ~initiator_cred:c#no_credential
    ~context:(Some c_ctx)
    ~target_name:c#no_name
    ~mech_type:Netmech_scram_gssapi.scram_mech
    ~req_flags:[]
    ~time_rec:None
    ~chan_bindings:None
    ~input_token:(Some tok4)
    ~out:(fun ~actual_mech_type ~output_context
	      ~output_token ~ret_flags ~time_rec
	      ~minor_status ~major_status () ->
		let (ce,re,flags) = major_status in
		if ce <> `None || re <> `None then
		  failwith("tok3: " ^ 
			     Netgssapi.string_of_major_status major_status);
		if (List.mem `Continue_needed flags) then
		  failwith("tok4: continue");
		if output_token <> "" then
		  failwith("tok4: not empty");
		()
	 )
    ()


(* Create a MIC code on the client, and verify it on the server: *)

let msg = "abcDEF"

let mic1 =
  c # get_mic
    ~context:c_ctx
    ~qop_req:None
    ~message:[Xdr_mstring.string_to_mstring  msg]
    ~out:(fun ~msg_token ~minor_status ~major_status () ->
	    let (ce,re,flags) = major_status in
	    if ce <> `None || re <> `None then
	      failwith("mic1: " ^ 
			 Netgssapi.string_of_major_status major_status);
	    msg_token
	 )
    ()

let() =
  s # verify_mic
    ~context:s_ctx
    ~message:[Xdr_mstring.string_to_mstring  msg]
    ~token:mic1
    ~out:(fun ~qop_state ~minor_status ~major_status () ->
	    let (ce,re,flags) = major_status in
	    if ce <> `None || re <> `None then
	      failwith("mic1 verification: " ^ 
			 Netgssapi.string_of_major_status major_status);
	 )
    ()

(* Create a MIC code on the server, and verify it on the client: *)

let mic2 =
  s # get_mic
    ~context:s_ctx
    ~qop_req:None
    ~message:[Xdr_mstring.string_to_mstring  msg]
    ~out:(fun ~msg_token ~minor_status ~major_status () ->
	    let (ce,re,flags) = major_status in
	    if ce <> `None || re <> `None then
	      failwith("mic2: " ^ 
			 Netgssapi.string_of_major_status major_status);
	    msg_token
	 )
    ()

let() =
  c # verify_mic
    ~context:c_ctx
    ~message:[Xdr_mstring.string_to_mstring  msg]
    ~token:mic2
    ~out:(fun ~qop_state ~minor_status ~major_status () ->
	    let (ce,re,flags) = major_status in
	    if ce <> `None || re <> `None then
	      failwith("mic2 verification: " ^ 
			 Netgssapi.string_of_major_status major_status);
	 )
    ()

(* Wrap a message on the client and unwrap it on the server: *)

let enc_msg1 =
  c # wrap
    ~context:c_ctx
    ~conf_req:true
    ~qop_req:None
    ~input_message:[Xdr_mstring.string_to_mstring  msg]
    ~output_message_preferred_type:`String
    ~out:(fun ~conf_state ~output_message ~minor_status ~major_status () ->
	    let (ce,re,flags) = major_status in
	    if ce <> `None || re <> `None then
	      failwith("enc_msg1: " ^ 
			 Netgssapi.string_of_major_status major_status);
	    output_message
	 )
    ()

let dec_msg1 =
  s # unwrap
    ~context:s_ctx
    ~input_message:enc_msg1
    ~output_message_preferred_type:`String
    ~out:(fun ~output_message ~conf_state ~qop_state ~minor_status
	      ~major_status () ->
	    let (ce,re,flags) = major_status in
	    if ce <> `None || re <> `None then
	      failwith("dec_msg1: " ^ 
			 Netgssapi.string_of_major_status major_status);
	    output_message
	 )
    ()

let () =
  if msg <> Xdr_mstring.concat_mstrings dec_msg1 then
    failwith "dec_msg1: cannot decode"

(* Wrap a message on the server and unwrap it on the client: *)

let enc_msg2 =
  s # wrap
    ~context:s_ctx
    ~conf_req:true
    ~qop_req:None
    ~input_message:[Xdr_mstring.string_to_mstring  msg]
    ~output_message_preferred_type:`String
    ~out:(fun ~conf_state ~output_message ~minor_status ~major_status () ->
	    let (ce,re,flags) = major_status in
	    if ce <> `None || re <> `None then
	      failwith("enc_msg2: " ^ 
			 Netgssapi.string_of_major_status major_status);
	    output_message
	 )
    ()

let dec_msg2 =
  c # unwrap
    ~context:c_ctx
    ~input_message:enc_msg2
    ~output_message_preferred_type:`String
    ~out:(fun ~output_message ~conf_state ~qop_state ~minor_status
	      ~major_status () ->
	    let (ce,re,flags) = major_status in
	    if ce <> `None || re <> `None then
	      failwith("dec_msg2: " ^ 
			 Netgssapi.string_of_major_status major_status);
	    output_message
	 )
    ()

let () =
  if msg <> Xdr_mstring.concat_mstrings dec_msg2 then
    failwith "dec_msg2: cannot decode"

(* helpers... *)

let dump s =
  let hex_line = Buffer.create 80 in
  let asc_line = Buffer.create 20 in
  let addr = ref 0 in

  let print_line n =
    printf "%04x: %-48s    %s\n"
      !addr (Buffer.contents hex_line) (Buffer.contents asc_line);
    Buffer.clear hex_line;
    Buffer.clear asc_line;
    addr := !addr + n in

  for k = 0 to String.length s - 1 do
    let c = s.[k] in
    bprintf hex_line "%02x " (Char.code c);
    if c >= ' ' && c <= '\x7e' then
      Buffer.add_char asc_line c
    else
      Buffer.add_char asc_line '.';
    if (k+1) mod 16 = 0 then print_line 16
  done;
  if Buffer.length asc_line > 0 then
    print_line 0
;;


