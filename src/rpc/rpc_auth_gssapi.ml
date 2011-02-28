(* $Id$ *)

(* TODO:
   - Develop ideas how to avoid that strings are copied that frequently 
   - error handling
   - sequence numbers
 *)

open Netgssapi
open Rpc_auth_gssapi_aux
open Printf

type rpc_context =
    { context : context;
      mutable ctx_continue : bool;
      ctx_handle : string;
      ctx_conn_id : Rpc_server.connection_id option;
      mutable ctx_svc_none : bool;       
        (* whether unprotected messages are ok *)
      mutable ctx_svc_integrity : bool;
        (* whether integrity-protected msgs are ok *)
      mutable ctx_svc_privacy : bool;
        (* whether privacy-protected msgs are ok *)
    }

type user_name_format =
    [ `Exported_name
    | `Exported_name_text
    | `Exported_name_no_oid
    ]

let as_string (sm,pos,len) =
  (* FIXME: also in netmech_scram_gssapi *)
  match sm with
    | `String s ->
        if pos=0 && len=String.length s then
          s
        else
          String.sub s pos len
    | `Memory m -> 
        let s = String.create len in
        Netsys_mem.blit_memory_to_string m pos s 0 len;
        s


let mk_mstring s =
  Xdr_mstring.string_based_mstrings # create_from_string
    s 0 (String.length s) false


let split_rpc_gss_data_t s =
  if String.length s < 4 then
    failwith "Rpc_auth_gssapi.split_rpc_gss_data_t";
  let seq_s = String.sub s 0 4 in
  let rest_s = String.sub s 4 (String.length s - 4) in
  let seq = Rtypes.read_uint4 seq_s 0 in
  (seq, rest_s)


let integrity_encoder (gss_api : Netgssapi.gss_api)
                      ctx cred1 rpc_gss_integ_data s =
  let data =
    Rtypes.uint4_as_string cred1.seq_num ^ s in
  let mic =
    gss_api # get_mic
      ~context:ctx.context
      ~qop_req:None
      ~message:(`String data, 0, String.length data)
      ~out:(fun ~msg_token ~minor_status ~major_status () ->
	      let (c_err, r_err, flags) = major_status in
	      if c_err <> `None || r_err <> `None then
		failwith("Rpc_auth_gssapi: \
                          Cannot obtain MIC: " ^ 
			   string_of_major_status major_status);
	      msg_token
	   )
      () in
  let integ =
    { databody_integ = data;
      checksum = mic;
    } in
  let xdr_val = Rpc_auth_gssapi_aux._of_rpc_gss_integ_data integ in
  Xdr.pack_xdr_value_as_string xdr_val rpc_gss_integ_data []


let integrity_decoder (gss_api : Netgssapi.gss_api)
                      ctx cred1 rpc_gss_integ_data s pos len =
  try
    let xdr_val =
      Xdr.unpack_xdr_value
	~pos ~len ~fast:true s rpc_gss_integ_data [] in
    let integ =
      _to_rpc_gss_integ_data xdr_val in
    let data =
      integ.databody_integ in
    gss_api # verify_mic
      ~context:ctx.context
      ~message:(`String data, 0, String.length data)
      ~token:integ.checksum
      ~out:(fun ~qop_state ~minor_status ~major_status () ->
	      let (c_err, r_err, flags) = major_status in
	      if c_err <> `None || r_err <> `None then
		failwith("Rpc_auth_gssapi: \
                          Cannot verify MIC: " ^ 
			   string_of_major_status major_status);
	   )
      ();
    let (seq, args) =
      split_rpc_gss_data_t data in
    if seq <> cred1.seq_num then
      failwith "Rpc_auth_gssapi: bad sequence number";
    (args, String.length args)
  with
    | _ ->
	failwith "Rpc_auth_gssapi: cannot decode integrity-proctected message"


let privacy_encoder (gss_api : Netgssapi.gss_api)
                     ctx cred1 rpc_gss_priv_data s =
  let data =
    Rtypes.uint4_as_string cred1.seq_num ^ s in
  gss_api # wrap
    ~context:ctx.context
    ~conf_req:true
    ~qop_req:None
    ~input_message:(`String data, 0, String.length data)
    ~output_message_preferred_type:`String
    ~out:(fun ~conf_state ~output_message ~minor_status ~major_status () ->
	    let (c_err, r_err, flags) = major_status in
	    if c_err <> `None || r_err <> `None then
	      failwith("Rpc_auth_gssapi: \
                          Cannot wrap message: " ^ 
			 string_of_major_status major_status);
	    if not conf_state then
	      failwith "Rpc_auth_gssapi: no privacy-ensuring wrapping possible";
	    let m = as_string output_message in
	    (* FIXME: it would be better if the result value of an encoder
	       was an mstring
	     *)
	    let priv =
	      { databody_priv = m } in
	    let xdr_val = Rpc_auth_gssapi_aux._of_rpc_gss_priv_data priv in
	    Xdr.pack_xdr_value_as_string xdr_val rpc_gss_priv_data []
	 )
    ()

let privacy_decoder (gss_api : Netgssapi.gss_api)
                     ctx cred1 rpc_gss_priv_data s pos len =
  try
    let xdr_val =
      Xdr.unpack_xdr_value
	~pos ~len ~fast:true s rpc_gss_priv_data [] in
    let priv =
      _to_rpc_gss_priv_data xdr_val in
    let data =
      priv.databody_priv in
    gss_api # unwrap
      ~context:ctx.context
      ~input_message:(`String data, 0, String.length data)
      ~output_message_preferred_type:`String
      ~out:(fun ~output_message ~conf_state ~qop_state ~minor_status 
	      ~major_status
	      () ->
		let (c_err, r_err, flags) = major_status in
		if c_err <> `None || r_err <> `None then
		  failwith("Rpc_auth_gssapi: \
                            Cannot unwrap message: " ^ 
			     string_of_major_status major_status);
		if not conf_state then
		  failwith 
		    "Rpc_auth_gssapi: no privacy-ensuring unwrapping possible";
		let m = as_string output_message in
		let (seq, args) =
		  split_rpc_gss_data_t m in
		if seq <> cred1.seq_num then
		  failwith "Rpc_auth_gssapi: bad sequence number";
		(args, String.length args)
	   )
      ()
  with
    | _ ->
	failwith "Rpc_auth_gssapi: cannot decode privacy-proctected message"


let server_auth_method 
      ?(require_privacy=false)
      ?(require_integrity=false)
      ?(shared_context=false)
      ?acceptor_cred
      ?(user_name_format = `Exported_name_no_oid)
      (gss_api : gss_api) : Rpc_server.auth_method =

  let acceptor_cred =
    match acceptor_cred with
      | None ->
	  gss_api # acquire_cred
	    ~desired_name:gss_api#no_name
	    ~time_req:`None
	    ~desired_mechs:[]
	    ~cred_usage:`Accept
	    ~out:(
	      fun ~cred ~actual_mechs ~time_rec ~minor_status ~major_status() ->
		let (c_err, r_err, flags) = major_status in
		if c_err <> `None || r_err <> `None then
		  failwith("Rpc_auth_gssapi: Cannot acquire default creds: " ^ 
			     string_of_major_status major_status);
		cred
	    )
	    ()
      | Some c -> c in

  let rpc_gss_cred_t =
    Xdr.validate_xdr_type
      Rpc_auth_gssapi_aux.xdrt_rpc_gss_cred_t in

  let rpc_gss_init_arg =
    Xdr.validate_xdr_type
      Rpc_auth_gssapi_aux.xdrt_rpc_gss_init_arg in

  let rpc_gss_init_res =
    Xdr.validate_xdr_type
      Rpc_auth_gssapi_aux.xdrt_rpc_gss_init_res in

  let rpc_gss_integ_data =
    Xdr.validate_xdr_type
      Rpc_auth_gssapi_aux.xdrt_rpc_gss_integ_data in

  let rpc_gss_priv_data =
    Xdr.validate_xdr_type
      Rpc_auth_gssapi_aux.xdrt_rpc_gss_priv_data in

        
  let ctx_by_handle = Hashtbl.create 42 in

  let handle_nr = ref 0 in

  let new_handle() =
    let n = !handle_nr in
    incr handle_nr;
    let random = String.make 16 '\000' in
    Netsys_rng.fill_random random;
    sprintf "%6d_%s" n (Digest.to_hex random) in

  ( object(self)
      method name = "RPCSEC_GSS"

      method flavors = [ "RPCSEC_GSS" ]

      method peek = `None

      method authenticate srv conn_id (details:Rpc_server.auth_details) auth =
	(* First decode the rpc_gss_cred_t structure in the header: *)
	try
	  let (_, cred_data) = details # credential in
	  let xdr_val =
	    Xdr.unpack_xdr_value
	      ~fast:true
	      cred_data
	      rpc_gss_cred_t
	      [] in
	  let cred =
	    _to_rpc_gss_cred_t xdr_val in
	  match cred with
	    | `_1 cred1 ->
		(* FIXME: Unless rpc_gss_data, check that the procedure is 0 *)
		let r =
		  match cred1.gss_proc with
		    | `rpcsec_gss_init ->
			self # auth_init srv conn_id details cred1
		    | `rpcsec_gss_continue_init ->
			self # auth_cont_init srv conn_id details cred1
		    | `rpcsec_gss_destroy ->
			self # auth_destroy srv conn_id details cred1
		    | `rpcsec_gss_data ->
			self # auth_data srv conn_id details cred1
		in
		let () = auth r in
		()
	with
	  | error ->
	      Netlog.logf `Err
		"Failed RPC authentication (GSS-API): %s"
		(Netexn.to_string error);
	      auth(Rpc_server.Auth_negative Rpc.Auth_failed)
		(* FIXME: better error codes *)

      method private get_token details =
	let body_data =
	  Rpc_packer.unpack_call_body_raw
	    details#message details#frame_len in
	let xdr_val =
	  Xdr.unpack_xdr_value
	    ~fast:true
	    body_data
	    rpc_gss_init_arg
	    [] in
	let token_struct =
	  _to_rpc_gss_init_arg xdr_val in
	token_struct.gss_token


      method private fixup_svc_flags ctx ret_flags =
	let have_privacy = List.mem `Conf_flag ret_flags in
	let have_integrity = List.mem `Integ_flag ret_flags in
	
	if require_privacy && not have_privacy then
	  failwith
	    "Rpc_auth_gssapi: Privacy requested but unavailable";
	if require_integrity && not have_integrity then
	  failwith
	    "Rpc_auth_gssapi: Integrity requested but unavailable";

	ctx.ctx_svc_none <- 
	  not require_privacy && not require_integrity;
	ctx.ctx_svc_integrity <-
	  not require_privacy && require_integrity;
	ctx.ctx_svc_privacy <-
	  require_privacy;


      method private verify_context ctx conn_id =
	( match ctx.ctx_conn_id with
	    | None -> ()
	    | Some id ->
		if id <> conn_id then
		  failwith "Rpc_auth_gssapi: this context is unavailable \
                            to this connection"
	)
	  (* CHECK: do we need to inquire_context, and to check whether
	     the context is fully established?
	   *)

      method private get_user ctx =
	let name =
	  gss_api # inquire_context
	    ~context:ctx.context
	    ~out:(fun ~src_name ~targ_name ~lifetime_req ~mech_type
		    ~ctx_flags ~locally_initiated ~is_open 
		    ~minor_status ~major_status
		    ()
		    ->
		      let (c_err, r_err, flags) = major_status in
		      if c_err <> `None || r_err <> `None then
			failwith("Rpc_auth_gssapi: Cannot extract name: " 
				   ^ string_of_major_status major_status);
		      if not is_open then
			failwith("Rpc_auth_gssapi: get_user: context is not \
                               fully established");
		      src_name
	       )
	  () in
	gss_api # export_name
	  ~name
	  ~out:(fun ~exported_name ~minor_status ~major_status () ->
		  let (c_err, r_err, flags) = major_status in
		  if c_err <> `None || r_err <> `None then
		    failwith("Rpc_auth_gssapi: Cannot export name: " 
			     ^ string_of_major_status major_status);
		  let k = ref 0 in
		  let (oid,name_str) =
		    try Netgssapi.decode_exported_name exported_name k
		    with _ -> assert false in
		  assert(!k = String.length exported_name);
		  (* Failed assertion -> The GSS-API object returns an
		     invalid export name
		   *)
		  match user_name_format with
		    | `Exported_name -> exported_name
		    | `Exported_name_text ->
			let oid_s =
			  Netgssapi.oid_to_string oid in
			oid_s ^ name_str
		    | `Exported_name_no_oid ->
			name_str
	       )
	  ()



      method private auth_init srv conn_id details cred1 =
	let (verf_flav, verf_data) = details # verifier in
	if cred1.handle <> "" then
	  failwith "Context handle is not empty";
	if verf_flav <> "AUTH_NONE" then
	  failwith "Bad verifier (1)";
	if verf_data <> "" then
	  failwith "Bad verifier (2)";
	gss_api # accept_sec_context
	  ~context:None
	  ~acceptor_cred
	  ~input_token:(self # get_token details)
	  ~chan_bindings:None
	  ~out:(
	    fun ~src_name ~mech_type ~output_context
	      ~output_token ~ret_flags ~time_rec 
	      ~delegated_cred ~minor_status ~major_status
	      () ->
		let (c_err, r_err, flags) = major_status in
		if c_err <> `None || r_err <> `None then
		  failwith("Rpc_auth_gssapi: Cannot accept token: " ^ 
			     string_of_major_status major_status);
		let h = new_handle() in
		let context =
		  match output_context with
		    | None ->
			failwith "Rpc_auth_gssapi: no context"
		    | Some c -> c in
		let cont = List.mem `Continue_needed flags in
		let ctx =
		  { context = context;
		    ctx_continue = cont;
		    ctx_handle = h;
		    ctx_conn_id = 
		      if shared_context then None else Some conn_id;
		    ctx_svc_none = false;
		    ctx_svc_integrity = false;
		    ctx_svc_privacy = false;
		  } in
		if not cont then
		  self#fixup_svc_flags ctx ret_flags;
		Hashtbl.replace ctx_by_handle h ctx;
		let reply =
		  { res_handle = h;
		    res_major =
		      if ctx.ctx_continue 
		      then gss_s_continue_needed
		      else gss_s_complete;
		    res_minor = zero;
		    res_seq_window = zero; (* FIXME *)
		    res_token = output_token
		  } in
		self # auth_init_result ctx reply
	  )
	  ()


      method private auth_cont_init srv conn_id details cred1 =
	let (verf_flav, verf_data) = details # verifier in
	if verf_flav <> "AUTH_NONE" then
	  failwith "Bad verifier (1)";
	if verf_data <> "" then
	  failwith "Bad verifier (2)";
	let h = cred1.handle in
	let ctx =
	  try Hashtbl.find ctx_by_handle h
	  with Not_found ->
	    failwith "Rpc_auth_gssapi: unknown context handle" in
	if not ctx.ctx_continue then
	  failwith "Rpc_auth_gssapi: cannot continue context establishment";
	self # verify_context ctx conn_id;
	gss_api # accept_sec_context
	  ~context:(Some ctx.context)
	  ~acceptor_cred
	  ~input_token:(self # get_token details)
	  ~chan_bindings:None
	  ~out:(
	    fun ~src_name ~mech_type ~output_context
	      ~output_token ~ret_flags ~time_rec 
	      ~delegated_cred ~minor_status ~major_status
	      () ->
		let (c_err, r_err, flags) = major_status in
		if c_err <> `None || r_err <> `None then
		  failwith("Rpc_auth_gssapi: Cannot accept token: " ^ 
			     string_of_major_status major_status);
		(* CHECK: do we need to check whether output_context is
		   the current context? Can this change? 
		 *)
		ctx.ctx_continue <- List.mem `Continue_needed flags;
		if not ctx.ctx_continue then
		  self#fixup_svc_flags ctx ret_flags;
		let reply =
		  { res_handle = h;
		    res_major =
		      if ctx.ctx_continue 
		      then gss_s_continue_needed
		      else gss_s_complete;
		    res_minor = zero;
		    res_seq_window = zero; (* FIXME *)
		    res_token = output_token
		  } in
		self # auth_init_result ctx reply
	  )
	  ()

      method private auth_init_result ctx reply =
	let xdr_val =
	  Rpc_auth_gssapi_aux._of_rpc_gss_init_res reply in
	let m =
	  Xdr.pack_xdr_value_as_mstrings
	    xdr_val rpc_gss_init_res [] in
	let (verf_flav, verf_data) =
	  if ctx.ctx_continue  then
	    ("AUTH_NONE", "")
	  else
	    let window_s =
	      Rtypes.uint4_as_string reply.res_seq_window in
	    let mic =
	      gss_api # get_mic
		~context:ctx.context
		~qop_req:None
		~message:(`String window_s, 0, String.length window_s)
		~out:(fun ~msg_token ~minor_status ~major_status () ->
			let (c_err, r_err, flags) = major_status in
			if c_err <> `None || r_err <> `None then
			  failwith("Rpc_auth_gssapi: \
                                  Cannot compute MIC: " ^ 
				     string_of_major_status major_status);
			msg_token
		     )
		() in
	    ("RPCSEC_GSS", mic) in
	Rpc_server.Auth_reply(m, verf_flav, verf_data)

      method private auth_data srv conn_id details cred1 =
	(* Get context: *)
	let h = cred1.handle in
	let ctx =
	  try Hashtbl.find ctx_by_handle h
	  with Not_found ->
	    failwith "Rpc_auth_gssapi: unknown context handle" in
	self # verify_context ctx conn_id;

	(* Verify the header first *)
	let (verf_flav, verf_data) = details # verifier in
	if verf_flav <> "RPCSEC_GSS" then
	  failwith "Rpc_auth_gssapi: Bad type of verifier";
	let pv = details # message in
	let n = Rpc_packer.extract_call_gssapi_header pv in
	let s = Rpc_packer.prefix_of_packed_value pv n in
	
	gss_api # verify_mic
	  ~context:ctx.context
	  ~message:(`String s, 0, String.length s)
	  ~token:verf_data
	  ~out:(fun ~qop_state ~minor_status ~major_status () ->
		  let (c_err, r_err, flags) = major_status in
		  if c_err <> `None || r_err <> `None then
		    failwith("Rpc_auth_gssapi: \
                                  Cannot verify MIC: " ^ 
			       string_of_major_status major_status);
	       )
	  ();

	(* Check sequence number *)
	(* TODO *)

	match cred1.service with
	  | `rpc_gss_svc_none ->
	      if not ctx.ctx_svc_none then
		failwith "Rpc_auth_gssapi: unexpected unprotected message";
	      self#auth_data_result ctx cred1.seq_num None None;

	  | `rpc_gss_svc_integrity ->
	      if not ctx.ctx_svc_integrity then
		failwith "Rpc_auth_gssapi: unexpected integrity-proctected \
                          message";
	      let encoder =
		integrity_encoder gss_api ctx cred1 rpc_gss_integ_data in
	      let decoder =
		integrity_decoder gss_api ctx cred1 rpc_gss_integ_data in
	      self#auth_data_result
		ctx cred1.seq_num (Some encoder) (Some decoder)

	  | `rpc_gss_svc_privacy ->
	      if not ctx.ctx_svc_privacy then
		failwith "Rpc_auth_gssapi: unexpected privacy-proctected \
                          message";
	      let encoder =
		privacy_encoder gss_api ctx cred1 rpc_gss_priv_data in
	      let decoder =
		privacy_decoder gss_api ctx cred1 rpc_gss_priv_data in
	      self # auth_data_result
		ctx cred1.seq_num (Some encoder) (Some decoder)

	      
      method private auth_data_result ctx seq enc_opt dec_opt =
	let seq_s =
	  Rtypes.uint4_as_string seq in
	let mic =
	  gss_api # get_mic
	    ~context:ctx.context
	    ~qop_req:None
	    ~message:(`String seq_s, 0, String.length seq_s)
	    ~out:(fun ~msg_token ~minor_status ~major_status () ->
		    let (c_err, r_err, flags) = major_status in
		    if c_err <> `None || r_err <> `None then
		      failwith("Rpc_auth_gssapi: \
                                  Cannot compute MIC: " ^ 
				 string_of_major_status major_status);
		    msg_token
		 )
	    () in
	Rpc_server.Auth_positive(
	  self#get_user ctx,
	  "RPCSEC_GSS", mic, enc_opt, dec_opt
	)

      method private auth_destroy srv conn_id details cred1 =
	let r =
	  self # auth_data srv conn_id details cred1 in
	match r with
	  | Rpc_server.Auth_positive(_, flav, mic, enc_opt, dec_opt) ->
	      (* Check that the input args are empty: *)
	      let raw_body =
		Rpc_packer.unpack_call_body_raw 
		  details#message details#frame_len in
	      let body_length =
		match dec_opt with
		  | None -> String.length raw_body
		  | Some dec -> 
		      let (b,n) = dec raw_body 0 (String.length raw_body) in
		      n in
	      if body_length <> 0 then
		failwith "Rpc_auth_gssapi: invalid destroy request";

	      (* Now destroy: *)
	      let h = cred1.handle in
	      Hashtbl.remove ctx_by_handle h;
	      
	      (* Create response: *)
	      let encoded_emptiness =
		match enc_opt with
		  | None -> ""
		  | Some enc -> enc "" in

	      (* Respond: *)
	      Rpc_server.Auth_reply([mk_mstring encoded_emptiness], flav, mic)
	  | _ ->
	      r
    end
  )

    
