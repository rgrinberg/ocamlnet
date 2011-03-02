(* $Id$ *)

(* TODO:
   - Develop ideas how to avoid that strings are copied that frequently 
   - error handling
   - sequence numbers
 *)

open Netgssapi
open Rpc_auth_gssapi_aux
open Printf

type support_level =
    [ `Required | `If_possible | `None ]

type user_name_interpretation =
    [ `Exported_name
    | `Exported_name_text
    | `Import of oid
    ]

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
      ~context:ctx
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
      ~context:ctx
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
    ~context:ctx
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
      ~context:ctx
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
		integrity_encoder 
		  gss_api ctx.context cred1 rpc_gss_integ_data in
	      let decoder =
		integrity_decoder 
		  gss_api ctx.context cred1 rpc_gss_integ_data in
	      self#auth_data_result
		ctx cred1.seq_num (Some encoder) (Some decoder)

	  | `rpc_gss_svc_privacy ->
	      if not ctx.ctx_svc_privacy then
		failwith "Rpc_auth_gssapi: unexpected privacy-proctected \
                          message";
	      let encoder =
		privacy_encoder gss_api ctx.context cred1 rpc_gss_priv_data in
	      let decoder =
		privacy_decoder gss_api ctx.context cred1 rpc_gss_priv_data in
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

    
let client_auth_method 
      ?(privacy=`If_possible)
      ?(integrity=`If_possible)
      ?(user_name_interpretation = `Exported_name_text)
      (gss_api : gss_api) : Rpc_client.auth_method =

  let default_initiator_cred() =
    gss_api # acquire_cred
      ~desired_name:gss_api#no_name
      ~time_req:`None
      ~desired_mechs:[]
      ~cred_usage:`Initiate
      ~out:(
	fun ~cred ~actual_mechs ~time_rec ~minor_status ~major_status() ->
	  let (c_err, r_err, flags) = major_status in
	  if c_err <> `None || r_err <> `None then
	    failwith("Rpc_auth_gssapi: Cannot acquire default creds: " ^ 
		       string_of_major_status major_status);
	  cred
      )
      () in

  let rpc_gss_cred_t =
    Xdr.validate_xdr_type
      Rpc_auth_gssapi_aux.xdrt_rpc_gss_cred_t in

  let rpc_gss_integ_data =
    Xdr.validate_xdr_type
      Rpc_auth_gssapi_aux.xdrt_rpc_gss_integ_data in

  let rpc_gss_priv_data =
    Xdr.validate_xdr_type
      Rpc_auth_gssapi_aux.xdrt_rpc_gss_priv_data in

  let ts =
    [ "init_arg", Rpc_auth_gssapi_aux.xdrt_rpc_gss_init_arg;
      "init_res", Rpc_auth_gssapi_aux.xdrt_rpc_gss_init_res
    ] in

  let ts_val =
    Xdr.validate_xdr_type_system ts in

  let session (m:Rpc_client.auth_method)
              (p:Rpc_client.auth_protocol)
              ctx service handle cur_seq_num
        : Rpc_client.auth_session =
    let seq_num_of_xid = Hashtbl.create 15 in
    ( object(self)
	method next_credentials client prog proc xid =
	  (* N.B. Exceptions raised here probably abort the client,
	     and fall through to the event loop
	   *)

	  let cred1 =
	    { gss_proc = `rpcsec_gss_data;
	      seq_num = !cur_seq_num;
	      service = service;
	      handle = handle
	    } in
	  let cred1_xdr = _of_rpc_gss_cred_t (`_1 cred1) in
	  let cred1_s =
	    Xdr.pack_xdr_value_as_string
	      cred1_xdr rpc_gss_cred_t [] in
	  
	  let h_pv =
	    Rpc_packer.pack_call_gssapi_header
	      prog xid proc "RPCSEC_GSS" cred1_s in
	  let h =
	    Rpc_packer.string_of_packed_value h_pv in
	  let mic =
	    gss_api # get_mic
	      ~context:ctx
	      ~qop_req:None
	      ~message:(`String h, 0, String.length h)
	      ~out:(fun ~msg_token ~minor_status ~major_status () ->
		      let (c_err, r_err, flags) = major_status in
		      if c_err <> `None || r_err <> `None then
			failwith("Rpc_auth_gssapi: \
                          Cannot obtain MIC: " ^ 
				   string_of_major_status major_status);
		      msg_token
		   )
	      () in

	  (* Save seq_num: *)
	  Hashtbl.replace seq_num_of_xid xid !cur_seq_num;

	  (* Increment cur_seq_num: *)
	  cur_seq_num := 
	    Rtypes.uint4_of_int64(
	      Int64.logand
		(Int64.succ (Rtypes.int64_of_uint4 !cur_seq_num))
		0xFFFF_FFFFL
	    );

	  let enc_opt, dec_opt =
	    match service with
	      | `rpc_gss_svc_none ->
		  None, None
		    
	      | `rpc_gss_svc_integrity ->
		  let encoder =
		    integrity_encoder gss_api ctx cred1 rpc_gss_integ_data in
		  let decoder =
		    integrity_decoder gss_api ctx cred1 rpc_gss_integ_data in
		  (Some encoder), (Some decoder)

	      | `rpc_gss_svc_privacy ->
		  let encoder =
		    privacy_encoder gss_api ctx cred1 rpc_gss_priv_data in
		  let decoder =
		    privacy_decoder gss_api ctx cred1 rpc_gss_priv_data in
		  (Some encoder), (Some decoder) in

	  ("RPCSEC_GSS", cred1_s,
	   "RPCSEC_GSS", mic,
	   enc_opt, dec_opt
	  )

	method server_rejects client xid code =
	  Hashtbl.remove seq_num_of_xid xid;
	  match code with
	    | Rpc.RPCSEC_GSS_credproblem | Rpc.RPCSEC_GSS_ctxproblem ->
		`Renew
	    | Rpc.Auth_too_weak ->
		`Next
	    | _ ->
		`Fail

	method server_accepts client xid verf_flav verf_data =
	  if verf_flav <> "RPCSEC_GSS" then
	    raise(Rpc.Rpc_server Rpc.Auth_invalid_resp);
	  let seq =
	    try Hashtbl.find seq_num_of_xid xid
	    with Not_found -> 
	      raise(Rpc.Rpc_server Rpc.Auth_invalid_resp) in
	  let seq_s =
	    Rtypes.uint4_as_string seq in
	  Hashtbl.remove seq_num_of_xid xid;
	  gss_api # verify_mic
	    ~context:ctx
	    ~message:(`String seq_s, 0, String.length seq_s)
	    ~token:verf_data
	    ~out:(fun ~qop_state ~minor_status ~major_status () ->
		    let (c_err, r_err, flags) = major_status in
		    if c_err <> `None || r_err <> `None then
		      raise(Rpc.Rpc_server Rpc.Auth_invalid_resp);
		 )
	    ()

	method auth_protocol = p

      end
    ) in

  let protocol (m:Rpc_client.auth_method) client cred
       : Rpc_client.auth_protocol =
    let first = ref true in
    let state = ref `Emit in
    let ctx = ref None in
    let input_token = ref "" in
    let handle = ref "" in
    let init_prog = ref None in
    let init_service = ref None in

    let get_context() =
      match !ctx with Some c -> c | None -> assert false in

    (* CHECK: what happens with exceptions thrown here? *)

    ( object(self)
	method state = !state

	method emit xid prog_nr vers_nr =
	  assert(!state = `Emit);
	  try
	    let prog =
	      match !init_prog with
		| None ->
		    let p =
		      Rpc_program.create
			prog_nr
			vers_nr
			ts_val
			[ "init", 
			  ((Rtypes.uint4_of_int 0), 
			   Xdr.X_type "init_arg", Xdr.X_type "init_res");
			] in
		    init_prog := Some p;
		    p
		| Some p -> p in
	    let req_flags =
	      ( if integrity=`If_possible || integrity=`Required then
		  [ `Integ_flag ]
		else
		  []
	      ) @
		( if privacy=`If_possible || privacy=`Required then
		    [ `Conf_flag ]
		  else
		    []
		) in
	    let (output_token, cont_needed, have_priv, have_integ) =
	      gss_api # init_sec_context
		~initiator_cred:cred
		~context:!ctx
		~target_name:gss_api#no_name 
		~mech_type:[||]
		~req_flags
		~time_rec:None
		~chan_bindings:None
		~input_token:(if !first then None else Some !input_token)
		~out:(fun ~actual_mech_type ~output_context ~output_token 
			~ret_flags ~time_rec ~minor_status ~major_status
			() ->
			  let (c_err, r_err, flags) = major_status in
			  if c_err <> `None || r_err <> `None then
			    failwith("Rpc_auth_gssapi: Cannot init sec ctx: " ^ 
				       string_of_major_status major_status);
			  ctx := output_context;
			  (output_token, 
			   List.mem `Continue_needed flags,
			   List.mem `Conf_flag ret_flags,
			   List.mem `Integ_flag ret_flags
			  )
		     )
		() in
	    let service_i =
	      match integrity with
		| `Required ->
		    if not have_integ && not have_priv then
		      failwith "Rpc_auth_gssapi: Integrity is not available";
		    `rpc_gss_svc_integrity
		| `If_possible ->
		    if have_integ then
		      `rpc_gss_svc_integrity
		    else
		      `rpc_gss_svc_none
		| `None ->
		    `rpc_gss_svc_none in
	    let service =
	      match privacy with
		| `Required ->
		    if not have_priv then
		      failwith "Rpc_auth_gssapi: Privacy is not available";
		    `rpc_gss_svc_privacy
		| `If_possible ->
		    if have_priv then
		      `rpc_gss_svc_privacy
		    else
		      service_i
		| `None ->
		    service_i in
	    init_service := Some service;
	    let cred1 =
	      `_1 { gss_proc = ( if !first then `rpcsec_gss_init
				 else `rpcsec_gss_continue_init );
		    seq_num = Rtypes.uint4_of_int 0;  (* FIXME *)
		    service = service;
		    handle = !handle
		  } in
	    let cred1_xdr = _of_rpc_gss_cred_t cred1 in
	    let cred1_s =
	      Xdr.pack_xdr_value_as_string
		cred1_xdr rpc_gss_cred_t [] in
	    let pv =
	      Rpc_packer.pack_call
		prog xid "init"
		"RPCSEC_GSS" cred1_s
		"AUTH_NONE" ""
		(Xdr.XV_opaque !handle) in
	    first := false;
	    state := `Receive xid;
	    pv
	  with error ->
	    Netlog.logf `Err
	      "Rpc_auth_gssapi: Error during message preparation: %s"
	      (Netexn.to_string error);
	    state := `Error;
	    raise error


	method receive pv =
	  try
	    let prog =
	      match !init_prog with
		| None -> assert false
		| Some p -> p in
	    let (xid, flav_name, flav_data, result_xdr) =
	      Rpc_packer.unpack_reply prog "init" pv in
	    assert( !state = `Receive xid );
	    
	    let res = _to_rpc_gss_init_res result_xdr in
	    let cont_needed =
	      res.res_major = gss_s_continue_needed in
	    
	    if not cont_needed && res.res_major <> gss_s_complete then
	      failwith
		(sprintf "Rpc_auth_gssapi: Got GSS-API error code %Ld"
		   (Rtypes.int64_of_uint4 res.res_major));
	    
	    if cont_needed then (
	      if flav_name <> "AUTH_NONE" || flav_data <> "" then
		failwith "Rpc_auth_gssapi: bad verifier";
	    )
	    else (
	      if flav_name <> "RPCSEC_GSS" then
		failwith "Rpc_auth_gssapi: bad verifier";
	      let window_s =
		Rtypes.uint4_as_string res.res_seq_window in
	      gss_api # verify_mic
		~context:(get_context())
		~message:(`String window_s, 0, String.length window_s)
		~token:flav_data
		~out:(fun ~qop_state ~minor_status ~major_status () ->
			let (c_err, r_err, flags) = major_status in
			if c_err <> `None || r_err <> `None then
			  failwith("Rpc_auth_gssapi: \
                                  Cannot verify MIC: " ^ 
				     string_of_major_status major_status);
			()
		     )
		()
	    );
	    
	    handle := res.res_handle;	  
	    input_token := res.res_token;
	    
	    if cont_needed then
	      state := `Emit
	    else
	      let c = get_context () in
	      let service =
		match !init_service with Some s -> s | None -> assert false in
	      let cs = ref (Rtypes.uint4_of_int 0) in
	      let s = 
		session 
		  m (self :> Rpc_client.auth_protocol) c service !handle cs in
	      state := `Done s
	  with error ->
	    Netlog.logf `Err
	      "Rpc_auth_gssapi: Error during message verification: %s"
	      (Netexn.to_string error);
	    state := `Error;
	    raise error

	method auth_method = m

      end
    ) in

  ( object(self)
      method name = "RPCSEC_GSS"

      method new_session client user_opt =
	let cred =
	  match user_opt with
	    | None ->
		default_initiator_cred()
	    | Some user ->
		let (input_name, input_name_type) =
		  match user_name_interpretation with
		    | `Exported_name ->
			(user, nt_export_name)
		    | `Exported_name_text ->
			let l = String.length user in
			( try
			    let k = String.index user '}' in
			    let oid = string_to_oid (String.sub user 0 (k+1)) in
			    let n = String.sub user (k+1) (l-k-1) in
			    (n, oid)
			  with _ ->
			    failwith
			      ("Rpc_auth_gssapi: cannot parse user name")
			)
		    | `Import input_name_type ->
			(user, input_name_type) in
		let name =
		  gss_api # import_name
		    ~input_name
		    ~input_name_type
		    ~out:(fun ~output_name ~minor_status ~major_status
			    () ->
			      let (c_err, r_err, flags) = major_status in
			      if c_err <> `None || r_err <> `None then
				failwith
				  ("Rpc_auth_gssapi: Cannot import name: "
				   ^ string_of_major_status major_status);
			      output_name
			 )
		    () in
		gss_api # acquire_cred
		  ~desired_name:name
		  ~time_req:`None
		  ~desired_mechs:[]
		  ~cred_usage:`Initiate
		  ~out:(
		    fun ~cred ~actual_mechs ~time_rec ~minor_status
		      ~major_status
		      () ->
			let (c_err, r_err, flags) = major_status in
			if c_err <> `None || r_err <> `None then
			  failwith
			    ("Rpc_auth_gssapi: Cannot acquire default creds: " 
			     ^ string_of_major_status major_status);
			cred
		  )
		  () in
	protocol (self :> Rpc_client.auth_method) client cred

    end
  )
