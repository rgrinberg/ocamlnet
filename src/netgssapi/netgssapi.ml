(* $Id$ *)

type oid = int array
type oid_set = oid list
type credential = < otype : [ `Credential ] >
type context = < otype : [ `Context ]; valid : bool >
type token = string
type interprocess_token = string
type calling_error =
    [ `None
    | `Inaccessible_read
    | `Inaccessible_write
    | `Bad_structure
    ]
type routine_error =
    [ `None
    | `Bad_mech
    | `Bad_name
    | `Bad_nametype
    | `Bad_bindings
    | `Bad_status
    | `Bad_mic
    | `No_cred
    | `No_context
    | `Defective_token
    | `Defective_credential
    | `Credentials_expired
    | `Context_expired
    | `Failure
    | `Bad_QOP
    | `Unauthorized
    | `Unavailable
    | `Duplicate_element
    | `Name_not_mn
    ]
type suppl_status =
    [ `Continue_needed
    | `Duplicate_token
    | `Old_token
    | `Unseq_token
    | `Gap_token
    ]
type major_status = calling_error * routine_error * suppl_status list
type minor_status = int32
type name = < otype : [ `Name ] >
type address =
    [ `Unspecified of string
    | `Local of string
    | `Inet of Unix.inet_addr
    | `Nulladdr
    | `Other of int32 * string
    ]
type channel_bindings = address * address * string
type cred_usage = [ `Initiate |`Accept | `Both ]
type qop = < otype : [ `QOP ] >
type message_buffer =
    [ `String of string
    | `Memory of Netsys_mem.memory
    ]
type message =
    (message_buffer * int * int)
type ret_flag =
    [ `Deleg_flag | `Mutual_flag | `Replay_flag | `Sequence_flag 
    | `Conf_flag | `Integ_flag | `Anon_flag | `Prot_ready_flag
    | `Trans_flag
    ]
type req_flag = 
    [ `Deleg_flag | `Mutual_flag | `Replay_flag | `Sequence_flag 
    | `Conf_flag | `Integ_flag | `Anon_flag
    ]

class type gss_api =
object
  method provider : string
  method no_credential : credential
  method accept_sec_context :
          't . context:context ->
               acceptor_cred:credential -> 
               input_token:token ->
               chan_bindings:channel_bindings option ->
               out:( src_name:name ->
		     mech_type:oid ->
		     output_token:token ->
		     ret_flags:ret_flag list ->
		     time_rec:float ->
		     delegated_cred:credential ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't 
		   ) -> unit -> 't

  method acquire_cred :
          't . desired_name:name ->
               time_req:[`None | `Indefinite | `This of float] ->
               desired_mechs:oid_set ->
               cred_usage:cred_usage  ->
               out:( cred:credential ->
		     actual_mechs:oid_set ->
		     time_rec:[ `Indefinite | `This of float] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method add_cred :
          't . input_cred:credential ->
               desired_name:name ->
               desired_mech:oid ->
               cred_usage:cred_usage ->
               initiator_time_req:[`None | `Indefinite | `This of float] ->
               acceptor_time_req:[`None | `Indefinite | `This of float] ->
               out:( output_cred:credential ->
		     actual_mechs:oid_set ->
		     initiator_time_rec:[ `Indefinite | `This of float] ->
		     acceptor_time_rec:[ `Indefinite | `This of float] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method canonicalize_name :
          't . input_name:name ->
               mech_type:oid ->
               out:( output_name:name ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method compare_name :
          't . name1:name ->
               name2:name ->
               out:( name_equal:bool ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method context_time :
          't . context:context ->
               out:( time_rec:float ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method display_name :
          't . input_name:name ->
               out:( output_name:string ->
		     output_name_type:oid ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method display_status :
          't . status:[ `Major of major_status | `Minor of minor_status ] ->
               mech_type: oid ->
               out:( status_strings: string list ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method duplicate_name :
          't . src_name:name ->
               out:( dest_name:name ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method export_name : 
          't . name:name ->
               out:( exported_name:string ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method export_sec_context :
          't . context:context ->
               out:( interprocess_token:interprocess_token ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method get_mic : 
          't . context:context ->
               qop_req:qop option ->
               message:message ->
               out:( msg_token:token ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method import_name :
          't . input_name:string ->
               input_name_type:oid ->
               out:( output_name:name ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method import_sec_context :
          't . interprocess_token:interprocess_token ->
               out:( context:context ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method indicate_mechs :
          't . out:( mech_set:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method init_sec_context :
          't . initiator_cred:credential ->
               context:context option ->
               target_name:name ->
               mech_type:oid -> 
               req_flags:req_flag list ->
               time_rec:float option ->
               chan_bindings:channel_bindings option ->
               input_token:token option ->
               out:( actual_mech_type:oid ->
		     output_token:token ->
		     ret_flags:ret_flag list ->
		     time_rec:[ `Indefinite | `This of float ] ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_context :
          't . context:context ->
               out:( src_name:name ->
                     targ_name:name ->
		     lifetime_req : [ `Indefinite | `This of float ] ->
		     mech_type:oid ->
		     ctx_flags:ret_flag list ->
		     locally_initiated:bool ->
		     is_open:bool ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_cred :
          't . cred:credential ->
               out:( name:name ->
		     lifetime: [ `Indefinite | `This of float ] ->
		     cred_usage:cred_usage ->
		     mechanisms:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_cred_by_mech :
          't . cred:credential ->
               mech_type:oid -> 
               out:( name:name ->
		     initiator_lifetime: [ `Indefinite | `This of float ] ->
		     acceptor_lifetime: [ `Indefinite | `This of float ] ->
		     cred_usage:cred_usage ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_mechs_for_name :
          't . name:name ->
               out:( mech_types:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method inquire_names_for_mech :
          't . mechanism:oid ->
               out:( name_types:oid_set ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't


  method process_context_token :
          't . context:context ->
               token:token ->
               out:( minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method unwrap :
          't . context:context ->
               input_message:message ->
               output_message_preferred_type:[ `String | `Memory ] ->
               out:( output_message:message ->
		     conf_state:bool ->
		     qop_state:qop ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method verify_mic :
          't . context:context ->
               message:message ->
               token:token ->
               out:( qop_state:qop ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method wrap :
          't . context:context ->
               conf_req:bool ->
               qop_req:qop option ->
               input_message:message ->
               output_message_preferred_type:[ `String | `Memory ] ->
               out:( conf_state:bool ->
		     output_message:message ->
		     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't

  method wrap_size_limit :
          't . context:context ->
               conf_req:bool ->
               qop_req:qop option ->
               req_output_size:int ->
               out:( max_input_size:int ->
                     minor_status:minor_status ->
		     major_status:major_status ->
		     unit ->
		     't
		   ) -> unit -> 't
end

let nt_hostbased_service =
  [| 1; 3; 6; 1; 5; 6; 2 |]

let nt_user_name =
  [| 1; 2; 840; 113554; 1; 2; 1; 1 |]

let nt_machine_uid_name =
  [| 1; 2; 840; 113554; 1; 2; 1; 2 |]

let nt_string_uid_name =
  [| 1; 2; 840; 113554; 1; 2; 1; 3 |]

let nt_anonymous =
  [| 1; 3; 6; 1; 5; 6; 3 |]

let nt_export_name =
  [| 1; 3; 6; 1; 5; 6; 4 |]

(* Encodings *)

(* This follows RFC 2078, but additional information about DER
   can also be found in ITU-T X.690:

     http://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf
 *)

let oid_to_string oid =
  "{" ^ String.concat " " (List.map string_of_int (Array.to_list oid)) ^ "}"


let oid_str_re = Netstring_pcre.regexp "[ \t\r\n]+|{|}"
let string_to_oid s =
  let rec cont1 l =
    match l with
      | Netstring_pcre.Delim "{" :: l' -> cont2 l'
      | Netstring_pcre.Delim "}" :: _ -> raise Not_found
      | Netstring_pcre.Delim _ :: l' -> cont1 l'   (* whitespace *)
      | _ -> raise Not_found 
  and cont2 l =  (* after "{" *)
    match l with
      | Netstring_pcre.Delim "{" :: _ -> raise Not_found
      | Netstring_pcre.Delim "}" :: l' -> cont3 l'
      | Netstring_pcre.Delim _ :: l' -> cont2 l'
      | Netstring_pcre.Text s :: l' -> int_of_string s :: cont2 l'
      | _ -> raise Not_found
  and cont3 l = (* after "}" *)
    match l with
      | Netstring_pcre.Delim ("{" | "}") :: _ -> raise Not_found
      | Netstring_pcre.Delim _ :: l' -> cont3 l'
      | [] -> []
      | _ -> raise Not_found 
  in

  let l =
    Netstring_pcre.full_split oid_str_re s in
  try
    Array.of_list(cont1 l)
  with
    | _ -> failwith "Netgssapi.string_to_oid"


let encode_subidentifier buf n =
  (* See 8.19 of ITU.T X.690 *)
  let rec encode n =
    if n < 128 then
      [ Char.chr n ]
    else
      (Char.chr ((n land 127) lor 128)) :: encode (n lsr 7) in
  if n < 0 then failwith "Netgssapi.encode_subidentifier";
  let l = encode n in
  List.iter (Buffer.add_char buf) l

let decode_subidentifier s cursor =
  let n = ref 0 in
  let s_len = String.length s in
  while !cursor < s_len && s.[ !cursor ] >= '\x80' do
    let c = Char.code (s.[ !cursor ]) - 128 in
    n := (!n lsl 7) lor c;
    incr cursor
  done;
  if !cursor < s_len then (
    let c = Char.code (s.[ !cursor ]) in
    n := (!n lsl 7) lor c;
    incr cursor;
    !n
  )
  else failwith "Netgssapi.decode_subidentifier"

let encode_definite_length buf n =
  (* See 8.1.3 of ITU-T X.690 *)
  let rec encode n =
    if n < 256 then
      [ Char.chr n ]
    else
      (Char.chr (n land 255)) :: encode (n lsr 8) in
  if n < 128 then (
    Buffer.add_char buf (Char.chr n)
  ) else (
    let l = encode n in
    Buffer.add_char buf (Char.chr (List.length l + 128));
    List.iter (Buffer.add_char buf) l
  )

let decode_definite_length s cursor =
  let s_len = String.length s in
  if !cursor < s_len then (
    let c = s.[ !cursor ] in
    incr cursor;
    if c < '\x80' then (
      Char.code c
    )
    else (
      let p = Char.code c - 128 in
      let n = ref 0 in
      for q = 1 to p do
	if !cursor < s_len then (
	  let c = s.[ !cursor ] in
	  incr cursor;
	  n := (!n lsl 8) lor Char.code c;
	)
	else failwith "Netgssapi.decode_definite_length"
      done;
      !n
    )
  )
  else failwith "Netgssapi.decode_definite_length"

let oid_to_der oid =
  match Array.to_list oid with
    | [] ->
	failwith "Netgssapi.oid_to_der: empty OID"
    | [ _ ] ->
	failwith "Netgssapi.oid_to_der: invalid OID"
    | top :: second :: subids ->
	if top < 0 || top > 5 then  (* actually only 0..2 possible *)
	  failwith "Netgssapi.oid_to_der: invalid OID";
	if second < 0 || second > 39 then
	  failwith "Netgssapi.oid_to_der: invalid OID";
	let subids_buf = Buffer.create 50 in
	List.iter (encode_subidentifier subids_buf) subids;
	let buf = Buffer.create 50 in
	Buffer.add_char buf '\x06';
	encode_definite_length buf (Buffer.length subids_buf + 1);
	Buffer.add_char buf (Char.chr (top * 40 + second));
	Buffer.add_buffer buf subids_buf;
	Buffer.contents buf

let der_to_oid der cursor =
  try
    let der_len = String.length der in
    if !cursor >= der_len then raise Not_found;
    let c = der.[ !cursor ] in
    incr cursor;
    if c <> '\x06' then raise Not_found;
    let oid_len = decode_definite_length der cursor in
    let lim = !cursor + oid_len in
    if lim > der_len then raise Not_found;
    if oid_len = 0 then raise Not_found;
    let c = Char.code der.[ !cursor ] in
    incr cursor;
    let top = c / 40 in
    let second = c mod 40 in
    let oid = ref [ second; top ] in
    while !cursor < lim do
      let subid = decode_subidentifier der cursor in
      oid := subid :: !oid;
    done;
    if !cursor <> lim then raise Not_found;
    Array.of_list (List.rev !oid)
  with
    | _ -> failwith "Netgssapi.der_to_oid"


let wire_encode_token oid token =
  try
    let buf = Buffer.create (50 + String.length token) in
    Buffer.add_char buf '\x60';
    let oid_as_der = oid_to_der oid in
    let len = String.length oid_as_der + String.length token in
    encode_definite_length buf len;
    Buffer.add_string buf oid_as_der;
    Buffer.add_string buf token;
    Buffer.contents buf
  with
    | _ -> failwith "Netgssapi.wire_encode_token"

let wire_decode_token s cursor =
  try
    let s_len = String.length s in
    if !cursor > s_len then raise Not_found;
    let c = s.[ !cursor ] in
    incr cursor;
    if c <> '\x60' then raise Not_found;
    let len = decode_definite_length s cursor in
    let lim = !cursor + len in
    if lim > s_len then raise Not_found;
    let oid = der_to_oid s cursor in
    if !cursor > lim then raise Not_found;
    let token = String.sub s !cursor (lim - !cursor) in
    cursor := lim;
    (oid, token)
  with 
    | _ -> failwith "Netgsspi.wire_decode_token"


let encode_exported_name mech_oid name =
  let buf = Buffer.create (50 + String.length name) in
  Buffer.add_string buf "\x04\x01";
  let mech_oid_der = oid_to_der mech_oid in
  let mech_oid_len = String.length mech_oid_der in
  if mech_oid_len > 65535 then 
    failwith "Netgssapi.encode_exported_name: OID too long";
  Buffer.add_char buf (Char.chr (mech_oid_len / 256));
  Buffer.add_char buf (Char.chr (mech_oid_len mod 256));
  Buffer.add_string buf mech_oid_der;
  let name_len = String.length name in
  let n3 = (name_len lsr 24) land 0xff in
  let n2 = (name_len lsr 16) land 0xff in
  let n1 = (name_len lsr 8) land 0xff in
  let n0 = name_len land 0xff in
  Buffer.add_char buf (Char.chr n3);
  Buffer.add_char buf (Char.chr n2);
  Buffer.add_char buf (Char.chr n1);
  Buffer.add_char buf (Char.chr n0);
  Buffer.add_string buf name;
  Buffer.contents buf


let decode_exported_name s cursor =
  try
    let s_len = String.length s in
    if !cursor + 4 > s_len then raise Not_found;
    let c0 = s.[ !cursor ] in
    incr cursor;
    let c1 = s.[ !cursor ] in
    incr cursor;
    let c2 = s.[ !cursor ] in
    incr cursor;
    let c3 = s.[ !cursor ] in
    incr cursor;
    if c0 <> '\x04' || c1 <> '\x01' then raise Not_found;
    let mech_oid_len =  (Char.code c2 lsl 8) + Char.code c3 in
    let mech_start = !cursor in
    if mech_start + mech_oid_len > s_len then raise Not_found;
    let mech_oid = der_to_oid s cursor in
    if !cursor <> mech_start + mech_oid_len then raise Not_found;
    if !cursor + 4 > s_len then raise Not_found;
    let n0 = Char.code s.[ !cursor ] in
    incr cursor;
    let n1 = Char.code s.[ !cursor ] in
    incr cursor;
    let n2 = Char.code s.[ !cursor ] in
    incr cursor;
    let n3 = Char.code s.[ !cursor ] in
    incr cursor;
    let name_len = (n0 lsl 24) lor (n1 lsl 16) lor (n2 lsl 8) lor (n3) in
    if !cursor + name_len > s_len then raise Not_found;
    let name = String.sub s !cursor name_len in
    cursor := !cursor + name_len;
    (mech_oid, name)
  with
    | _ -> failwith "Netgssapi.decode_exported_name"
