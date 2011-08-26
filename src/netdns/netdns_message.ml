(* $Id$ *)

open Netdns_lexer
open Printf

type domain = string list

type opcode =
    [ `QUERY | `IQUERY | `STATUS | `Unknown of int ]

type rcode =
    [ `No_error | `Format_error | `Server_failure | `Name_error 
    | `Not_implemented | `Refused | `Unknown of int
    ]
	
type type_ =
    [ `A | `NS | `MD | `MF | `CNAME | `SOA | `MB | `MG | `MR | `NULL
    | `WKS | `PTR | `HINFO | `MINFO | `MX | `TXT
    | `Unknown of int
    ]

type qtype =
    [ type_ | `AXFR | `MAILB | `MAILA | `ANY ]

type qclass =
    [ `IN_ | `Unknown of int ]

type msg_header =
    { msg_id : int;
      msg_is_query : bool;
      msg_opcode : opcode;
      msg_is_aa : bool;
      msg_is_trunc : bool;
      msg_rd : bool;
      msg_ra : bool;
      msg_rcode : rcode;
    }

type msg_iheader =
    {
      msg_qdcount : int;
      msg_ancount : int;
      msg_nscount : int;
      msg_arcount : int
    }


type q_entry =
    { q_name : string list;
      q_type : qtype;
      q_class : qclass;
    }


type soa =
    { soa_mname : domain;
      soa_rname : domain;
      soa_serial : Rtypes.uint4;
      soa_refresh : Rtypes.int4;
      soa_retry : Rtypes.int4;
      soa_expire : Rtypes.int4;
      soa_minimum : Rtypes.uint4;
    }


type generic_rr =
    [ `CNAME of domain
    | `HINFO of string * string  (* cpu, os *)
    | `MX of int * domain    (* preference, exchanger *)
    | `NS of domain 
    | `PTR of domain
    | `SOA of soa
    | `TXT of string list
    | `Unknown
    ]

type in_rr =
    [ `A of string    (* Internet address in format x.x.x.x *)
	(* WKS not supported *)
    ]
  

type rr =
    [ generic_rr | in_rr ]


type rr_entry =
    { rr_name : domain;
      rr_type : type_;
      rr_class : qclass;
      rr_ttl : Rtypes.uint4;
      rr : rr
    }

type msg =
    { msg_header : msg_header;
      msg_question : q_entry list;
      msg_answer : rr_entry list;
      msg_authority : rr_entry list;
      msg_additional : rr_entry list
    }

exception Cannot_generate_message of string

let cannot_gen_msg s =
  raise (Cannot_generate_message s)


let is_sound re =
  match re.rr with
    | `A _        -> re.rr_type = `A
    | `CNAME _    -> re.rr_type = `CNAME
    | `HINFO(_,_) -> re.rr_type = `HINFO
    | `MX(_,_)    -> re.rr_type = `MX
    | `NS _       -> re.rr_type = `NS
    | `PTR _      -> re.rr_type = `PTR
    | `SOA _      -> re.rr_type = `SOA
    | `TXT _      -> re.rr_type = `TXT
    | `Unknown    -> false


(**********************************************************************)
(* Parsing                                                            *)
(**********************************************************************)

let parse_header lexbuf =
  let id = uint2 lexbuf in
  let bits = uint2 lexbuf in
  let is_query = bits land 0x8000 = 0 in
  let opcode = 
    match (bits land 0x7800) lsr 11 with
      | 0 -> `QUERY
      | 1 -> `IQUERY
      | 2 -> `STATUS
      | n -> `Unknown n in
  let is_aa = bits land 0x0400 <> 0 in
  let is_trunc = bits land 0x0200 <> 0 in
  let rd = bits land 0x0100 <> 0 in
  let ra = bits land 0x0080 <> 0 in
  let rcode = 
    match bits land 0x000f with
      | 0 -> `No_error
      | 1 -> `Format_error
      | 2 -> `Server_failure
      | 3 -> `Name_error
      | 4 -> `Not_implemented
      | 5 -> `Refused
      | n -> `Unknown n in
  let qdcount = uint2 lexbuf in
  let ancount = uint2 lexbuf in
  let nscount = uint2 lexbuf in
  let arcount = uint2 lexbuf in


  { msg_id = id;
    msg_is_query = is_query;
    msg_opcode = opcode;
    msg_is_aa = is_aa;
    msg_is_trunc = is_trunc;
    msg_rd = rd;
    msg_ra = ra;
    msg_rcode = rcode;
  },
  {
    msg_qdcount = qdcount;
    msg_ancount = ancount;
    msg_nscount = nscount;
    msg_arcount = arcount;
  }


let parse_question hdr s lexbuf =
  
  let rec parse_entry() =
    let q_name = domain_name 0 s lexbuf in
    let q_type = 
      match qtype lexbuf with
	| #type_ as t -> t
	| _ -> raise(Bad_message "Unsupported query type") in
    let q_class = qclass lexbuf in
    { q_name = q_name;
      q_type = q_type;
      q_class = q_class
    }
  in

  let rec parse_entries n =
    if n = 0 then
      []
    else
      let e = parse_entry() in
      e :: parse_entries (n-1) in

  parse_entries hdr.msg_qdcount


let parse_rr count hdr s lexbuf =

  let parse_entry() =
    let name = domain_name 0 s lexbuf in
    let typ = 
      match qtype lexbuf with
	| #type_ as t -> t
	| `AXFR -> `Unknown 252
	| `MAILB -> `Unknown 253
	| `MAILA -> `Unknown 254
	| `ANY -> `Unknown 255 in
    let clas = qclass lexbuf in
    let ttl = uint4 lexbuf in
    let rdlength = uint2 lexbuf in
    
    let start = Lexing.lexeme_end lexbuf in

    let rr =
      match clas, typ with
	| _, `CNAME ->
	    let cname = domain_name 0 s lexbuf in
	    `CNAME cname

	| _, `HINFO ->
	    let cpu = character_string lexbuf in
	    let os = character_string lexbuf in
	    `HINFO(cpu,os)

	| _, `MX ->
	    let pref = uint2 lexbuf in
	    let exch = domain_name 0 s lexbuf in
	    `MX(pref,exch)

	| _, `NS ->
	    let nsname = domain_name 0 s lexbuf in
	    `NS nsname

	| _, `PTR ->
	    let ptrname = domain_name 0 s lexbuf in
	    `PTR ptrname

	| _, `SOA ->
	    let mname = domain_name 0 s lexbuf in
	    let rname = domain_name 0 s lexbuf in
	    let serial = uint4 lexbuf in
	    let refresh = int4 lexbuf in
	    let retry = int4 lexbuf in
	    let expire = int4 lexbuf in
	    let minimum = uint4 lexbuf in
	    `SOA
	      { soa_mname = mname;
		soa_rname = rname;
		soa_serial = serial;
		soa_refresh = refresh;
		soa_retry = retry;
		soa_expire = expire;
		soa_minimum = minimum
	      }

	| _, `TXT ->
	    let s' = String.create rdlength in
	    for k = 0 to rdlength - 1 do
	      s'.[k] <- single_char lexbuf
	    done;
	    let lexbuf' = Lexing.from_string s' in
	    
	    let l = character_strings lexbuf' in
	    `TXT l

	| `IN_, `A ->
	    let a0,a1,a2,a3 = Rtypes.dest_uint4 (uint4 lexbuf) in
	    `A (sprintf "%d.%d.%d.%d"
		  (Char.code a0)
		  (Char.code a1)
		  (Char.code a2)
		  (Char.code a3))

	| _, _ ->
	    (* Simply skip rdlength bytes *)
	    if rdlength > 0 then
	      skip rdlength lexbuf;
	    `Unknown in

    (* Check that current position is consistent with rdlength *)
    let end_ = Lexing.lexeme_end lexbuf in
    
    if end_ - start <> rdlength then
      raise(Bad_message "RR has incorrect length");

    { rr_name = name;
      rr_type = typ;
      rr_class = clas;
      rr_ttl = ttl;
      rr = rr
    }

  in
  let rec parse_entries n =
    if n = 0 then
      []
    else
      let e = parse_entry() in
      e :: parse_entries (n-1) in

  parse_entries count


let parse s =
  let lexbuf = Lexing.from_string s in
  let hdr, ihdr = parse_header lexbuf in
  let q = parse_question ihdr s lexbuf in
  let answer = parse_rr ihdr.msg_ancount hdr s lexbuf in
  let authority = parse_rr ihdr.msg_nscount hdr s lexbuf in
  let additional = parse_rr ihdr.msg_arcount hdr s lexbuf in
  at_eof lexbuf;
  { msg_header = hdr;
    msg_question = q;
    msg_answer = answer;
    msg_authority = authority;
    msg_additional = additional
  }


let peek_header s =
  let lexbuf = Lexing.from_string s in
  let hdr, _ = parse_header lexbuf in
  hdr

(**********************************************************************)
(* Printing                                                           *)
(**********************************************************************)

let print_character_string buf s =
  if String.length s > 255 then
    invalid_arg "Netdns_message.print: String too long";
  Buffer.add_char buf (Char.chr (String.length s));
  Buffer.add_string buf s


let print_domain_name buf nametbl dname =

  let rec print_labels dname =
    match dname with
      | [] ->
	  Buffer.add_char buf '\000'

      | label :: dname' ->
	  ( let l = String.length label in
	    if l = 0 then
	      cannot_gen_msg "Empty label not allowed";
	    if l > 63 then
	      cannot_gen_msg "Domain label too long";
	    try
	      let pos = Hashtbl.find nametbl dname in (* or Not_found *)
	      let k0 = pos lsr 8 in
	      let k1 = pos land 0xff in
	      Buffer.add_char buf (Char.chr (192 + k0));
	      Buffer.add_char buf (Char.chr k1)
	    with
	      | Not_found ->
		  let pos = Buffer.length buf in
		  if pos < 16384 then
		    Hashtbl.replace nametbl dname pos;
		  Buffer.add_char buf (Char.chr l);
		  Buffer.add_string buf label;
		  print_labels dname'
	  )
  in

  let length =
    List.fold_left
      (fun acc label -> acc + String.length label + 1)
      1  (* for the empty root label *)
      dname in

  if length > 255 then
    cannot_gen_msg "Domain name too long";

  print_labels dname


let print_uint2 buf n =
  Buffer.add_char buf (Char.chr ((n lsr 8) land 0xff));
  Buffer.add_char buf (Char.chr (n land 0xff))


let print_uint4 buf n =
  Buffer.add_string buf (Rtypes.uint4_as_string n)


let print_int4 buf n =
  Buffer.add_string buf (Rtypes.int4_as_string n)


let print_header buf nametbl hdr ihdr =
  print_uint2 buf hdr.msg_id;
  let opcode =
    match hdr.msg_opcode with
      | `QUERY -> 0
      | `IQUERY -> 1
      | `STATUS -> 2
      | `Unknown n -> n land 15 in
  let rcode =
    match hdr.msg_rcode with
      | `No_error -> 0
      | `Format_error -> 1
      | `Server_failure -> 2
      | `Name_error -> 3
      | `Not_implemented -> 4
      | `Refused ->5
      | `Unknown n -> n land 15 in
  let bits =
    (if hdr.msg_is_query then 0 else 0x8000) 
    lor (opcode lsl 11)
    lor (if hdr.msg_is_aa then 0x0400 else 0)
    lor (if hdr.msg_is_trunc then 0x0200 else 0)
    lor (if hdr.msg_rd then 0x0100 else 0)
    lor (if hdr.msg_ra then 0x0080 else 0)
    lor rcode in
  print_uint2 buf bits;

  print_uint2 buf ihdr.msg_qdcount;
  print_uint2 buf ihdr.msg_ancount;
  print_uint2 buf ihdr.msg_nscount;
  print_uint2 buf ihdr.msg_arcount


let int_of_qtype =
  function
    | `A -> 1
    | `NS -> 2
    | `MD -> 3
    | `MF -> 4
    | `CNAME -> 5
    | `SOA -> 6
    | `MB -> 7
    | `MG -> 8
    | `MR -> 9
    | `NULL -> 10
    | `WKS -> 11
    | `PTR -> 12
    | `HINFO -> 13
    | `MINFO -> 14
    | `MX -> 15
    | `TXT -> 16
    | `AXFR -> 252 
    | `MAILB -> 253
    | `MAILA -> 254
    | `ANY -> 255
    | `Unknown n -> n land 0xffff

let int_of_qclass =
  function
    | `IN_ -> 1
    | `Unknown n -> n land 0xffff

let print_q_entry buf nametbl qe =
  print_domain_name buf nametbl qe.q_name;
  print_uint2 buf (int_of_qtype qe.q_type);
  print_uint2 buf (int_of_qclass qe.q_class)


let print_rr_entry buf nametbl patches rre =
  print_domain_name buf nametbl rre.rr_name;
  print_uint2 buf (int_of_qtype (rre.rr_type :> qtype));
  print_uint2 buf (int_of_qclass (rre.rr_class :> qclass));
  print_uint4 buf rre.rr_ttl;
  let rdlength_pos = Buffer.length buf in
  print_uint2 buf 0;   (* rdlength; later patched *)
  let start = Buffer.length buf in
  ( match rre.rr with
      | `CNAME cname ->
	  if rre.rr_type <> `CNAME then
	    invalid_arg "Netdns_message.print: rr_type does not match rr";
	  print_domain_name buf nametbl cname
	    
      | `HINFO(cpu,os) ->
	  if rre.rr_type <> `HINFO then
	    invalid_arg "Netdns_message.print: rr_type does not match rr";
	  print_character_string buf cpu;
	  print_character_string buf os
	    
      | `MX(pref,exch) ->
	  if rre.rr_type <> `MX then
	    invalid_arg "Netdns_message.print: rr_type does not match rr";
	  print_uint2 buf pref;
	  print_domain_name buf nametbl exch
	    
      | `NS name ->
	  if rre.rr_type <> `NS then
	    invalid_arg "Netdns_message.print: rr_type does not match rr";
	  print_domain_name buf nametbl name
	    
      | `PTR name ->
	  if rre.rr_type <> `PTR then
	    invalid_arg "Netdns_message.print: rr_type does not match rr";
	  print_domain_name buf nametbl name
	    
      | `SOA soa ->
	  if rre.rr_type <> `SOA then
	    invalid_arg "Netdns_message.print: rr_type does not match rr";
	  print_domain_name buf nametbl soa.soa_mname;
	  print_domain_name buf nametbl soa.soa_rname;
	  print_uint4 buf soa.soa_serial;
	  print_int4 buf soa.soa_refresh;
	  print_int4 buf soa.soa_retry;
	  print_int4 buf soa.soa_expire;
	  print_uint4 buf soa.soa_minimum
	    
      | `TXT l ->
	  if rre.rr_type <> `TXT then
	    invalid_arg "Netdns_message.print: rr_type does not match rr";
	  List.iter
	    (fun s ->
	       print_character_string buf s)
	    l
	    
      | `A addr ->
	  if rre.rr_type <> `A then
	    invalid_arg "Netdns_message.print: rr_type does not match rr";
	  if rre.rr_class <> `IN_ then
	    invalid_arg "Netdns_message.print: rr_class must be `IN_ for A queries";
	  let a =
	    Scanf.sscanf 
	      addr
	      "%u.%u.%u.%u%!"
	      (fun a0 a1 a2 a3 ->
		 if a0 > 255 || a1 > 255 || a2 > 255 || a3 > 255 then
		   invalid_arg "Netdns_message.print: invalid A address";
		 let c0 = Char.chr a0 in
		 let c1 = Char.chr a1 in
		 let c2 = Char.chr a2 in
		 let c3 = Char.chr a3 in
		 Rtypes.uint4_as_string (Rtypes.mk_uint4 (c0,c1,c2,c3))) in
	  Buffer.add_string buf a

      | `Unknown ->
	  invalid_arg "Netdns_message.print: Cannot print `Unknown RR entries"
  );
  let end_ = Buffer.length buf in
  let rdlength = end_ - start in
  if rdlength > 65535 then
    cannot_gen_msg "RR entry too long";
  patches := ( (rdlength_pos, rdlength lsr 8)
	       :: (rdlength_pos+1, rdlength land 0xff) 
	       :: !patches)


let print m =
  let buf = Buffer.create 512 in
  let nametbl = Hashtbl.create 50 in
  let ihdr =
    { msg_qdcount = List.length m.msg_question;
      msg_ancount = List.length m.msg_answer;
      msg_nscount = List.length m.msg_authority;
      msg_arcount = List.length m.msg_additional;
    } in

  if ihdr.msg_qdcount > 65535 then
    cannot_gen_msg "Too many questions";
  if ihdr.msg_ancount > 65535 then
    cannot_gen_msg "Too many answer RR entries";
  if ihdr.msg_nscount > 65535 then
    cannot_gen_msg "Too many authority RR entries";
  if ihdr.msg_arcount > 65535 then
    cannot_gen_msg "Too many additional RR entries";

  print_header buf nametbl m.msg_header ihdr;

  let patches = ref [] in
  List.iter
    (print_q_entry buf nametbl)
    m.msg_question;
  List.iter
    (print_rr_entry buf nametbl patches)
    m.msg_answer;
  List.iter
    (print_rr_entry buf nametbl patches)
    m.msg_authority;
  List.iter
    (print_rr_entry buf nametbl patches)
    m.msg_additional;

  let s = Buffer.contents buf in
  List.iter
    (fun (pos, c) ->
       s.[pos] <- Char.chr c)
    !patches;

  s
