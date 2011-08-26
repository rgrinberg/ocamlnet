(* $Id$ *)

{

  exception Bad_message of string


  let max_domain_pointers = 10

}

rule domain_name length msg = parse
  (* length: How many chars have already been parsed
   * msg: The whole message in UDP format
   *)
  | '\000' 
      { (* A label with length zero ends the domain name *)
	if length = 255 then
	  raise(Bad_message "Domain name too long");
	[]
      }
  | [ '\001' - '\063' ] as c
      { (* Uncompressed label *)
	let l = Char.code c in
	if length + l + 1 > 255 then
	  raise(Bad_message "Domain name too long");
	let s = String.create l in
	for k = 0 to l - 1 do
	  s.[k] <- single_char lexbuf;
	done;
	s :: domain_name (length+l+1) msg lexbuf
      }
  | ([ '\192' - '\255' ] as c1) (_ as c2)
      { (* Compressed label *)
	let offset = (Char.code c1 - 192) * 256 + Char.code c2 in
	let lexbuf' = Lexing.from_string msg in
	for k = 1 to offset do
	  ignore(single_char lexbuf')
	done;
	domain_name length msg lexbuf'
	  (* TODO: Prevent recursive names *)
      }
  | _ 
      { raise(Bad_message "Invalid length byte found in domain name") }
  | eof 
      { raise(Bad_message "EOF in domain name") }


and character_strings = parse
  | _ as c 
      { let l = Char.code c in
	let s = String.create l in
	for k = 0 to l - 1 do
	  s.[k] <- single_char lexbuf
	done;
	s :: character_strings lexbuf
      }
  | eof
      { [] }


and character_string = parse
  | _ as c
      { let l = Char.code c in
	let s = String.create l in
	for k = 0 to l - 1 do
	  s.[k] <- single_char lexbuf
	done;
	s
      }
  | eof 
      { raise(Bad_message "EOF in character string") }


and single_char = parse
  | _ as c 
      { c }
  | eof 
      { raise(Bad_message "EOF in domain or character string") }


and skip n = parse   (* n>=1 *)
  | _
      { if n > 1 then skip (n-1) lexbuf else () }
  | eof
      { raise(Bad_message "EOF in skipped message part") }


and qtype = parse
  (* TYPE *)
  | '\000' '\001'  { `A }
  | '\000' '\002'  { `NS }
  | '\000' '\003'  { `MD }   (* obsolete *)
  | '\000' '\004'  { `MF }   (* obsolete *)
  | '\000' '\005'  { `CNAME }
  | '\000' '\006'  { `SOA }
  | '\000' '\007'  { `MB }   (* experimental *)
  | '\000' '\008'  { `MG }   (* experimental *)
  | '\000' '\009'  { `MR }   (* experimental *)
  | '\000' '\010'  { `NULL } (* experimental *)
  | '\000' '\011'  { `WKS }
  | '\000' '\012'  { `PTR }
  | '\000' '\013'  { `HINFO }
  | '\000' '\014'  { `MINFO }
  | '\000' '\015'  { `MX }
  | '\000' '\016'  { `TXT }

  (* QTYPES *)
  | '\000' '\252'  { `AXFR }
  | '\000' '\253'  { `MAILB }
  | '\000' '\254'  { `MAILA }
  | '\000' '\255'  { `ANY }   (* "*" *)

  | (_ as c1) (_ as c2)
      { `Unknown(Char.code c1 * 256 + Char.code c2) }
  | eof 
      { raise(Bad_message "EOF in (q)type") }


and qclass = parse
  | '\000' '\001'  { `IN_ }
  | (_ as c1) (_ as c2)
      { `Unknown(Char.code c1 * 256 + Char.code c2) }
  | eof
      { raise(Bad_message "EOF in class") }

and uint4 = parse
  | (_ as c1) (_ as c2) (_ as c3) (_ as c4)
      { Rtypes.mk_uint4 (c1,c2,c3,c4) }
  | eof
      { raise(Bad_message "EOF in uint4") }

and int4 = parse
  | (_ as c1) (_ as c2) (_ as c3) (_ as c4)
      { Rtypes.mk_int4 (c1,c2,c3,c4) }
  | eof
      { raise(Bad_message "EOF in int4") }

and uint2 = parse
  | (_ as c1) (_ as c2)
      { Char.code c1 * 256 + Char.code c2 }
  | eof
      { raise(Bad_message "EOF in uint2") }

and at_eof = parse
  | eof { () }
  | _ 
      { raise(Bad_message "EOF missing") }
