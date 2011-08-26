(* $Id$ *)

(** Parsing and printing of DNS messages *)

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

type q_entry =
    { q_name : domain;
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


val is_sound : rr_entry -> bool
  (** Test whether:
    * - [rr_entry] matches [rr]
   *)

val parse : string -> msg
  (** Parses a DNS message. May raise [Netdns_lexer.Bad_message] *)

val peek_header : string -> msg_header
  (** Returns the header of the message (without parsing the rest).
   * May raise [Netdns_lexer.Bad_message].
   *)

exception Cannot_generate_message of string
  (** Raised when [print] cannot create the string *)

val print : msg -> string
  (** Prints a DNS message to a string or raise [Cannot_generate_message] *)
