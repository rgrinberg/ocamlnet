(* $Id$ 
 * ----------------------------------------------------------------------
 * Nethttp: Basic definitions for the HTTP protocol
 *)

(** {1 Basic definitions for the HTTP protocol} *)

(** These definitions can be used by both HTTP clients and servers, and by
  * protocols in the middle, e.g. CGI.
 *)

type protocol_version = 
    int * int           (* (major,minor) number *)
      (** A pair of a major and minor version number *)

type protocol_attribute =
  [ `Secure_https
  ]

type protocol =
  [ `Http of (protocol_version * protocol_attribute list)
  | `Other
  ]
  (** The base protocol. RFC 2145 defines how to interpret major and minor numbers.
   *  In particular, we have:
   * - [`Http((0,9),_)] is the ancient HTTP version 0.9
   * - [`Http((1,n),_)] is the HTTP protocol 1.n.  It is expected that all these versions
   *   are compatible to each other except negotiable features.
   * - [`Http((m,_),_)] for m>1 is regarded as unknown protocol, incompatible to 
   *   any [`Http((1,n),_)]
   * - [`Other] is anything else (unrecognizes protocol)
   *
   *)

val string_of_protocol : protocol -> string
  (** Returns the string representation, e.g. "HTTP/1.0". Fails for [`Other] *)

val protocol_of_string : string -> protocol
  (** Parses the protocol string, e.g. "HTTP/1.0". Returns [`Other] for unrecognized
    * strings
   *)

type http_status =   (* Status codes from RFC 2616 *)
  (* 1xx: (informational) *)
  [ `Continue
  | `Switching_protocols 
  (* 2xx: (successful) *)
  | `Ok
  | `Created
  | `Accepted
  | `Non_authoritative
  | `No_content
  | `Reset_content
  | `Partial_content
  (* 3xx: (redirection) *)
  | `Multiple_choices
  | `Moved_permanently
  | `Found
  | `See_other
  | `Not_modified
  | `Use_proxy
  | `Temporary_redirect
  (* 4xx: (client error) *)
  | `Bad_request
  | `Unauthorized
  | `Payment_required
  | `Forbidden
  | `Not_found
  | `Method_not_allowed
  | `Not_acceptable
  | `Proxy_auth_required
  | `Request_timeout
  | `Conflict
  | `Gone
  | `Length_required
  | `Precondition_failed
  | `Request_entity_too_large
  | `Request_uri_too_long
  | `Unsupported_media_type
  | `Requested_range_not_satisfiable
  | `Expectation_failed
  (* 5xx: (server error) *)
  | `Internal_server_error
  | `Not_implemented
  | `Bad_gateway
  | `Service_unavailable
  | `Gateway_timeout
  | `Http_version_not_supported 
  ]
  (** HTTP response status:
   * 
   * {b Informational (1xx):}
   * - [`Continue]
   * - [`Switching_protocols]
   *
   * {b Successful (2xx):}
   * - [`Ok]
   * - [`Created]
   * - [`Accepted]
   * - [`Non_authoritative]
   * - [`No_content]
   * - [`Reset_content]
   * - [`Partial_content]
   *
   * {b Redirection (3xx):}
   * - [`Multiple_choices]
   * - [`Moved_permanently]
   * - [`Found]
   * - [`See_other]
   * - [`Not_modified]
   * - [`Use_proxy]
   * - [`Temporary_redirect]
   * 
   * {b Client error (4xx):}
   * - [`Bad_request]
   * - [`Unauthorized]
   * - [`Payment_required]
   * - [`Forbidden]
   * - [`Not_found]
   * - [`Method_not_allowed]
   * - [`Not_acceptable]
   * - [`Proxy_auth_required]
   * - [`Request_timeout]
   * - [`Conflict]
   * - [`Gone]
   * - [`Length_required]
   * - [`Precondition_failed]
   * - [`Request_entity_too_large]
   * - [`Request_uri_too_long]
   * - [`Unsupported_media_type]
   * - [`Request_range_not_satisfiable]
   * - [`Expectation_failed]
   *
   * {b Server Error (5xx):}
   * - [`Internal_server_error]
   * - [`Not_implemented]
   * - [`Bad_gateway]
   * - [`Service_unavailable]
   * - [`Gateway_timeout]
   * - [`Http_version_not_supported]
   *)


val int_of_http_status : http_status -> int
  (** Returns the integer code for a status value *)

val http_status_of_int : int -> http_status
  (** Returns the status value for an integer code, or raises [Not_found] *)

val string_of_http_status : http_status -> string
  (** Returns the informational text for a status value *)
  (* See also Netcgi.status_line *)

type http_method = string * string
  (** Method name, URI *)

type cache_control_token =
    [ `No_store
    | `Max_age of int
    | `Max_stale of int option
    | `Min_fresh of int
    | `No_transform
    | `Only_if_cached
    | `Public
    | `Private of string list
    | `No_cache of string list
    | `Must_revalidate
    | `Proxy_revalidate
    | `S_maxage of int
    | `Extension of string * string option
    ]
    (** The cache control token for the [Cache-control] header *)

type etag =
    [ `Weak of string
    | `Strong of string
    ]
   (** Entity tags can be weak or strong *)

val weak_validator_match : etag -> etag -> bool
  (** Whether the tags match weakly (see RFC 2616 for definition) *)

val strong_validator_match : etag -> etag -> bool
  (** Whether the tags match strongly (see RFC 2616 for definition) *)

exception Bad_header_field of string
  (** Raised when a header field cannot be parsed. The string argument is the
    * name of the failing function
   *)

class type http_header = Netmime.mime_header
class type http_header_ro = Netmime.mime_header_ro
  (** The HTTP header is represented as MIME header *)

class type http_trailer = Netmime.mime_header
class type http_trailer_ro = Netmime.mime_header_ro
  (** The HTTP trailer is represented as MIME header *)

val status_of_cgi_header : http_header -> (int * string)
  (** Returns the status code and the status text corresponding to the [Status]
    * header
   *)

type cookie = (* Cgi.cookie =  *)
    { cookie_name : string;
        (** The name of the cookie *)
      cookie_value : string;
        (** The value of the cookie. There are no restrictions on the
	 * value of the cookie
	 *)
      cookie_expires : float option;
        (** Expiration:
	 *  - [None]: the cookie expires when the browser session ends.
         *  - [Some t]: the cookie expires at the time [t] (seconds since
	 *    the epoch)
         *) 
      cookie_domain : string option;
        (** Cookies are bound to a certain domain, i.e. the browser sends
	 * them only when web pages of the domain are requested:
	 *
	 *  - [None]: the domain is the hostname of the server
         *  - [Some domain]: the domain is [domain]
         *)
      cookie_path : string option;
        (** Cookies are also bound to certain path prefixes, i.e. the browser
	 * sends them only when web pages at the path or below are requested.
	 *
	 * - [None]: the path is script name + path_info
         * - [Some p]: the path is [p]. With [Some "/"] you can disable the
	 *   path restriction completely.
         *)
      cookie_secure : bool;
        (** Cookies are also bound to the type of the web server: 
	 * [false] means servers without SSL, [true] means servers with
	 * activated SSL ("https").
	 *)
    }

val decode_query : string -> (string * string)
  (** Splits the URI into a "script name" and a "query string" *)

val split_host_port : string -> (string * int option)
  (** Splits the [Host] header in hostname and optional port number.
    * Fails on syntax error
   *)

val uripath_encode : string -> string
  (** Encodes unsafe characters in URI paths. The slash character is not encoded.
    * This function should only be applied to the part before '?'.
   *)

val uripath_decode : string -> string
  (** Decodes %XX sequences in URI paths. %2F is forbidden (failure).
    * This function should only be applied to the part before '?'.
   *)


(**********************************************************************)

(** {2 Parsing and Printing of Headers} *)

module Header : sig
  (** This module is a parser/printer for the header fields used in HTTP/1.1.
    * The [get_*] functions generally raise [Not_found] when the queried header
    * is not present. If the syntax of the field is a comma-separated list of
    * multiple values, the [get_*] functions generally merge all headers of 
    * the same type. The order is preserved in this case. The list [[]] means
    * that the header exists, but only with empty value. For example,
    *
    * {[
    * Accept: text/html
    * Accept: text/plain
    * ]}
    *
    * would be returned as [["text/html",[],[]; "text/plain", [],[]]] by [get_accept].
    * The header
    *
    * {[Accept:]}
    *
    * would be returned as [[]].
    *
    * The [set_*] functions generally produce only a single header with comma-
    * separated values. Existing header are overwritten/removed.
    *
    * To remove a header, simply use the [delete_field] method of [http_header].
    *
    * Error behaviour: The [get_*] functions raise [Bad_header_field] when they
    * cannot parse a header field. The [set_*] functions raise [Invalid_argument]
    * when an invalid value is passed to them (only very few functions do that).
    * The argument of both exceptions is the function name.
   *)

  val get_accept : #http_header_ro -> (string *
		                 (string * string) list *
			        (string * string) list) list
    (** Returns the [Accept] header as list of triples 
      * [(media_range, media_range_params, accept_params)].
      * If there are [accept_params], the first such parameter is always ["q"].
      *
      * All present [Accept] headers are merged. The function returns [[]] when
      * there is at least one [Accept] header, but none of the headers has a
      * non-empty value. The function raises [Not_found] if there no such headers
      * at all (which should be interpreted as [ ["*/*",[],[] ] ]).
     *)

  val best_media_type : #http_header_ro -> string list -> ( string * (string * string) list )
    (** Returns the best media type for a header and a list of supported types.
      * If any type is acceptable, "*/*" will be returned. If no type is acceptable,
      * "" will be returned.
      * The supported media types should be sorted such that the best type
      * is mentioned first.
      *
      * Known bug: The rule that media ranges are sorted by degree of "specificness"
      * is not implemented, e.g. that "text/html" is preferred over other "text/*"
      * when both have the same "q" value. 
     *)

  val set_accept : #http_header -> (string *
		                 (string * string) list *
			        (string * string) list) list -> unit
    (** Sets the [Accept] header *)

  val get_accept_charset : #http_header_ro -> (string *
					 (string * string) list) list
    (** Returns the [Accept-charset] header as list of pairs [(charset,params)].
      * The only mentioned parameter in RFC 2616 is ["q"].
      *
      * All present [Accept-charset] headers are merged. The function raises [Not_found]
      * when there is no [Accept-charset] header (which should be interpreted as
      * [ ["*",[]] ]).
     *)

  val best_charset : #http_header_ro -> string list -> string
    (** Returns the best charset for a header and a list of supported charsets.
      * If any charset is acceptable, "*" will be returned.
      * The supported charsets should be sorted such that the best charset
      * is mentioned first.
      *
      * This function already implements the special handling of ISO-8859-1
      * mentioned in RFC 2616.
     *)

  val set_accept_charset : #http_header -> (string *
					 (string * string) list) list -> unit
    (** Sets the [Accept-charset] header *)

  val get_accept_encoding : #http_header_ro -> (string *
					(string * string) list) list
    (** Returns the [Accept-encoding] header as list of pairs [(coding,params)].
      * The only mentioned parameter in RFC 2616 is ["q"]. The RFC describes
      * compatibility problems with the "q" parameter.
      *
      * All present [Accept-encoding] headers are merged. The function raises [Not_found]
      * when there is no [Accept-encoding] header (which should be interpreted as
      * [ ["identity",[]] ]). The return value [[]] must be interpreted as
      * [ ["identity",[]] ].
     *)

  val best_encoding : #http_header_ro -> string list -> string
    (** Returns the best encoding for a header and a list of supported encodings.
      * If anything else fails, "identity" will be returned.
      * The supported encodings should be sorted such that the best encoding
      * is mentioned first.
     *)

  val set_accept_encoding : #http_header -> (string *
					(string * string) list) list -> unit
    (** Sets the [Accept-encoding] header *)

  val get_accept_language : #http_header_ro -> (string *
					(string * string) list) list
    (** Returns the [Accept-language] header as list of pairs [(lang_range,params)].
      * The only mentioned parameter in RFC 2616 is ["q"].
      *
      * All present [Accept-language] headers are merged. The function raises [Not_found]
      * when there is no [Accept-language] header (which should be interpreted as
      * [ ["*",[]] ]). 
     *)

  val set_accept_language : #http_header -> (string *
					(string * string) list) list -> unit
    (** Sets the [Accept-language] header *)

  val get_accept_ranges : #http_header_ro -> string list
    (** Returns the [Accept-ranges] header as list of tokens.
      * 
      * All present [Accept-ranges] headers are merged. The function raises [Not_found]
      * when there is no [Accept-ranges] header. The RFC leaves it open how this is
      * to be interpreted in general.
     *)

  val set_accept_ranges : #http_header -> string list -> unit
    (** Sets the [Accept-ranges] header *)

  val get_age : #http_header_ro -> float
    (** Returns the [Age] header as number
     *)

  val set_age : #http_header -> float -> unit
    (** Sets the [Age] header *)

  val get_allow : #http_header_ro -> string list
    (** Returns the [Allow] header as list of tokens.
      *
      * All present [Allow] headers are merged. The function raises [Not_found]
      * when there is no [Allow] header. The RFC leaves it open how this is
      * to be interpreted in general.
     *)

  val set_allow : #http_header -> string list -> unit
    (** Sets the [Allow] header *)

  val get_authorization : #http_header_ro -> (string * (string * string) list)
    (** Returns the [Authorization] header as pair [(auth_scheme,auth_params)],
      * or raises [Not_found] if not present.
      *
      * The "Basic" authentication scheme is represented specially as
      * [("basic", [ "credentials", creds ])] where [creds] are the
      * Base64-encoded credentials.
     *)

  val set_authorization : #http_header -> 
                               (string * (string * string) list) -> unit
    (** Sets the [Authorization] header.
      * The "Basic" authentication scheme is represented as explained for
      * [get_authorization].
     *)

  val get_cache_control : #http_header_ro -> cache_control_token list
    (** Returns the [Cache-control] header as list of tokens.
      *
      * All present [Cache-control] headers are merged. The function raises [Not_found]
      * when there is no [Cache-control] header. 
     *)

  val set_cache_control : #http_header -> cache_control_token list -> unit
    (** Sets the [Cache-control] header *)

  val get_connection : #http_header_ro -> string list
    (** Returns the [Connection] header as list of tokens.
      *
      * All present [Connection] headers are merged. The function raises [Not_found]
      * when there is no [Connection] header. 
      *
      * The Connection header must be ignored when received from a HTTP/1.0 client.
     *)

  val set_connection : #http_header -> string list -> unit
    (** Sets the [Connection] header *)

  val get_content_encoding : #http_header_ro -> string list
    (** Returns the [Content-encoding] header as list of tokens.
      *
      * All present [Content-encoding] headers are merged. The function raises [Not_found]
      * when there is no [Content-encoding] header. 
     *)

  val set_content_encoding : #http_header -> string list -> unit
    (** Sets the [Content-encoding] header *)

  val get_content_language : #http_header_ro -> string list
    (** Returns the [Content-language] header as list of tokens.
      *
      * All present [Content-language] headers are merged. The function raises [Not_found]
      * when there is no [Content-language] header. 
     *)

  val set_content_language : #http_header -> string list -> unit
    (** Sets the [Content-language] header *)

  val get_content_length : #http_header_ro -> int64
    (** Returns the [Content-length] header as number. If the number is too big for
      * int64, the exception [Bad_header_field "Content-length"] will be raised.
      * Raises [Not_found] when the header is missing.
     *)

  val set_content_length : #http_header -> int64 -> unit
    (** Sets the [Content-length] header *)

  val get_content_location : #http_header_ro -> string
    (** Returns the [Content-location] header as string.  Raises [Not_found] when the 
      * header is missing.
     *)

  val set_content_location : #http_header -> string -> unit
    (** Sets the [Content-location] header *)

  val get_content_md5 : #http_header_ro -> string
    (** Returns the [Content-MD5] header as string. The Base64 encoding has not been
      * touched. Raises [Not_found] when the header is missing.
     *)

  val set_content_md5 : #http_header -> string -> unit
    (** Sets the [Content-MD5] header *)

  val get_content_range : #http_header_ro -> [ `Bytes of (int64*int64) option * int64 option ]
    (** Returns the [Content-range] header as 
      * [`Bytes(byte_range_resp_spec, instance_length)]. The option value [None]
      * corresponds to "*" in the RFC.
      * Raises [Not_found] when the header is missing.
     *)

  val set_content_range : #http_header -> 
                          [ `Bytes of (int64*int64) option * int64 option ] -> unit
    (** Sets the [Content-range] header *)

  val get_content_type : #http_header_ro -> string * (string * string) list
    (** Returns the [Content-type] header as pair [(media_type, params)].
      * Raises [Not_found] when the header is missing.
     *)

  val set_content_type : #http_header -> string * (string * string) list -> unit
    (** Sets the [Content-type] header *)

  val get_date : #http_header_ro -> float
    (** Returns the [Date] header as number (seconds since the Epoch).
      * Raises [Not_found] when the header is missing.
     *)

  val set_date : #http_header -> float -> unit
    (** Sets the [Date] header *)

  val get_etag : #http_header_ro -> etag
    (** Returns the [Etag] header.
      * Raises [Not_found] when the header is missing.
     *)

  val set_etag : #http_header -> etag -> unit
    (** Sets the [Etag] header *)

  val get_expect : #http_header_ro -> (string * string option * (string * string) list) list
    (** Returns the [Expect] header as list of triples [(token,value,params)].
      *
      * All present [Expect] headers are merged. The function raises [Not_found]
      * when there is no [Expect] header. 
     *)

  val set_expect : #http_header -> 
                   (string * string option * (string * string) list) list -> unit
    (** Sets the [Expect] header *)

  val get_expires : #http_header_ro -> float
    (** Returns the [Expires] header as number (seconds since the Epoch).
      * Raises [Not_found] when the header is missing.
     *)

  val set_expires : #http_header -> float -> unit
    (** Sets the [Expires] header *)

  val get_from : #http_header_ro -> string
    (** Returns the [From] header as string.
      * Raises [Not_found] when the header is missing.
     *)

  val set_from : #http_header -> string -> unit
    (** Sets the [From] header *)

  val get_host : #http_header_ro -> string * int option
    (** Returns the [Host] header as pair [(host,port)].
      * Raises [Not_found] when the header is missing.
     *)

  val set_host : #http_header -> string * int option -> unit
    (** Sets the [Host] header *)

  val get_if_match : #http_header_ro -> etag list option
    (** Returns the [If-match] header. The value [None] means "*".
      * Raises [Not_found] when the header is missing.
     *)

  val set_if_match : #http_header -> etag list option -> unit
    (** Sets the [If-match] header *)

  val get_if_modified_since : #http_header_ro -> float
    (** Returns the [If-modified-since] header as number (seconds since the Epoch).
      * Raises [Not_found] when the header is missing.
     *)

  val set_if_modified_since : #http_header -> float -> unit
    (** Sets the [If-modified-since] header *)

  val get_if_none_match : #http_header_ro -> etag list option
    (** Returns the [If-none-match] header. The value [None] means "*".
      * Raises [Not_found] when the header is missing.
     *)

  val set_if_none_match : #http_header -> etag list option -> unit
    (** Sets the [If-none-match] header *)

  val get_if_range : #http_header_ro -> [ `Etag of etag | `Date of float ]
    (** Returns the [If-range] header. 
      * Raises [Not_found] when the header is missing.
     *)
  val set_if_range : #http_header -> [ `Etag of etag | `Date of float ] -> unit
    (** Sets the [If-range] header *)

  val get_if_unmodified_since : #http_header_ro -> float
    (** Returns the [If-unmodified-since] header as number (seconds since the Epoch).
      * Raises [Not_found] when the header is missing.
     *)

  val set_if_unmodified_since : #http_header -> float -> unit
    (** Sets the [If-unmodified-since] header *)

  val get_last_modified : #http_header_ro -> float
    (** Returns the [Last-modified] header as number (seconds since the Epoch).
      * Raises [Not_found] when the header is missing.
     *)

  val set_last_modified : #http_header -> float -> unit
    (** Sets the [Last-modified] header *)

  val get_location : #http_header_ro -> string
    (** Returns the [Location] header as string.
      * Raises [Not_found] when the header is missing.
     *)
  val set_location : #http_header -> string -> unit
    (** Sets the [Location] header *)

  val get_max_forwards : #http_header_ro -> int
    (** Returns the [Max-forwards] header as number.
      * Raises [Not_found] when the header is missing.
     *)

  val set_max_forwards : #http_header -> int -> unit
    (** Sets the [Max-forwards] header *)

  val get_pragma : #http_header_ro -> (string * string option) list
    (** Returns the [Pragma] header as list of pairs [(token,value)].
      *
      * All present [Pragma] headers are merged. The function raises [Not_found]
      * when there is no [Pragma] header. 
     *)

  val set_pragma : #http_header -> (string * string option) list -> unit
    (** Sets the [Pragma] header *)

  val get_proxy_authenticate : #http_header_ro -> 
                                 (string * (string * string) list) list
    (** Returns the [Proxy-authenticate] header as list of challenges
      * [(auth_scheme,auth_params)].
      *
      * All present [Proxy-authenticate] headers are merged. The function raises [Not_found]
      * when there is no [Proxy-authenticate] header. 
     *)

  val set_proxy_authenticate : #http_header -> 
                                 (string * (string * string) list) list -> unit
    (** Sets the [Proxy-authenticate] header *)

  val get_proxy_authorization : #http_header_ro -> (string * (string * string) list)
    (** Returns the [Proxy-authorization] header as pair 
      * [(auth_scheme,auth_params)].Raises [Not_found] when the header is
      * missing.
      *
      * The "Basic" authentication scheme is represented specially as
      * [("basic", [ "credentials", creds ])] where [creds] are the
      * Base64-encoded credentials.
     *)

  val set_proxy_authorization : #http_header -> 
                                   (string * (string * string) list)-> unit
    (** Sets the [Proxy-authorization] header 
      * The "Basic" authentication scheme is represented as explained for
      * [get_proxy_authorization].
      *)

  val get_range : #http_header_ro -> [`Bytes of (int64 option * int64 option) list ]
    (** Returns the [Range] header as [`Bytes ranges], where the list [ranges]
      * has elements of the form [(Some first_pos, Some last_pos)],
      * [(Some first_pos, None)] (prefix range), or [(None, Some last_pos)] (suffix
      * range).
      * Raises [Not_found] when the header is missing.
     *)

  val set_range : #http_header -> [`Bytes of (int64 option * int64 option) list ] -> unit
    (** Sets the [Range] header *)

  val get_referer : #http_header_ro -> string
    (** Returns the [Referer] header as string.
      * Raises [Not_found] when the header is missing.
     *)

  val get_referrer : #http_header_ro -> string
    (** Same, for addicts of correct orthography *)

  val set_referer : #http_header -> string -> unit
    (** Sets the [Referer] header *)

  val set_referrer : #http_header -> string -> unit
    (** Same, for addicts of correct orthography *)

  val get_retry_after : #http_header_ro -> [ `Date of float | `Seconds of int ]
    (** Returns the [Retry-after] header.
      * Raises [Not_found] when the header is missing.
     *)

  val set_retry_after : #http_header -> [ `Date of float | `Seconds of int ] -> unit
    (** Sets the [Retry-after] header *)

  val get_server : #http_header_ro -> string
    (** Returns the [Server] header as uninterpreted string (including comments).
      * Raises [Not_found] when the header is missing.
     *)

  val set_server : #http_header -> string -> unit
    (** Sets the [Server] header *)

  val get_te : #http_header_ro -> (string *
		              (string * string) list *
		              (string * string) list) list
    (** Returns the [TE] header as list of triples 
      * [(te_token, te_params, accept_params)].
      * If there are [accept_params], the first such parameter is always ["q"].
      *
      * All present [TE] headers are merged. The function returns [[]] when
      * there is at least one [TE] header, but none of the headers has a
      * non-empty value. The function raises [Not_found] if there no such headers
      * at all.
     *)

  val set_te : #http_header -> (string *
		              (string * string) list *
		              (string * string) list) list -> unit
    (** Sets the [TE] header *)

  val get_trailer : #http_header_ro -> string list
    (** Returns the [Trailer] header as list of field names.
      *
      * All present [Trailer] headers are merged. The function returns [[]] when
      * there is at least one [Trailer] header, but none of the headers has a
      * non-empty value. The function raises [Not_found] if there no such headers
      * at all.
     *)

  val set_trailer : #http_header -> string list -> unit
    (** Sets the [Trailer] header *)

  val get_transfer_encoding : #http_header_ro -> (string * (string * string) list) list
    (** Returns the [Transfer-encoding] header as list of pairs
      * [(token, params)].
      *
      * All present [Transfer-encoding] headers are merged. The function returns [[]] when
      * there is at least one [Transfer-encoding] header, but none of the headers has a
      * non-empty value. The function raises [Not_found] if there no such headers
      * at all.
     *)

  val set_transfer_encoding : #http_header -> (string * (string * string) list) list -> unit
    (** Sets the [Transfer-encoding] header *)

  val get_upgrade : #http_header_ro -> string list
    (** Returns the [Upgrade] header as list of products.
      * 
      * All present [Upgrade] headers are merged. The function returns [[]] when
      * there is at least one [Upgrade] header, but none of the headers has a
      * non-empty value. The function raises [Not_found] if there no such headers
      * at all.
     *)

  val set_upgrade : #http_header -> string list -> unit
    (** Sets the [Upgrade] header *)

  val get_user_agent : #http_header_ro -> string
    (** Returns the [User-agent] header as uninterpreted string (including comments).
      * Raises [Not_found] if the header is missing.
     *)

  val set_user_agent : #http_header -> string -> unit
    (** Sets the [User-agent] header *)

  val get_vary : #http_header_ro -> [ `Star | `Fields of string list ]
    (** Returns the [Vary] header.
      * Raises [Not_found] if the header is missing.
     *)

  val set_vary : #http_header -> [ `Star | `Fields of string list ] -> unit
    (** Sets the [Vary] header *)
(*
  val get_via : #http_header_ro -> (string option * string * string * string option) list
    (** Returns the [Via] header as list of tuples
      * [(proto_name, proto_version, received_by, comment)].
      *
      * All present [Via] headers are merged. 
      * Raises [Not_found] if the header is missing.
     *)
 *)
(*
  val set_via : #http_header -> (string option * string * string * string option) list -> unit
    (** Sets the [Via] header *)
 *)
(*
  val get_warning : #http_header_ro -> (int * string * string * float option) list
    (** Returns the [Warning] header as list of tuples
      * [(code, agent, text, date)].
      *
      * All present [Warning] headers are merged. 
      * Raises [Not_found] if the header is missing.
     *)
 *)
(*
  val set_warning : #http_header -> (int * string * string * float option) list -> unit
    (** Sets the [Warning] header *)
 *)

  val get_www_authenticate : #http_header_ro -> 
                                 (string * (string * string) list) list
    (** Returns the [WWW-Authenticate] header as list of challenges
      * [(auth_scheme,auth_params)].
      *
      * All present [WWW-Authenticate] headers are merged. 
      * Raises [Not_found] if the header is missing.
     *)

  val set_www_authenticate : #http_header -> 
                               (string * (string * string) list) list -> unit
    (** Sets the [WWW-Authenticate] header *)

  val get_cookie : #http_header_ro -> (string * string) list
    (** Get the cookies as (name,value) pairs (or Not_found) *)

  val set_cookie : #http_header -> (string * string) list -> unit
    (** Set the [Cookie] header. Note: This does not set cookies in the client, use 
      * [set_set_cookie] instead!
     *)

(*
  val get_set_cookie : #http_header_ro -> cookie list
 *)

  val set_set_cookie : #http_header -> cookie list -> unit
    (** Set the [Set-Cookie] header
     *)


end
