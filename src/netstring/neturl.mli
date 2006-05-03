(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* This module applies already O'Caml-3 features. *)

(** Uniform Resource Locators (URLs)
 *
 * {b Contents}
 *
 * - {!Neturl.interface}
 * - {!Neturl.tutorial}
 *)

(** {1:interface Interface}
 *
 * This module provides functions to parse URLs, to print URLs, to
 * store URLs, to modify URLs, and to apply relative URLs.
 *
 * URLs are strings formed according to pattern (1) or (2):
 *
 * + [scheme://user;userparams:password\@host:port/path;params?query#fragment]
 * + [scheme:other;params?query#fragment]
 *
 * The word at the beginning of the URL identifies the URL scheme
 * (such as "http" or "file"). Depending on the scheme, not all of the
 * parts are allowed, or parts may be omitted. This module defines the
 * type [url_syntax] whose values describe which parts are allowed/required/
 * not allowed for a concrete URL scheme (see below).
 *
 * Not all characters are allowed in a URL. Some characters are allowed,
 * but have the special task to separate the various parts of the URL
 * (reserved characters).
 * However, it is possible to include even invalid or reserved characters
 * as normal content by applying the [%]-encoding on these characters:
 * A ['%'] indicates that an encoded character follows, and the character
 * is denoted by a two-digit hexadecimal number (e.g. [%2f] for ['/']).
 * In the following descriptions, the term "encoded string" means a string
 * containing such [%]-encoded characters, and the "decoded string" means a
 * string not containing such characters.
 * See the module {!Netencoding.Url} for functions encoding or decoding
 * strings.
 *
 * The type [url] describes values storing the components of a URL,
 * and the [url_syntax] for the URL. In general, the components are
 * stored as encoded strings; however, not for all components the
 * [%]-encoding is applicable.
 *
 * For convenience, the functions creating, modifying, and accessing
 * URLs can handle both encoded and decoded strings. In order to
 * avoid errors, the functions pass strings even in their decoded form.
 *
 * Note that there is currently no function to compare URLs. The
 * canoncical comparison ( [=] ) is not applicable because the same URL
 * may be written in different ways.
 *
 * Note that nothing is said about the character set/encoding of URLs.
 * Some protocols and standards prefer UTF-8 as fundamental encoding
 * and apply the [%]-encoding on top of it; i.e. the byte sequence
 * representing a character in UTF-8 is [%]-encoded. 
 *
 * {b Standards Compliance}
 *
 * This module implements RFC 1738 and RFC 1808. There is also a newer
 * RFC, 2396, updating the former RFCs, but this module is not fully 
 * compatible with RFC 2396. The following (minor) problems may occur:
 *
 * - The module escapes more characters than needed. All characters that
 *   are "unsafe" or "reserved" in either RFC document are escaped.
 * - URL parameters (appended with a ";") are handled as in RFCs 1738/1808.
 *   In RFC 2396, every path component may have parameters, and the
 *   algorithm to resolve relative URLs is different in this point.
 *   If it is required to apply RFC 2396, one can disable URL parameters
 *   in the syntax, and extract them from the path by a self-written
 *   postprocessor. Usually, this is only required for [imap] URLs.
 *
 * In one point, RFC 2396 is preferred:
 *
 * - Authorities may be terminated by a question mark, as in
 *   ["http://host?query"]. This is illegal in RFC 1738. The consequence
 *   is, however, that question marks in user strings must be escaped.
 *)

exception Malformed_URL
(** Raised by a number of functions when encountering a badly formed
 * URL.
 *)

val extract_url_scheme : string -> string
  (** Returns the URL scheme from the string representation of an URL. 
   * E.g. [extract_url_scheme "http://host/path" = "http"]. 
   * The scheme name is always converted to lowercase characters.
   * Raises [Malformed_URL] if the scheme name is not found.
   *)

type url_syntax_option =
    Url_part_not_recognized  (** The part, even if there, is not even recognized *)
  | Url_part_allowed         (** The part can be present *)
  | Url_part_required        (** The part must be present *)


type url_syntax =
    { url_enable_scheme    : url_syntax_option;
      url_enable_user      : url_syntax_option;
      url_enable_user_param: url_syntax_option;
      url_enable_password  : url_syntax_option;
      url_enable_host      : url_syntax_option;
      url_enable_port      : url_syntax_option;
      url_enable_path      : url_syntax_option;
      url_enable_param     : url_syntax_option;
      url_enable_query     : url_syntax_option;
      url_enable_fragment  : url_syntax_option;
      url_enable_other     : url_syntax_option;
      url_accepts_8bits    : bool;
      url_is_valid         : url -> bool;
      url_enable_relative  : bool;
    }
(** Values of type [url_syntax] describe which components of an URL are
 * recognized, which are allowed (and optional), and which are required.
 * Not all combinations are valid; the predicate expressed by the
 * function [url_syntax_is_valid] must hold.
 *
 * The function [url_is_valid] is applied when a fresh URL is created
 * and must return [true]. This function allows it to add an arbitrary
 * validity criterion to [url_syntax]. (Note that the URL passed to 
 * this function is not fully working; you can safely assume that the
 * accessor functions [url_scheme] etc. can be applied to it.)
 *
 * Switch [url_accepts_8bit]: If [true], the bytes with code 128 to
 * 255 are treated like alphanumeric characters; if [false] these bytes
 * are illegal (but it is still possible to include such byte in their
 * encoded form: [%80] to [%FF]).
 *
 * Switch [url_enable_relative]: If [true], the syntax allows relative
 * URLs in principle. Actually, parsing of relative URLs is possible
 * when the optional parts are flagged as [Url_part_allowed] and not
 * as [Url_part_required]. However, it is useful to specify URL syntaxes
 * always as absolute URLs, and to weaken them on demand when a relative
 * URL is found by the parser. This switch enables that. In particular,
 * the function [partial_url_syntax] checks this flag.
 *)

and url
 (** Values of type [url] describe concrete URLs. Every URL must have
 * a fundamental [url_syntax], and it is only possible to create URLs
 * conforming to the syntax. See [make_url] for further information.
 *)
;;



val url_syntax_is_valid : url_syntax -> bool
  (** Checks whether the passed [url_syntax] is valid. This means:
   * - If passwords are recognized, users (and hosts) must be recognized, too
   * - If ports are recognized, hosts must be recognized, too
   * - If users are recognized, hosts must be recognized, too
   * - Either the syntax recognizes one of the phrases
   *   \{ user, password, host, port, path \}, or the syntax recognized
   *   the phrase 'other'.
   *)


val partial_url_syntax : url_syntax -> url_syntax
  (** Transforms the syntax into another syntax where all required parts are
   * changed into optional parts.
   *)


(* Note that all following url_syntaxes do not allow 8bit bytes. *)

val null_url_syntax   : url_syntax
  (** An URL syntax that recognizes nothing. Use this as base for your own
   * definitions, e.g.
   * {[
   * let my_syntax = { null_url_syntax with
   *                     url_enable_host = Url_part_required; ... }
   * ]}
   *)

val ip_url_syntax : url_syntax
  (** Syntax for IP based protocols. This syntax allows scheme, user,
   * password, host, port, path, param, query, fragment, but not "other".
   * It does not accept 8 bit bytes.
   *)

val common_url_syntax : (string, url_syntax) Hashtbl.t
  (** Syntax descriptions for common URL schemes. The key of the hashtable
   * is the scheme name, and the value is the corresponding syntax.
   *
   * - ["file"]: scheme, host?, path
   * - ["ftp"]: scheme, user?, password?, host, port?, path?, param?
   *   Note: param is not checked.
   * - ["http"], ["https"]: 
   *   scheme, user?, password?, host, port?, path?, query?
   * - ["mailto"]: scheme, other, query? (RFC 2368)
   * - ["pop"], ["pops"]: scheme, user?, user_param?, password?, host, port?
   *   Note: user_param is not checked.
   *   (RFC 2384)
   * - ["imap"], ["imaps"]: scheme, user?, user_param?, password?, host, port?,
   *   path?, query? (RFC 2192)
   *   Note: "param" is intentionally not recognized to get the resolution of
   *   relative URLs as described in the RFC. When analysing this kind of URL,
   *   it is recommended to re-parse it with "param" enabled.
   * - ["news"]: scheme, other (RFC 1738)
   * - ["nntp"], ["nntps"]: scheme, host, port?, path (with two components) 
   *   (RFC 1738)
   * - ["data"]: scheme, other (RFC 2397). "other" is not further decomposed.
   * - ["ipp"], ["ipps"]: scheme, host, port? , path?, query? (RFC 3510)
   * - ["cid"], ["mid"]: Content/message identifiers: scheme, other
   *
   * Notes:
   * - These syntax descriptions can be weakened for partial/relative URLs 
   *   by changing the required parts to optional parts: See the function
   *   [partial_url_syntax].
   * - None of the descriptions allows fragments. These can be enabled by
   *   setting [url_enable_fragment] to [Url_part_allowed]. E.g.
   *   {[ { file_url_syntax with url_enable_fragment = Url_part_allowed } ]}
   * - 8 bit bytes are not accepted
   * - A large number of standardised scheme syntaxes are not available,
   *   e.g. gopher, prospero, wais. The selection is a bit subjective,
   *   but I have tried to omit protocols that are no longer in common
   *   use, or that are very special.
   * - The LDAP URL syntax (RFC 1959) does not fit into our scheme, it
   *   is omitted for now because of this.
   *)

val null_url : url
  (** A URL without any component and [null_url_syntax]
   *)

val make_url :
      ?encoded:bool ->
      ?scheme:string ->
      ?user:string ->
      ?user_param:string list ->
      ?password:string ->
      ?host:string ->
      ?port:int ->
      ?path:string list ->
      ?param:string list ->
      ?query:string ->
      ?fragment:string ->
      ?other:string ->
      url_syntax ->
      url
  (** Creates a URL from components:
   * 
   * - The components [scheme] and [host] are simple strings to which the
   *   [%]-encoding is not applicable.
   * - The component [port] is a simple number. Of course, the [%]-encoding
   *   is not applicable, too.
   * - The components [user], [password], [query], [fragment], and [other]
   *   are strings which may contain [%]-encoded characters. By default,
   *   you can pass any string for these components, and problematic characters 
   *   are automatically encoded. If you set [encoded:true], the passed
   *   strings must already be encoded, but the function checks whether
   *   the encoding is syntactically correct.
   *   Note that for [query] even the characters ['?'] and ['='] are encoded
   *   by default, so you need to set [encoded:true] to pass a reasonable
   *   query string.
   * - The components [user_param], [path] and [param] are lists of strings which may
   *   contain [%]-encoded characters. Again, the default is to pass
   *   decoded strings to the function, and the function encodes them
   *   automatically, and by setting [encoded:true] the caller is responsible
   *   for encoding the strings. Passing empty lists for these components
   *   means that they are not part of the constructed URL.
   *   See below for the respresentation of these components.
   *
   * The strings representing the components do not
   * contain the characters separating the components from each other. 
   *
   * The created URL must conform to the [url_syntax], i.e.:
   * - The URL must only contain components which are recognized by the
   *   syntax
   * - The URL must contain components which are required by the syntax
   * - The URL must fulfill the predicate expressed by the [url_is_valid]
   *   function of the syntax.
   *
   * The path of a URL is represented as a list of ['/']-separated path
   * components. i.e.
   * 
   * [ [ s1; s2; ...; sN ] ]  represents the path  
   *                        [s1 ^ "/" ^ s2 ^ "/" ^ ... ^ "/" ^ sN]
   *
   * As special cases:
   * -  [[]]                   is the non-existing path
   * -  [[ "" ]]               is ["/"]
   * -  [[ "";"" ]]            is illegal
   * 
   * Except of [s1] and [sN], the path components must not be empty strings.
   *
   * To avoid ambiguities, it is illegal to create URLs with both relative
   * paths ([s1 <> ""]) and host components.
   *
   * Parameters of URLs ([param] and [user_param]) are components 
   * beginning with [';']. The list
   * of parameters is represented as list of strings where the strings
   * contain the value following [';'].
   *)

val modify_url :
      ?syntax:url_syntax ->
      ?encoded:bool ->
      ?scheme:string ->
      ?user:string ->
      ?user_param:string list ->
      ?password:string ->
      ?host:string ->
      ?port:int ->
      ?path:string list ->
      ?param:string list ->
      ?query:string ->
      ?fragment:string ->
      ?other:string ->
      url ->
      url
  (** Modifies the passed components and returns the modified URL. 
   * The modfied URL shares unmodified components with the original
   * URL.
   *)

val remove_from_url :
      ?scheme:bool ->
      ?user:bool ->
      ?user_param:bool ->
      ?password:bool ->
      ?host:bool ->
      ?port:bool ->
      ?path:bool ->
      ?param:bool ->
      ?query:bool ->
      ?fragment:bool ->
      ?other:bool ->
      url ->
      url
  (** Removes the [true] components from the URL, and returns the modified
   * URL.
   * The modfied URL shares unmodified components with the original
   * URL.
   *)

val default_url :
      ?encoded:bool -> 
      ?scheme:string ->
      ?user:string ->
      ?user_param:string list ->
      ?password:string ->
      ?host:string ->
      ?port:int ->
      ?path:string list ->
      ?param:string list ->
      ?query:string ->
      ?fragment:string ->
      ?other:string ->
      url ->
      url
  (** Adds missing components and returns the modified URL.
   * The modfied URL shares unmodified components with the original
   * URL.
   *)

val undefault_url :
      ?scheme:string ->
      ?user:string ->
      ?user_param:string list ->
      ?password:string ->
      ?host:string ->
      ?port:int ->
      ?path:string list ->
      ?param:string list ->
      ?query:string ->
      ?fragment:string ->
      ?other:string ->
      url ->
      url
  (** Removes components from the URL if they have the passed value, and
   * returns the modified URL.
   * Note: The values must always be passed in {b encoded} form!
   * The modfied URL shares unmodified components with the original
   * URL.
   *)

val url_syntax_of_url : url -> url_syntax
  (** Returns the [url_syntax] record of a URL. *)

val url_of_string : url_syntax -> string -> url
  (** Parses the passed string according to the passed [url_syntax]. *)

val string_of_url : url -> string
  (** Returns the URL as string *)

val parse_url : 
      ?schemes:(string, url_syntax) Hashtbl.t ->
      ?base_syntax:url_syntax -> 
      ?accept_8bits:bool ->
      ?enable_fragment:bool ->
      string -> url
  (** Parses the string and returns the URL the string represents.
   * If the URL is absolute (i.e. begins with a scheme like
   * "http:..."), the syntax will be looked up in [schemes].
   * If the URL is relative, the [base_syntax] will be taken
   * if passed. Without [base_syntax], relative URLs cannot be
   * parsed.
   *
   * @param schemes This hashtable maps scheme names to syntax descriptions.
   *   The default is [common_url_syntax].
   * @param base_syntax If passed, the function can parse relative URLs
   *   according to this syntax. If not passed, the function will raise
   *   [Malformed_URL] on a relative URL.
   * @param accept_8bits If [false], the default, it depends on the
   *   syntax descriptions in [schemes] whether 8 bit characters are
   *   accepted in the input or not. If [true], 8 bit characters are
   *   always accepted.
   * @param enable_fragment If [false], the default, it depends on the
   *   syntax descriptions in [schemes] whether fragment identifiers
   *   (e.g. "#fragment") are recognized or not. If [true], fragments
   *   are always recognized.
   *)

val fixup_url_string : string -> string
  (** Escapes some unsafe or "unwise" characters that are commonly used
    * in URL strings: space, < > \{ \} \[ \] ^ \\ | and double quotes.
    * Call this function before parsing the URL to support these
    * characters.
   *)

val url_provides :
      ?scheme:bool ->
      ?user:bool ->
      ?user_param:bool ->
      ?password:bool ->
      ?host:bool ->
      ?port:bool ->
      ?path:bool ->
      ?param:bool ->
      ?query:bool ->
      ?fragment:bool ->
      ?other:bool ->
      url ->
      bool
  (** Returns [true] iff the URL has all of the components passed with
   * [true] value.
   *)

val url_scheme    :                  url -> string
val url_user      : ?encoded:bool -> url -> string
val url_user_param: ?encoded:bool -> url -> string list
val url_password  : ?encoded:bool -> url -> string
val url_host      :                  url -> string
val url_port      :                  url -> int
val url_path      : ?encoded:bool -> url -> string list
val url_param     : ?encoded:bool -> url -> string list
val url_query     : ?encoded:bool -> url -> string
val url_fragment  : ?encoded:bool -> url -> string
val url_other     : ?encoded:bool -> url -> string
  (** Return components of the URL. The functions return decoded strings
   * unless [encoded:true] is set.
   * If the component does not exist, the exception [Not_found]
   * is raised.
   *)

val split_path : string -> string list
  (** Splits a ['/']-separated path into components (e.g. to set up the
   * [path] argument of [make_url]).
   * E.g. 
   * {[
   * split_path "a/b/c" = [ "a"; "b"; "c" ],
   * split_path "/a/b"  = [ ""; "a"; "b" ],
   * split_path "a/b/"  = [ "a"; "b"; "" ] ]}
   *)

val join_path : string list -> string
  (** Concatenates the path components (reverse function of split_path).
   *)

val norm_path : string list -> string list
  (** Removes ["."] and [".."] from the path if possible. Deletes double slashes.
   *
   * {b Examples}
   *
   * {ul
   * {- [norm_path ["."] = []]
   *
   *           means: "." = ""}
   * {- [norm_path ["."; ""] = []]
   *
   *           means: "./" = ""}
   * {- [norm_path ["a"; "."] = ["a"; ""]]
   *
   *           means: "a/." = "a/"}
   * {- [norm_path ["a"; "b"; "."] = ["a"; "b"; ""]]
   * 
   *           means: "a/b/." = "a/b/"}
   * {- [norm_path ["a"; "."; "b"; "."] = ["a"; "b"; ""]]
   *
   *           means: "a/./b/." = "a/b/"}
   * {- [norm_path [".."] = [".."; ""]]
   *
   *           means: ".." = "../"}
   * {- [norm_path [".."; ""] = [".."; ""]]
   *
   *           means: "../" = "../"}
   * {- [norm_path ["a"; "b"; ".."; "c" ] = ["a"; "c"]]
   *
   *           means: "a/b/../c" = "a/c"}
   * {- [norm_path ["a"; "b"; ".."; "c"; ""] = ["a"; "c"; ""]]
   *
   *           means: "a/b/../c/" = "a/c/"}
   * {- [norm_path ["";"";"a";"";"b"] = [""; "a"; "b"]]
   *
   *           means: "//a//b" = "/a/b"}
   * {- [norm_path ["a"; "b"; ""; ".."; "c"; ""] = ["a"; "c"; ""]]
   *
   *           means: "a/b//../c/" = "a/c/"}
   * {- [norm_path ["a"; ".."] = []]
   *
   *           means: "a/.." = ""}
   * }
   *)


val apply_relative_url : url -> url -> url
  (** [apply_relative_url base rel]:
   * Interprets [rel] relative to [base] and returns the new URL. This
   * function implements RFC 1808.
   *
   * It is not necessary that [rel] has the same syntax as [base].
   * Note, however, that it is checked whether the resulting URL is syntactically
   * correct with the syntax of [base]. If not, the exception [Malformed_URL] 
   * will be raised.
   *)

val ensure_absolute_url : ?base:url -> url -> url
  (** If the anonymous URL is absolute, it is just returned as result of 
   * this function. If the URL is relative, it is tried to make it 
   * absolute by resolving it relative to [base]. If there is no [base],
   * this will fail, and the function raises [Malformed_URL].
   *)

val file_url_of_local_path : ?getcwd:(unit -> string) -> string -> url
  (** Generates a URL with "file" scheme from the passed path name. The
   * URL is always absolute, i.e. the current directory is prepended if the
   * path is not absolute.
   *
   * Note that no character set conversions are performed.
   *
   * Win32: The input path name may use forward or backward slashes. 
   * Absolute paths with drive letters and UNC paths are recognised.
   * Relative paths with drive letters, however, are not recognised
   * (e.g. ["c:file"]), as it is not possible to access the drive-specific
   * working directory from the O'Caml runtime.
   *
   * Cygwin: The input path name may use forward or backward slashes. 
   * Absolute paths with drive letters and UNC paths are recognised.
   * The former are translated to ["/cygdrive"] names.
   *
   * @param getcwd The function returns the path taken as current working 
   *   directory. Note that for
   *   Win32 this must be either an absolute name with drive letter,
   *   or an UNC path. Default: [Sys.getcwd]
   *)

val local_path_of_file_url : url -> string
  (** Extracts the path from an absolute file URL, and returns a 
   * correct path name.
   *
   * If the URL is not a file URL, or is not absolute, the function will
   * fail.
   *
   * Win32: The URL must either contain a drive letter, or must refer
   * to another host.
   *
   * Cygwin: Drive letters and remote URLs are recognised.
   *)


val print_url : url -> unit
  (** Printer for the toploop. *)

(* ---------------------------------------------------------------------- *)

(* Special accessors *)

(*
val mailto_recipients : url -> Netaddress.mailbox list
- Returns all recipients, incl the recpients in the "to" header

val ftp_type : url -> XXX
- Returns the FTP transfer type

val auth_param : url -> XXX
- if the URL has an AUTH style [user_param] the value is returned

val data_content_type : url -> XXX
val data_contents : url -> XXX
- Decomposes "data" URLs
*)

(* ---------------------------------------------------------------------- *)

(* EXAMPLES:
 *
 * let http = Hashtbl.find common_url_syntax "http";;
 * let u = url_of_string http "http://g:pw@host/a/%62/";;
 * string_of_url u;;
 *   --> "http://g:pw@host/a/%62/"
 * url_scheme u;;
 *   --> "http"
 * url_user u;;
 *   --> "g"
 * url_password u;;
 *   --> "pw"
 * url_host u;;
 *   --> "host"
 * url_path u;;
 *   --> [ ""; "a"; "b"; "" ]          (* sic! *)
 * url_path ~encoded:true u;;
 *   --> [ ""; "a"; "%62"; "" ]
 * let v = make_url 
 *   ~path:[ ".."; "c" ]
 *   ~fragment:"near-the-#-character"
 *   { (partial_url_syntax http) with url_enable_fragment = Url_part_allowed };;
 * string_of_url v;;
 *   --> "../c#near-the-%23-character"
 * let u' = modify_url ~syntax:(url_syntax_of_url v) u;;
 *    (* u does not permit fragments *)
 * let w = apply_relative_url u' v;;
 * string_of_url w;;
 *   --> "http://g:pw@host/c#near-the-%23-character"
 *)

(* ---------------------------------------------------------------------- *)

(** {1:tutorial Tutorial} 
 *
 * This module is a quite flexible parser for various kinds of URLs
 * occuring in practice. The syntax is configurable such that one
 * URL module can handle a lot of URL types in a generic way.
 *
 * {b Generic Parsing}
 *
 * In order to parse an absolute URL (beginning with a scheme identifier 
 * like "http:...") of unknown type just call
 * {[ 
 * let url = parse_url "http://me\@server/directory"
 * ]}
 *
 * By default, this function can parse all URL types listed
 * at {!Neturl.common_url_syntax}. However, the default configuration
 * implies also that
 * - relative URLs cannot be parsed
 * - fragment identifiers are rejected (i.e. the part after the hash
 *   mark like in "http://server/document#location")
 * - characters are rejected when the most significant bit (MSB) is set
 *
 * The latter two features can be simply enabled by passing the
 * arguments [~enable_fragment:true] and [~accept_8bits:true], 
 * respectively.
 *
 * The restriction that relative URLs are rejected has to do with
 * the problem that context information is missing. Because the scheme
 * identifier (like "http") is not available, the function does not
 * know which syntax the relative URL should have.  For example,
 * the relative URL [dir/file?x=1] is differently parsed when
 * it is taken relative to an [http] URL and when it is interpreted
 * relative to an [ftp] URL. In the first case, the path component
 * of the URL is ["dir/file"] and the query component is ["?x=1"],
 * but in the latter case the path component is ["dir/file?x=1"],
 * and a query component is not allowed.
 *
 * The solution is that the syntax of the base URL, relative to which
 * the URL is seen, must be passed as additional argument. Under the
 * assumption that [base_url] is the base URL, use
 * {[
 * let url = parse_url 
 *             ~base_syntax:(url_syntax_of_url base_url) 
 *             "/dir/file?x=1"
 * ]}
 * Of course, this assumes that the base URL is known when the url
 * is parsed.
 *
 * {b Parsing For a Certain Syntax}
 *
 * The function [url_of_string] is also a parser, but you must pass
 * the URL syntax as argument, e.g.
 * {[
 * let url = url_of_string syntax "ipp://server/printer"
 * ]}
 * Pass as [syntax] one of the elements of {!Neturl.common_url_syntax},
 * e.g.
 * {[ let syntax = Hashtbl.find common_url_syntax "ipp" ]}
 * or a self-defined syntax.
 *
 * {b Printing URLs}
 *
 * This is much easier, just call [string_of_url] to convert an URL
 * to a string. It is ensured that every URL always has an
 * unambiguous representation as string.
 *
 * {b URL Components}
 *
 * Internally, the parsed URL is decomposed into its components. This
 * module supports two ways of decomposition:
 * + [scheme://user;userparams:password\@host:port/path;params?query#fragment]
 * + [scheme:other;params?query#fragment]
 *
 * The first form is used for services that directly connect to a
 * certain service running on a certain host. The second form can
 * be used for everything else not falling under this category.
 *
 * Examples:
 * {ul
 * {- [http://me:abrakadabra\@server/dir?x=5#section1]
 *
 *    scheme=["http"], user=["me"], password=["abrakadabra"], host=["server"],
 *    path=["/dir"], query=["x=5"], fragment=["section1"] }
 * {- [pop://you;auth=digest-md5\@mail]
 *
 *    scheme=["pop"], user=["you"], user_params=[["auth=digest-md5"]], host=["mail"] }
 * {- [mailto:gerd\@gerd-stolpmann.de?cc=you\@domain.com]
 * 
 *    scheme=["mailto"], other=["gerd\@gerd-stolpmann.de"],
 *    query=["cc=you\@domain.com"] }
 * }
 *
 * It is important to mention that the decomposition is not fully
 * performed, but only down to a certain level. For example, the
 * query ["x=5"] could be further analysed and be split into
 * the syntactic parts ["x"] and ["5"]. However, this is not done,
 * just because the author seeked a compromise between the depth
 * of analysis and the genericy of application.
 *
 * {b URL Escaping}
 *
 * In order to represent the so-called unsafe characters, one can
 * use [%]-escaping in URLs. For example, this URL contains a 
 * password with [\@], an unsafe character encoded as [%40]:
 * {[ http://user:!$%40?\@server ]}
 * The question is how this module handles such escapings.
 *
 * It is surprising that the URL parser does not decode these
 * escaped forms (it checks, however, whether they are syntactically
 * correct). Internally, the components are stored as parsed,
 * and one can even retrieve them in their original form.
 * The function [url_password] returns the password component.
 * Applied to the above URL, one can get the password in
 * its original, "encoded" form, or as decoded string:
 * - [url_password ~encoded:true url] returns ["!$%40?"]
 * - [url_password url] returns ["!$\@?"]
 *
 * {b Representation of URL Components}
 *
 * The URL components can be retrieved with the functions
 * - [url_scheme]
 * - [url_user]
 * - [url_user_param]
 * - [url_password]
 * - [url_host]
 * - [url_port]
 * - [url_path]
 * - [url_param]
 * - [url_query]
 * - [url_fragment]
 * - [url_other]
 *
 * Most components are just strings. Of course, the port number is an
 * integer.
 *
 * The path component ([url_path]) has a non-obvious representation. The path
 * is represented as string list, e.g. "a/b/c" is represented
 * as [ ["a";"b";"c"] ]. Note, however, that absolute paths have
 * an empty string at the beginning of the list, e.g.
 * "/a/b/" is [ [""; "a"; "b"; "" ] ]. In most cases, the paths
 * found in URLs are absolute, and because of this it is quite
 * common to find this empty string at the beginning of the
 * path list. The corner cases are:
 * - [ [] ] is used when the path is missing in the URL
 * - [ [ "" ] ] is "/"
 * - [ [ ""; "" ] ] is considered as illegal
 *
 * The last two cases are somewhat arbitrary.
 *
 * There is the helper function [split_path] to convert the string
 * representation of paths into the list representation.
 *
 * The parameters ([url_user_param] and [url_param]) are lists, too.
 * A parameter starts with a semicolon as delimiter and runs until
 * the next component, which can be another parameter. The contents,
 * i.e. the values after the semicolons are put into the list. For example,
 * the parameter ";auth=unix;type=i" is represented as
 * [ ["auth=unix"; "type=i"] ].
 *
 * {b Hint: Getting Query Arguments}
 *
 * The query component is represented as a single string. When queries
 * use the standard syntax "name1=value1&name2=value2&...", one can
 * parse this string using
 * {[
 * let args = Netencoding.Url.dest_url_encoded_parameters
 *              (url_query ~encoded:true url)
 * ]}
 * Note that [encoded:true] is needed.
 *
 * {b Creating and Modifying URLs}
 *
 * In order to create a URL for a certain syntax, call [make_url]:
 * {[
 * let url = make_url 
 *             ~scheme:"http"
 *             ~user:"user"
 *             ~password:"!$\@?"
 *             ~host:"server"
 *             syntax
 * ]}
 * It is checked whether the URL conforms to the passed syntax. By default,
 * the components are passed in decoded form, and [make_url] automatically
 * encodes them if necessary (here, for example, the at sign in the
 * password). Alternatively, one can set [~encoded:true], and pass the
 * already escaped components. In this case, [make_url] checks whether
 * the encoding is sufficient to represent the URL as string.
 *
 * The functions [modify_url], [default_url], [undefault_url], and
 * [remove_from_url] can be used to modify an existing URL.
 *
 * {b Relative URLs}
 *
 * A URL is relative when the scheme identifier at the beginning is
 * omitted. In this case, the URL can be transformed to an absolute
 * URL when the base URL is known. The algorithm for this is defined
 * in RFC 1808, and quite complicated. It is implemented in
 * [apply_relative_url], but usually {!Neturl.ensure_absolute_url}
 * is the more convenient function. Just call
 * {[ let url' = ensure_absolute_url ~base url ]}
 * to convert [url] to its absolute counterpart [url'] when it is
 * relative, and to pass the URL unchanged when it is already
 * absolute.
 *)



(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 2.5  2006/03/06 16:59:24  stolpmann
 * Release.
 *
 * Revision 2.4  2006/03/06 16:31:03  stolpmann
 * Fix: Double slashes ( // ) in paths are now tolerated if used in an
 * unambiguous way.
 *
 * Fix: In apply_relative_url, a case was forgotten: If the base URL
 * has a host name but does not have a path, the missing path / is
 * silently added.
 *
 * New: fixup_url_string
 *
 * Revision 2.3  2004/07/18 16:36:24  stolpmann
 * 	New: Neturl.file_url_of_local_path, local_path_of_file_url.
 * 	URL updates according to RFC 2396.
 *
 * Revision 2.2  2004/07/12 22:57:01  stolpmann
 * 	Added: parse_url, ensure_absolute_url
 * 	Support for userparam.
 * 	Syntax can prevent relative URLs.
 * 	A lot of new syntaxes (pop, imap, ...)
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.3  2000/06/26 22:57:49  gerd
 * 	Change: The record 'url_syntax' has an additional component
 * 'url_accepts_8bits'. Setting this option to 'true' causes that
 * the bytes >= 0x80 are no longer rejected.
 *
 * Revision 1.2  2000/06/25 22:55:47  gerd
 * 	Doc update.
 *
 * Revision 1.1  2000/06/24 20:19:59  gerd
 * 	Initial revision.
 *
 * 
 *)
