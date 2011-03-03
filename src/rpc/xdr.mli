(* $Id$ *)

(** External Data Representation *)

(** This module supports the "external data representation", or XDR
 * for short. XDR is a means to pack structured values as strings and
 * it serves to transport such values across character streams even
 * between computers with different architectures.
 *
 * XDR values must be formed according to an XDR type. Such types are
 * usually written in a notation that is close to the C notation of
 * structured types. There are some important details where XDR is
 * better than C:
 * - direct support for strings
 * - arrays may have fixed or variable length
 * - unions must have a discriminator
 * - no pointers. The notation [*t] is allowed, but means something
 *   different, namely "t option" in O'Caml notation.
 * - recursive types are possible and behave like recursive types in
 *   O'Caml. For example,
 *   {[
 *     struct *list {
 *       int value;
 *       next list;
 *     }
 *   ]}
 *   is a list of integer values and equivalent to
 *   {[
 *     type intlist = intlist_el option
 *      and intlist_el = { value : int; next : intlist }
 *   ]}
 *
 * See RFC 1014 for details about XDR.
 *
 * This module defines:
 * - XDR types
 * - XDR type terms
 * - XDR type systems
 * - XDR type term systems
 * 
 * In "type terms" you can see the components from which the type has been
 * formed, while a "type" is an opaque representation that has been checked
 * and for that some preprocessing has been done.
 *
 * A "type system" is a collection of several types that have names and that
 * can refer to previously defined types (i.e. a sequence of "typedef"s).
 * As with simple types, there is an extensive and an opaque representation.
 *
 * A typical way of using this module is to define an "XDR type term system"
 * by simply writing an O'Caml expression. After that, this system is validated
 * and you get the "type system". From now on, you can refer to the types
 * defined in the system by name and get the corresponding "XDR types".
 * Once you have an XDR type you can use it to pack or unpack an XDR value.
 *)

open Rtypes;;

(** Terms that describe possible XDR types:
 *
 * - [X_int]:                      integer (32 bit)
 * - [X_uint]:                     unsigned integer
 * - [X_hyper]:                    hyper (64 bit signed integer)
 * - [X_uhyper]:                   unsigned hyper
 * - [X_enum [x1,i1; ...]]:        [enum { x1 = i1, ... }]
 * - [X_float]:                    float (32 bit fp number)
 * - [X_double]:                   double (64 bit fp number)
 * - [X_opaque_fixed n]:           [opaque[n]]
 * - [X_opaque n]:                 [opaque<n>]
 * - [X_string n]:                 [string<n>]
 * - [X_mstring(name,n)]:          [_managed string<n>] (see below)
 * - [X_array_fixed (t,n)]:        [t[n]]
 * - [X_array (t,n)]:              [t<n>]
 * - [X_struct [x1,t1;...]]:       [struct { t1 x1; ...}]
 * - [X_union_over_int
 *     ([n1,t1;...], None)]:       [union switch(int) {case n1: t1; ...}]
 * - [X_union_over_int
 *     ([n1,t1;...], Some t)]:     [union switch(int) {case n1: t1; ...; default t}]
 * - [X_union_over_uint
 *     ([n1,t1;...], None)]:       [union switch(unsigned int) {case n1: t1; ...}]
 * - [X_union_over_uint
 *     ([n1,t1;...], Some t)]:     [union switch(unsigned int)
 *                                 {case n1: t1; ...; default t}]
 * - [X_union_over_enum
 *     (e, [n1,t1;...], None)]:    [union switch(e) {case n1:t1; ...}]
 *                                 where e is an enumeration type
 * - [X_union_over_enum
 *     (e, [n1,t1;...], Some t)]:  [union switch(e) {case n1:t1; ...; default t}]
 *                                 where e is an enumeration type
 * - [X_void]:                     void
 *
 *
 * The [X_type] constructor is only useful for types interpreted relative to
 * a type system. Then it refers to a named type in this system.
 *
 * The [X_param] constructor includes a reference to an arbitrary type
 * which must first be given when packing or unpacking values.
 * (A "lazy" type reference.) Additionally, the values for parameters
 * may be encrypted or decrypted.
 *
 * Example how to define a recursive type:
 *
 * [X_rec ("a", X_array ( X_struct ["value", X_int; "next", X_refer "a"], 1))]
 *
 * {b Managed strings} are represented as [X_mstring(name,n)]. The [name] refers
 * to the preferred factory for managed strings (needs to be passed to the
 * XDR unpacker). Values for managed strings are objects of type
 * {!Xdr_mstring.mstring}.
 *)

type xdr_type_term =
  | X_int
  | X_uint
  | X_hyper
  | X_uhyper
  | X_enum of (string * int4) list
  | X_float
  | X_double
  | X_opaque_fixed of uint4
  | X_opaque of uint4
  | X_string of uint4
  | X_mstring of string * uint4
  | X_array_fixed of xdr_type_term * uint4
  | X_array of xdr_type_term * uint4
  | X_struct of (string * xdr_type_term) list
  | X_union_over_int of (int4 * xdr_type_term) list *
                          xdr_type_term option
  | X_union_over_uint of (uint4 * xdr_type_term) list *
                           xdr_type_term option
  | X_union_over_enum of xdr_type_term * (string * xdr_type_term) list *
                           xdr_type_term option
  | X_void
  | X_type of string
  | X_param of string
  | X_rec of (string * xdr_type_term)      (* define a recursive type *)
  | X_refer of string                      (* refer to a recursive type *)
;;

type xdr_type;;
(** This is the validated version of [xdr_type_term]. Note that it does not
 * contain [X_type] constructors, i.e. is completely expanded.
 * It is allowed that an [xdr_type] contains [X_param] constructors (parameters).
 * The set of occurring parameters can be determined very quickly for an
 * [xdr_type].
 *)

type xdr_type_term_system = (string * xdr_type_term) list;;
(** Bind names to types. In a correct system you can only refer to
 * previously defined types, i.e. the type called [n] must be defined
 * in the list before it can be used via [X_type n].
 * It is possible to use this module without the means of type
 * systems, but often code is more readable if types are defined
 * in an environment allowing bindings to names.
 *)

type xdr_type_system;;
(** A validated type system. *)

val x_bool : xdr_type_term;;
(** Common abbreviation for boolean types. Has values [xv_true] and [xv_false]. *)

val x_optional : xdr_type_term -> xdr_type_term
(** Common abbreviation for optional types. Has values [xv_none] and
 * [xv_some v].
 *)

val x_opaque_max : xdr_type_term
(** Common abbreviation for opaque data of arbitrary length *)

val x_string_max : xdr_type_term
(** Common abbreviation for strings of arbitrary length *)

val x_mstring_max : string -> xdr_type_term
(** Common abbreviation for mstrings of arbitrary length *)

val x_array_max : xdr_type_term -> xdr_type_term
(** Common abbreviation for arrays of arbitrary length *)

(** Values possible for XDR types. This is straight-forward, except the
 * "_fast" variants:
 *)

type xdr_value =
  | XV_int of int4
  | XV_uint of uint4
  | XV_hyper of int8
  | XV_uhyper of uint8
  | XV_enum of string
  | XV_float of fp4
  | XV_double of fp8
  | XV_opaque of string
  | XV_string of string
  | XV_array of xdr_value array
  | XV_struct of (string * xdr_value) list
  | XV_union_over_int of (int4 * xdr_value)
  | XV_union_over_uint of (uint4 * xdr_value)
  | XV_union_over_enum of (string * xdr_value)
  | XV_void
  (* New in 0.4: *)
  | XV_enum_fast of int
      (** The integer is the _position_ in the [X_enum] list, sorted by
       * enum values (ascending). For example, if we have
       * [X_enum [ "A", 4; "B", 2; "C", 6 ]]
       * the element "B" has the position 0, because 2 is the lowest
       * number
       *)
  | XV_struct_fast of xdr_value array
      (** The array elements are in the same order as declared in [X_struct] *)
  | XV_union_over_enum_fast of (int * xdr_value)
      (** The integer is the _position_ in the [X_enum] list. "position"
       * means the same as for [XV_enum_fast]
       *)
  (* New in 3.0: *)
  | XV_array_of_string_fast of string array
      (** To be used with an [X_array] or [X_array_fixed] with an inner
          type of [X_string]
       *)
  | XV_mstring of Xdr_mstring.mstring
 
  (* TODO: arrays of int, uint, hyper, uhyper, opaque, float, double *)

val xv_true : xdr_value
val xv_false : xdr_value
  (** See [x_bool] *)

val xv_none : xdr_value
val xv_some : xdr_value -> xdr_value
  (** See [x_optional] *)

(* Destructors. *)

exception Dest_failure
  (** raised if the dest_* function are applied to non-matching xdr_value
   *)

val dest_xv_int : xdr_value -> int4
val dest_xv_uint : xdr_value -> uint4
val dest_xv_hyper : xdr_value -> int8
val dest_xv_uhyper : xdr_value -> uint8
val dest_xv_enum : xdr_value -> string
val dest_xv_enum_fast : xdr_value -> int
val dest_xv_float : xdr_value -> fp4
val dest_xv_double : xdr_value -> fp8
val dest_xv_opaque : xdr_value -> string
val dest_xv_string : xdr_value -> string
val dest_xv_mstring : xdr_value -> Xdr_mstring.mstring
val dest_xv_array : xdr_value -> xdr_value array
val dest_xv_array_of_string_fast : xdr_value -> string array
val dest_xv_struct : xdr_value -> (string * xdr_value) list
val dest_xv_struct_fast : xdr_value -> xdr_value array
val dest_xv_union_over_int : xdr_value -> int4 * xdr_value
val dest_xv_union_over_uint : xdr_value -> uint4 * xdr_value
val dest_xv_union_over_enum : xdr_value -> string * xdr_value
val dest_xv_union_over_enum_fast : xdr_value -> int * xdr_value
val dest_xv_void : xdr_value -> unit

val map_xv_enum_fast : xdr_type -> xdr_value -> int32
  (** Works for both [XV_enum] and [XV_enum_fast] *)

val map_xv_struct_fast : xdr_type -> xdr_value -> xdr_value array
  (** Works for both [XV_struct] and [XV_struct_fast] *)

val map_xv_union_over_enum_fast : xdr_type -> xdr_value ->
                                                      int * int32 * xdr_value
  (** Works for both [XV_union_over_enum] and [XV_union_over_enum_fast].
   * Returns the triple [(k,i,x)]:
   * - [k]: Position of the selected value in the [T_enum] array
   * - [i]: value of the enum
   * - [x]: selected arm of the union
   *)


(* This exception is used in unpack_xdr_value. *)

exception Xdr_format of string
  (** Format error found during unpacking a string *)

exception Xdr_format_message_too_long of xdr_value
  (** The message is too long and cannot be packed into a string *)

(** You must use these two functions to obtain validated types and
 * type systems. They fail with "validate_xdr_type" resp.
 * "validate_xdr_type_system" if the parameters are incorrect.
 *)

val validate_xdr_type : xdr_type_term -> xdr_type
val validate_xdr_type_system : xdr_type_term_system -> xdr_type_system

val params : xdr_type -> string list
  (** return the [X_param] parameters contained in the type *)

(** Get the unvalidated version back:
 *
 * - Note that [X_type] constructions are always resolved
 *)

val xdr_type_term : xdr_type -> xdr_type_term
val xdr_type_term_system : xdr_type_system -> xdr_type_term_system

(** You can expand any type term relative to a (validated) type system.
 * For example:
 *   [expanded_xdr_type sys1 (X_type "xy")]
 * extracts the type called "xy" defined in sys1.
 * Expansion removes all X_type constructions in a type term.
 *)

val expanded_xdr_type : xdr_type_system -> xdr_type_term -> xdr_type
val expanded_xdr_type_term : xdr_type_term_system -> xdr_type_term
                               -> xdr_type_term

val are_compatible : xdr_type -> xdr_type -> bool
(** [are_compatible]: currently not implemented *)

val value_matches_type : xdr_value -> xdr_type -> (string*xdr_type) list -> bool
(** Is the value properly formed with respect to this type? The third
 * argument of this function is a list of parameter instances. Note that
 * all parameters must be instantiated to compare a value with a type
 * and that the parameters instances are not allowed to have parameters
 * themselves.
 *
 * Encrypted parameters are not supported here.
 *)

(** {2 Packing and unpacking} *)

(** [pack_xdr_value v t p print]: Serialize v into a string conforming to
 *   the XDR standard where v matches t. In p the parameter instances are
 *   given. All parameters must be given, and the parameters must not contain
 *   parameters themselves. The fourth argument, print, is a function
 *   which is evaluated for the pieces of the resultant string. You can use
 *   pack_xdr_value_as_string to get the whole string at once.
 *
 * [unpack_xdr_value s t p]: Unserialize a string to a value
 *   matching t. If this operation fails you get an Xdr_format
 *   exception explaining what the reason for the failure is.
 *   Mostly the cause for failures is that t isn't the type
 *   of the value.
 *   Note that there are some implementation restrictions limiting
 *   the number of elements in array, strings and opaque fields.
 *   If you get such an error this normally still means that
 *   the value is not of the expected type, because these limits
 *   have no practical meaning (they are still higher than the
 *   usable address space).
 *)

(** {b Encryption:} The [encode] and [decode] functions can be used
 * to encrypt/decrypt parameters (placeholders in the type marked with
 * [X_param pname] for a parameter name [pname]). The [encode] argument
 * may list for each parameter an encoding function:
 * 
 * {[ encode = [ pname1, encoder1; pname2, encoder2; ... ] ]}
 * 
 * The functions [encoder] are called with the XDR-packed parameter value,
 * and return the encrypted value.
 *
 * Likewise, the [decode] argument may list for each parameter a decoding
 * function:
 * 
 * {[ decode = [ pname1, decoder1; pname2, decoder2; ... ] ]}
 *
 * The call style of the decoder functions is a bit more complicated, though.
 * They are called as
 *
 * {[ let (xdr_s, n) = decoder s pos len ]}
 *
 * meaning that the [decoder] starts decoding at position [pos] of string [s],
 * and that at most [len] bytes can be decoded. It returns the decoded
 * string [xdr_s] (which is then unpacked), and in [n] the number of
 * consumed input bytes is returned.
 *)

type encoder = string -> string
  (** see text above *)
type decoder = string -> int -> int -> (string * int)
  (** see text above *)

val pack_xdr_value : ?encode:(string * encoder) list ->
                     xdr_value -> xdr_type -> (string*xdr_type) list ->
                     (string -> unit) -> unit
val pack_xdr_value_as_string :
                     ?rm:bool ->
                     ?encode:(string * encoder) list ->
                     xdr_value -> xdr_type -> (string*xdr_type) list ->
                       string
  (** rm: If true, four null bytes are prepended to the string for the
   *     record mark
   *)

val pack_xdr_value_as_mstrings :
       ?encode:(string * encoder) list ->
       xdr_value -> xdr_type -> (string*xdr_type) list -> 
         Xdr_mstring.mstring list
  (** The concatanated mstrings are the packed representation *)



val unpack_xdr_value : ?pos:int -> ?len:int -> ?fast:bool -> ?prefix:bool ->
                       ?mstring_factories:Xdr_mstring.named_mstring_factories->
                       ?decode:(string * decoder) list ->
                       string -> xdr_type -> (string * xdr_type) list ->
                       xdr_value
val unpack_xdr_value_l : ?pos:int -> ?len:int -> ?fast:bool -> ?prefix:bool ->
                        ?mstring_factories:Xdr_mstring.named_mstring_factories->
                         ?decode:(string * decoder) list ->
                         string -> xdr_type -> (string * xdr_type) list ->
                         (xdr_value * int)
  (** [fast]: whether to prefer the new "fast" value representation
   * (default: false).
   * The [fast] flag {b must} be set to unpack for types that have been
   * created with [ocamlrpcgen].
   *
   * [prefix]: whether it is ok that the string is longer than the message
   *   (default: false)
   *
   * [mstring_factories]: when a [T_mstring(name,_)] type is found, the
   * factory is looked up in this hash table under [name], and if this
   * fails under the name ["*"]. If there is no such
   * factory, unpacking fails! (Default: empty table.)
   *
   * The variant [unpack_xdr_value_l] returns not only the decoded value,
   * but also the actual length in bytes.
   *)
