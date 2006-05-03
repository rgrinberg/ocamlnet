(* $Id$
 * ----------------------------------------------------------------------
 * PXP: The polymorphic XML parser for Objective Caml.
 * Copyright by Gerd Stolpmann. See LICENSE for details.
 *)

(** Support module for Alain Frisch's [ulex] lexer generator
 *
 * {b Contents}
 * - {!Netulex.modules}
 * - {!Netulex.tutorial}
 *
 * The sub module [ULB] is a Unicode-based lexing buffer that
 * reads encoded strings and makes them available to the lexer
 * as both Unicode arrays and UTF-8 strings.
 *
 * The sub module [Ulexing] is a replacement for the module
 * in [ulex] with the same name. It uses [ULB] to represent
 * the main lexing buffer. It is much faster than the original
 * [Ulexing] implementation when the scanned text is UTF-8
 * encoded and [Ulexing.utf8_lexeme] is frequently called to
 * get the lexeme strings. Furthermore, it can process input
 * data of all encodings available to [Netconversion]. It is,
 * however, no drop-in replacement as it has a different
 * signature.
 *
 * To enable this version of [Ulexing], simply put an
 * [open Netulex] before using the [ulex] lexers.
 *)

(** {1:modules Modules} *)


module ULB : sig
  (** This module provides the [unicode_lexbuf] record with
   * access functions. In this record, the data is available
   * in two forms: As an array of Unicode code points
   * [ulb_chars], and as string of encoded chars [ulb_rawbuf].
   * Both buffers are synchronised by [ulb_chars_pos]. This
   * array stores where every character of [ulb_chars] can be
   * found in [ulb_rawbuf].
   *)

  type unicode_lexbuf =
      private
	{ mutable ulb_encoding : Netconversion.encoding;
                (** The character encoding of [ulb_rawbuf] *)
	  mutable ulb_encoding_start : int;
	        (** The first character position to which [ulb_encoding]
		 * applies (the encoding of earlier positions is
		 * lost)
		 *)
	  mutable ulb_rawbuf : string;
                (** The encoded string to analyse *)
	  mutable ulb_rawbuf_len : int;
                (** The filled part of [ulb_rawbuf] *)
	  mutable ulb_rawbuf_end : int;
                (** The analysed part of [ulb_rawbuf]. We have always
	         * [ulb_rawbuf_end <= ulb_rawbuf_len]. The analysed part
	         * may be shorter than the filled part because there is
	         * not enough space in [ulb_chars], or because the filled
	         * part ends with an incomplete multi-byte character
	         *)
	  mutable ulb_rawbuf_const : bool;
	        (** Whether [ulb_rawbuf] is considered as a constant. If
		 * [true], it is never blitted.
		 *)
	  mutable ulb_chars : int array;
                (** The analysed part of [ulb_rawbuf] as array of Unicode
	         * code points. Only the positions 0 to [ulb_chars_len-1]
	         * of the array are filled.
	         *)
	  mutable ulb_chars_pos : int array;
                (** For every analysed character this array stores the
   	         * byte position where the character begins in [ulb_rawbuf].
	         * In addition, the array contains at [ulb_chars_len] the
	         * value of [ulb_rawbuf_end].
		 *
	         * This array is one element longer than [ulb_chars].
	         *)
	  mutable ulb_chars_len : int;
                (** The filled part of [ulb_chars] *)
	  mutable ulb_eof : bool;
                (** Whether EOF has been seen *)
	  mutable ulb_refill : string -> int -> int -> int;
	        (** The refill function *)
	  mutable ulb_enc_change_hook : unicode_lexbuf -> unit;
	        (** This function is called when the encoding changes *)
	  mutable ulb_cursor : Netconversion.cursor;
	        (** Internally used by the implementation *)
	}

  val from_function : 
    ?raw_size:int -> 
    ?char_size:int -> 
    ?enc_change_hook:(unicode_lexbuf -> unit) ->
    refill:(string -> int -> int -> int) ->
    Netconversion.encoding -> 
      unicode_lexbuf
  (** Creates a [unicode_lexbuf] to analyse strings of the 
   * passed [encoding] coming from the [refill] function.
   *
   * @param raw_size The initial size for [ulb_rawbuf]. Defaults to 512
   * @param char_size The initial size for [ulb_chars]. Defaults to 256
   * @param enc_change_hook This function is called when the encoding
   *   is changed, either by this module, or by the user calling
   *   [set_encoding].
   * @param refill This function is called with arguments [ulb_rawbuf],
   *   [ulb_rawbuf_len], and [l], where 
   *   [l = String.length ulb_rawbuf - ulb_rawbuf_len] is the free
   *   space in the buffer. The function should fill new bytes into
   *   this substring, and return the number of added bytes. The
   *   return value 0 signals EOF.
   *)

  val from_in_obj_channel :
    ?raw_size:int -> 
    ?char_size:int -> 
    ?enc_change_hook:(unicode_lexbuf -> unit) ->
    Netconversion.encoding -> 
    Netchannels.in_obj_channel ->
      unicode_lexbuf
  (** Creates a [unicode_lexbuf] to analyse strings of the 
   * passed [encoding] coming from the object channel.
   *
   * @param raw_size The initial size for [ulb_rawbuf]. Defaults to 512
   * @param char_size The initial size for [ulb_chars]. Defaults to 256
   * @param enc_change_hook This function is called when the encoding
   *   is changed, either by this module, or by the user calling
   *   [set_encoding].
   *)

  val from_string :
        ?enc_change_hook:(unicode_lexbuf -> unit) ->
        Netconversion.encoding -> string -> unicode_lexbuf
  (** Creates a [unicode_lexbuf] analysing the passed string encoded in
   * the passed encoding. This function copies the input string.
   *
   * @param enc_change_hook This function is called when the encoding
   *   is changed, either by this module, or by the user calling
   *   [set_encoding]
   *)

  val from_string_inplace :
        ?enc_change_hook:(unicode_lexbuf -> unit) ->
        Netconversion.encoding -> string -> unicode_lexbuf
  (** Creates a [unicode_lexbuf] analysing the passed string encoded in
   * the passed encoding. This function does not copy the input string,
   * but uses it directly as [ulb_rawbuf]. The string is not modified by [ULB],
   * but the caller must ensure that other program parts do not
   * modify it either.
   *
   * @param enc_change_hook This function is called when the encoding
   *   is changed, either by this module, or by the user calling
   *   [set_encoding]
   *)

  val delete :
        int -> unicode_lexbuf -> unit
  (** Deletes the number of characters from [unicode_lexbuf].
   *  These characters
   *  are removed from the beginning of the buffer, i.e.
   *  [ulb_chars.(n)] becomes the new first character of the
   *  buffer. All three buffers [ulb_rawbuf], [ulb_chars], and
   *  [ulb_chars_pos] are blitted as necessary.       
   *
   *  When the buffer is already at EOF, the function fails.
   *
   *  For efficiency, it should be tried to call [delete] as seldom as
   *  possible. Its speed is linear to the number of characters to move.
   *)

  val refill :
	unicode_lexbuf ->
	  unit
  (** Tries to add characters to the [unicode_lexbuf] by calling the 
   * [ulb_refill] function. When the buffer is already at EOF, the 
   * exception [End_of_file] is raised, and the buffer is not modified.
   * Otherwise, the [ulb_refill] function is called to
   * add new characters. If necessary, [ulb_rawbuf], [ulb_chars], and
   * [ulb_chars_pos] are enlarged such that it is ensured that either
   * at least one new character is added, or that EOF is found for
   * the first time
   * In the latter case, [ulb_eof] is set to [true] (and the next call
   * of [refill_unicode_lexbuf] will raise [End_of_file]).
   *)

  val set_encoding :
        Netconversion.encoding -> unicode_lexbuf -> unit
   (** Sets the [encoding] to the passed value. This only affects future
    * [refill] calls. The hook [enc_change_hook] is invoked when defined.
    *)

  val close :
        unicode_lexbuf -> unit
  (** Sets [ulb_eof] of the [unicode_lexbuf]. The rest of the buffer
   * is not modified
   *)

  val utf8_sub_string : int -> int -> unicode_lexbuf -> string
    (** The two [int] arguments are the position and length of a sub
     * string of the lexbuf that is returned as UTF8 string. Position
     * and length are given as character multiples, not byte multiples.
     *)

  val utf8_sub_string_length : int -> int -> unicode_lexbuf -> int
    (** Returns [String.length(utf8_sub_string args)]. Tries not to
     * allocate the UTF-8 string.
     *)

end (* module ULB *)


module Ulexing : sig
  (** This is a lexing buffer for [ulex]. *)

  type lexbuf
  exception Error
    (** Lexical error *)

  val from_ulb_lexbuf : ULB.unicode_lexbuf -> lexbuf
    (** Creates a new [lexbuf] from the [unicode_lexbuf]. After that,
     * the [unicode_lexbuf] must no longer be modified.
     *)
  val lexeme_start: lexbuf -> int
    (** The character position of the start of the lexeme *)
  val lexeme_end: lexbuf -> int
    (** The character position of the end of the lexeme *)
  val lexeme_length: lexbuf -> int
    (** The length of the lexeme in characters *)
  val lexeme: lexbuf -> int array
    (** Returns the lexeme as array of Unicode code points *)
  val lexeme_char: lexbuf -> int -> int
    (** Returns the code point of a certain character of the
     * lexeme
     *)
  val sub_lexeme: lexbuf -> int -> int -> int array
    (** Returns a substring of the lexeme as array of Unicode
     * code points. The first [int] is the characater position
     * where to start, the second [int] is the number of 
     * characters.
     *)
  val utf8_lexeme: lexbuf -> string
    (** Returns the lexeme as UTF-8 encoded string *)
  val utf8_sub_lexeme: lexbuf -> int -> int -> string
    (** Returns a substring of the lexeme as UTF-8 encoded
     * string. The first [int] is the characater position
     * where to start, the second [int] is the number of
     * characters.
     *)              
  val utf8_sub_lexeme_length: lexbuf -> int -> int -> int
    (** Same as
     * String.length(utf8_sub_lexeme args), i.e. returns
     * the number of bytes a certain sub lexeme will have
     * when encoded as UTF-8 string.
     *)
    
  (**/**)

  (* "Internal" interface. This must match ulex's ones. *)
  val start: lexbuf -> unit
  val next: lexbuf -> int
  val mark: lexbuf -> int -> unit
  val backtrack: lexbuf -> int
end


(** {1:tutorial Tutorial}
 *
 * Of course, you need Alain Frisch's [ulex] utility first. It installs
 * itself under the name [ulex] as findlib library.
 *
 * Next, write your lexer, e.g. (line numbers in brackets):
 *
 * {[
 * [1] open Netulex
 * [2] let digits = lexer
 * [3]   | ['0'-'9']+ -> `Number(int_of_string(Ulexing.utf8_lexeme lexbuf))
 * [4]   | 8364       -> `Euro_sign   (* Code point #8364 in Unicode *)
 * ]}
 *
 * This is a very trivial example. The lexer accepts sequences of digits,
 * and returns them as `Number tokens. Furthermore, the euro sign is
 * recognized and returned as `Euro_sign. Note that in the first case
 * {!Netulex.Ulexing.utf8_lexeme}
 * is called to retrieve the current lexeme as UTF-8 string. (Well,
 * digits are a bad example, as they are only ASCII, and UTF-8 is not
 * really needed. Imagine you want to extend the scanner to other
 * number systems represented in the Unicode character set.)
 *
 * Line 1 is quite important. If you don't open [Netulex], the generated
 * [ulex] code will use the version of the [Ulexing] module coming with [ulex],
 * and not this one.
 *
 * Call the lexer as follows (line numbers in brackets):
 * {[
 * [5] let sample = "42543\226\130\172";;
 * [6] let ulb = Netulex.ULB.from_string `Enc_utf8 sample;;
 * [7] let lexbuf = Netulex.Ulexing.from_ulb_lexbuf ulb;;
 * [8] let first_token = digits lexbuf;;
 * ]}
 *
 * Now, [first_token] is [`Number 42543]. After
 *
 * {[
 * [9] let second_token = digits lexbuf;;
 * ]}
 *
 * this variable is set to [`Euro_sign], because the three-byte sequence
 * "\226\130\172" represents the euro sign in UTF-8.
 *
 * In line 6, the encoding [`Enc_utf8] selects that [sample] is an
 * UTF-8 string. You can pass here any encoding the {!Netconversion}
 * module understands.
 *
 * If you would like to scan from another source, just change line 6,
 * e.g.
 * 
 * {[
 * [6'] let ulb = Netulex.ULB.from_in_obj_channel ch
 * ]}
 *
 * where [ch] is any input channel the {!Netchannels} module supports.
 * For example, to read from a file:
 * 
 * {[
 * let ch = new Netchannels.input_channel (open_in "filename")
 * ]}
 *
 * You should compile the examples with
 *
 * {[
 * ocamlfind ... -package ulex,netstring -syntax camlp4o ...
 * ]}
 *
 * For the syntax of the lexer rules, see the documentation coming
 * with [ulex].
 *)
