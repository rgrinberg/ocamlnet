(* $Id$
 * ----------------------------------------------------------------------
 *)

(** Internal access to the character conversion database
 *
 * This is an internal module.
 *)

type from_uni_list =
    U_nil
  | U_single of (int*int)
  | U_double of (int*int * int*int)
  | U_array of int array
;;
  (* A representation of (int*int) list that is optimized for the case that
   * lists with 0 and 1 and 2 elements are the most frequent cases.
   *)


val get_to_unicode : string -> int array 

val get_from_unicode : string -> from_uni_list array
  (* These functions get the conversion tables from local encodings to
   * Unicode and vice versa.
   * It is normally not necessary to access these tables; the 
   * Netconversion module does it already for you.
   *
   * The argument is the internal name of the encoding. (E.g. if 
   * encoding = `Enc_iso88591, the internal name is "iso88591", i.e.
   * the "`Enc_" prefix is removed. However, for "composite encodings"
   * like `Enc_eucjp things are more complicated.)
   *
   * Specification of the conversion tables:
   *
   * to_unicode: maps a local code to Unicode, i.e.
   *    let m = Hashtbl.find `Enc_isoXXX to_unicode in
   *    let unicode = m.(isocode) 
   *    - This may be (-1) to indicate that the code point is not defined.
   *
   * from_unicode: maps Unicode to a local code, i.e.
   *    let m = Hashtbl.find `Enc_isoXXX from_unicode in
   *    let l = m.(unicode land mask)
   *    Now search in l the pair (unicode, isocode), and return isocode.
   *    Where mask = Array.length from_unicode - 1
   *)

val lock : unit -> unit
  (* In multi-threaded applications: obtains a lock which is required to
   * Lazy.force the values found in to_unicode and from_unicode.
   * In single-threaded applications: a NO-OP
   *)

val unlock : unit -> unit
  (* In multi-threaded applications: releases the lock which is required to
   * Lazy.force the values found in to_unicode and from_unicode.
   * In single-threaded applications: a NO-OP
   *)


val init_mt : (unit -> unit) -> (unit -> unit) -> unit
  (* Internally used; see netstring_mt.ml *)


(* ---------------------------------------- *)

(* The following comment was written when the conversion module belonged
 * to the PXP package (Polymorhic XML Parser).
 *)

(* HOW TO ADD A NEW 8 BIT CODE:
 *
 * It is relatively simple to add a new 8 bit code to the system. This
 * means that the parser can read and write files with the new encoding;
 * this does not mean that the parser can represent the XML tree internally
 * by the new encoding.
 *
 * - Put a new unimap file into the "mappings" directory. The file format
 *   is simple; please look at the already existing files. 
 *   The name of the file determines the internal name of the code:
 *   If the file is called <name>.unimap, the code will be called
 *   `Enc_<name>.
 *
 * - Extend the type "encoding" in pxp_types.mli and pxp_types.ml
 *
 * - Extend the two functions encoding_of_string and string_of_encoding
 *   in pxp_types.ml
 *
 * - Recompile the parser
 *
 * Every encoding consumes at least 3kB of memory, but this may be much more 
 * if the code points are dispersed on the Unicode code space.
 *
 * Perhaps the addition of new codes will become even simpler in future
 * versions of PXP; but it is currently more important to support 
 * non-8-bit codes, too.
 * Every contribution of new codes to PXP is welcome!
 *)


(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 2.4  2004/07/09 00:17:47  stolpmann
 * 	ocamldoc
 *
 * Revision 2.3  2003/06/03 19:00:14  stolpmann
 * 	new implementation basing on Netdb
 *
 * Revision 2.2  2002/06/23 19:47:29  stolpmann
 * 	Improved representation of character mappings.
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.2  2000/08/29 00:47:24  gerd
 * 	New type for the conversion Unicode to 8bit.
 * 	Conversion tables are now lazy. Thus also mutexes are required.
 *
 * Revision 1.1  2000/08/13 00:02:57  gerd
 * 	Initial revision.
 *
 *
 * ======================================================================
 * OLD LOGS FROM THE PXP PACKAGE (FILE NAME pxp_mappings.mli):
 * 
 * Revision 1.1  2000/07/27 00:40:02  gerd
 * 	Initial revision.
 *
 * 
 *)
