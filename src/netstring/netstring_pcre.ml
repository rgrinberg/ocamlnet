(* $Id$
 * ----------------------------------------------------------------------
 *
 *)
(* `ANCHORED: virtually prepends "^" to the regexp *)
(* Unfortunately, Pcre.get_substring will not raise Not_found if there is
   * no matched string. Instead, it returns "", but this value cannot be
   * distinguished from an empty match.
   * The workaround is to call Pcre.get_substring_ofs first. This function
   * will raise Not_found if there is not any matched string.
   *
   * NOTE: Current versions of Pcre do return Not_found!
   *)
(* See also the comment for [matched_string] *)
(* matches a backslash and a digit, or a single dollar or a single
   * backslash.
   *)
(* Convert \n to $n etc. *)
(* Unfortunately we cannot just replace \ by $. *)
(*
 * Uncomment for next version of Pcre
let substitute_first ?groups ~pat ~subst s =
  Pcre.substitute_substrings_first ~rex:pat ~subst:(fun r -> subst r s) s
;;
*)
(* Do it yourself in the meantime *)
(* or Not_found *)
(* Copied from Str for exact compatibility: *)
(* or Not_found *)
(* or Not_found *)
(* Copied from Str for exact compatibility: *)
(* or Not_found *)
type regexp

type split_result =
  | Text of string | Delim of string | Group of int * string | NoGroup

type result

let regexp _ = invalid_arg "Netstring_pcre: unavailable"
  
let regexp_case_fold _ = invalid_arg "Netstring_pcre: unavailable"
  
let quote _ = invalid_arg "Netstring_pcre: unavailable"
  
let regexp_string _ = invalid_arg "Netstring_pcre: unavailable"
  
let regexp_string_case_fold _ = invalid_arg "Netstring_pcre: unavailable"
  
let string_match _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let search_forward _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let search_backward _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let matched_string _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let match_beginning _ = invalid_arg "Netstring_pcre: unavailable"
  
let match_end _ = invalid_arg "Netstring_pcre: unavailable"
  
let matched_group _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let group_beginning _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let group_end _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let global_replace _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let replace_first _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let global_substitute _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let substitute_first _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let split _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let bounded_split _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let split_delim _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let bounded_split_delim _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let full_split _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let bounded_full_split _ _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let string_before _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let string_after _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let first_chars _ _ = invalid_arg "Netstring_pcre: unavailable"
  
let last_chars _ _ = invalid_arg "Netstring_pcre: unavailable"
  

