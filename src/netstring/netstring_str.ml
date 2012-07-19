(* $Id$
 * ----------------------------------------------------------------------
 *
 *)
open Printf
  
module Debug = struct let enable = ref false
                         end
  
let dlog = Netlog.Debug.mk_dlog "Netstring_str" Debug.enable
  
let dlogr = Netlog.Debug.mk_dlogr "Netstring_str" Debug.enable
  
let () = Netlog.Debug.register_module "Netstring_str" Debug.enable
  
let explode s =
  let l = String.length s in
  let rec loop k = if k < l then s.[k] :: (loop (k + 1)) else [] in loop 0
  
let implode l =
  let n = List.length l in
  let s = String.create n in
  let k = ref 0 in (List.iter (fun c -> (s.[!k] <- c; incr k)) l; s)
  
let quote_set s =
  let l = explode s in
  let have_circum = List.mem '^' l in
  let have_minus = List.mem '-' l in
  let have_rbracket = List.mem ']' l in
  let l1 =
    List.filter (fun c -> (c <> '^') && ((c <> '-') && (c <> ']'))) l in
  let l2 = if have_rbracket then ']' :: l1 else l1 in
  let l3 = if have_circum then l2 @ [ '^' ] else l2 in
  let l4 = if have_minus then l3 @ [ '-' ] else l3 in
  let s4 = implode l4 in
  let s' =
    match s4 with
    | "" -> failwith "Netstring_str.quote_set: empty"
    | "^" -> "^"
    | "^-" -> "[-^]"
    | _ -> "[" ^ (s4 ^ "]")
  in
    (if !Debug.enable
     then dlogr (fun () -> sprintf "quote_set: orig: %s - quoted: %s" s s')
     else ();
     s')
  
(**********************************************************************)
(* HAVE_PCRE                                                          *)
(**********************************************************************)
(* This implementation of Netstring_str uses the PCRE engine. The
 * syntax for regular expressions is compatible with previous versions.
 *)
(**********************************************************************)
(* Parsing types *)
(* literal characters (except NUL) *)
(* NUL characer *)
(* . but no newline *)
(* emptiness *)
(* x* *)
(* x+ *)
(* x? *)
(* [...] *)
(* [^...] *)
(* ^ *)
(* $ *)
(* x\|y *)
(* \(...\) *)
(* \i *)
(* x{n,m}. m=-1 means infinite *)
(* \w *)
(* \W *)
(* \< *)
(* \> *)
(* \b *)
(* \B *)
(* \` *)
(* \' *)
(**********************************************************************)
(* Final types *)
(**********************************************************************)
(* Parse Str-style regexps, and convert to Pcre-style regexps *)
(* {...} is not implemented in Str *)
(*
      	| '{' -> next_noesc();
	         let n1 = ref None in
	         let n2 = ref None in

		 let j = ref 0 in
		 if !k < l & !c >= '0' & !c <= '9' then begin
		   while !k < l & !c >= '0' & !c <= '9' do
		     j := 10* !j + (Char.code !c - Char.code '0');
		     next_noesc()
		   done;
		   n1 := Some !j
                 end;
		 
		 if !k < l & !n1 <> None & !c = '}' then begin
		   next();
		   t1 := Tinterval (!t1, !j, !j)
		 end
		 else begin
		   
		   if !k >= l or !c <> ',' then
		     failwith "regexp: error in {...} phrase";

		   next_noesc();
		   j := 0;
		   
		   if !k < l & !c >= '0' & !c <= '9' then begin
		     while !k < l & !c >= '0' & !c <= '9' do
		       j := 10* !j + (Char.code !c - Char.code '0');
		       next_noesc()
		     done;
		     n2 := Some !j
                   end;
		   
		   if !k >= l || !c <> '}' then
		     failwith "regexp: error in {...} phrase";

		   next();
		   ( match !n1 with
		     None ->
		       ( match !n2 with
		           None ->
			     failwith "regexp: error in {...} phrase";
		         | Some m2 ->
			     t1 := Tinterval (!t1, 0, m2)
		       )
		   | Some m1 ->
		       ( match !n2 with
		           None ->
			     t1 := Tinterval (!t1, m1, -1)
		         | Some m2 ->
			     t1 := Tinterval (!t1, m1, m2)
		       )
                   )
		 end
 *)
(*
      |	'w' -> next(); Twordchar
      |	'W' -> next(); Tnowordchar
 *)
(*
      |	'B' -> next(); Tnowordbound
      |	'<' -> next(); Twordbeg
      |	'>' -> next(); Twordend
      |	'`' -> next(); Tbegbuf
      |	'\'' -> next(); Tendbuf
 *)
(* the character after [ or [^ ? *)
(* TODO: check for predefined sets *)
(* range *)
(* for print_set *)
(* Pcre.quote "\000" returns nonsense *)
(* Put parentheses around \n to disambiguate from \nn *)
(* Print ret, but put parentheses around ret *)
(* No additional parentheses needed *)
(* Print (?:ret). This is the "neutral" form of grouping that only
	 * changes precedence
	 *)
(**********************************************************************)
(* Emulation *)
(* Note that Pcre.quote is incorrect for NUL chars, which simply remain
     in place, although they need to be encoded
   *)
(* This returns, of course, a Str-syntax regexp! *)
(* let string_partial_match = Netstring_pcre.string_partial_match ;; *)
(* N/A *)
(* replace_matched: n/a *)
(**********************************************************************)
(* ENABLE_STR_EXTERNALS                                               *)
(**********************************************************************)
(* We use here the Str externals directly, and reimplement parts of
   the Str module to make it thread-safe
 *)
type regexp = Str.regexp

type split_result = Str.split_result = | Text of string | Delim of string

type result = { pos : int; sr : int array }

(* sr.(2*k) is the beginning of group k
   sr.(2*k+1) is the end of group k
   sr.(0) is match beginning
   sr.(1) is match end
 *)
let match_beg sr = sr.(0)
  
let match_e sr = sr.(1)
  
let group_beg sr k = sr.(k + k)
  
let group_e sr k = sr.((k + k) + 1)
  
let n_groups sr = ((Array.length sr) - 2) lsr 1
  
(* Groups are numbered 1 .. n_groups *)
external re_string_match : regexp -> string -> int -> int array =
  "re_string_match"
  
external re_partial_match : regexp -> string -> int -> int array =
  "re_partial_match"
  
external re_search_forward : regexp -> string -> int -> int array =
  "re_search_forward"
  
external re_search_backward : regexp -> string -> int -> int array =
  "re_search_backward"
  
external re_replacement_text : string -> int array -> string -> string =
  "re_replacement_text"
  
let regexp s =
  let re = Str.regexp s
  in
    (if !Debug.enable then dlogr (fun () -> sprintf "regexp: %s" s) else ();
     re)
  
let regexp_case_fold s =
  let re = Str.regexp_case_fold s
  in
    (if !Debug.enable
     then dlogr (fun () -> sprintf "regexp_case_fold: %s" s)
     else ();
     re)
  
let quote s =
  let s' = Str.quote s
  in
    (if !Debug.enable
     then dlogr (fun () -> sprintf "quote: orig: %s - quoted: %s" s s')
     else ();
     s')
  
let regexp_string = Str.regexp_string
  
let regexp_string_case_fold = Str.regexp_string_case_fold
  
let return_result pos sr = { pos = pos; sr = sr; }
  
let string_match pat s pos =
  let sr = re_string_match pat s pos
  in if (Array.length sr) > 0 then Some (return_result pos sr) else None
  
let search_forward pat s pos =
  let sr = re_search_forward pat s pos
  in
    (if (Array.length sr) = 0 then raise Not_found else ();
     ((sr.(0)), (return_result pos sr)))
  
let search_backward pat s pos =
  let sr = re_search_backward pat s pos
  in
    (if (Array.length sr) = 0 then raise Not_found else ();
     ((sr.(0)), (return_result pos sr)))
  
let matched_string result s =
  (if ((match_beg result.sr) < 0) || ((match_e result.sr) < 0)
   then raise Not_found
   else ();
   String.sub s (match_beg result.sr)
     ((match_e result.sr) - (match_beg result.sr)))
  
let match_beginning result =
  (if (match_beg result.sr) < 0 then raise Not_found else ();
   match_beg result.sr)
  
let match_end result =
  (if (match_e result.sr) < 0 then raise Not_found else ();
   match_e result.sr)
  
let matched_group result n s =
  (if (n < 0) || (n > (n_groups result.sr)) then raise Not_found else ();
   if n = 0
   then matched_string result s
   else
     (let gbeg = group_beg result.sr n in
      let gend = group_e result.sr n
      in
        (if (gbeg < 0) || (gend < 0) then raise Not_found else ();
         String.sub s gbeg (gend - gbeg))))
  
let group_beginning result n =
  (if (n < 0) || (n > (n_groups result.sr)) then raise Not_found else ();
   if n = 0
   then match_beginning result
   else
     (let gbeg = group_beg result.sr n
      in if gbeg < 0 then raise Not_found else gbeg))
  
let group_end result n =
  (if (n < 0) || (n > (n_groups result.sr)) then raise Not_found else ();
   if n = 0
   then match_e result.sr
   else
     (let gend = group_e result.sr n
      in if gend < 0 then raise Not_found else gend))
  
let substitute_first pat subst s =
  try
    let (pos, m) = search_forward pat s 0
    in
      String.concat ""
        [ Str.string_before s pos; subst m s;
          Str.string_after s (match_end m) ]
  with | Not_found -> s
  
exception Cont of int
  
let global_substitute pat subst s =
  let l = String.length s in
  let b = Buffer.create (l / 2) in
  let rec loop k =
    try
      if k <= l
      then
        (let (pos, m) = search_forward pat s k
         in
           (* or Not_found *)
           (Buffer.add_string b (String.sub s k (pos - k));
            let repl = subst m s
            in
              (Buffer.add_string b repl;
               let pos' = match_end m
               in
                 if pos = pos'
                 then
                   (if pos < l then Buffer.add_char b s.[pos] else ();
                    raise (Cont (pos' + 1)))
                 else raise (Cont pos'))))
      else ()
    with | Cont k_next -> loop k_next
    | Not_found -> Buffer.add_string b (String.sub s k (l - k))
  in (loop 0; Buffer.contents b)
  
let replace_matched repl m s = re_replacement_text repl m.sr s
  
let global_replace pat repl s =
  global_substitute pat (replace_matched repl) s
  
let replace_first pat repl s = substitute_first pat (replace_matched repl) s
  
(* The splitting functions are practically copied from str.ml *)
let opt_search_forward re s pos =
  try Some (search_forward re s pos) with | Not_found -> None
  
let opt_search_forward_progress expr text start =
  match opt_search_forward expr text start with
  | None -> None
  | Some (pos, m) ->
      if (match_end m) > start
      then Some (pos, m)
      else
        if start < (String.length text)
        then opt_search_forward expr text (start + 1)
        else None
  
let bounded_split expr text num =
  let start =
    match string_match expr text 0 with | Some m -> match_end m | None -> 0 in
  let rec split accu start n =
    if start >= (String.length text)
    then accu
    else
      if n = 1
      then (Str.string_after text start) :: accu
      else
        (match opt_search_forward_progress expr text start with
         | None -> (Str.string_after text start) :: accu
         | Some (pos, m) ->
             split ((String.sub text start (pos - start)) :: accu)
               (match_end m) (n - 1))
  in List.rev (split [] start num)
  
let split expr text = bounded_split expr text 0
  
let bounded_split_delim expr text num =
  let rec split accu start n =
    if start > (String.length text)
    then accu
    else
      if n = 1
      then (Str.string_after text start) :: accu
      else
        (match opt_search_forward_progress expr text start with
         | None -> (Str.string_after text start) :: accu
         | Some (pos, m) ->
             split ((String.sub text start (pos - start)) :: accu)
               (match_end m) (n - 1))
  in if text = "" then [] else List.rev (split [] 0 num)
  
let split_delim expr text = bounded_split_delim expr text 0
  
let bounded_full_split expr text num =
  let rec split accu start n =
    if start >= (String.length text)
    then accu
    else
      if n = 1
      then (Text (Str.string_after text start)) :: accu
      else
        (match opt_search_forward_progress expr text start with
         | None -> (Text (Str.string_after text start)) :: accu
         | Some (pos, m) ->
             let s = matched_string m text
             in
               if pos > start
               then
                 split
                   ((Delim s) ::
                     (Text (String.sub text start (pos - start))) :: accu)
                   (match_end m) (n - 1)
               else split ((Delim s) :: accu) (match_end m) (n - 1))
  in List.rev (split [] 0 num)
  
let full_split expr text = bounded_full_split expr text 0
  
let string_before = Str.string_before
  
let string_after = Str.string_after
  
let first_chars = Str.first_chars
  
let last_chars = Str.last_chars
  

