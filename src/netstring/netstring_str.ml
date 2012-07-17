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
(* Alternate implementation without Pcre: Just use Str directly *)
let mutex = !Netsys_oothr.provider#create_mutex ()
  
let protect f arg = Netsys_oothr.serialize mutex f arg
  
type regexp = Str.regexp

type split_result = Str.split_result = | Text of string | Delim of string

type result =
  { pos : int; match_beg : int; match_end : int; group_beg : int array;
    group_end : int array
  }

let regexp =
  protect
    (fun s ->
       let re = Str.regexp s
       in
         (if !Debug.enable
          then dlogr (fun () -> sprintf "regexp: %s" s)
          else ();
          re))
  
let regexp_case_fold =
  protect
    (fun s ->
       let re = Str.regexp_case_fold s
       in
         (if !Debug.enable
          then dlogr (fun () -> sprintf "regexp_case_fold: %s" s)
          else ();
          re))
  
let quote =
  protect
    (fun s ->
       let s' = Str.quote s
       in
         (if !Debug.enable
          then dlogr (fun () -> sprintf "quote: orig: %s - quoted: %s" s s')
          else ();
          s'))
  
let regexp_string = protect Str.regexp_string
  
let regexp_string_case_fold = protect Str.regexp_string_case_fold
  
let n_groups = 9
  
let return_result pos =
  let r =
    {
      pos = pos;
      match_beg = (try Str.match_beginning () with | Not_found -> (-1));
      match_end = (try Str.match_end () with | Not_found -> (-1));
      group_beg = Array.create n_groups (-1);
      group_end = Array.create n_groups (-1);
    }
  in
    (for g = 0 to n_groups - 1 do
       r.group_beg.(g) <-
         (try Str.group_beginning (g + 1)
          with | Not_found | Invalid_argument _ -> (-1));
       r.group_end.(g) <-
         (try Str.group_end (g + 1)
          with | Not_found | Invalid_argument _ -> (-1))
     done;
     r)
  
let string_match pat s =
  protect
    (fun pos ->
       if Str.string_match pat s pos then Some (return_result pos) else None)
  
let search_forward pat s =
  protect
    (fun pos ->
       let i = Str.search_forward pat s pos in (i, (return_result pos)))
  
let search_backward pat s =
  protect
    (fun pos ->
       let i = Str.search_backward pat s pos in (i, (return_result pos)))
  
let matched_string result s =
  (if (result.match_beg < 0) or (result.match_end < 0)
   then raise Not_found
   else ();
   String.sub s result.match_beg (result.match_end - result.match_beg))
  
let match_beginning result =
  (if result.match_beg < 0 then raise Not_found else (); result.match_beg)
  
let match_end result =
  (if result.match_end < 0 then raise Not_found else (); result.match_end)
  
let matched_group result n s =
  (if (n < 0) || (n >= (Array.length result.group_beg))
   then raise Not_found
   else ();
   if n = 0
   then matched_string result s
   else
     (let gbeg = result.group_beg.(n - 1) in
      let gend = result.group_end.(n - 1)
      in
        (if (gbeg < 0) or (gend < 0) then raise Not_found else ();
         String.sub s gbeg (gend - gbeg))))
  
let group_beginning result n =
  (if (n < 0) || (n >= (Array.length result.group_beg))
   then raise Not_found
   else ();
   if n = 0
   then match_beginning result
   else
     (let gbeg = result.group_beg.(n - 1)
      in if gbeg < 0 then raise Not_found else gbeg))
  
let group_end result n =
  (if (n < 0) || (n >= (Array.length result.group_end))
   then raise Not_found
   else ();
   if n = 0
   then match_end result
   else
     (let gend = result.group_end.(n - 1)
      in if gend < 0 then raise Not_found else gend))
  
let global_replace pat templ =
  protect (fun s -> Str.global_replace pat templ s)
  
let replace_first pat templ =
  protect (fun s -> Str.replace_first pat templ s)
  
let global_substitute pat subst =
  protect
    (fun s ->
       let xsubst s = let r = return_result 0 in subst r s
       in Str.global_substitute pat xsubst s)
  
let substitute_first pat subst =
  protect
    (fun s ->
       let xsubst s = let r = return_result 0 in subst r s
       in Str.substitute_first pat xsubst s)
  
let split sep = protect (fun s -> Str.split sep s)
  
let bounded_split sep s = protect (fun max -> Str.bounded_split sep s max)
  
let split_delim sep = protect (fun s -> Str.split_delim sep s)
  
let bounded_split_delim sep s =
  protect (fun max -> Str.bounded_split_delim sep s max)
  
let full_split sep = protect (fun s -> Str.full_split sep s)
  
let bounded_full_split sep s =
  protect (fun max -> Str.bounded_full_split sep s max)
  
let string_before = Str.string_before
  
let string_after = Str.string_after
  
let first_chars = Str.first_chars
  
let last_chars = Str.last_chars
  

