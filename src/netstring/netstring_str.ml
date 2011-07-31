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
type setatom =
  | Schar of char | Srange of (char * char)
  and set =
  setatom list

type re_term =
  | Texact of string
  | Tnullchar
  | Tany
  | Tnull
  | Tconcat of re_term list
  | Tstar of re_term
  | Tplus of re_term
  | Toption of re_term
  | Tset of set
  | Tnegset of set
  | Tbegline
  | Tendline
  | Talt of re_term list
  | Tgroup of (int * re_term)
  | Trefer of int
  | Tinterval of (re_term * int * int)
  | Twordchar
  | Tnowordchar
  | Twordbeg
  | Twordend
  | Twordbound
  | Tnowordbound
  | Tbegbuf
  | Tendbuf

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
type regexp = Pcre.regexp

type split_result = Str.split_result = | Text of string | Delim of string

type result = Netstring_pcre.result

(**********************************************************************)
(* Parse Str-style regexps, and convert to Pcre-style regexps *)
let scan_str_regexp re_string =
  let l = String.length re_string in
  let k = ref (-1) in
  let c = ref ' ' in
  let esc = ref false in
  let group = ref 1 in
  let n_open_groups = ref 0 in
  let closed_groups = Array.create 10 false in
  let next () =
    (incr k;
     if !k < l
     then
       (let c1 = re_string.[!k]
        in
          if c1 = '\\'
          then
            if !k < l
            then (incr k; c := re_string.[!k]; esc := true)
            else failwith "regexp: bad backslash"
          else (esc := false; c := c1))
     else ()) in
  let next_noesc () =
    (incr k; if !k < l then (c := re_string.[!k]; esc := false) else ()) in
  let rec scan_alternative () =
    let t1 = scan_concatenation ()
    in
      if !k < l
      then
        if !esc & (!c = '|')
        then
          (next ();
           (match scan_alternative () with
            | Talt alist -> Talt (t1 :: alist)
            | t -> Talt [ t1; t ]))
        else t1
      else t1
  and scan_concatenation () =
    let t1 = scan_repetition ()
    in
      if t1 = Tnull
      then t1
      else
        (let t2 = scan_concatenation ()
         in
           match t2 with
           | Tnull -> t1
           | Texact s2 ->
               (match t1 with
                | Texact s1 -> Texact (s1 ^ s2)
                | _ -> Tconcat [ t1; t2 ])
           | Tconcat clist -> Tconcat (t1 :: clist)
           | _ -> Tconcat [ t1; t2 ])
  and scan_repetition () =
    let t1 = ref (scan_literal_or_group ()) in
    let continue = ref true
    in
      (while !continue do
         if (!k < l) & (not !esc)
         then
           (match !c with
            | '*' -> (next (); t1 := Tstar !t1)
            | '+' -> (next (); t1 := Tplus !t1)
            | '?' -> (next (); t1 := Toption !t1)
            | (* {...} is not implemented in Str *)
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
                _ -> continue := false)
         else continue := false done;
       !t1)
  and scan_literal_or_group () =
    if !k >= l
    then Tnull
    else
      if !esc
      then
        (match !c with
         | '(' ->
             (next ();
              let n = !group
              in
                (incr group;
                 incr n_open_groups;
                 let t = scan_alternative ()
                 in
                   (decr n_open_groups;
                    if (!k < l) & (!esc & (!c = ')'))
                    then (next (); closed_groups.(n) <- true; Tgroup (n, t))
                    else failwith "regexp: closing paranthesis \\) not found")))
         | '1' .. '9' ->
             let n = (Char.code !c) - (Char.code '0')
             in
               if closed_groups.(n)
               then (next (); Trefer n)
               else failwith "regexp: bad reference to group"
         | (*
      |	'w' -> next(); Twordchar
      |	'W' -> next(); Tnowordchar
 *)
             'b' -> (next (); Twordbound)
         | (*
      |	'B' -> next(); Tnowordbound
      |	'<' -> next(); Twordbeg
      |	'>' -> next(); Twordend
      |	'`' -> next(); Tbegbuf
      |	'\'' -> next(); Tendbuf
 *)
             '\\' -> (next (); Texact (String.make 1 '\\'))
         | '|' -> Tnull
         | ')' ->
             if !n_open_groups > 0
             then Tnull
             else failwith "regexp: unmatched closing parenthesis"
         | ch -> (next (); Texact (String.make 1 ch)))
      else
        (match !c with
         | '*' -> Tnull
         | '+' -> Tnull
         | '?' -> Tnull
         | '{' -> Tnull
         | '^' -> (next (); Tbegline)
         | '$' -> (next (); Tendline)
         | '.' -> (next (); Tany)
         | '\000' -> (next (); Tnullchar)
         | '[' ->
             (next_noesc ();
              if !k < l
              then
                (let negated = ref false in
                 let set = ref [] in
                 let add_char c = set := (Schar c) :: !set in
                 let add_range c1 c2 = set := (Srange (c1, c2)) :: !set
                 in
                   (if !c = '^' then (next_noesc (); negated := true) else ();
                    let continue = ref true in
                    let first = ref true
                    in
                      (* the character after [ or [^ ? *)
                      (* TODO: check for predefined sets *)
                      (* range *)
                      (while !continue & (!k < l) do
                         (match () with
                          | () when
                              (!c = '[') &
                                (((!k + 1) < l) & (re_string.[!k + 1] = ':'))
                              ->
                              failwith
                                "regexp: Character classes such as [[:digit:]] not implemented"
                          | () when (!c = ']') & (not !first) ->
                              (next (); continue := false)
                          | () when
                              ((!k + 2) < l) &
                                ((re_string.[!k + 1] = '-') &
                                   (re_string.[!k + 2] <> ']'))
                              ->
                              (add_range !c re_string.[!k + 2];
                               next_noesc ();
                               next_noesc ();
                               next_noesc ();
                               first := false)
                          | () ->
                              (add_char !c; next_noesc (); first := false))
                         done;
                       if !continue
                       then failwith "regexp: closing bracket ] not found"
                       else ();
                       if !negated then Tnegset !set else Tset !set)))
              else failwith "regexp: closing bracket ] not found")
         | ch -> (next (); Texact (String.make 1 ch)))
  in
    try (next (); scan_alternative ())
    with | Failure msg -> failwith (msg ^ (" - regexp: " ^ re_string))
  
let pcre_safe_quote c = (* for print_set *)
  match c with
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' -> String.make 1 c
  | '\000' -> "\\000"
  | _ -> "\\" ^ (String.make 1 c)
  
let rec print_pcre_regexp ret =
  match ret with
  | Texact s -> Pcre.quote s
  | Tnullchar -> (* Pcre.quote "\000" returns nonsense *) "[\\000]"
  | Tany -> "."
  | Tnull -> "(?:)"
  | Tconcat l -> String.concat "" (List.map print_pcre_regexp l)
  | Tstar ret' -> (print_pcre_subregexp ret') ^ "*"
  | Tplus ret' -> (print_pcre_subregexp ret') ^ "+"
  | Toption ret' -> (print_pcre_subregexp ret') ^ "?"
  | Tset s -> "[" ^ ((print_set s) ^ "]")
  | Tnegset s -> "[^" ^ ((print_set s) ^ "]")
  | Talt l -> String.concat "|" (List.map print_pcre_subregexp l)
  | Tgroup (_, ret') -> "(" ^ ((print_pcre_regexp ret') ^ ")")
  | Trefer n -> (* Put parentheses around \n to disambiguate from \nn *)
      "(?:\\" ^ ((string_of_int n) ^ ")")
  | Tinterval (ret', m, n) ->
      (print_pcre_subregexp ret') ^
        ("{" ^
           ((string_of_int m) ^
              ("," ^ ((if n >= 0 then string_of_int n else "") ^ "}"))))
  | Tbegline -> "^"
  | Tendline -> "(?:$)"
  | Twordchar -> "\\w"
  | Tnowordchar -> "\\W"
  | Twordbeg -> "\\b(?=\\w)"
  | Twordend -> "(?<=\\w)\\b"
  | Twordbound -> "\\b"
  | Tnowordbound -> "\\B"
  | Tbegbuf -> "\\A"
  | Tendbuf -> "\\z"
and print_pcre_subregexp ret =
  (* Print ret, but put parentheses around ret *)
  match ret with
  | Tset _ | Tnegset _ | Tgroup (_, _) ->
      (* No additional parentheses needed *) print_pcre_regexp ret
  | _ ->
      (* Print (?:ret). This is the "neutral" form of grouping that only
	 * changes precedence
	 *)
      "(?:" ^ ((print_pcre_regexp ret) ^ ")")
and print_set s =
  String.concat ""
    (List.map
       (function
        | Schar c -> pcre_safe_quote c
        | Srange (c1, c2) ->
            (pcre_safe_quote c1) ^ ("-" ^ (pcre_safe_quote c2)))
       s)
  
(**********************************************************************)
(* Emulation *)
let regexp s =
  let ret = scan_str_regexp s in
  let s' = print_pcre_regexp ret
  in
    (if !Debug.enable
     then dlogr (fun () -> sprintf "regexp: orig: %s - translated: %s" s s')
     else ();
     Pcre.regexp ~flags: [ `MULTILINE ] s')
  
let regexp_case_fold s =
  let ret = scan_str_regexp s in
  let s' = print_pcre_regexp ret
  in
    (if !Debug.enable
     then
       dlogr
         (fun () ->
            sprintf "regexp_case_fold: orig: %s - translated: %s" s s')
     else ();
     Pcre.regexp ~flags: [ `MULTILINE; `CASELESS ] s')
  
let pcre_quote s =
  (* Note that Pcre.quote is incorrect for NUL chars, which simply remain
     in place, although they need to be encoded
   *)
  let s1 = Pcre.quote s in
  let s' = Pcre.qreplace ~pat: "\\000" ~templ: "\\000" s1
  in
    (if !Debug.enable
     then dlogr (fun () -> sprintf "quote: orig: %s - quoted: %s" s s')
     else ();
     s')
  
let unsafe_str_re = Pcre.regexp "[\\]\\[+*?.\\\\^$]"
  
let quote s = (* This returns, of course, a Str-syntax regexp! *)
  Pcre.replace ~rex: unsafe_str_re ~templ: "\\$&" s
  
let regexp_string s = Pcre.regexp ~flags: [ `MULTILINE ] (pcre_quote s)
  
let regexp_string_case_fold s =
  Pcre.regexp ~flags: [ `MULTILINE; `CASELESS ] (pcre_quote s)
  
let string_match = Netstring_pcre.string_match
  
(* let string_partial_match = Netstring_pcre.string_partial_match ;; *)
(* N/A *)
let search_forward = Netstring_pcre.search_forward
  
let search_backward = Netstring_pcre.search_backward
  
let matched_string = Netstring_pcre.matched_string
  
let match_beginning = Netstring_pcre.match_beginning
  
let match_end = Netstring_pcre.match_end
  
let matched_group = Netstring_pcre.matched_group
  
let group_beginning = Netstring_pcre.group_beginning
  
let group_end = Netstring_pcre.group_end
  
let global_replace pat templ s = Netstring_pcre.global_replace pat templ s
  
let replace_first pat templ s = Netstring_pcre.replace_first pat templ s
  
let global_substitute = Netstring_pcre.global_substitute
  
let substitute_first = Netstring_pcre.substitute_first
  
(* replace_matched: n/a *)
let split = Netstring_pcre.split
  
let bounded_split = Netstring_pcre.bounded_split
  
let split_delim = Netstring_pcre.split_delim
  
let bounded_split_delim = Netstring_pcre.bounded_split_delim
  
let tr_split_result r =
  List.map
    (function
     | Pcre.Text t -> Text t
     | Pcre.Delim d -> Delim d
     | _ -> assert false)
    (List.filter
       (function | Pcre.Group (_, _) | Pcre.NoGroup -> false | _ -> true) r)
  
let full_split sep s = tr_split_result (Netstring_pcre.full_split sep s)
  
let bounded_full_split sep s max =
  tr_split_result (Netstring_pcre.bounded_full_split sep s max)
  
let string_before = Netstring_pcre.string_before
  
let string_after = Netstring_pcre.string_after
  
let first_chars = Netstring_pcre.first_chars
  
let last_chars = Netstring_pcre.last_chars
  

