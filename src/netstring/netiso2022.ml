(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* --- global comment

type iso2022_state =
    { g0 : iso2022_type * encoding;
      g1 : iso2022_type * encoding;
      g2 : iso2022_type * encoding;
      g3 : iso2022_type * encoding;
      gl : [ `None | `G0 | `G1 | `G2 | `G3 ];
      gr : [ `None | `G1 | `G2 | `G3 ];
      enc_width : [ `Seven | `Eight ];
      ss_area : [ `L | `R ];
    }

and iso2022_type = 
    [ `T94_lo  (* one byte, 94 chars, select code points 0x21-0x7E *)
    | `T94_up  (* one byte, 94 chars, select code points 0xA1-0xFE *)
    | `T96_lo  (* one byte, "96" chars, select code points 0x00-0x7F *)
    | `T96_up  (* one byte, "96" chars, select code points 0x80-0xFF *)
    | `T94x94  (* two bytes, 94 x 94 chars *)
    | `T96x96  (* two bytes, 96 x 96 chars *)
    ]
;;

(* Note:
 * For C0 and C1 control elements the encoding `Enc_iso88591 with type
 * `T96_lo and `T96_up, respectively, is used.
 *)


(* type iso2022jp_state = subset of iso2022_state
 * = { g0 : [ `Enc_usascii | `Enc_jis0201_roman | `Enc_jis0208_94x94 | 
              `Enc_jis0212_94x94 ];
       g1 : [ `Enc_empty ];
       g2 : [ `Enc_empty ];
       g3 : [ `Enc_empty ];
       gl : [ `G0 ];
       gr : [ `None ];
       width : [ `Seven ];
       ss_area : [ `GL ];
 *)


type length_of_iso2022_type =
    function
	`T94_lo | `T94_up | `T96_lo | `T96_up -> 1
      | `T94x94 | `T96x96 -> 2
;;


let iso2022_type =
  (* Final byte, iso2022_type, encoding.
   * Note: Final bytes have separate code spaces for `T94_xx, `T96_xx,
   * `T94x94, and `T96x96.
   * 
   *)
  [ (* `T94_xx: *)
    0x42, `T94_lo, `Enc_usascii;
    0x49, `T94_up, `Enc_jis0201;
    0x4A, `T94_lo, `Enc_jis0201;
    0x7E, `T94_lo, `Enc_empty;
    (* `T96_xx: *)
    0x41, `T96_up, `Enc_iso88591;
    0x42, `T96_up, `Enc_iso88592;
    0x43, `T96_up, `Enc_iso88593;
    0x44, `T96_up, `Enc_iso88594;
    0x46, `T96_up, `Enc_iso88597;
    0x47, `T96_up, `Enc_iso88596;
    0x48, `T96_up, `Enc_iso88598;
    0x4C, `T96_up, `Enc_iso88595;
    0x4D, `T96_up, `Enc_iso88599;
    0x56, `T96_up, `Enc_iso885910;
    0x5F, `T96_up, `Enc_iso885914;
    0x62, `T96_up, `Enc_iso885915;
    0x66, `T96_up, `Enc_iso885916;
    0x7E, `T96_lo, `Enc_empty;
    (* `T94x94: *)
    0x40, `T94x94, `Enc_jis0208_94x94;
    0x42, `T94x94, `Enc_jis0208_94x94;
    0x44, `T94x94, `Enc_jis0212_94x94;
    0x7E, `T94x94, `Enc_empty;
    (* `T96x96: *)
    0x7E, `T96x96, `Enc_empty;
  ]
;;


let get_isoenc criterion final =
  try
    let (_, t, e) =
      List.find 
	criterion
	iso2022_type
    in
    (t,e)
  with
      Not_found -> raise Malformed_code
;;

let get_94 =
  get_isoenc (fun fb, t, e -> fb = final && (t = `T94_lo || t = `T94_up)) ;;

let get_96 =
  get_isoenc (fun fb, t, e -> fb = final && (t = `T96_lo || t = `T96_up));;

let get_94x94 =
  get_isoenc (fun fb, t, e -> fb = final && t = `T94x94);;

let get_96x96 =
  get_isoenc (fun fb, t, e -> fb = final && t = `T96x96);;

type esc_spec =
    C of int
  | R of int * int
;;


type multicode_string = (encoding * string) list ;;

(* IDEA:
 * ustring_of_multicode_string : encoding -> multicode_string -> string
 * uarray_of_multicode_string : encoding -> multicode_string -> int array
 *
 * multicode_string_of_ustring : ?ctrl_as_ascii:bool -> encoding list -> string -> multicode_string
 *)

type iso2022_decomposition =
    { dc_encoding : encoding;
      dc_iso2022_type : iso2022_type;
      dc_area : [ `L | `R ];  (* for both control and graphic codes *)
      dc_pos : int;
      mutable dc_len : int;
    }
;;


exception SS2;;
exception SS3;;


let iso2022_esc_codes =
  let ignore_esc = (fun state final -> state) in
  let fail_esc = (fun state final -> raise Malformed_code) in

  [ (* Note: Dynamically redefinable charsets are not supported. These have
     * an additonal 0x20 before the final byte, e.g.
     * 0x28 0x20 0x40 to designate dynamic charset 0x40 for g0 
     *)

    (* Designate charset of type: 94 chars, 1 byte *)
    [ `C 0x28; `R(0x40,0x7E) ], 
      (fun state final -> { state with g0 <- get_94 final });
    [ `C 0x29; `R(0x40,0x7E) ], 
      (fun state final -> { state with g1 <- get_94 final });
    [ `C 0x2A; `R(0x40,0x7E) ], 
      (fun state final -> { state with g2 <- get_94 final });
    [ `C 0x2B; `R(0x40,0x7E) ], 
      (fun state final -> { state with g3 <- get_94 final });

    (* Designate charset of type: 96 chars, 1 byte *)
    [ `C 0x2D; `R(0x40,0x7E) ], 
      (fun state final -> { state with g1 <- get_96 final });
    [ `C 0x2E; `R(0x40,0x7E) ], 
      (fun state final -> { state with g2 <- get_96 final });
    [ `C 0x2F; `R(0x40,0x7E) ], 
      (fun state final -> { state with g3 <- get_96 final });

    (* Designate charset of type: 94 chars, 2 bytes *)
    [ `C 0x24; `R(0x40,0x42) ],
      (fun state final -> { state with g0 <- get_94x94 final });
    [ `C 0x24; `C 0x28; `R(0x43,0x5F) ], 
      (fun state final -> { state with g0 <- get_94x94 final });
    [ `C 0x24; `C 0x28; `C 0x7E ], 
      (fun state final -> { state with g0 <- get_94x94 final });
    [ `C 0x24; `C 0x29; `R(0x40,0x5F) ], 
      (fun state final -> { state with g1 <- get_94x94 final });
    [ `C 0x24; `C 0x29; `C 0x7E ], 
      (fun state final -> { state with g1 <- get_94x94 final });
    [ `C 0x24; `C 0x2A; `R(0x40,0x5F) ], 
      (fun state final -> { state with g2 <- get_94x94 final });
    [ `C 0x24; `C 0x2A; `C 0x7E ], 
      (fun state final -> { state with g2 <- get_94x94 final });
    [ `C 0x24; `C 0x2B; `R(0x40,0x5F) ], 
      (fun state final -> { state with g3 <- get_94x94 final });
    [ `C 0x24; `C 0x2B; `C 0x7E ], 
      (fun state final -> { state with g3 <- get_94x94 final });
   
    (* Designate charset of type: 96 chars, 2 bytes *)
    [ `C 0x24; `C 0x2D; `R(0x40,0x5F) ], 
      (fun state final -> { state with g1 <- get_96x96 final });
    [ `C 0x24; `C 0x2D; `C 0x7E ], 
      (fun state final -> { state with g1 <- get_96x96 final });
    [ `C 0x24; `C 0x2E; `R(0x40,0x5F) ], 
      (fun state final -> { state with g2 <- get_96x96 final });
    [ `C 0x24; `C 0x2E; `C 0x7E ], 
      (fun state final -> { state with g2 <- get_96x96 final });
    [ `C 0x24; `C 0x2F; `R(0x40,0x5F) ],
      (fun state final -> { state with g3 <- get_96x96 final });
    [ `C 0x24; `C 0x2F; `C 0x7E ], 
      (fun state final -> { state with g3 <- get_96x96 final });
 
    (* encodings with more than 2 bytes are not supported: *)
    [ `C 0x24; `R(0x28,0x2B); `R(0x60,0x7D) ],
      fail_esc;
    [ `C 0x24; `R(0x2D,0x2F); `R(0x60,0x7D) ],
      fail_esc;

    (* Designate control elements: *)
    [ `C 0x21; `R(0x40,0x7E) ],
      ignore_esc;
    [ `C 0x22; `R(0x40,0x7E) ],
      ignore_esc;

    (* LS0, LS1, SS2, SS3 are always accepted, independent of the 
     * designated control elements.
     *
     * C0 is always mapped to Unicode U+0000 to U+001F.
     * C1 is always mapped to Unicode U+0080 to U+008F.
     *)

    (* Revisions: (Ignored) *)
    [ `C 0x26; `R(0x40,0x7E) ],
      ignore_esc;

    (* Announcements: It is not checked whether they hold *)
    [ `C 0x20; `C 0x41 ],
      (fun state final -> { state with gl = `G0 });
    [ `C 0x20; `C 0x42 ],
      ignore_esc;
    [ `C 0x20; `C 0x43 ],
      (fun state final -> 
	 match state.enc_width with
	     `Eight -> { state with gl = `G0; gr = `G1 }
	   | `Seven -> raise Malformed_code);
    [ `C 0x20; `C 0x44 ],
      (fun state final -> 
	 match state.enc_width with
	     `Eight -> { state with gl = `G0; gr = `G1 }
	   | `Seven -> state);
    [ `C 0x20; `R(0x45,0x4A) ],
      ignore_esc;
    [ `C 0x20; `C 0x4B ],
      (fun state final ->
	 if state.enc_width = `Seven then raise Malformed_code;
	 state);
    [ `C 0x20; `R(0x4C,0x5B) ],
      ignore_esc;
    [ `C 0x20; 'C 0x5C ],
      (fun state final -> 
	 match state.enc_width with
	     `Eight -> { state with ss_area = `GR }
	   | `Seven -> state);
    [ `C 0x20; `R(0x5D,0x7F) ],
      ignore_esc;

    (* Shift functions: *)
    [ `C 0x6E ],
      (fun state final -> { state with gl = `G2 });
    [ `C 0x6F ],
      (fun state final -> { state with gl = `G3 });
    [ `C 0x4E ],
      (fun state final -> raise SS2);   (* special case *)
    [ `C 0x4F ],
      (fun state final -> raise SS3);   (* special case *)
    [ `C 0x7E ],
      (fun state final -> 
	 match state.enc_width with
	     `Eight -> { state with gr = `G1 }
	   | `Seven -> { state with gl = `G1 });
    [ `C 0x7D ],
      (fun state final -> 
	 match state.enc_width with
	     `Eight -> { state with gr = `G2 }
	   | `Seven -> { state with gl = `G2 });
    [ `C 0x7C ],
      (fun state final -> 
	 match state.enc_width with
	     `Eight -> { state with gr = `G3 }
	   | `Seven -> { state with gl = `G3 });
    (* Without ESC: 
     * 0x0F = SI or LS0, 
     * 0x0E = SO or LS1, 
     * 0x8E = SS2, 
     * 0x8F = SS3
     *)

    (* Unsupported: *)
    [ `C 0x64 ],           (* Data delimiter *)
      fail_esc;
    [ `C 0x25 ],           (* Other coding system *)
      fail_esc;
    [ `C 0x23 ],           (* Single control functions *)
      fail_esc;
  ]
;;


exception Esc_match of (iso2022_state -> int -> iso2022_state) * int ;;


let scan_iso2022 init_state =
  ( match init_state.enc_width with
	`Seven ->
	  if init_state.gr <> `None || init_state.ss_area <> 'L then
	    invalid_arg "Netiso2022.scan_iso2022";
      | _ ->
	  ()
  );

  fun s_in p_in l_in ->
    assert(p_in >= 0 && p_in + l_in <= String.length s_in && l_in >= 0);

    (* k: counts the bytes
     * n: counts the characters
     *)

    let p = ref p_in in
    let p_max = p_in + l_in in

    let state = ref init_state in
    let gl_encoding = ref `Enc_empty in
    let gl_type = ref `T96_lo in
    let gl_len = ref 0 in
    let gr_encoding = ref `Enc_empty in
    let gr_type = ref `T96_lo in
    let gr_len = ref 0 in

    let set_state st =
      state := st;
      let tl, el =
	match st.gl with
	    `None -> (`T96_lo, `Enc_empty)
	  | `G0   -> st.g0
	  | `G1   -> st.g1
	  | `G2   -> st.g2
	  | `G3   -> st.g3 in
      let tr, er =
	match st.gr with
	    `None -> (`T96_lo, `Enc_empty)
	  | `G1   -> st.g1
	  | `G2   -> st.g2
	  | `G3   -> st.g3 in
      let ll = length_of_iso2022_type tl in
      let lr = length_of_iso2022_type tr in
      gl_encoding := el;
      gl_type := tl;
      gl_len := if el = `Enc_empty then 0 else ll;
      gr_encoding := er;
      gr_type := tr;
      gr_len := if er = `Enc_empty then 0 else lr;
    in

    set_state !state;    (* initialize! *)

    let decomp_list = ref [] in

    let add_dc enc t area len =
      (* Add a new decomposition element to the list. If possible, make
       * the last element longer.
       *)
      match decomp_list with
	  dc0 :: dc' when ((dc0.dc_pos + dc0.dc_len = !p) && 
			   (dc0.dc_encoding = enc) &&
			   (dc0.dc_iso2022_type = t) &&
			   (dc0.dc_area = area )) ->
	    dc0.dc_len <- dc0.dc_len + len;
	| _ ->
	    let dc = { dc_encoding = enc;
		       dc_iso2022_type = t;
		       dc_area = area;
		       dc_pos = !p;
		       dc_len = len;
		     } in
	    decomp_list := dc :: !decomp_list
    in

    let do_single_shift (t,e) =
      (* Interpret SS2 or SS3 *)
      let l = length_of_iso2022_type t in
      if !p + l >= p_max then raise Malformed_code;
      if e = `Enc_empty then raise Malformed_code;
      (* Check first byte: *)
      let x1 = s_in.[ !p ] in
      ( match t with
	    `T94_lo | `T94_hi | `T94x94 ->
	      ( match !state.ss_area with
		    `L ->
		      if x1 < '\033' || x1 > '\126' then raise Malformed_code;
		  | `R ->
		      if x1 < '\161' || x1 > '\254' then raise Malformed_code;
	      )
	  | `T96_lo | `T96_hi | `T96x96 ->
	      ( match !state.ss_area with
		    `L ->
		      if x1 < '\032' || x1 > '\127' then raise Malformed_code;
		  | `R ->
		      if x1 < '\160' then raise Malformed_code;
	      )
      );
      if l = 2 then begin
	(* Check second byte: *)
	let x2 = s_in.[ !p+1 ] in
	( match t with
	      `T94x94 ->
		( match !state.ss_area with
		      `L ->
			if x1 < '\033' || x1 > '\126' then raise Malformed_code;
		    | `R ->
			if x1 < '\161' || x1 > '\254' then raise Malformed_code;
		)
	    | `T96x96 ->
		( match !state.ss_area with
		      `L ->
			if x1 < '\032' || x1 > '\127' then raise Malformed_code;
		    | `R ->
			if x1 < '\160' then raise Malformed_code;
		)
	    | _ ->
		assert false
	);
      end;
      add_dc e t !state.ss_area l;
      p := !p + l
    in

    while !p < p_max do
      match s_in.[ !p ] with
	| '\014' ->
	    (* SO or LS1 *)
	    set_state { !state with gl = `G1 };
	    incr p;
	| '\015' ->
	    (* SI or LS0 *)
	    set_state { !state with gl = `G0 };
	    incr p;
	| '\027' ->
	    (* Escape sequence *)
	    ( try
		List.iter
		  (fun seq fn ->
		     let j = ref 1 in
		     let n = Array.length seq in
		     let matches = ref true in
		     while !j <= n && !matches do
		       if !p + !j >= p_max then raise Malformed_code;
		       let fc = Char.code s_in.[ !p + !j ] in
		       matches := (
			 match seq.( !j - 1 ) with
			     `C code -> 
			       fc = code
			   | `R (code_from, code_to) -> 
			       fc >= code_from && fc <= code_to );
		       incr j
		     done;
		     if !matches then
		       raise (Esc_match(fn, !j))
		  )
		  iso2022_esc_codes;
		(* Nothing matches: *)
		raise Malformed_code;
	      with
		  Esc_match(fn, esc_len) ->
		    let final = Char.code s_in.[ !p + esc_len - 1 ] in
		    p := !p + esc_len;
		    ( try set_state (fn !state final) with
			  SS2 -> do_single_shift !state.g2
			| SS3 -> do_single_shift !state.g3
		    );
	    )
	| '\000'..'\031' as x ->
	    (* C0 *)
	    add_dc `Enc_iso88591 `T96_lo `L 1;
	    incr p;
	| ('\032' | '\127') as x when (!gl_type=`T94_lo || 
				       !gl_type=`T94_hi ||
				       !gl_type=`T94x94) ->
	    if !gl_len = 0 then raise Malformed_code;
	    (* 32 and 127 are interpreted as SPACE and DELETE *)
	    add_dc `Enc_iso88591 `T96_lo `L 1;
	    incr k
	| '\033'..'\126' as x ->
	    (* GL *)
	    ( match !gl_len with
		  0 -> 
		    raise Malformed_code
		| 1 -> 
		    add_dc !gl_encoding !gl_type `L 1
		| 2 -> 
		    if !p+1 >= p_max then raise Malformed_code;
		    let y = s_in.[ !p + 1 ] in
		    ( match !gl_type with
			     `T94x94 ->
			       if y < '\033' || y > '\126' then 
				 raise Malformed_code;
			   | `T96x96 ->
			       if y < '\032' || y > '\127' then 
				 raise Malformed_code;
			   | _ ->
			       assert false
		    );
		    add_dc !gl_encoding !gl_type `L 2
	    );
	    p := !p + !gl_len;
	| '\143' ->
	    (* SS2 *)
	    if !state.enc_width = `Seven then raise Malformed_code;
	    incr k;
	    do_single_shift (!state.g2)
	| '\144' ->
	    (* SS3 *)
	    if !state.enc_width = `Seven then raise Malformed_code;
	    incr k;
	    do_single_shift (!state.g3)
	| '\128'..'\159' as x ->
	    (* C1 *)
	    if !state.enc_width = `Seven then raise Malformed_code;
	    add_dc `Enc_iso88591 `T96_up `R 1;
	    incr k;
	| '\160'..'\255' as x ->
	    (* GR *)
	    if !state.enc_width = `Seven then raise Malformed_code;

	    if (x='\160' || x='\255') && (!gr_type=`T94_lo ||
					  !gr_type=`T94_hi ||
					  !gr_type=`T94x94) 
	    then
	      raise Malformed_code;

	    ( match !gr_len with
		  0 -> 
		    raise Malformed_code
		| 1 -> 
		    add_dc !gr_encoding !gr_type `L 1
		| 2 -> 
		    if !p+1 >= p_max then raise Malformed_code;
		    let y = s_in.[ !p + 1 ] in
		    ( match !gr_type with
			     `T94x94 ->
			       if y < '\161' || y > '\254' then 
				 raise Malformed_code;
			   | `T96x96 ->
			       if y < '\160' then 
				 raise Malformed_code;
			   | _ ->
			       assert false
		    );
		    add_dc !gr_encoding !gr_type `L 2
	    );
	    p := !p + !gr_len;
    done;

    !decomp_list
;;

--- end global comment *)


(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.2  2003/06/04 19:21:53  stolpmann
 * 	compile fixes
 *
 * Revision 1.1  2003/06/03 18:52:29  stolpmann
 * 	intial revision
 *
 * 
 *)
