(*
 * $Id$
 *)

(* This is an XDR implementation.
 * See RFC 1014
 *)

open Rtypes;;

exception Propagate of string;;

(**********************************************************************)

(* auxiliary stuff: *)

let aux_cmp (ha,sa) (hb,sb) =
  if ha = hb then
    compare sa sb
  else
    ha - hb
;;


let all_distinct_q l =
  (* returns whether all elements of l are distinct *)
  let a =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l) in
  Array.sort aux_cmp a;
  let distinct = ref true in
  let k = ref 0 in
  while !distinct && !k < Array.length a - 1 do
    let (ha,sa) = a.( !k ) in
    let (hb,sb) = a.( !k + 1 ) in
    distinct := (ha != hb) && (sa <> sb);
    incr k
  done;
  !distinct
;;


let all_distinct =
  function
      []
    | [_] -> true
    | [a;b] -> a <> b
    | l -> all_distinct_q l
;;


let sub_set_q l1 l2 =
  (* returns whether all elements of l1 occur in l2 *)
  let a1 =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l1) in
  let a2 =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l2) in
  Array.sort aux_cmp a1;
  Array.sort aux_cmp a2;
  let occurs = ref true in
  let k1 = ref 0 in
  let k2 = ref 0 in
  while !occurs && !k1 < Array.length a1 && !k2 < Array.length a2 do
    let (h1,s1) = a1.( !k1 ) in
    let found = ref false in
    while not !found && !k2 < Array.length a2 do
      let (h2,s2) = a2.( !k2 ) in
      found := (h1 == h2) && (s1 = s2);
      if not !found then incr k2
    done;
    occurs := !found;
    incr k1
  done;
  !occurs
;;


let sub_set l1 l2 =
  match (l1,l2) with
      ([],_) -> true
    | ([x],_) -> List.mem x l2
    | _ -> sub_set_q l1 l2
;;


(* (* currently unused! *)
let equal_sets_q l1 l2 =
  (* returns whether all elements of l1 occur in l2, and vice versa *)
  let a1 =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l1) in
  let a2 =
    Array.map (fun s -> (Hashtbl.hash s), s) (Array.of_list l2) in
  Array.sort aux_cmp a1;
  Array.sort aux_cmp a2;
  let equal = ref true in
  let k1 = ref 0 in
  let k2 = ref 0 in
  let k2_match = ref false in
      (* can only be false when !k2 = 0 *)
  while !equal && !k1 < Array.length a1 && !k2 < Array.length a2 do
    let (h1,s1) = a1.( !k1 ) in
    let (h2,s2) = a2.( !k2 ) in
    if (h1 == h2) && (s1 = s2) then (
      incr k1;
      k2_match := true;   (* a match for the k2 element has been found *)
    ) else (
      if !k2_match then (
	incr k2;
	while !k2 < Array.length a2 && (h2,s2) = a2.( !k2 ) do
	  incr k2
	done;
	if !k2 < Array.length a2 then (
	  let (h2',s2') = a2.( !k2 ) in
	  if (h1 == h2') && (s1 = s2') then (
	    incr k1;
	  )
	  else equal := false
	)
	else equal := false
      )
      else equal := false
    )
  done;
  if !equal && !k1 = Array.length a1 && !k2 < Array.length a2 && !k1 > 0 then (
    (* !k1 > 0 ==> a1 is not empty && !k2_match,
     * !k2 < length a2 ==> a2 is not empty
     *)
    let (h2,s2) = a2.( !k2 ) in
    incr k2;
    while !k2 < Array.length a2 && (h2,s2) = a2.( !k2 ) do
      incr k2
    done;
  );
  !equal && !k1 = Array.length a1 && !k2 = Array.length a2
;;


let equal_sets l1 l2 =
  match (l1,l2) with
      ([],[])    -> true
    | ([x],[y])  -> x = y
    | _ -> equal_sets_q l1 l2
;;
*)

(**********************************************************************)
(* definition of XDR types and type systems                           *)
(**********************************************************************)

(* restriction: it is not allowed to have an X_param as enumerator type
 * in union_over_enum. There must always be a real X_enum or, in a
 * type system, a resolvable X_type at this position.
 *)


type xdr_type_term =
    X_int
  | X_uint
  | X_hyper
  | X_uhyper
  | X_enum of (string * int4) list
  | X_float
  | X_double
  | X_opaque_fixed of uint4
  | X_opaque of uint4
  | X_string of uint4
  | X_array_fixed of xdr_type_term * uint4
  | X_array of       xdr_type_term * uint4
  | X_struct of (string * xdr_type_term) list
  | X_union_over_int of
      (int4   * xdr_type_term) list * xdr_type_term option
  | X_union_over_uint of
      (uint4  * xdr_type_term) list * xdr_type_term option
  | X_union_over_enum of
      xdr_type_term * (string * xdr_type_term) list * xdr_type_term option
  | X_void
  | X_type of string
  | X_param of string
  | X_rec of (string * xdr_type_term)      (* define a recursive type *)
  | X_refer of string                      (* refer to a recursive type *)
;;


module StringSet = Set.Make(String)
;;


type xdr_type =
  { mutable term   : xdr_term;
    mutable params : StringSet.t
      (* "params" is normally only non-empty in the top node *)
  }
and xdr_term =
    T_int
  | T_uint
  | T_hyper
  | T_uhyper
  | T_enum of (string * int32) array
      (* array must be sorted by ascending int32 *)
  | T_float
  | T_double
  | T_opaque_fixed of uint4
  | T_opaque of uint4
  | T_string of uint4
  | T_array_fixed of xdr_type * uint4
  | T_array of       xdr_type * uint4
  | T_struct of (string * xdr_type) array
  | T_union_over_int of
      (int4, xdr_type) Hashtbl.t * xdr_type option
  | T_union_over_uint of
      (uint4, xdr_type) Hashtbl.t * xdr_type option
  | T_union_over_enum of
      xdr_type * xdr_type option array * xdr_type option
      (* The array corresponds to the T_enum array. None means that the
       * constant is not mapped.
       *)
  | T_void
  | T_param of string
  | T_rec of (string * xdr_type)
  | T_refer of (string * xdr_type)
;;

type xdr_type_term_system =
  (string * xdr_type_term) list
;;

type xdr_type_system =
  (string * xdr_type) list
  (* export xdr_type_system in an opaque manner *)



let x_bool =
  X_enum ["FALSE", int4_of_int 0; "TRUE", int4_of_int 1]
;;


let x_optional t =
  X_union_over_enum
    (x_bool,
     ["TRUE", t; "FALSE", X_void],
     None)
;;


let x_opaque_max =
  X_opaque (mk_uint4 ('\063', '\255', '\255', '\255'));;

let x_string_max =
  X_string (mk_uint4 ('\063', '\255', '\255', '\255'));;

let x_array_max t =
  X_array (t,  (mk_uint4 ('\063', '\255', '\255', '\255')));;

(**********************************************************************)
(* definition of XDR values                                           *)
(**********************************************************************)

type xdr_value =
    XV_int of int4
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
  | XV_enum_fast of int
  | XV_struct_fast of xdr_value array
  | XV_union_over_enum_fast of (int * xdr_value)
;;

let xv_true = XV_enum_fast 1 (* "TRUE" *);;
let xv_false = XV_enum_fast 0 (*  "FALSE" *);;

let xv_none = XV_union_over_enum_fast (0,XV_void);;
let xv_some v = XV_union_over_enum_fast (1,v);;

exception Dest_failure

let dest_xv_int v =
  match v with XV_int x -> x | _ -> raise Dest_failure;;
let dest_xv_uint v =
  match v with XV_uint x -> x | _ -> raise Dest_failure;;
let dest_xv_hyper v =
  match v with XV_hyper x -> x | _ -> raise Dest_failure;;
let dest_xv_uhyper v =
  match v with XV_uhyper x -> x | _ -> raise Dest_failure;;
let dest_xv_enum v =
  match v with XV_enum x -> x | _ -> raise Dest_failure;;
let dest_xv_enum_fast v =
  match v with XV_enum_fast x -> x | _ -> raise Dest_failure;;
let dest_xv_float v =
  match v with XV_float x -> x | _ -> raise Dest_failure;;
let dest_xv_double v =
  match v with XV_double x -> x | _ -> raise Dest_failure;;
let dest_xv_opaque v =
  match v with XV_opaque x -> x | _ -> raise Dest_failure;;
let dest_xv_string v =
  match v with XV_string x -> x | _ -> raise Dest_failure;;
let dest_xv_array v =
  match v with XV_array x -> x | _ -> raise Dest_failure;;
let dest_xv_struct v =
  match v with XV_struct x -> x | _ -> raise Dest_failure;;
let dest_xv_struct_fast v =
  match v with XV_struct_fast x -> x | _ -> raise Dest_failure;;
let dest_xv_void v =
  match v with XV_void -> () | _ -> raise Dest_failure;;

let dest_xv_union_over_int v =
  match v with XV_union_over_int x -> x | _ -> raise Dest_failure;;

let dest_xv_union_over_uint v =
  match v with XV_union_over_uint x -> x | _ -> raise Dest_failure;;

let dest_xv_union_over_enum v =
  match v with XV_union_over_enum x -> x | _ -> raise Dest_failure;;

let dest_xv_union_over_enum_fast v =
  match v with XV_union_over_enum_fast x -> x | _ -> raise Dest_failure;;


let fail_map_xv_enum_fast () =
  failwith "Xdr.map_xv_enum_fast" ;;

let map_xv_enum_fast t v =
  match t.term with
      T_enum l ->
	let m = Array.length l in
	( match v with
	      XV_enum_fast k ->
		if k >= 0 && k < m then
		  snd(Array.unsafe_get l k)
		else
		  fail_map_xv_enum_fast()
	    | XV_enum name ->
		let k = ref 0 in
		while !k < m && (fst l.( !k ) <> name) do
		  incr k
		done;
		if !k >= m then
		  fail_map_xv_enum_fast();
		snd(l.( !k ))
	    | _ ->
		fail_map_xv_enum_fast()
	)
    | _ ->
	fail_map_xv_enum_fast()
;;


let fail_map_xv_struct_fast () =
  failwith "Xdr.map_xv_struct_fast" ;;

let map_xv_struct_fast t v =
  match t.term with
      T_struct decl ->
	let m = Array.length decl in
	( match v with
	      XV_struct_fast x ->
		let k = Array.length x in
		if k = m then
		  x
		else
		  fail_map_xv_struct_fast()
	    | XV_struct l ->
		( try
		    Array.map
		      (fun (name,y) -> List.assoc name l)
		      decl
		  with
		      Not_found -> fail_map_xv_struct_fast()
		)
	    | _ ->
		fail_map_xv_struct_fast()
	)
    | _ ->
	fail_map_xv_struct_fast()
;;


let fail_map_xv_union_over_enum_fast () =
  failwith "Xdr.map_xv_struct_fast" ;;

let map_xv_union_over_enum_fast t v =
  match t.term with
      T_union_over_enum( { term = T_enum e }, u, u_dfl ) ->
	let m = Array.length e in
	assert( m = Array.length u );
	( match v with
	      XV_union_over_enum_fast(k, x) ->
		if k >= 0 && k < m then
		  (k, (snd e.(k)), x)
		else
		  fail_map_xv_union_over_enum_fast()
	    | XV_union_over_enum(name, x) ->
		let k = ref 0 in
		while !k < m && fst(e.( !k )) <> name do
		  incr k
		done;
		if !k >= m then
		  fail_map_xv_union_over_enum_fast();
		(!k, (snd e.(!k)), x)
	    | _ ->
		fail_map_xv_union_over_enum_fast()
	)
    | _ ->
	fail_map_xv_union_over_enum_fast()
;;



exception Xdr_format of string;;
exception Xdr_format_message_too_long of xdr_value;;
(* raised in unpack_xdr_value if the byte stream does not match
 * the expected type. The string is an explanation and might be
 * useful while debugging. In the special case Xdr_format_message_too_long
 * there are more bytes than expected, but a prefix matches the type.
 * The prefix is returned as xdr_value.
 *)

(**********************************************************************)
(* check if XDR types are well-formed                                 *)
(**********************************************************************)

(* TODO: check on recursions without finite fix point. *)


let rec validate_xdr_type_i1
        (r:xdr_type_term -> xdr_type)
        (b:(string * xdr_type) list)
        (t:xdr_type_term)
      : xdr_type =

  (* r: function that resolves X_type references
   * t: the xdr_type_term to validate
   * b: list of recursive bindings
   *
   * raise Not_found on any error
   *)

  let mktype tm = { term = tm; params = StringSet.empty } in

  match t with
    X_int    -> mktype T_int
  | X_uint   -> mktype T_uint
  | X_hyper  -> mktype T_hyper
  | X_uhyper -> mktype T_uhyper
  | X_float  -> mktype T_float
  | X_double -> mktype T_double
  | X_void   -> mktype T_void
  | X_enum e ->
      let e_names, e_values = List.split e in
      if all_distinct e_names && all_distinct e_values then
	let ea =
	  Array.map
	    (fun (n,i) -> (n, Rtypes.int32_of_int4 i))
	    (Array.of_list e) in
	Array.sort (fun (_,i) (_,i') -> compare i i') ea;
	mktype (T_enum ea)
      else
	raise (Propagate "Bad enumeration type: double values")
  | X_opaque_fixed n    -> mktype (T_opaque_fixed n)
  | X_opaque n          -> mktype (T_opaque n)
  | X_string n          -> mktype (T_string n)
  | X_array_fixed (s,n) -> mktype (T_array_fixed(validate_xdr_type_i1 r b s, n))
  | X_array (s,n)       -> mktype (T_array      (validate_xdr_type_i1 r b s, n))
  | X_struct s ->
      let s_names, s_types = List.split s in
      if all_distinct s_names then
	mktype
	  (T_struct
	     (Array.of_list
		(List.map (fun (n,x) -> n,validate_xdr_type_i1 r b x) s)))
      else
	raise (Propagate "Bad struct type: components with same names found")
  | X_union_over_int (u, default) ->
      let u_values, u_types = List.split u in
      if all_distinct u_values then begin
	let default' =
	  match default with
	    Some d -> Some (validate_xdr_type_i1 r b d)
	  | None   -> None
	in
	let htbl = Hashtbl.create(List.length u) in
	List.iter
	  (fun (n,x) ->
	     let x' = validate_xdr_type_i1 r b x in
	     Hashtbl.add htbl n x')
	  u;
	mktype(T_union_over_int(htbl, default'))
      end
      else
	raise (Propagate "Bad union_over_int type: variants found with same tags")
  | X_union_over_uint (u,default) ->
      let u_values, u_types = List.split u in
      if all_distinct u_values then begin
	let default' =
	  match default with
	    Some d -> Some (validate_xdr_type_i1 r b d)
	  | None   -> None
	in
	let htbl = Hashtbl.create(List.length u) in
	List.iter
	  (fun (n,x) ->
	     let x' = validate_xdr_type_i1 r b x in
	     Hashtbl.add htbl n x')
	  u;
	mktype(T_union_over_uint(htbl, default'))
      end
      else
	raise (Propagate "Bad union_over_uint type: variants found with same tags")
  | X_union_over_enum (e,u,default) ->
      let e' = validate_xdr_type_i1 r b e in
      let u_values, u_types = List.split u in
      let el =
	match e'.term with
	  T_enum x -> x
	| _ -> raise (Propagate "Bad union_over_enum type: discriminator is not enumerator")
      in
      let el_names, el_values = List.split (Array.to_list el) in
      if all_distinct u_values && sub_set u_values el_names then begin
	   let default' =
	     match default with
	       Some d -> Some (validate_xdr_type_i1 r b d)
	     | None   -> None
	   in
	   mktype
	     (T_union_over_enum
		(e',
		 Array.map
		   (fun (name, _) ->
		      try Some(validate_xdr_type_i1 r b (List.assoc name u))
		      with Not_found -> default'
		   )
		   el,
		 default'))
	 end
      else
	raise (Propagate "Bad union_over_enum type: variants found with identical tags")
  | X_type _ ->
      r t
  | X_param p ->
      mktype (T_param p)
  | X_rec (name, s) ->
      let node = mktype T_void in
      let t' = validate_xdr_type_i1 r ((name,node)::b) s in
      node.term <- T_rec (name, t');
      node

  | X_refer name ->
      mktype (T_refer (name, List.assoc name b))
;;


let rec find_params (t:xdr_type) : StringSet.t =
  (* collect all parameters *)
  match t.term with
    T_param p ->
      StringSet.singleton p
  | T_array_fixed (t',n) ->
      find_params t'
  | T_array (t',n) ->
      find_params t'
  | T_struct s ->
      Array.fold_left
        (fun set (s,t') -> StringSet.union (find_params t') set)
        StringSet.empty
        s
  | T_union_over_int (htbl,def_opt) ->
      Hashtbl.fold
        (fun n t' set -> StringSet.union (find_params t') set)
        htbl
        (match def_opt with
	     None -> StringSet.empty
	   | Some def -> find_params def)
  | T_union_over_uint (htbl,def_opt) ->
      Hashtbl.fold
        (fun n t' set -> StringSet.union (find_params t') set)
        htbl
        (match def_opt with
	     None -> StringSet.empty
	   | Some def -> find_params def)
  | T_union_over_enum (e,u,def_opt) ->
      Array.fold_left (fun set t' ->
			 match t' with
			     Some t'' -> StringSet.union (find_params t'') set
			   | None     -> set)
                      (match def_opt with
			   None -> StringSet.empty
			 | Some def -> find_params def)
                      u
  | T_rec (_,t') ->
      find_params t'
  | _ ->
      StringSet.empty
;;


let rec validate_xdr_type (t:xdr_type_term) : xdr_type =
  let r n =
    raise (Propagate "Cannot resolve X_type element")
  in
  try
    let t' = validate_xdr_type_i1 r [] t in
    let pl = find_params t' in
    t'.params <- pl;
    t'
  with
    Not_found ->
      failwith "Xdr.validate_xdr_type: unspecified error"
  | Propagate s ->
      failwith ("Xdr.validate_xdr_type: " ^ s)
;;


let rec expand_X_type (s:xdr_type_system) (t:xdr_type_term) : xdr_type =
  match t with
    X_type n ->
      begin
	let rec r s1 s2 =
	  match s2 with
	    []       -> raise (Propagate ("Cannot resolve X_type " ^ n))
	  | (n',t') :: s2' ->
	      if n = n' then
		t'
	      else
		r (s1 @ [n',t']) s2'
	in
	r [] s
      end
  | _ ->
      raise (Propagate "Found X_type where it must not occur")
;;


let validate_xdr_type_system (s:xdr_type_term_system) : xdr_type_system =
  let names = List.map fst s in
  if all_distinct names then begin
    let rec r (s1:xdr_type_system) (s2:xdr_type_term_system) =
      match s2 with
	[]           -> []
      |	(n,t) :: s2' ->
	  let t2 =
	  begin
	    try
	      let t' = validate_xdr_type_i1 (expand_X_type s1) [] t in
	      let pl = find_params t' in
	      t'.params <- pl;
	      t'
	    with
	      Not_found -> failwith "Xdr.validate_xdr_type_system: unspecified error"
	    | Propagate s -> failwith ("Xdr.validate_xdr_type_system: " ^ s)
	  end
	  in
	  (n,t2)::(r (s1 @ [n,t2]) s2')
    in
    r [] s
  end
  else
    failwith "Xdr.validate_xdr_type_system: type system has members with same names"
;;


(**********************************************************************)
(* the reverse way                                                    *)
(**********************************************************************)


let rec xdr_type_term (t:xdr_type) : xdr_type_term =
  let conv_list l =
    List.map (fun (x, t') -> x, xdr_type_term t') l in
  let conv_htbl htbl =
    Hashtbl.fold (fun x t' l -> (x, xdr_type_term t') :: l) htbl [] in
  let conv_option p =
    match p with None -> None | Some t' -> Some (xdr_type_term t') in

  match t.term with
    T_int    -> X_int
  | T_uint   -> X_uint
  | T_hyper  -> X_hyper
  | T_uhyper -> X_uhyper
  | T_enum l -> X_enum (Array.to_list
			  (Array.map
			     (fun (n,i) -> (n,Rtypes.int4_of_int32 i))
			     l))
  | T_float  -> X_float
  | T_double -> X_double
  | T_void   -> X_void
  | T_param p        -> X_param p
  | T_opaque_fixed n -> X_opaque_fixed n
  | T_opaque n       -> X_opaque n
  | T_string n       -> X_string n
  | T_array_fixed (t', n) -> X_array_fixed (xdr_type_term t',n)
  | T_array (t', n)       -> X_array       (xdr_type_term t',n)
  | T_struct s       -> X_struct (conv_list (Array.to_list s))
  | T_rec (n, t')    -> X_rec (n, xdr_type_term t')
  | T_refer (n, t')  -> X_refer n
  | T_union_over_int (u,d)  -> X_union_over_int  (conv_htbl u, conv_option d)
  | T_union_over_uint (u,d) -> X_union_over_uint (conv_htbl u, conv_option d)
  | T_union_over_enum ( { term = T_enum e } as e_term ,u,d) ->
      let u' =
	List.flatten
	  (Array.to_list
	     (Array.mapi
		(fun k t'_opt ->
		   match t'_opt with
		       Some t' ->
			 let name = fst(e.(k)) in
			 [ name, xdr_type_term t' ]
		     | None ->
			 []
		)
		u
	     )
	  )
      in
      X_union_over_enum (xdr_type_term e_term, u', conv_option d)
  | _ ->
      assert false
;;


let xdr_type_term_system (s:xdr_type_system) : xdr_type_term_system =
  List.map (fun (n,t) -> n,xdr_type_term t) s
;;


(**********************************************************************)
(* expand X_type members relative to given systems                    *)
(**********************************************************************)

(* The implemantation of "expanded_xdr_type_term" repeats many phrases
 * that have been defined for "validate_xdr_type" in a very similar
 * way.
 * TODO: Currently many checks have been left out
 *)


let rec expanded_xdr_type_term (s:xdr_type_term_system) (t:xdr_type_term)
        : xdr_type_term =
  match t with
    X_array_fixed (t',n) ->
      X_array_fixed ((expanded_xdr_type_term s t'), n)
  | X_array (t',n) ->
      X_array ((expanded_xdr_type_term s t'), n)
  | X_struct st ->
      let s_names, s_types = List.split st in
      X_struct
	(List.combine
	   s_names
	   (List.map (expanded_xdr_type_term s) s_types))
  | X_union_over_int (u,default) ->
      let u_values, u_types = List.split u in
      let default' =
	match default with
	  Some d -> Some (expanded_xdr_type_term s d)
	| None   -> None
      in
      X_union_over_int
	(List.combine
	   u_values
	   (List.map (expanded_xdr_type_term s) u_types), default')
  | X_union_over_uint (u,default) ->
      let u_values, u_types = List.split u in
      let default' =
	match default with
	  Some d -> Some (expanded_xdr_type_term s d)
	| None   -> None
      in
      X_union_over_uint
	(List.combine
	   u_values
	   (List.map (expanded_xdr_type_term s) u_types), default')
  | X_union_over_enum (e,u,default) ->
      let u_values, u_types = List.split u in
      let default' =
	match default with
	  Some d -> Some (expanded_xdr_type_term s d)
	| None   -> None
      in
      X_union_over_enum
	( (expanded_xdr_type_term s e),
	 (List.combine
	    u_values
	    (List.map (expanded_xdr_type_term s) u_types)),
	 default')
  | X_type n ->
      let rec r s1 s2 =
	match s2 with
	  [] ->
	    failwith ("Xdr.expanded_xdr_type_term: cannot resolve X_type " ^ n)
	| (n',t') :: s2' ->
	      if n = n' then
		expanded_xdr_type_term s1 t'
	      else
		r (s1 @ [n',t']) s2'
      in
      r [] s
  | X_rec (n, t') ->
      X_rec (n, expanded_xdr_type_term s t')
  | _ ->
      t
;;


let expanded_xdr_type (s:xdr_type_system) (t:xdr_type_term) : xdr_type =
  try
    validate_xdr_type_i1 (expand_X_type s) [] t
  with
    Not_found -> failwith "Xdr.expanded_xdr_type: unspecified error"
  | Propagate s -> failwith ("Xdr.expanded_xdr_type: " ^ s)
;;


(**********************************************************************)
(* test on compatibility                                              *)
(**********************************************************************)

let are_compatible (s1:xdr_type) (s2:xdr_type) : bool =
  (* implementation:
   * enum, struct and union members can be swapped
   *)

  failwith "Xdr.are_compatible: not implemented"

;;


(**********************************************************************)
(* common implementation of value_matches_type & pack_xdr_value       *)
(**********************************************************************)

(* pack: interestingly, two loops over the value where one loop only
   determines the size of the final buffer are _faster_ than a single
   loop over the value doing everything. Whoever understands that.
 *)


let ( ++ ) x y =
  (* pre: x >= 0 && y >= 0 *)
  let s = x + y in
  if s < 0 then raise Not_found;  (* overflow *)
  s


let pack_size
      (v:xdr_value)
      (t:xdr_type)
      (get_param:string->xdr_type)
    : int =

  let rec get_size v t =
    match t.term with
      | T_int ->
	  4
      | T_uint ->
	  4
      | T_hyper ->
	  8
      | T_uhyper ->
	  8
      | T_enum e ->
	  4
      | T_float ->
	  4
      | T_double ->
	  8
      | T_opaque_fixed n ->
	  int_of_uint4 n
      | T_opaque n ->
	  let m = int_of_uint4 n in
	  let x = dest_xv_opaque v in
	  let x_len = String.length x in
	  let x_len_mod_4 = x_len land 3 in
	  if x_len <= m then
	    (if x_len_mod_4 = 0
	     then x_len + 4 
	     else x_len + 8 - x_len_mod_4
	    )
	  else
	    raise Not_found
      | T_string n ->
	  let m = int_of_uint4 n in
	  let x = dest_xv_string v in
	  let x_len = String.length x in
	  let x_len_mod_4 = x_len land 3 in
	  if x_len <= m then begin
	    (if x_len_mod_4 = 0
	     then x_len + 4 
	     else x_len + 8 - x_len_mod_4
	    )
	  end
	  else
	    raise Not_found
      | T_array_fixed (t',n) ->
	  let m = int_of_uint4 n in
	  let x = dest_xv_array v in
	  if Array.length x = m then begin
	    let s = ref 0 in
	    Array.iter
	      (fun v' -> s := !s ++ get_size v' t')
	      x;
	    !s
	  end
	  else
	    raise Not_found
      | T_array (t',n) ->
	  let m = int_of_uint4 n in
	  let x = dest_xv_array v in
	  if Array.length x <= m then begin
	    let s = ref 4 in
	    Array.iter
	      (fun v' -> s := !s ++ get_size v' t')
	      x;
	    !s
	  end
	  else
	    raise Not_found
      | T_struct s ->
	  let v_array = map_xv_struct_fast t v in
	  let sum = ref 0 in
	  Array.iteri
	    (fun k v_component ->
	       sum := !sum ++ get_size v_component (snd s.(k)))
	    v_array;
	  !sum
      | T_union_over_int (u,default) ->
	  let i,x = dest_xv_union_over_int v in
	  let t' =
      	    try
	      Hashtbl.find u i
	    with
		Not_found ->
		  match default with
		      Some d -> d
		    | None   -> raise Not_found
	  in
	  4 ++ get_size x t'
      | T_union_over_uint (u,default) ->
	  let i,x = dest_xv_union_over_uint v in
	  let t' =
      	    try
	      Hashtbl.find u i
	    with
		Not_found ->
		  match default with
		      Some d -> d
		    | None   -> raise Not_found
	  in
	  4 ++ get_size x t'
      | T_union_over_enum (et,u,default) ->
	  let k,i,x = map_xv_union_over_enum_fast t v in
	  let t' =
	    match u.(k) with
		Some u_t -> u_t
	      | None     ->
		  ( match default with
			Some d -> d
		      | None -> raise Not_found
		  )
	  in
	  4 ++ get_size x t'
      | T_void ->
	  0
      | T_param n ->
	  let t' = get_param n in
	  get_size v t'
      | T_rec (n, t') ->
	  get_size v t'
      | T_refer (n, t') ->
	  get_size v t'
  in
  get_size v t


let pack_buf
      ?(rm = false)
      (v:xdr_value)
      (t:xdr_type)
      (get_param:string->xdr_type)
    : string =

  let size = 
    (if rm then 4 else 0) ++
      pack_size v t get_param in  (* all sanity checks are done here! *)

  let buf = String.create size in
  let buf_len = ref 0 in

  if rm then (
    buf.[0] <- '\000';
    buf.[1] <- '\000';
    buf.[2] <- '\000';
    buf.[3] <- '\000';
    buf_len := 4
  );

  let print_string s l =  (* assert(l=String.length s) *)
    let n = 4-(l land 3) in
    if n < 4 then begin
      String.unsafe_blit s 0 buf !buf_len l;
      let l0 = !buf_len + l in
      if n >= 1 then String.unsafe_set buf l0 '\000';
      if n >= 2 then String.unsafe_set buf (l0 + 1) '\000';
      if n >= 3 then String.unsafe_set buf (l0 + 2) '\000';
      buf_len := l0 + n
    end
    else begin
      String.unsafe_blit s 0 buf !buf_len l;
      buf_len := !buf_len + l
    end
  in

  let rec pack v t =
    match t.term with
	T_int ->
	  let x = dest_xv_int v in
	  Rtypes.write_int4_unsafe buf !buf_len x;
	  buf_len := !buf_len + 4
      | T_uint ->
	  let x = dest_xv_uint v in
	  Rtypes.write_uint4_unsafe buf !buf_len x;
	  buf_len := !buf_len + 4
      | T_hyper ->
	  let x = dest_xv_hyper v in
	  Rtypes.write_int8_unsafe buf !buf_len x;
	  buf_len := !buf_len + 8
      | T_uhyper ->
	  let x = dest_xv_uhyper v in
	  Rtypes.write_uint8_unsafe buf !buf_len x;
	  buf_len := !buf_len + 8
      | T_enum e ->
	  let i = map_xv_enum_fast t v in
	  Rtypes.write_int4_unsafe buf !buf_len (int4_of_int32 i);
	  buf_len := !buf_len + 4
      | T_float ->
	  let x = dest_xv_float v in
	  let s = fp4_as_string x in
	  String.unsafe_blit s 0 buf !buf_len 4;
	  buf_len := !buf_len + 4
      | T_double ->
	  let x = dest_xv_double v in
	  let s = fp8_as_string x in
	  String.unsafe_blit s 0 buf !buf_len 8;
	  buf_len := !buf_len + 8
      | T_opaque_fixed n ->
	  let x = dest_xv_opaque v in
	  print_string x (String.length x)
      | T_opaque n ->
	  let x = dest_xv_opaque v in
	  let x_len = String.length x in
	  Rtypes.write_uint4_unsafe buf !buf_len (uint4_of_int x_len);
	  buf_len := !buf_len + 4;
	  print_string x x_len
      | T_string n ->
	  let x = dest_xv_string v in
	  let x_len = String.length x in
	  Rtypes.write_uint4_unsafe buf !buf_len (uint4_of_int x_len);
	  buf_len := !buf_len + 4;
	  print_string x x_len
      | T_array_fixed (t',n) ->
	  let x = dest_xv_array v in
	  Array.iter
	    (fun v' -> pack v' t')
	    x
      | T_array (t',n) ->
	  let x = dest_xv_array v in
	  Rtypes.write_uint4_unsafe buf !buf_len
	    (uint4_of_int (Array.length x));
	  buf_len := !buf_len + 4;
	  Array.iter
	    (fun v' -> pack v' t')
	    x
      | T_struct s ->
	  let v_array = map_xv_struct_fast t v in
	  Array.iteri
	    (fun k v_component ->
	       pack v_component (snd s.(k)))
	    v_array
      | T_union_over_int (u,default) ->
	  let i,x = dest_xv_union_over_int v in
	  let t' =
      	    try
	      Hashtbl.find u i
	    with
		Not_found ->
		  match default with
		      Some d -> d
		    | None   -> raise Not_found
	  in
	  Rtypes.write_int4_unsafe buf !buf_len i;
	  buf_len := !buf_len + 4;
	  pack x t'
      | T_union_over_uint (u,default) ->
	  let i,x = dest_xv_union_over_uint v in
	  let t' =
      	    try
	      Hashtbl.find u i
	    with
		Not_found ->
		  match default with
		      Some d -> d
		    | None   -> raise Not_found
	  in
	  Rtypes.write_uint4_unsafe buf !buf_len i;
	  buf_len := !buf_len + 4;
	  pack x t'
      | T_union_over_enum (et,u,default) ->
	  let k,i,x = map_xv_union_over_enum_fast t v in
	  let t' =
	    match u.(k) with
		Some u_t -> u_t
	      | None     ->
		  ( match default with
			Some d -> d
		      | None -> raise Not_found
		  )
	  in
	  Rtypes.write_int4_unsafe buf !buf_len (int4_of_int32 i);
	  buf_len := !buf_len + 4;
	  pack x t'
      | T_void ->
	  ()
      | T_param n ->
	  let t' = get_param n in
	  pack v t'
      | T_rec (n, t') ->
	  pack v t'
      | T_refer (n, t') ->
	  pack v t'
  in
  pack v t;
  buf


let value_matches_type
    (v:xdr_value)
    (t:xdr_type)
    (p:(string * xdr_type) list)
  : bool =
  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty t'.params) p then
    try
      ignore(pack_size v t (fun n -> List.assoc n p));
      true
    with
      _ ->      (* we assume here that no other errors can occur *)
      	false
  else
    false
;;


(**********************************************************************)
(* pack and unpack values                                             *)
(**********************************************************************)

let pack_xdr_value
    (v:xdr_value)
    (t:xdr_type)
    (p:(string * xdr_type) list)
    (print:string->unit)
  : unit =

  (* DEBUG *)
  (* List.iter (fun pn -> prerr_endline ("param " ^ pn)) t.params; *)

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty t'.params) p then
    try
      print (pack_buf v t (fun n -> List.assoc n p))
    with
      any ->
	(* DEBUG *)
	(* prerr_endline (Netexn.to_string any); *)
      	failwith "Xdr.pack_xdr_value"
  else
    failwith "Xdr.pack_xdr_value"
;;


let pack_xdr_value_as_string
    ?(rm = false)
    (v:xdr_value)
    (t:xdr_type)
    (p:(string * xdr_type) list)
  : string =

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty t'.params) p then
    try
      pack_buf ~rm v t (fun n -> List.assoc n p)
    with
      any ->
	(* DEBUG *)
	(* prerr_endline (Netexn.to_string any); *)
      	failwith "Xdr.pack_xdr_value_as_string"
  else
    failwith "Xdr.pack_xdr_value_as_string"
;;


let find_enum (e : (string * int32) array) (i : int32) =
  let rec loop lb ub =
    (* The element is between lb and ub *)
    if lb > ub then raise Not_found;
    let m = (ub + lb) lsr 1 in
    let x_m = snd(e.(m)) in
    if i = x_m then
      (* Found! *)
      m
    else if i < x_m then
      loop lb (m-1)
    else
      (* i > x_m *)
      loop (m+1) ub
  in
  loop 0 (Array.length e - 1)
;;


exception Xdr_format_too_short

let read_string str k k_end n =
  let m = if n mod 4 = 0 then n else n+4-(n mod 4) in
  if !k + m <= k_end then begin
    k := !k + m;
    let s = String.create n in
    String.unsafe_blit str (!k - m) s 0 n;
    s
  end
  else
    raise Xdr_format_too_short

let unpack_term
    ?(pos = 0)
    ?len
    ?(fast = false)
    ?(prefix = false)
    (str:string)
    (t:xdr_type)
    (get_param:string->xdr_type)
  : xdr_value * int =

  let len =
    match len with
	None -> String.length str - pos
      | Some l -> l
  in

  if pos < 0 || len < 0 || len > String.length str - pos then
    invalid_arg "Xdr.unpack_xdr_value";

  let k_end = pos+len in
  let k = ref pos in

  let read_fp4 () =
    if !k + 4 <= k_end then begin
      k := !k + 4;
      mk_fp4 (str.[ !k - 4 ], str.[ !k - 3 ], str.[ !k - 2 ], str.[ !k - 1 ])
    end
    else
      raise Xdr_format_too_short
  in

  let read_fp8 () =
    if !k + 8 <= k_end then begin
      k := !k + 8;
      mk_fp8 (str.[ !k - 8 ], str.[ !k - 7 ], str.[ !k - 6 ], str.[ !k - 5 ],
              str.[ !k - 4 ], str.[ !k - 3 ], str.[ !k - 2 ], str.[ !k - 1 ])
    end
    else
      raise Xdr_format_too_short
  in

  let rec unpack t =
    match t.term with
      T_int ->
	let k' = !k in
	k := !k + 4;
	if !k > k_end then raise Xdr_format_too_short;
	XV_int (Rtypes.read_int4_unsafe str k')
    | T_uint ->
	let k' = !k in
	k := !k + 4;
	if !k > k_end then raise Xdr_format_too_short;
	XV_uint (Rtypes.read_uint4_unsafe str k')
    | T_hyper ->
	let k' = !k in
	k := !k + 8;
	if !k > k_end then raise Xdr_format_too_short;
	XV_hyper (Rtypes.read_int8_unsafe str k')
    | T_uhyper ->
	let k' = !k in
	k := !k + 8;
	if !k > k_end then raise Xdr_format_too_short;
	XV_uhyper (read_uint8_unsafe str k')
    | T_enum e ->
	let k' = !k in
	k := !k + 4;
	if !k > k_end then raise Xdr_format_too_short;
	let i = Rtypes.int32_of_int4(Rtypes.read_int4_unsafe str k') in
	( try
	    let k = find_enum e i in
	    (* returns array position, or Not_found *)
	    if fast then
	      XV_enum_fast k
	    else
	      XV_enum(fst(e.(k)))
	  with
	    Not_found -> raise (Xdr_format "value not included in enumeration")
	)
    | T_float ->
	XV_float (read_fp4 ())
    | T_double ->
	XV_double (read_fp8())
    | T_opaque_fixed n ->
	XV_opaque (read_string str k k_end (int_of_uint4 n))
    | T_opaque n ->
	let k' = !k in
	k := !k + 4;
	if !k > k_end then raise Xdr_format_too_short;
	let m = Rtypes.read_uint4_unsafe str k' in
	(* Test: m <= n as unsigned int32: *)
	let m32 = logical_int32_of_uint4 m in
	let n32 = logical_int32_of_uint4 n in
	if (m32 >= 0l && (m32 <= n32 || n32 < 0l)) || (m32 < 0l && n32 <= m32)
	then
	  XV_opaque (read_string str k k_end (int_of_uint4 m))
	else
	  raise (Xdr_format "maximum length of field exceeded")
    | T_string n ->
	let k' = !k in
	k := !k + 4;
	if !k > k_end then raise Xdr_format_too_short;
	let m = Rtypes.read_uint4_unsafe str k' in
	(* Test: m <= n as unsigned int32: *)
	let m32 = logical_int32_of_uint4 m in
	let n32 = logical_int32_of_uint4 n in
	if (m32 >= 0l && (m32 <= n32 || n32 < 0l)) || (m32 < 0l && n32 <= m32)
	then
	  XV_string (read_string str k k_end (int_of_uint4 m))
	else
	  raise (Xdr_format "maximum length of field exceeded")
    | T_array_fixed (t',n) ->
	let p = int_of_uint4 n in
	let a = Array.create p XV_void in
	for i = 0 to p-1 do
	  Array.unsafe_set a i (unpack t')
	done;
	XV_array a
    | T_array (t',n) ->
	let k' = !k in
	k := !k + 4;
	let m = Rtypes.read_uint4 str k' in
	let q = logical_int32_of_uint4 n in
	let p = int_of_uint4 m in
	let p32 = Int32.of_int p in
	(* Test: p <= q, as unsigned int32: *)
	if (p32 >= 0l && (p32 <= q || q < 0l)) || (p32 < 0l && q <= p32) 
	then begin
	  let a = Array.create p XV_void in
	  for i = 0 to p-1 do
	    Array.unsafe_set a i (unpack t')
	  done;
	  XV_array a
	end
	else
	  raise (Xdr_format "maximum length of array exceeded")
    | T_struct s ->
	if fast then
	  XV_struct_fast
	    ( Array.map
		(fun (name,t') -> unpack t')
		s
	    )
	else
	  XV_struct
	    (List.map
	       (fun (name,t') -> (name,unpack t'))
	       (Array.to_list s)
	    )
    | T_union_over_int (u,default) ->
	let k' = !k in
	k := !k + 4;
	let n = Rtypes.read_int4 str k' in
	let t' =
	  try
	    Hashtbl.find u n
	  with
	    Not_found ->
	      match default with
		None   -> raise (Xdr_format "undefined discriminator")
	      |	Some d -> d
	in
	XV_union_over_int (n, unpack t')
    | T_union_over_uint (u,default) ->
	let k' = !k in
	k := !k + 4;
	let n = Rtypes.read_uint4 str k' in
	let t' =
	  try
	    Hashtbl.find u n
	  with
	    Not_found ->
	      match default with
		None   -> raise (Xdr_format "undefined discriminator")
	      |	Some d -> d
	in
	XV_union_over_uint (n, unpack t')
    | T_union_over_enum ( { term = T_enum e },u,default) ->
	let k' = !k in
	k := !k + 4;
	let i = Rtypes.int32_of_int4 (Rtypes.read_int4 str k') in
	let k =
	  try
	    find_enum e i
	    (* returns array position, or Not_found *)
	  with
	    Not_found -> raise (Xdr_format "value not included in enumeration")
	in
	let t' =
	  match u.(k) with
	      Some u_t -> u_t
	    | None ->
		( match default with
		      Some d -> d
		    | None ->
			raise (Xdr_format "undefined discriminator"))
	in
	if fast then
	  XV_union_over_enum_fast(k, unpack t')
	else
	  let name = fst(e.(k)) in
	  XV_union_over_enum(name, unpack t')
    | T_void ->
	XV_void
    | T_param p ->
	let t' = get_param p in
	unpack t'
    | T_rec (n, t') ->
	unpack t'
    | T_refer (n, t') ->
	unpack t'
    | _ ->
	assert false
  in
  try
    let v = unpack t in
    if prefix || !k = k_end then
      (v, !k - pos)
    else
      raise (Xdr_format_message_too_long v)
  with
      Cannot_represent _ ->
	raise (Xdr_format "implementation restriction")
    | Out_of_range ->
	raise (Xdr_format "message too short")
    | Xdr_format_too_short ->
	raise (Xdr_format "message too short")
;;


let unpack_xdr_value
    ?pos ?len ?fast ?prefix
    (str:string)
    (t:xdr_type)
    (p:(string * xdr_type) list)
  : xdr_value =

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty t'.params) p then

    fst(unpack_term ?pos ?len ?fast ?prefix str t (fun n -> List.assoc n p))

  else
    failwith "Xdr.unpack_xdr_value"
;;


let unpack_xdr_value_l
    ?pos ?len ?fast ?prefix
    (str:string)
    (t:xdr_type)
    (p:(string * xdr_type) list)
  : xdr_value * int =

  if StringSet.for_all (fun n -> List.mem_assoc n p) t.params &&
     List.for_all (fun (n,t') -> StringSet.is_empty t'.params) p then

    unpack_term ?pos ?len ?fast ?prefix str t (fun n -> List.assoc n p)

  else
    failwith "Xdr.unpack_xdr_value"
;;

