(* $Id$ *)

open Netsys_mem

class type mstring =
object
  method length : int
  method blit_to_string : int -> string -> int -> int -> unit
  method blit_to_memory : int -> memory -> int -> int -> unit
  method as_string : string * int
  method as_memory : memory * int
  method preferred : [ `Memory | `String ]
end


class type mstring_factory =
object
  method create_from_string : string -> int -> int -> bool -> mstring
  method create_from_memory : memory -> int -> int -> bool -> mstring
end

type named_mstring_factories =
    (string, mstring_factory) Hashtbl.t

let sbm s pos len : mstring =
  if len < 0 || pos < 0 || pos > String.length s - len then
    invalid_arg "Xdr_mstring.sbm";
  ( object
      method length = len
      method blit_to_string mpos u upos l =
	if l < 0 then
	  invalid_arg "Xdr_mstring#blit_to_string";
	if mpos < 0 || mpos > len - l then
	  invalid_arg "Xdr_mstring#blit_to_string";
	if upos < 0 || upos > String.length u - l then
	  invalid_arg "Xdr_mstring#blit_to_string";
	String.blit s (pos+mpos) u upos l
      method blit_to_memory mpos u upos l =
	if l < 0 then
	  invalid_arg "Xdr_mstring#blit_to_memory";
	if mpos < 0 || mpos > len - l then
	  invalid_arg "Xdr_mstring#blit_to_memory";
	if upos < 0 || upos > Bigarray.Array1.dim u - l then
	  invalid_arg "Xdr_mstring#blit_to_memory";
	Netsys_mem.blit_string_to_memory s (pos+mpos) u upos l
      method as_string = (s,pos)
      method as_memory =
	let m = Bigarray.Array1.create Bigarray.char Bigarray.c_layout len in
	Netsys_mem.blit_string_to_memory s pos m 0 len;
	(m,0)
      method preferred = `String
    end
  )


let mbm m pos len : mstring =
  if len < 0 || pos < 0 || pos > Bigarray.Array1.dim m - len then
    invalid_arg "Xdr_mstring.mbm";
  ( object
      method length = len
      method blit_to_string mpos u upos l =
	if l < 0 then
	  invalid_arg "Xdr_mstring#blit_to_string";
	if mpos < 0 || mpos > len - l then
	  invalid_arg "Xdr_mstring#blit_to_string";
	if upos < 0 || upos > String.length u - l then
	  invalid_arg "Xdr_mstring#blit_to_string";
	Netsys_mem.blit_memory_to_string m (pos+mpos) u upos l
      method blit_to_memory mpos u upos l =
	if l < 0 then
	  invalid_arg "Xdr_mstring#blit_to_memory";
	if mpos < 0 || mpos > len - l then
	  invalid_arg "Xdr_mstring#blit_to_memory";
	if upos < 0 || upos > Bigarray.Array1.dim u - l then
	  invalid_arg "Xdr_mstring#blit_to_memory";
	Bigarray.Array1.blit
	  (Bigarray.Array1.sub m (pos+mpos) l)
	  (Bigarray.Array1.sub u upos l)
      method as_string =
	let s = String.create len in
	Netsys_mem.blit_memory_to_string m pos s 0 len;
	(s,0)
      method as_memory = (m,pos)
      method preferred = `Memory
    end
  )


let string_based_mstrings : mstring_factory =
  ( object
      method create_from_string s pos len must_copy =
	if must_copy then
	  let s' = String.sub s pos len in
	  sbm s' 0 len
	else
	  sbm s pos len
      method create_from_memory m pos len must_copy =
	let s = String.create len in
	Netsys_mem.blit_memory_to_string m pos s 0 len;
	sbm s 0 len
    end
  )
	  

let memory_based_mstrings_1 create : mstring_factory =
  ( object
      method create_from_string s pos len must_copy =
	let m = create len in
	Netsys_mem.blit_string_to_memory s pos m 0 len;
	mbm m 0 len
      method create_from_memory m pos len must_copy =
	if must_copy then (
	  let m' = create len in
	  Bigarray.Array1.blit
	    (Bigarray.Array1.sub m pos len)
	    (Bigarray.Array1.sub m' 0 len);
	  mbm m' 0 len
	)
	else
	  mbm m pos len
    end
  )

let memory_based_mstrings =
  memory_based_mstrings_1  
    (Bigarray.Array1.create Bigarray.char Bigarray.c_layout)

let paligned_memory_based_mstrings =
  memory_based_mstrings_1
    (fun n ->
       Netsys_mem.alloc_memory_pages n
    )

let memory_pool_based_mstrings pool =
  memory_based_mstrings_1
    (fun n ->
       if n <= Netsys_mem.pool_block_size pool then
	 Netsys_mem.pool_alloc_memory pool
       else
	 failwith "memory_pool_based_mstrings: string too large for pool"
    )


let length_mstrings mstrings =
  List.fold_left (fun acc ms -> acc + ms#length) 0 mstrings

let concat_mstrings (mstrings : mstring list) =
  match mstrings with
    | [] -> ""
    | _ ->
	let length = length_mstrings mstrings in
	let s = String.create length in
	let p = ref 0 in
	List.iter
	  (fun ms ->
	     let l = ms#length in
	     ms # blit_to_string 0 s !p l;
	     p := !p + l
	  )
	  mstrings;
	s

let prefix_mstrings mstrings n =
  let length = length_mstrings mstrings in
  if n < 0 || n > length then failwith "prefix_mstrings";
  let s = String.create n in
  let p = ref 0 in
  ( try
      List.iter
	(fun ms ->
	   if !p >= n then raise Exit;
	   let l = ms#length in
	   let l' = min l (n - !p) in
	   ms # blit_to_string 0 s !p l';
	   p := !p + l'
	)
	mstrings
    with Exit -> ()
  );
  s

let blit_mstrings_to_memory mstrings mem =
  let length = length_mstrings mstrings in
  if length > Bigarray.Array1.dim mem then
    failwith "blit_mstrings_to_memory";
  let p = ref 0 in
  List.iter
    (fun ms ->
       let l = ms#length in
       ms # blit_to_memory 0 mem !p l;
       p := !p + l
    )
    mstrings
