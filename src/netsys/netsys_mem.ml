(* $Id$ *)

type memory = 
    (char,Bigarray.int8_unsigned_elt,Bigarray.c_layout) Bigarray.Array1.t

external netsys_blit_memory_to_string :
           memory -> int -> string -> int -> int -> unit
  = "netsys_blit_memory_to_string"

external netsys_blit_string_to_memory : 
           string -> int -> memory ->  int -> int -> unit
  = "netsys_blit_string_to_memory"

let blit_memory_to_string mem memoff s soff len =
  let memlen = Bigarray.Array1.dim mem in
  let slen = String.length s in
  if len < 0 || memoff < 0 || memoff > memlen - len || 
     soff < 0 || soff > slen - len 
  then
    invalid_arg "Netsys_mem.blit_memory_to_string";
  netsys_blit_memory_to_string mem memoff s soff len

let blit_string_to_memory s soff mem memoff len =
  let memlen = Bigarray.Array1.dim mem in
  let slen = String.length s in
  if len < 0 || memoff < 0 || memoff > memlen - len || 
     soff < 0 || soff > slen - len 
  then
    invalid_arg "Netsys_mem.blit_string_to_memory";
  netsys_blit_string_to_memory s soff mem memoff len

external memory_address : memory -> nativeint
  = "netsys_memory_address"

external getpagesize : unit -> int
  = "netsys_getpagesize"

external netsys_alloc_memory_pages : nativeint -> int -> memory
  = "netsys_alloc_memory_pages"

let alloc_memory_pages ?(addr=0n) len =
  netsys_alloc_memory_pages addr len

external alloc_aligned_memory : int -> int -> memory
  = "netsys_alloc_aligned_memory"

external netsys_map_file : 
           Unix.file_descr -> int64 -> nativeint -> bool -> int -> memory
  = "netsys_map_file"

let memory_map_file fd ?(pos=0L) ?(addr=0n) shared size =
  netsys_map_file fd pos addr shared size

external memory_unmap_file : memory -> unit
  = "netsys_memory_unmap_file"

external as_value : memory -> int -> 'a
  = "netsys_as_value"

external netsys_init_string : memory -> int -> int -> unit
  = "netsys_init_string"

let init_string_bytelen len =
  let ws = Sys.word_size / 8 in  (* word size in bytes *)
  ((len + ws) / ws + 1) * ws
  

exception Out_of_space

let _ = 
  Callback.register_exception "Netsys_mem.Out_of_space" Out_of_space



let init_string mem offset len =
  let ws = Sys.word_size / 8 in  (* word size in bytes *)
  let memlen = Bigarray.Array1.dim mem in
  if offset < 0 || len < 0 then
    invalid_arg "Netsys_mem.init_string";
  let blen = init_string_bytelen len in
  if blen > memlen - offset then
    raise Out_of_space;
  netsys_init_string mem offset len;
  (ws, blen)


type init_value_flag =
  | Copy_bigarray
  | Copy_custom
  | Copy_atom
  | Copy_simulate

external init_value : 
  memory -> int -> 'a -> init_value_flag list -> (int * int)
  = "netsys_init_value"


external netsys_mem_read : Unix.file_descr -> memory -> int -> int -> int
  = "netsys_mem_read"

let mem_read fd mem off len =
  if len < 0 || off < 0 || len > Bigarray.Array1.dim mem - off then
    invalid_arg "Netsys_mem.mem_read";
  netsys_mem_read fd mem off len

    
