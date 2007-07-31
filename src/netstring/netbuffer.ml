(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

type t = 
    { mutable buffer : string;
      mutable length : int;
      create_length : int;
    }

(* To help the garbage collector:
 * The 'buffer' has a minimum length of 31 bytes. This minimum can still
 * be stored in the minor heap.
 * The 'buffer' has a length which is always near a multiple of two. This
 * limits the number of different bucket sizes, and simplifies reallocation
 * of freed memory.
 *)

(* Optimal string length:
 * Every string takes: 1 word for the header, enough words for the 
 * contents + 1 Null byte (for C compatibility).
 * If the buffer grows, it is best to use a new string length such
 * that the number of words is exactly twice as large as for the previous
 * string.
 * n:              length of the previous string in bytes
 * w:              storage size of the previous string in words
 * n':             length of the new string in bytes
 * w' = 2*w:       storage size of the new string in words
 *
 * w = (n+1) / word_length + 1
 *            [it is assumed that (n+1) is always a multiple of word_length]
 *
 * n' = (2*w - 1) * word_length - 1
 *
 * n' = [2 * ( [n+1] / word_length + 1) - 1] * word_length - 1
 *    = ...
 *    = (2*n + 2) + word_length - 1
 *    = 2 * n + word_length + 1
 *
 * n'+1 is again a multiple of word_length:
 * n'+1 = 2*n + 2 + word_length
 *      = 2*(n+1) + word_length
 *      = a multiple of word_length because n+1 is a multiple of word_length
 *)

let word_length = Sys.word_size / 8       (* in bytes *)

let create n =
  { buffer = String.create (max n 31); length = 0; create_length = n }

let reset b =
  let n = b.create_length in
  b.buffer <- String.create (max n 31);
  b.length <- 0

let contents b =
  String.sub b.buffer 0 b.length

let e_get() =
  invalid_arg "Netbuffer.get"

let get b k =
  if k < 0 || k >= b.length then e_get();
  String.unsafe_get b.buffer k
    
let nth = get

let sub b k n =
  if k < 0 || n < 0 || k+n > b.length then
    raise (Invalid_argument "Netbuffer.sub");
  String.sub b.buffer k n

let blit_to_string b srcpos dest destpos n =
  if srcpos < 0 || n < 0 || srcpos+n > b.length then
    raise (Invalid_argument "Netbuffer.blit_to_string");
  String.blit b.buffer srcpos dest destpos n

let blit = blit_to_string

    
let unsafe_buffer b =
  b.buffer

let length b =
  b.length

let ensure_space b n =
  (* Ensure that there are n bytes space in b *)
  if n > String.length b.buffer then begin
    let rec new_size s =
      if s >= n then s else new_size(2*s + word_length + 1)
    in
    let size = min (new_size (String.length b.buffer)) Sys.max_string_length in
    if size < n then
      failwith "Netbuffer: string too large";
    let buffer' = String.create size in
    String.blit b.buffer 0 buffer' 0 b.length;
    b.buffer <- buffer'
  end

let add_sub_string_int b s k l =
  ensure_space b (l + b.length);
  String.blit s k b.buffer b.length l;
  b.length <- b.length + l

let add_sub_string b s k l =
  if k < 0 || l < 0 || k+l > String.length s then
    invalid_arg "Netbuffer.add_sub_string";
  add_sub_string_int b s k l

let add_substring = add_sub_string

let add_string b s =
  add_sub_string b s 0 (String.length s)

let add_char b c =
  ensure_space b (b.length+1);
  String.unsafe_set b.buffer b.length c;
  b.length <- b.length + 1

let add_inplace ?len b f =
  let len' =
    match len with
	Some l -> 
	  ensure_space b (b.length + l);
	  l
      | None ->
	  ensure_space b (b.length + 1);
	  String.length b.buffer - b.length
  in
  let n = f b.buffer b.length len' in
  b.length <- b.length + n;
  n

let add_buffer b1 b2 =
  let len = b1.length + b2.length in
  ensure_space b1 len;
  String.blit b2.buffer 0 b1.buffer b1.length b2.length;
  b1.length <- len


let insert_sub_string_int b p s k l =
  if p < 0 || p > b.length then
    invalid_arg "Netbuffer.insert_sub_string";
  ensure_space b (l + b.length);
  String.blit b.buffer p b.buffer (p+l) (b.length - p);
  String.blit s k b.buffer p l;
  b.length <- b.length + l

let insert_sub_string b p s k l =
  if k < 0 || l < 0 || k+l > String.length s then
    invalid_arg "Netbuffer.insert_sub_string";
  insert_sub_string_int b p s k l

let insert_string b p s =
  insert_sub_string_int b p s 0 (String.length s)

let insert_char b p c =
  if p < 0 || p > b.length then
    invalid_arg "Netbuffer.insert_char";
  ensure_space b (1 + b.length);
  String.blit b.buffer p b.buffer (p+1) (b.length - p);
  b.buffer.[p] <- c;
  b.length <- b.length + 1


let e_set() =
  invalid_arg "Netbuffer.set"

let set b k c =
  if k < 0 || k >= b.length then e_set();
  String.unsafe_set b.buffer k c

let put_string b p s =
  if p < 0 || p > b.length then
    invalid_arg "Netbuffer.put_string";
  let len = max b.length (p + String.length s) in
  ensure_space b len;
  String.blit s 0 b.buffer p (String.length s);
  b.length <- len

let blit_from_string src srcpos b p n =
  if p < 0 || p > b.length || srcpos < 0 || n < 0 || srcpos+n > String.length src then
    invalid_arg "Netbuffer.blit_from_string";
  let len = max b.length (p + n) in
  ensure_space b len;
  String.blit src srcpos b.buffer p n;
  b.length <- len


let delete b k l =
  (* deletes l bytes at position k in b *)
  let n = String.length b.buffer in
  if k+l <> n & k <> n then
    String.blit b.buffer (k+l) b.buffer k (n-l-k);
  b.length <- b.length - l;
  ()

let try_shrinking b =
  (* If the buffer size decreases drastically, reallocate the buffer *)
  if b.length < (String.length b.buffer / 2) then begin
    let rec new_size s =
      if s >= b.length then s else new_size(2*s + word_length + 1)
    in
    let buffer' = String.create (new_size 31) in
    String.blit b.buffer 0 buffer' 0 b.length;
    b.buffer <- buffer'
  end 

let clear b =
  delete b 0 (b.length)
  
let index_from b k c =
  if k > b.length then
    raise (Invalid_argument "Netbuffer.index_from");
  let p = String.index_from b.buffer k c in
  if p >= b.length then raise Not_found;
  p

let print_buffer b =
  Format.printf
    "<NETBUFFER: %d/%d>"
    b.length
    (String.length b.buffer)
;;
