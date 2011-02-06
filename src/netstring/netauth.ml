(* $Id$ *)

let xor_s s u =
  let s_len = String.length s in
  let u_len = String.length u in
  assert(s_len = u_len);
  let x = String.create s_len in
  for k = 0 to s_len-1 do
    x.[k] <- Char.chr ((Char.code s.[k]) lxor (Char.code u.[k]))
  done;
  x

let hmac ~h ~b ~l ~k ~message =
  if String.length k > b then
    failwith "Netauth.hmac: key too long";
  
  let k_padded = k ^ String.make (b - String.length k) '\000' in
  let ipad = String.make b '\x36' in
  let opad = String.make b '\x5c' in
  h((xor_s k_padded opad) ^ (h ((xor_s k_padded ipad) ^ message)))
