(* Testing in the toploop *)

#use "topfind";;
#require "netmech-scram";;
open Netmech_scram;;
let () = Debug.enable := true;;
let c = create_client_session (profile `GSSAPI) "gerd" "xxx";;
let s = create_server_session (profile `GSSAPI) (fun username -> if username = "gerd" then let salt = create_salt() in (salt_password "xxx" salt 4096, salt, 4096) else raise Not_found);;
let c1 = client_emit_message c;;
server_recv_message s c1;;
let s1 = server_emit_message s;;
client_recv_message c s1;;
let cf = client_emit_message c;;
server_recv_message s cf;;
let sf = server_emit_message s;;
client_recv_message c sf;;
