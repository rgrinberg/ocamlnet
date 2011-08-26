#use "topfind";;
#require "rpc,pcre";;
#load "dns.cma";;


open Netdns_message;;
open Netdns_client;;

let query msg =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_DGRAM 0 in
  Unix.connect s (Unix.ADDR_INET (Unix.inet_addr_of_string "192.168.0.1", 53));
  let req = Netdns_message.print msg in
  ignore(Unix.send s req 0 (String.length req) []);
  let resp = String.create 512 in
  let n = Unix.recv s resp 0 512 [] in
  String.sub resp 0 n


let hdr = { msg_id = 0; msg_is_query = true; msg_opcode = `QUERY; msg_is_aa = false; msg_is_trunc = false; msg_rd = false; msg_ra = false; msg_rcode = `No_error };;

let q = [ { q_name = [ "ice"; "lan"; "gerd-stolpmann"; "de" ]; q_type = `A; q_class = `IN } ];;

let m = { msg_header = hdr; msg_question = q; msg_answer = []; msg_authority = []; msg_additional = [] };;

let esys = Unixqueue.create_unix_event_system();;

let cu = Netdns_client.create (`Socket(Rpc.Udp, `Internet(Unix.inet_addr_of_string "192.168.0.1", 53), Netdns_client.default_socket_config)) esys;;

let ct = Netdns_client.create (`Socket(Rpc.Tcp, `Internet(Unix.inet_addr_of_string "192.168.0.1", 53), Netdns_client.default_socket_config)) esys;;

let r = Netdns_resolver.stub_resolver [ Unix.inet_addr_of_string "192.168.0.1" ] esys;;

let x = ref None;;

Netdns_resolver.debug := true;;
