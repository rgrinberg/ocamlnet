#use "topfind";;
#require "unix,equeue,netstring";;
#load "netclient.cma";;
open Ftp_client;;

let connect() =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.inet_addr_of_string "127.0.0.1" in
  Unix.connect s (Unix.ADDR_INET(addr, 21));
  s
;;

let login () =
  let s = connect() in
  let c = new ftp_client_pi s in
  c # add_cmd (`USER "gerd");
  c # add_cmd (`PASS "Stolp!32ps");
  c
;;


let recv (c : Ftp_client.ftp_client_pi) name =
   let b = Buffer.create 10 in
   let ch = (new Netchannels.output_buffer b :> Netchannels.out_obj_channel) in
   c # add_cmd `PASV;
   c # add_cmd (`TYPE `Image);
   c # add_cmd (`RETR(name, (fun _ -> `File_structure ch)));
   b
;;


let recv_actv (c : Ftp_client.ftp_client_pi) name =
   let b = Buffer.create 10 in
   let ch = (new Netchannels.output_buffer b :> Netchannels.out_obj_channel) in
   c # add_cmd `PORT;
   c # add_cmd (`TYPE `Image);
   c # add_cmd (`RETR(name, (fun _ -> `File_structure ch)));
   b
;;


let recv_ebcdic (c : Ftp_client.ftp_client_pi) name =
   let b = Buffer.create 10 in
   let ch = (new Netchannels.output_buffer b :> Netchannels.out_obj_channel) in
   c # add_cmd `PASV;
   c # add_cmd (`TYPE (`EBCDIC None));
   c # add_cmd (`RETR(name, (fun _ -> `File_structure ch)));
   b
;;


let recv_ebcdic_recs (c : Ftp_client.ftp_client_pi) name =
   let b = Buffer.create 10 in
   let ch = (new Netchannels.output_buffer b :> Netchannels.out_obj_channel) in
   let ch' = new Ftp_data_endpoint.write_out_record_channel
                ~repr:(`EBCDIC `Enc_cp1047) ch in
   c # add_cmd `PASV;
   c # add_cmd (`TYPE (`EBCDIC None));
   c # add_cmd (`RETR(name, (fun _ -> `Record_structure ch')));
   b
;;


let list (c : Ftp_client.ftp_client_pi) =
   let b = Buffer.create 10 in
   let ch = (new Netchannels.output_buffer b :> Netchannels.out_obj_channel) in
   c # add_cmd `PASV;
   c # add_cmd (`TYPE `Image);
   c # add_cmd (`LIST(None, (fun _ -> `File_structure ch)));
   b
;;


let login_sru() =
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let addr = Unix.inet_addr_of_string "205.149.70.48" in
  Unix.connect s (Unix.ADDR_INET(addr, 21));
  let c = new ftp_client_pi s in
  c # add_cmd (`USER "anonymous");
  c # add_cmd (`CWD "anonymou.191");
  c
;;


let c = new ftp_client ();;
c # add (new connect_method ~host:"127.0.0.1" ());;
c # add ~onerror:(fun _ -> ()) (new set_mode_method `Block_mode);;
c # add (new login_method ~user:"gerd" ~get_password:(fun () -> "XXX") ~get_account:(fun () -> "") ());;
let buffer = Buffer.create 1000;;
let ch = new Netchannels.output_buffer buffer;;
c # add (new list_method ~dir:(`NVFS "") ~representation:(`ASCII None) ~store:(fun _ -> `File_structure ch) ());;
c # run();;
