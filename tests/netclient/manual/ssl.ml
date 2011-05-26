#use "topfind";;
#require "netclient";;
#require "equeue-ssl";;

open Http_client;;

Ssl.init();;

let doit ?(proxy=false) () =
  let p = new pipeline in
  let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
  let tct = Https_client.https_transport_channel_type ctx in
  p # configure_transport Http_client.https_cb_id tct ;

  if proxy then
    p # set_socks5_proxy "127.0.0.1" 1080;

  let m1 = new get "https://godirepo.camlcity.org/" in
  p#add m1;
  p#run();
  m1

