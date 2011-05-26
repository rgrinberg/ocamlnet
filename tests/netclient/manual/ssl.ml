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

  if proxy then (
    p # set_proxy "voip.camlcity.org" 3128;
    p # set_proxy_auth "gerd" "BadNauHeimer"
  );

  let m1 = new get "https://www.camlcity.org/" in
  p#add m1;
  p#run();
  m1

