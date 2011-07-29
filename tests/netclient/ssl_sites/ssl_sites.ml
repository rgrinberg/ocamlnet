open Printf

let urls =
  [ "https://www.wellsfargo.com/";
    "https://godirepo.camlcity.org/";
    "http://www.gerd-stolpmann.de";
    "https://broking.postbank.de/templates/index.jsp";
    "https://adwords.google.de/select/";
    "https://www.1822direkt.com/1822central/cms/47.jsp";
    "https://intranet.fr-aktuell.de/webabo/start.do?";
    "https://www.cortalconsors.de/euroWebDe/-";
    "https://www.collmex.de/cgi-bin/cgi.exe?35335,0,login";
    "https://www.comfi.com/reg/?l="
  ] 

let () =
  Ssl.init();
  Http_client.Convenience.configure_pipeline
  (fun p ->
     let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context in
     let tct = Https_client.https_transport_channel_type ctx in
     p # configure_transport Http_client.https_cb_id tct
  );

  let errors = ref 0 in

  List.iter
    (fun url ->
       let t0 = Unix.time() in
       printf "URL %s: %!" url;
       ( try
	   let _ = Http_client.Convenience.http_get url in ()
	 with
	   | error ->
	       printf "Error %s\n%!" (Netexn.to_string error);
	       incr errors
       );
       let t1 = Unix.time() in
       if t1 -. t0 > 10.0 then (
	 printf "TOO SLOW\n%!";
	 incr errors
       )
       else
	 printf "OK\n%!"
    )
    urls;

  printf "Errors: %d\n" !errors;
  if !errors > 0 then (
    printf "*** TEST FAILED!\n%!";
    exit 1
  )
