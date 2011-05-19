(* args_ajp.ml *)

open Args_common
open Printf

let () =
  let port = 8009 in
  printf "AJP servlet listening on port %i.\n%!" port;
  Netcgi_ajp.run ~port ~config (fun cgi -> main (cgi :> Netcgi.cgi))
