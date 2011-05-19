(* args_scgi.ml *)

open Args_common

let () =
  let port = 8888 in
  Printf.printf "SCGI server running on port %i\n%!" port;
  Netcgi_scgi.run ~config ~port (fun cgi -> main (cgi :> Netcgi.cgi))
