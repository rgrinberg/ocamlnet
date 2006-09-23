(* 	$Id: args_fcgi.ml,v 1.2 2005/10/19 20:27:00 chris_77 Exp $	 *)

open Args_common

let () =
  try
    Netcgi_fcgi.run ~config (fun cgi -> main (cgi :> Netcgi.cgi))

  with Unix.Unix_error(Unix.ENOTSOCK, "accept", _) ->
    (* stdin is not a socket, launch an external server. *)
    let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any, 8888) in
    Netcgi_fcgi.run ~config (fun cgi -> main (cgi :> Netcgi.cgi)) ~sockaddr
