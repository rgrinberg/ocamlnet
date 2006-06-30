(* 	$Id: args_cgi.ml,v 1.2 2005/10/19 20:27:00 chris_77 Exp $	 *)

open Args_common

let () =
  Netcgi_cgi.run ~config main
