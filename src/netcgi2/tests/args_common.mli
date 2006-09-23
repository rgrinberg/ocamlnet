(* 	$Id: args_show.mli,v 1.2 2005/10/19 20:28:05 chris_77 Exp $	 *)


val config : Netcgi.config

val main : Netcgi.cgi -> unit
  (** [script run] will run the script contained in this library on
      the connector given through the [run] function. *)
