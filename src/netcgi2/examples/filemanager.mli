(* $Id: filemanager.mli,v 1.1 2005/09/04 11:45:21 chris_77 Exp $ *)

open Netcgi

val script :
  (config:config -> arg_store:arg_store -> output_type:output_type ->
    (#Netcgi.cgi -> unit) -> unit)
  -> unit
