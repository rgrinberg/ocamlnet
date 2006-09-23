(* $Id: filemanager_mod.ml,v 1.1 2005/09/04 11:45:22 chris_77 Exp $ *)

(** This program must be compiled to .cmo.  Moreover, put in a Apache
    conf file:

    CamlLoad nums.cma
    CamlLoad numerix/numerix.cma
    CamlLoad cryptokit/cryptokit.cma
*)

let () =
  Filemanager.script (fun ~config ~arg_store ~output_type f ->
			Netcgi_mod.run ~config ~arg_store ~output_type f)
