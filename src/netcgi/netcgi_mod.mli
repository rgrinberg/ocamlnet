(* netcgi_mod.mli

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

(** Apache "mod" connector.
 *
 * See the {!Netcgi_mod.setup} section at the end of this file to know
 * how to configure Apache.
 *)

open Netcgi

(** The usual [cgi] class with an additional method to access Apache
    specificities.  *)
class type cgi =
object
  inherit Netcgi.cgi

  method request : Apache.Request.t
    (** The underlying apache request structure. *)
end


val run :
  ?config:config ->
  ?output_type:output_type ->
  ?arg_store:arg_store ->
  ?exn_handler:exn_handler ->
  (cgi -> unit) -> unit
  (** [run f] register the function [f] as a main function of the
      script.  Each call to the script will execute [f cgi].  The code
      outside [f] will be executed only once (when the script is
      loaded into memory) which allows to cache database connections,
      etc.  (The code stays in memory unless you restart the server or
      the file changes on disk.)

      @param config Default: {!Netcgi.default_config}
      @param output_type Default: [`Direct ""]
      @param arg_store Default: [`Automatic] for all arguments.
      @param exn_handler See {!Netcgi.exn_handler}.  Default: delegate
      all exceptions to the default handler.  *)


(* ---------------------------------------------------------------------- *)

(** {2:setup Setup}

    In order to use the above function, you need to have mod_caml
    installed (in fact, only partly, as we only use [Apache] and
    [Mod_caml] -- and thus also [Mod_caml_config]).


    You need to put in Apache configuration file (e.g. in
    /etc/apache/conf.d/mod_caml) the following lines:
    {v
    LoadModule caml_module /usr/lib/apache/1.3/mod_caml.so
    CamlLoad netstring/netsctring.cma
    CamlLoad netcgi/netcgi_mod.cma
    v}
    Of course, you need to adapt the paths of necessary.  The CamlLoad
    lines suppose that netcgi/ is a subdirectory of the default ocaml
    library location `ocamlc -where`, otherwise you need to use the
    full path.

    You need also to tell Apache how to detect whether a script is to
    be handled by netcgi_mod, either by putting them in a special
    directory (here /caml-bin/):
    {v
    Alias /caml-bin/ /path/to/your/scripts/
    <Location /caml-bin>
      SetHandler ocaml-bytecode
      CamlHandler Netcgi_mod.handler
      Options ExecCGI
      Allow from all
    </Location>
    v}
    or by distinguishing them by their extension (here .cmo):
    {v
    CamlHandler Netcgi_mod.handler
    AddHandler ocaml-bytecode .cmo
    v}
*)

