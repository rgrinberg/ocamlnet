(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Implementation of FastCGI *)

(** This code is copyright 2003 Eric Stokes, and may be used under
either, the GNU GPL, or the same license as ocamlnet *)

open Netcgi_env
open Netcgi_types
open Netcgi

(** This function "serves". It accepts fastcgi connections, builds
  {!Netcgi_types.cgi_activation} objects from them and calls a
  function which you supply, passing the activation object as an
  argument.  It gets all the stuff that it needs to add to the
  [cgi_activation] object from the fcgiRequest structure (req). That
  structure is created when the request is read from the web server
  by fcgi_accept. The stuff that is in there (environment vars,
  stdin, etc) is pretty straitforward, if you're curious, link to the
  low level library, call fcgi_accept yourself, and print it all
  out. Yes, you can make a web app from just the low level library
  (see netcgi_fcgi_10.mli), and that is sometimes useful, for example
  when all you care about is performance. *)
val serv: ?config:cgi_config -> 
          (Netcgi_types.cgi_activation -> unit) -> operating_type -> unit

(** This function allow you to accept one connection, and get an
  environment object for it. Note that if you decide to operate this
  way, you must build an activation object, and when you are done call
  (your activation object)#output#close_out (). It you don't do this
  you will run out of file descriptors very soon *)
val get_fcgi_env : ?config:cgi_config ->
                   unit -> Netcgi_env.cgi_environment

(** This function will accept one connection and build a cgi activation
  object for you. You have the same responsibility as above, you must
  call (your activation object)#output#close_out (), otherwise you will
  run out of fds very soon. *)
val get_fcgi_activation : ?config:cgi_config -> 
                          operating_type -> Netcgi_types.cgi_activation
