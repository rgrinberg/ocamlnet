(* $Id$ *)

(** Virtual Execution
  *
  * The "vexec" protocol allows to invoke a routine over a socket like
  * a Unix command. One can pass an environment, a command name, and
  * command arguments. One is connected with stdin, stdout and stderr.
  *
  * "vexec" is only available over Unix domain sockets.
 *)

open Netplex_types

class type vexec_provider =
object
  method name : string
    (** Service name *)

  method exec : 
    ptype:parallelization_type ->
    stdin:Unix.file_descr -> 
    stdout:Unix.file_descr -> 
    stderr:Unix.file_descr ->
    env:string array -> 
    cmd:string array ->
      int
	(** Execute the command and return the exit code. The descriptors
          * must be closed by the command.
          *)
end


val create_vexec_service : vexec_provider -> socket_service

val connect_vexec_service :
       when_connected:(stdin:Unix.file_descr ->
			stdout:Unix.file_descr -> 
			stderr:Unix.file_descr ->
			unit) ->
       when_terminated:(int -> unit) ->
       name:string ->
       env:string array ->
       cmd:string array ->
       container ->
       unit
  (** Connect asynchronously to the vexec service [name]. When connected,
    * [when_connected] is called back. When the command terminates, the
    * function [when_terminated] is called back with the exit code.
    *
    * [when_connected] may be programmed synchronously provided ...
   *)

