(* $Id$ *)

(** Netplex support for RPC servers (TCP only) *)

open Netplex_types

val rpc_factory :
      ?socket_config:('a -> Rpc_server.socket_config) ->
      configure:(config_file -> address -> 'a) ->
      name:string ->
      setup:(Rpc_server.t -> 'a -> unit) ->
      unit ->
         processor_factory
  (** A factory for TCP-based RPC servers. In [name] the name of the processor
    * is defined. This name can be referenced from the config file like:
    *
    * {[
    *    processor {
    *        type = "the passed [name]";
    *        ...
    *    }
    * ]}
    *
    * Currently, there is only one parameter [portmapper]. If set to true,
    * the Internet port is registered with the portmapper program.
    * This will fail if there are several Internet bindings with distinct
    * port numbers!
    *
    * Further parameters and subsections can be defined by the user. These
    * parameters should be parsed and checked for correctness by the
    * [configure] callback. The result of [configure] is also passed 
    * to [setup]. The [configure] function is called just before the service is
    * added to Netplex (i.e. from the controller context).
    *
    * The [Rpc_server.t] structure is created every time a new connection
    * is accepted. Of course, this is done from the context of the container.
    * The [Rpc_server.t] is created without any binding; the user must
    * bind procedures in the [setup] function. This can be done by calling
    * [Rpc_server.bind] or the ocamlrpcgen-generated function (by the
    * [-srv2] switch of ocamlrpcgen).
    *
    * TODO:
    * - Portmapper: Bei Start des Services _synchron_ eintragen, bei Stopp
    *   austragen.
   *)
