(* $Id$ *)

(** Netplex support for RPC servers (TCP only) *)

open Netplex_types

val rpc_factory :
      configure:(config_file -> address -> 'a) ->
      ?socket_config:('a -> Rpc_server.socket_config) ->
      ?hooks:('a -> processor_hooks) ->
      ?supported_ptypes:parallelization_type list ->
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
    * to [setup] and other optional functions. The [configure] function is
    * called just before the service is
    * added to Netplex (i.e. from the controller context).
    *
    * The return value of [configure] can be of any type. This value
    * exists once for every time the processor is instantiated (used) by a
    * service. It is not only useful for passing configuration values
    * down, but also for storing intermediate results while establishing
    * the service.
    *
    * The [Rpc_server.t] structure is created every time a new connection
    * is accepted. Of course, this is done from the context of the container.
    * The [Rpc_server.t] is created without any binding; the user must
    * bind procedures in the [setup] function. This can be done by calling
    * [Rpc_server.bind] or the ocamlrpcgen-generated function (by the
    * [-srv2] switch of ocamlrpcgen).
    *
    * For example, a minimal factory for an RPC service may look like:
    *
    * {[
    *   let proc_ping () = ()     (* Trivial RPC procedure *)
    *
    *   let my_factory() =
    *     Rpc_netplex.rpc_factory
    *       ~name:"my_rpc_service"
    *       ~factory:(fun _ _ -> ())
    *       ~setup:(fun rpc () ->
    *                 My_rpc_srv.bind ~proc_ping rpc
    *              )
    *       ()
    * ]}
    *
    * @param socket_config An optional function computing the socket
    * configuration. By default, [Rpc_server.default_socket_config] is used.
    * Special socket configuration would, for example, allow SSL.
    *
    * @param hooks An optional function returning the hooks to call.
    * See [Netplex_types.processor_hooks] for documentation.
    *
    * @param supported_ptypes Which parallelization types are supported
    * by the service. By default, both multi-processing and multi-threading
    * are included in this list.
   *)
