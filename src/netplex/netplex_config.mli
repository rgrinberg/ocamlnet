(* $Id$ *)

(** Read the configuration file *)

open Netplex_types

exception Config_error of string

val read_config_file : string -> config_file

val read_netplex_config : 
      parallelization_type ->
      logger_factory list ->
      workload_manager_factory list ->
      processor_factory list -> 
      config_file ->
        netplex_config
  (** Reads a configuration file like:
    *
    * {[ netplex {
    *      controller {
    *          ...
    *          logging {
    *          ...
    *          };
    *      };
    *      service {
    *          name = "name_of_service";
    *          protocol {
    *              name = "name_of_protocol";
    *              lstn_backlog = <n>;
    *              lstn_reuseaddr = <bool>;
    *              so_keepalive = <bool>;
    *              address {
    *                  type = "local";
    *                  path = "socketname";
    *              };
    *              address {
    *                  type = "internet";
    *                  bind = "bind_address:port";
    *              };
    *              ...
    *          };
    *          processor {
    *              type = "type_of_processor";
    *              ...
    *          };
    *          workload_manager {
    *              type = "type_of_manager";
    *              ...
    *          };
    *      }
    *   }
    * ]}
    *
    * The [controller] section is explained in {!Netplex_controller}.
    * 
    * The [logging] section is explained in {!Netplex_log}.
    *
    * The [service] section may also contain two parameters [user] and
    * [group]. They instruct the service container to drop root privileges
    * and to become the configured user and group. Note that this is only
    * possible in multi-processing mode.
    *
    * Address types:
    * - [local]: This is a OS-dependent default IPC mechanism for local
    *   connections. On Unix, [local] means Unix Domain sockets. On Win32,
    *   [local] means the [w32_pipe_file] mechanism. In [path], the name
    *   of the Unix Domain socket or the file with the pipe name must be
    *   specified.
    * - [internet]: Internet socket. In [bind] the bind address and the
    *   bind port are given.
    * - [unixdomain]: Unix domain sockets. In [path] give the path.
    * - [socket_file]: An emulation of Unix Domain sockets: A server socket
    *   bound to [127.0.0.1] and an anonymous port is used instead. The
    *   port number is written to a file. The file must be given as [path].
    * - [w32_pipe]: Win32 named pipes. The name of the pipe is given
    *   as [path].
    * - [w32_pipe_file]: An emulation of Unix Domain sockets: A named
    *   pipe with an unpredictable random name is created instead. The
    *   name of this pipe is written to the file given by [path]
    * - [container]: this special address causes that a separate [local]
    *   socket is created for each started container. The name of the
    *   socket file is automatically chosen. The names of the socket
    *   files can be queried with {!Netplex_cenv.lookup_container_sockets}.
    *   This type of socket is useful to control the load sent to each
    *   container directly, e.g. to drive a farm of worker processes.
    * 
   *)
