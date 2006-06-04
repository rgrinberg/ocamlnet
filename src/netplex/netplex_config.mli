(* $Id$ *)

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
    * The [service] section may also contain two parameters [user] and
    * [group]. They instruct the service container to drop root privileges
    * and to become the configured user and group. Note that this is only
    * possible in multi-processing mode.
   *)
