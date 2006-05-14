(* $Id$ *)

open Netplex_types

type cmdline_config

val args : unit -> ((Arg.key * Arg.spec * Arg.doc) list * cmdline_config)
  (** let (opt_list, cmdline_cfg) = args():
    * Returns [opt_list] for inclusion in the [Arg.parse] option list.
    * The effects are stored in the [cmdline_cfg] value.
   *)

val config_filename : cmdline_config -> string
  (** Returns the filename of the configuration file *)

val pidfile : cmdline_config -> string option
  (** Returns the location of the PID file (if any) *)

val foreground : cmdline_config -> bool
  (** Returns whether the daemon runs in the foreground *)

val startup : 
      parallelizer ->
      create_logger_config list ->
      create_workload_config list ->
      create_processor_config list -> 
      cmdline_config -> 
        unit
  (** Parses the configuration file and starts the Netplex daemon.
    * Fails with [Netplex_config.Config_error] when an error in the
    * configuration file is detected.
   *)

