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
      ?late_initializer:(config_file -> controller -> unit) ->
      ?config_parser:(string -> config_file) ->
      parallelizer ->
      logger_factory list ->
      workload_manager_factory list ->
      processor_factory list -> 
      cmdline_config -> 
        unit
  (** Parses the configuration file and starts the Netplex daemon.
    * Fails with [Netplex_config.Config_error] when an error in the
    * configuration file is detected.
    *
    * The [late_initializer] is called after the Netplex controller has been
    * fully initialized, and before the main event loop is entered. You can
    * perform here further initializations, e.g. starting helper threads.
    *
    * The [config_parser] is by default [Netplex_config.read_config_file].
    * You can override it by whatever parser you would like to use.
   *)

