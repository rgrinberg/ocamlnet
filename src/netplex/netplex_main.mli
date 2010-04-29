(* $Id$ *)

(** Main program for Netplex servers *)

open Netplex_types

type cmdline_config

val args : 
       ?defaults:cmdline_config ->
       unit -> ((Arg.key * Arg.spec * Arg.doc) list * cmdline_config)
  (** [let (opt_list, cmdline_cfg) = args()]:
    * Returns [opt_list] for inclusion in the [Arg.parse] option list.
    * The effects made available by the returned [cmdline_cfg] value.
    * 
    * @param defaults The default argument values
   *)

val create : ?config_filename:string ->
             ?pidfile:string option ->
             ?foreground:bool ->
             unit -> cmdline_config
  (** Creates the command-line configuration object *)

val modify : ?config_filename:string ->
             ?pidfile:string option ->
             ?foreground:bool ->
             cmdline_config -> cmdline_config
  (** Modifies the command-line configuration object *)

val config_filename : cmdline_config -> string
  (** Returns the filename of the configuration file, or the default
      if it has not been set on the command-line
   *)

val config_filename_opt : cmdline_config -> string option
  (** Returns the filename of the configuration file, or [None] if it
      has not been set on the command-line
   *)

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
    *
    * As side-effect, the current logger of the {!Netlog} module is set
    * to selected Netplex logger. Note that this setting remains active
    * even after [startup] returns to the caller. Also note that log messages
    * submitted via {!Netlog} appear as from component "netplex.other"
    * if they are sent from outside of a container.
   *)


(** {2 Tutorial}
  *
  * The typical main program for a [Netplex] server system looks like:
  *
  * {[ 
  *   let my_factories = ...
  *
  *   let start() =
  *    let opts, cmdconf = Netplex_main.args() in
  *    Arg.parse 
  *      opts
  *      (fun s -> raise(Arg.Bad ("Unknown arg: " ^ s))) 
  *      "usage: protoserver";
  *    let par = Netplex_mp.mp() in  (* or Netplex_mt.mt() *)
  *    Netplex_main.startup
  *      par
  *      Netplex_log.logger_factories
  *      Netplex_workload.workload_manager_factories
  *      my_factories
  *      cmdconf
  *
  *   Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
  *   start()
  * ]}
  *
  * This main program enables:
  * - The standard command-line arguments [-conf], [-pid] and [-fg] are 
  *   understood
  * - The configuration file is parsed
  * - The configuration can refer to all loggers and workload managers
  *   coming with Netplex
  * - The parallelizer is selected: Here, it is multi-processing
  *   (Netplex_mp). One could also select multi-threading (Netplex_mt)
  * - The processors defined by [my_factories] are made available
  *   for connection processing
 *)
