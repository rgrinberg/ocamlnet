(* $Id$ *)

(** Basic logging facility

    The purpose of this module is to define a mutable logging function
    which ensures to be always pointing to a valid logging implementation.
    By default, the function prints the messages to stderr, prepended
    by a timestamp. In Netplex context, the implementation is changed so
    that the Netplex logger is used instead (see {!Netplex_log}), and
    the [log] and [logf] functions below have the same effect as
    the counterparts in {!Netplex_cenv} with the same name.
 *)

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]
  (** The log levels (syslog-like) *)

type logger =
    level -> string -> unit
  (** A logging function takes a level, and the message *)

val channel_logger : out_channel -> level -> logger
  (** [channel_logger ch lev]: Prints all messages with a level of [lev]
      and higher to the channel [ch]. The output buffer is flushed after
      each message,
   *)

val current_logger : logger ref
  (** The variable containing the current logger function.
      Defaults to [channel_logger stderr `Debug] at program startup.
      Assign something else to this variable in order to change the
      log destination or the log level.

      Note that the function {!Netplex_main.run} modifies [current_logger]
      so log messages are forwarded to the Netplex logger. It is then the
      Netplex framework that determines log destination and level.
   *)

val log : level -> string -> unit
  (** Writes a log message *)

val logf : level -> ('a, unit, string, unit) format4 -> 'a
  (** Writes a log message like [printf] *)

val level_weight : level -> int
  (** A number correspondig to the level *)

val level_of_string : string -> level
val string_of_level : level -> string
  (** Convert level names to strings and vice versa *)

val level_names : string array
  (** The level names indexed by weight *)


module Debug : sig
  (** Controlling messages for debugging Ocamlnet *)
  
  (** Many Ocamlnet modules can emit debug messages. For a module
      [M] (e.g. {!Rpc_client}) there is usually a sub module 
      [M.Debug] providing functions controlling the debug messages.
      These functions send the messages to this [Debug] module, and from here
      they are forwarded to {!Netlog.log} (with a level of [`Debug]).

      In this module everything is by default initialized to enable
      debug messages (i.e., [current_dlogger] is set to [fwd_dlogger],
      and this function sends the messages to [current_logger] where they are
      printed by [channel_logger stderr `Debug]). However, the debugged
      modules also need to be enabled individually. One can do this
      by setting the variable [M.Debug.enable] to [true], or by calling
      [enable_module] or [enable_all] (see below).

      Although this debug interface is mainly intended for Ocamlnet
      itself, it is not restricted to this. In order to use it for
      a user module, one has to register the boolean variable that
      controls whether debug messages are enabled (see [register_module]
      below). This should be done at module initialization time, and
      {b before} any thread is spawned.
   *)

  type dlogger =
      string -> string -> unit
    (** Debug logger: The first string is the module name, and the second 
        is the message
     *)

  val fwd_dlogger : dlogger
    (** The standard debug logger simply prepends the module name to the
        message (separated by ": "), and calls the current logger to
        print it
     *)

  val null_dlogger : dlogger
    (** The "logger" not printing anything *)

  val current_dlogger : dlogger ref
    (** The current debug logger. This is initialized to [fwd_dlogger] at
        program startup. Set this variable to [null_logger] to completely
        turn off debug logging of Ocamlnet.
     *)

  val log : string -> string -> unit
    (** Writes a log message *)

  val logf : string -> ('a, unit, string, unit) format4 -> 'a
    (** Writes a log message like [printf] *)

  val enable_module : string -> unit 
  val disable_module : string -> unit 
    (** Enable or disable the passed module [M]
        (e.g. [enable_module "Rpc_client"]). No exception is raised when
        the module name is invalid! Because of this, it is usually safer
        to change the variable [M.Debug.enable] directly (e.g.
        [Rpc_client.Debug.enable := true]).
     *)

  val enable_all : unit -> unit
    (** Enable all modules *)

  val disable_all : unit -> unit
    (** Disable all modules (the default) *)

  val names : unit -> string list
    (** Return the possible module names for [enable_module] and
        [disable_module]
     *)

  (** {2 For debugged Ocamlnet modules} *)

  val register_module : string -> bool ref -> unit
    (** Registers the [enable] variable of the named module *)

  val mk_dlog : string -> bool ref -> (string -> unit)
    (** [let dlog = mk_dlog "M" enable]: The conditional debug function *)

  val mk_dlogr : string -> bool ref -> ((unit -> string) -> unit)
    (** [let dlogr = mk_dlog "M" enable]: The conditional debug function *)

end
