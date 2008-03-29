(* $Id$ *)

(** Loggers *)

open Netplex_types

val level_weight : level -> int

val level_of_string : string -> level

val channel_logger : ?fmt:string -> out_channel -> logger
  (** Outputs messages to the channel. [fmt] is the format string (see
      below)
   *)

val stderr_logger_factory : logger_factory
  (** Reads a logging section like
    *
    * {[ logging {
    *       type = "stderr";
    *       format = "format string";
    *    }
    * ]}
    *
    * The format string is optional. The format string may include variable
    * parts in the syntax [$name] or [${name}]. The following variable
    * specifications are defined:
    * - [timestamp]: the time in standard format
    * - [timestamp:<format>] the time in custom format where [<format>] is a
    *   {!Netdate} format string
    * - [timestamp:unix]: the time in seconds since the epoch
    * - [component]: the name of the component emitting the log message
    * - [subchannel]: the name of the subchannel
    * - [level]: the log level
    * - [message]: the log message
   *)


val file_logger : ?fmt:string -> string -> logger
  (** Writes messages to this file *)

val file_logger_factory : logger_factory
  (** Reads a logging section like
    *
    * {[ logging {
    *       type = "file";
    *       file = "/path/to/logfile";
    *       format = "format string";
    *    }
    * ]}
   *)

class type multi_file_config =
object
  method log_directory : string
  method log_files :
    (string * string * [ level | `All ] * string * string) list
    (** Triples [ (component, subchannel, max_level, file, format) ]. Use [*] as 
        wildcard in [component] and [subchannel].

        Currently, [`All] is a synonym for the [`Debug] level.
      *)
end

val multi_file_logger : multi_file_config -> logger

val multi_file_logger_factory : logger_factory
  (**  Reads a logging section like
    *
    * {[ logging {
    *       type = "multi_file";
    *       directory = "/path/to/logdir";
    *       format = "format string";
    *       file {
    *           component = "name_of_component";
    *           subchannel = "name_of_subchannel";
    *           max_level = "max_level";
    *           file = "logfile";
    *           format = "format string";
    *       };
    *       file { ... }; ...
    *    }
    * ]}
    *
    * [format] is optional.
    *
    * If [component] is missing it will default to "*". If [subchannel]
    * is missing it defaults to "*", too. If [max_level]
    * is omitted it is assumed to be "all".
   *)

val logger_factories :  logger_factory list
  (** All built-in logger factories *)


val debug_scheduling : bool ref
  (** If set to true, the controller and a few other components output
    * a lot of debug messages, mostly scheduling-related.
   *)

val debug_containers : bool ref
  (** If set to true, the containers output when they are started and
    * stopped, and when new connections are accepted.
   *)
