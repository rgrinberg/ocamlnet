(* $Id$ *)

open Netplex_types

val level_weight : level -> int

val level_of_string : string -> level

val channel_logger : out_channel -> logger

val create_stderr_logger_config : create_logger_config
  (** Reads a logging section like
    *
    * {[ logging {
    *       type = "stderr";
    *    }
    * ]}
   *)


val file_logger : string -> logger
  (** Writes messages to this file *)

val create_file_logger_config : create_logger_config
  (** Reads a logging section like
    *
    * {[ logging {
    *       type = "file";
    *       file = "/path/to/logfile";
    *    }
    * ]}
   *)

class type multi_file_config =
object
  method log_directory : string
  method log_files :
    (string * [ level | `All ] * string) list
    (** Triples [ (component, max_level, file) ]. Use [*] as wildcard in
      * component.
      *
      * Currently, [`All] is a synonym for [`Debug].
      *)
end

val multi_file_logger : multi_file_config -> logger

val create_multi_file_logger_config : create_logger_config
  (**  Reads a logging section like
    *
    * {[ logging {
    *       type = "multi_file";
    *       directory = "/path/to/logdir";
    *       file {
    *           component = "name_of_component";
    *           max_level = "max_level";
    *           file = "logfile";
    *       };
    *       file { ... }; ...
    *    }
    * ]}
    *
    * If [component] is missing it will default to "*". If [max_level]
    * is omitted it is assumed to be "all".
   *)

