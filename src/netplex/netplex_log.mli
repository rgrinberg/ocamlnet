(* $Id$ *)

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]

val level_weight : level -> int


class type logger =
object
  method log : component:string -> level:level -> message:string -> unit
  method reopen : unit -> unit
  method max_level : level
  method set_max_level : level -> unit
end


val channel_logger : out_channel -> logger

val file_logger : string -> logger
  (** Writes messages to this file *)


class type multi_file_config =
object
  method log_directory : string
  method log_files :
    (string * [ level | `All ] * string) list
    (** Triples [ (component, max_level, file) ]. Use [*] as wildcard in
      * component
      *)
end

val multi_file_logger : multi_file_config -> logger

