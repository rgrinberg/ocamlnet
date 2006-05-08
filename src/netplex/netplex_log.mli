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


class channel_logger : out_channel -> logger
