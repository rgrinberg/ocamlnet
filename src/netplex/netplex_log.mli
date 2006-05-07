(* $Id$ *)

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]

val level_weight : level -> int


class type logger =
object
  method log : component:string -> level:level -> message:string -> unit
end
