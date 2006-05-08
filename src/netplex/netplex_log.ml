(* $Id$ *)

open Printf

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]

let level_weight =
  function
    | `Emerg   -> 0
    | `Alert   -> 1
    | `Crit    -> 2
    | `Err     -> 3
    | `Warning -> 4
    | `Notice  -> 5
    | `Info    -> 6
    | `Debug   -> 7

let level_names =
  [| "emerg"; "alert"; "crit"; "err"; "warning"; "notice"; "info"; "debug" |]

class type logger =
object
  method log : component:string -> level:level -> message:string -> unit
  method reopen : unit -> unit
  method max_level : level
  method set_max_level : level -> unit
end


class channel_logger out : logger =
object(self)
  val mutable max_level = `Info

  method log ~component ~level ~message =
    let w = level_weight level in
    if w <= level_weight max_level then (
      fprintf out "[%s] [%s] [%s] %s\n%!"
	(Netdate.format "%c" (Netdate.create
				~zone:(Netdate.localzone)
				(Unix.time())))
	component
	level_names.(w)
	message
    )


  method reopen() = ()

  method max_level = max_level

  method set_max_level l = max_level <- l

end
