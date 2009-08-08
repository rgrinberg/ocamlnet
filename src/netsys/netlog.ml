(* $Id$ *)

open Printf

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]

type logger =
    level -> string -> unit

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

let string_of_level lev =
  level_names.( level_weight lev )


let level_of_string s =
  let s = String.lowercase s in
  match s with
    | "emerg"   -> `Emerg
    | "alert"   -> `Alert
    | "crit"    -> `Crit
    | "err"     -> `Err
    | "warning" -> `Warning
    | "notice"  -> `Notice
    | "info"    -> `Info
    | "debug"   -> `Debug
    | _         -> failwith ("Unknown level: " ^ s)

let weekday =
  [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]

let month =
  [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun";
     "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let channel_logger ch max_lev lev msg = 
  if level_weight lev <= level_weight max_lev then (
    let t = Unix.localtime(Unix.gettimeofday()) in
    let s =   (* Netdate is unavailable here *)
      sprintf
	"[%s %s %2d %02d:%02d:%02d %4d] [%s] %s\n"
	weekday.(t.Unix.tm_wday)
	month.(t.Unix.tm_mon)
	t.Unix.tm_mday
	t.Unix.tm_hour
	t.Unix.tm_min
	t.Unix.tm_sec
	(1900 + t.Unix.tm_year)
	(string_of_level lev)
	msg in
    output_string ch s;
    flush ch
  )
    

let current_logger =
  ref(channel_logger Pervasives.stderr `Debug)


let log lev msg =
  !current_logger lev msg

let logf level fmt =
  Printf.ksprintf (log level) fmt

module Debug = struct
  type dlogger =
      string -> string -> unit

  let fwd_dlogger mname msg =
    log `Debug (mname ^ ": " ^ msg)

  let null_dlogger _ _ = ()

  let current_dlogger =
    ref fwd_dlogger

  let log mname msg =
    !current_dlogger mname msg

  let logf mname fmt =
    Printf.ksprintf (log mname) fmt

  let registry = Hashtbl.create 11

  let register_module mname evar =
    Hashtbl.replace registry mname evar

  let set_module mname b =
    try
      let evar = Hashtbl.find registry mname in
      evar := b
    with Not_found -> ()

  let set_all_modules b =
    Hashtbl.iter
      (fun _ evar -> evar := b)
      registry

  let enable_module mname =
    set_module mname true

  let disable_module mname =
    set_module mname false

  let enable_all () =
    set_all_modules true

  let disable_all () =
    set_all_modules false

  let names() =
    List.sort
      compare
      (Hashtbl.fold (fun name _ acc -> name::acc) registry [])

  let mk_dlog mname enable msg =
    if !enable then
      log mname msg

  let mk_dlogr mname enable f =
    if !enable then
      log mname (f())
end
