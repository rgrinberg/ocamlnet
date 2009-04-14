(* $Id$ *)

(** Definitions common to {!Unixqueue and {!Unixqueue2} *)

(** These are internals of Ocamlnet! *)

open Printf


(* [group] and [wait_id] are now objects. The structural equality
 * ( = ) compares object IDs if applied to objects, so that this
 * is exactly what we need. It is no longer necessary to manage
 * the IDs ourselves, because the language already manages object IDs.
 *
 * This has also the advantage that groups can now have additional
 * properties.
 *)

class group_object =
object
  val mutable terminating = false
      (* Whether the group is terminating *)
  method is_terminating = terminating
  method terminate() = terminating <- true
end

type group = group_object

class wait_object =
object
end

type wait_id = wait_object

type operation =
    Wait_in  of Unix.file_descr
  | Wait_out of Unix.file_descr
  | Wait_oob of Unix.file_descr
  | Wait of wait_id

type event =
    Input_arrived of (group * Unix.file_descr)
  | Output_readiness of (group * Unix.file_descr)
  | Out_of_band of (group * Unix.file_descr)
  | Timeout of (group * operation)
  | Signal
  | Extra of exn

type resource_prop =
    group * float * float
    (* group, timeout value, time of last event *)


type event_system_t = 
    < new_group : unit -> group;
      new_wait_id : unit -> wait_id;
      exists_resource : operation -> bool;
      add_resource : group -> (operation * float) -> unit;
      add_close_action : group -> (Unix.file_descr * (Unix.file_descr -> unit)) -> unit;
      add_abort_action : group -> (group -> exn -> unit) -> unit;
      remove_resource : group -> operation -> unit;
      add_handler : group -> (event_system_t -> event Equeue.t -> event -> unit) -> unit;
      add_event : event -> unit;
      clear : group -> unit;
      run : unit -> unit;
      is_running : bool;
      once : group -> float -> (unit -> unit) -> unit;
      exn_log : ?suppressed:bool -> ?to_string:(exn -> string) -> ?label:string -> exn -> unit;
      debug_log : ?label:string -> string -> unit
    >

class type event_system =
object
  method new_group : unit -> group
  method new_wait_id : unit -> wait_id
  method exists_resource : operation -> bool
  method add_resource : group -> (operation * float) -> unit
  method add_close_action : group -> (Unix.file_descr * (Unix.file_descr -> unit)) -> unit
  method add_abort_action : group -> (group -> exn -> unit) -> unit
  method remove_resource : group -> operation -> unit
  method add_handler : group -> (event_system_t -> event Equeue.t -> event -> unit) -> unit
  method add_event : event -> unit
  method clear : group -> unit
  method run : unit -> unit
  method is_running : bool
  method once : group -> float -> (unit -> unit) -> unit
  method exn_log : ?suppressed:bool -> ?to_string:(exn -> string) -> ?label:string -> exn -> unit
  method debug_log : ?label:string -> string -> unit
end



type handler =
    event_system_t -> event Equeue.t -> event -> unit


exception Abort of (group * exn)


let () =
  Netexn.register_printer
    (Abort(new group_object, Not_found))
    (fun e ->
       match e with
	 | Abort(g,e') ->
	     "Unixqueue.Abort(" ^ string_of_int(Oo.id g) ^ 
	       ", " ^ Netexn.to_string e' ^ ")"
	 | _ -> assert false
    )


let string_of_fd fd =
  Int64.to_string (Netsys.int64_of_file_descr fd)
;;


let string_of_op =
  function
      Wait_in fd   -> sprintf "Wait_in(%s)" (string_of_fd fd)
    | Wait_out fd  -> sprintf "Wait_out(%s)" (string_of_fd fd)
    | Wait_oob fd  -> sprintf "Wait_oob(%s)" (string_of_fd fd)
    | Wait id      -> sprintf "Wait(wait_id %d)" (Oo.id id)
;;


let string_of_event ev =
  match ev with
    Input_arrived (g,fd) ->
      sprintf "Input(group %d, fd %s)" (Oo.id g) (string_of_fd fd)
  | Output_readiness (g, fd) ->
      sprintf "Output(group %d, fd %s)" (Oo.id g) (string_of_fd fd)
  | Out_of_band (g, fd) ->
      sprintf "Out_of_band(group %d, fd %s)" (Oo.id g) (string_of_fd fd)
  | Timeout (g, op) ->
      sprintf "Timeout(group %d, %s)" (Oo.id g) (string_of_op op)
  | Signal ->
      "Signal"
  | Extra x ->
      sprintf "Extra(%s)" (Netexn.to_string x)
;;


let debug_mode = ref false;;

let debug_print s =
  if !debug_mode then
    prerr_endline("Unixqueue debug msg: " ^ Lazy.force s)
;;


let set_debug_mode b =
  debug_mode := b;
  Equeue.set_debug_mode b
;;

