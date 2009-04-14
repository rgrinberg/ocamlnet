(* $Id$ *)

(** Support for pluggable resolvers *)

(** A resolver maps several kinds of names to addresses, or vice versa.
    Currently, only DNS host lookups are supported (but this can be extended
    if needed).

    The plugin mechanism allows one to change the name resovler Ocamlnet
    uses. Resolvers can be both synchronous or asynchronous. Note however,
    that the default resolver is synchronous and simply bases on
    [Unix.gethostbyname].
 *)

(** {1 Asynchronous Interface} *)

(** The following types are the same as in {!Uq_engines}, here only 
    redefined for systematic reasons
 *)

type 't engine_state =
  [ `Working of int
  | `Done of 't
  | `Error of exn
  | `Aborted
  ]

class type [ 't ] engine = object
  method state : 't engine_state
  method abort : unit -> unit
  method request_notification : (unit -> bool) -> unit
  method event_system : Unixqueue.event_system
end

(** Exceptions *)

exception Host_not_found of string
  (** This host cannot be resolved *)

(** The type of resolvers: *)

class type resolver =
object
  method host_by_name : 
           string -> Unixqueue.event_system -> Unix.host_entry engine
    (** Look up the passed host name up. The implementation can be synchronous
        or asynchronous. In the first case, an engine is returned that is
        already in one of the states [`Done he] or [`Error e] where [he] is
        the host entry, or [e] is an exception like [Host_not_found]. In this
        case the passed event system is ignored. If the implementation is
        asynchronous, the caller must run the event system until the 
        state of the engine transitions to [`Done he] or [`Error e].
     *)
end

(** {1 Synchronous Interface} *)

val get_host_by_name : ?resolver:resolver -> string -> Unix.host_entry
  (** Look up the host, and return the host entry
      or raise the exception [Host_not_found].

      If a [resolver] is passed, this resolver is used, otherwise the
      pluggable resolver is used.
   *)


(** {1 Plugins} *)

val current_resolver : unit -> resolver
  (** Returns the pluggable resolver *)

val set_current_resolver : resolver -> unit
  (** Set the pluggable resolver *)
