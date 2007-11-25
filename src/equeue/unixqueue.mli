(* 
 * $Id$
 *)

open Unix;;
open Sys;;

(**********************************************************************)
(*                          System events                             *)
(**********************************************************************)

(* written by Gerd Stolpmann *)

(** This module generalizes the [Unix.select] function. The idea is to have
 * an event queue (implemented by {!Equeue}) that manages all file events that
 * can be watched by a [Unix.select] call. As {b event} is considered when there
 * is something to do for a file descriptor (reading, writing, accepting 
 * out-of-band data), but also the condition that for a certain period 
 * of time ("timeout") nothing
 * has happened. Furthermore, a signal is also considered as an event,
 * and it is also possible to have user-generated extra events.
 *
 * These events are queued up, and they are presented to event handlers
 * that may process them.
 *
 * You can describe what types of event conditions are watched by adding
 * "resources". You can think a resource being a condition (bound to
 * a real resource of the operating system) for which 
 * events are generated if the condition becomes true.
 *)

(** {b THREAD-SAFETY}
 * 
 * Since release 1.2 of Equeue, this module serializes automatically.
 * You can call functions for the same event system from different
 * threads. This requires some special initialization, see {!Unixqueue_mt}.
 *
 * Note that the underlying {!Equeue} module is reentrant, but not
 * serializing. (It is not recommended (and not necessary) to call
 * functions of the Equeue module directly in multi-threaded programs.)
 *
 * The TCL extension is not thread-safe.
 *)

type group = Unixqueue_util.group
  (** A group is an abstract tag for a set of events, resources, and
   * event handlers. Usually every event handler creates a new group,
   * and all events and resources processed by the handler are 
   * members of this group.
   *)

exception Abort of (group * exn);;
  (** Event handlers can raise this exception to cancel a group 
   * of handlers, events, and resources. If an abort action
   * is defined for the group, it will be executed. Next, all members
   * of the group are removed from the event system. 
   *
   * First argument is the group. The second argument
   * is an arbitrary exception (must not be [Abort] again) which is
   * passed to the abort action.
   *)


type wait_id = Unixqueue_util.wait_id
  (** A wait identifier is used to distinguish between several
   * timers, see type [operation].
   *)


type operation = Unixqueue_util.operation =
    Wait_in  of file_descr          (** wait for input data *)
  | Wait_out of file_descr          (** wait until output can be written *)
  | Wait_oob of file_descr          (** wait for out-of-band data *)
  | Wait of wait_id                 (** wait only for timeout *)
  (** An [operation] specifies the condition to wait for. Every kind
   * of operation may have an associated timer (not only [Wait]).
   *)


type event = Unixqueue_util.event =
    Input_arrived of    (group * file_descr)  (** Input data has arrived *)
  | Output_readiness of (group * file_descr)  (** Output is possible now *)
  | Out_of_band of      (group * file_descr)  (** OOB data has arrived *)
  | Timeout of          (group * operation)   (** A timer has expired *)
  | Signal                                    (** A signal has happened *)
  | Extra of exn                              (** User-generated event *)
  (** An [event] is triggered when the condition of an [operation]
   * becomes true, when a signal happens, or when the event is
   * (artificially) added to the event queue ([add_event], below).
   * The events resulting from an [operation] carry the group of
   * the resource with them. 
   *
   * The event [Signal] is triggered when the [EINTR] condition is
   * caught; this normally means that a signal has just been delivered.
   * The generation of [Signal] events should be considered as
   * unreliable, not every signal delivery can be detected. Reasons for
   * the unrealiability are that user-supplied code happens to
   * get the [EINTR] condition and not the [Unixqueue] event loop,
   * and that there are known race conditions in the O'Caml signal
   * handling routines that may cause signals to be lost. However,
   * it can be expected that almost all signals will trigger [Signal].
   *
   * The event [Extra] can only be artificially added to the queue,
   * and the argument of [Extra] is an exception value that distinguishes
   * between several kinds of user-generated events.
   *)

class type event_system =
object
  (* Public interface *)
  method new_group : unit -> group
  method new_wait_id : unit -> wait_id
  method exists_resource : operation -> bool
  method add_resource : group -> (operation * float) -> unit
  method add_close_action : group -> (file_descr * (file_descr -> unit)) -> unit
  method add_abort_action : group -> (group -> exn -> unit) -> unit
  method remove_resource : group -> operation -> unit
  method add_handler : group -> (event_system -> event Equeue.t -> event -> unit) -> unit
  method add_event : event -> unit
  method clear : group -> unit
  method run : unit -> unit
  method is_running : bool
  method once : group -> float -> (unit -> unit) -> unit
  method exn_log : ?suppressed:bool -> ?to_string:(exn -> string) -> ?label:string -> exn -> unit
  method debug_log : ?label:string -> string -> unit
  (* Protected interface *)
  method private setup : unit -> (file_descr list * file_descr list * file_descr list * float)
  method private queue_events : (file_descr list * file_descr list * file_descr list) -> bool
  method private source : event Equeue.t -> unit
end
  (** The [event_system] manages events, handlers, resources, groups,
   * etc. It is now a class type, and you may invoke the operations directly
   * for the class. The operations are still available as functions (below).
   *
   * A {b resource} is an operation with an optional timer. The operation
   * describes the condition to watch for, and the timer defines the
   * maximum period of time for that. If the condition becomes true,
   * an [Input_arrived], [Output_readiness], or [Out_of_band] event
   * will be triggered. If the timer expires, a [Timeout] event will be
   * generated. After the event the resource remains active, and the
   * timeout period begins anew.
   *
   * A resource is usually bound to a file descriptor. It is allowed
   * to watch the same descriptor for several different conditions,
   * but it is forbidden to watch the same descriptor for the same kind
   * of condition several times.
   *
   * As a special case, the operation [Wait] is not bound to a
   * file descriptor, but simply starts a timer. The argument of [Wait]
   * can be used to distinguish between several timers that are active
   * at the same time.
   *
   * {b Event handlers} get the events one after the other, and 
   * process them. When a handler is called for an event, there are
   * several possible reactions: (1) The handler can return normally,
   * which means that the event has been accepted, and will not be
   * passed to any other handler. (2) The handler can raise
   * {!Equeue.Reject}, which means that the handler cannot process
   * the event, and that another handler should get it. (3) The handler
   * can raise {!Equeue.Terminate} which means that the event has been
   * accepted, and that the handler is terminated (it will never be
   * called again). (4) The handler can raise [Abort] which means that
   * the event is deferred, and that a special abort mechanism is
   * triggered (see the description for [Abort] above), this is also
   * terminates the handler. The deferred event will again be processed
   * in the future. (5) The handler can raise any other exception.
   * This causes that the event is deferred, and the exception falls
   * through to the caller of [run].
   *
   * {b Groups} are used to simplify the association of events to
   * handlers, and to simplify the termination of handlers (see [clear]).
   * If an event is associated with a group, only handlers associated with
   * the same group will get them.
   *
   * There is a special {b Close handler} which is useful to close file
   * descriptors no longer needed. It is called when all resources are
   * removed from the event system dealing with the file descriptor. 
   * The close handler should close the descriptor. Note that close handlers
   * are only useful under certain circumstances.
   * 
   *)

class select_event_system : unit -> event_system
  (** The standalone, select-based implementation of an event system *)

class unix_event_system : unit -> event_system
  (** {b Deprecated.} Compatibility name for [select_event_system] *)

val select_event_system : unit -> event_system
  (** Create a new, empty, select-based event system *)

val create_unix_event_system : unit -> event_system
  (** Create a new, empty event system using the configurable
      factory. {b Use this function to create the [event_system]
      if you don't have special wishes about its kind.}
   *)

val set_event_system_factory : (unit -> event_system) -> unit
  (** Sets the factory function for [create_unix_event_system] *)

val new_group : event_system -> group
  (** Create a new, empty group for the event system *)


val new_wait_id : event_system -> wait_id
  (** Create a new unique wait identifier *)


val exists_resource : event_system -> operation  -> bool
  (** Find out if a specific resource already exists (or better: is
   * already watched by an operation).
   *)


val add_resource : event_system -> group -> (operation * float) -> unit
  (** Add a resource such that it is watched for conditions described
   * by the [operation] for the period given by the [float] number.
   * A negative number means that the resource is watched for an infinite
   * period. The resource becomes a member of the [group].
   *
   * You cannot add the same operation several times;
   * if you try it the second operation is silently dropped.
   *
   * The resource remains even if it has generated an event. The timeout
   * period starts again in this case.
   *)


val add_close_action : 
  event_system -> group -> (file_descr * (file_descr -> unit)) 
    -> unit
  (** A close action is added for the file descriptor. The action callback
   * (which gets the descriptor as argument) is called when there is not
   * any watched resource remaining for this descriptor.
   *
   * This may be useful if the descriptor can be closed in this case.
   *
   * The close action becomes member of the passed [group]. The only
   * effect of this is that the action is removed when the [clear] function
   * is called.
   *
   * You can only add (set) one close action for every descriptor.
   *)


val add_abort_action : 
  event_system -> group -> (group -> exn -> unit)
    -> unit
  (** An abort action is added to the group. The action callback is
   * called when an arbitrary handler raises [Abort(g,exn)] where
   * [g] is the group the abort action is member of. In this case,
   * the callback function is invoked with the group and [exn] as
   * arguments. After that, the group is cleared.
   *
   * You can only add (set) one abort action for every group.
   *)


val remove_resource : event_system -> group -> operation -> unit
  (** Removes the operation from the watch list of the group.
   * It is an error if the operation is member of another group.
   * If the operation cannot be found at all, the exception [Not_found]
   * will be raised.
   *
   * The removal of resources may trigger close actions.
   *)


val add_handler : 
  event_system -> group -> (event_system -> event Equeue.t -> event -> unit) 
    -> unit
  (** Add an event handler that is associated to the given group. There
   * may be several handlers for a group.
   * 
   * The handler callback function is invoked when there is an event
   * that could be processeable by the handler. As outlined above, the
   * callback function can accept or reject the event, it can terminate
   * itself, and it can abort the whole group.
   *)


val add_event : event_system -> event -> unit
  (** Add an additional event. The event will be processed after the 
   * current list of events is done.
   *)


val clear : event_system -> group -> unit
  (** Terminate the whole group. This means that the handlers of the
   * group are not called any longer, and that all resources and actions
   * are removed. It is possible that there are pending events after
   * termination, but these will be usually be dropped because there is
   * no handler for them.
   *
   * When a group is terminated, it is not allowed to refer to the
   * group any longer. Functions will raise [Invalid_argument] if this
   * is tried nevertheless.
   *)


val run : event_system -> unit
  (** Starts the event loop. This means that the resources are watched,
   * and that events are generated, and that handlers are called.
   *
   * The event loop returns normally when there are not any resources
   * and not any events in the queue. The loop raises
   * {!Equeue.Out_of_handlers} if there are resources but no handlers
   * to process their events. It is possible that exceptions raised
   * from handlers fall through to the [run] call.
   *
   * After the exception is caught and processed, the event loop
   * can be restarted.
   *)

val is_running : event_system -> bool
  (** Whether the event loop is running *)

val once : event_system -> group -> float -> (unit -> unit)
  -> unit
  (** Arranges that the callback function is called once after the 
   * passed period of time (the [float] argument) has elapsed.
   *
   * The arrangement is member of the passed group. By clearing the
   * group, the timer is deleted, too.
   *)

val exn_log : event_system ->
              ?suppressed:bool -> ?to_string:(exn -> string) -> 
              ?label:string -> exn -> unit
  (** Exceptions log: In event-based programming, it is sometimes not
   * possible to handle exceptions appropriately. It is also bad not
   * to handle them at all. For these cases, the exceptions log might
   * be an alternative: Instead of letting exceptions fall through to
   * the caller in an uncoordinated way, it is better to catch them
   * at the right moment, and to log them for further analysis.
   *
   * By default, [exn_log] does nothing. In debug mode, however, the
   * exceptions are reported as part of the debug log.
   *
   * {b Example:} A typical candidate for the exceptions log are
   * cleanup actions within exception handlers, e.g.
   *
   * {[try ...
   * with Processing_error ->
   *        try
   *          cleanup();   (* e.g. close file descriptors *)
   *          ...
   *        with nested_error ->
   *           Unixqueue.exn_log ~suppressed:true nested_error
   * ]}
   *
   * This is especially useful when the processing error is likely
   * to cause follow-up errors in the cleanup action. For normal
   * operation, one can ignore such errors, but for debugging it is
   * very useful to know that these exceptions happen.
   *
   * @param suppressed This flag indicates that the exception is not
   *   re-raised after logging. Just a hint for debugging. Default
   *   is [false].
   * @param to_string This function is called to convert the exception
   *   into a printable string. Default is [Printexc.to_string].
   * @param label The label is included in the log output. This is
   *   useful to describe where the log message is generated.
   *)

val debug_log : event_system -> ?label:string -> string -> unit
  (** Outputs a message in the debug log (when enabled).
   *
   * @param label The label is included in the log output. This is
   *   useful to describe where the log message is generated.
   *)

val set_debug_mode : bool -> unit
  (** Whether to output debug messages. Output goes to stderr.
   * Setting the debug mode implies setting Equeue's debug mode.
   *
   * The debug messages may really help debugging event systems. 
   * Unfortunately, some understanding of the internal processing
   * is required to interpret debug protocols.
   *)

(* val attach_to_tcl_queue : event_system -> (event_system -> unit) -> unit
 *
 * This is no longer supported here. Look into Uq_tcl for the modern
 * way to integrate with the tcl/tk event system
 *)

(**/**)

val init_mt : (unit -> ((unit -> unit) * (unit -> unit))) -> unit
(* A private function to initialize multi-threading. *)

