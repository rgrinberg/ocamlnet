(* $Id$ *)

(** Netplex toolkit *)

open Netplex_types


(** Same as [processor], but the methods [process] and [supported_ptypes]
  * are flagged as [virtual]
 *)
class type virtual v_processor =
object
  inherit processor_hooks

  method virtual process : 
           when_done:(unit -> unit) ->
           container -> Unix.file_descr -> string -> unit
  method virtual supported_ptypes : parallelization_type list
end


class empty_processor_hooks : unit -> processor_hooks
  (** This is an empty set of processor hooks, i.e. all methods are empty
   *)

class virtual processor_base :  processor_hooks -> v_processor
  (** A virtual (incomplete) base class for processors. As argument the
    * user-supplied hooks are passed in. Use this class as in:
    *
    * {[
    *    class my_processor hooks =
    *    object(self)
    *      inherit Netplex_kit.processor_base hooks
    *      method process ~when_done container fd proto_name = ...
    *      method supported_ptypes = ...
    *    end
    * ]}
    *
    * In order to run actions from hooks, redefine the hook methods as in:
    *
    * {[
    *    class my_processor hooks =
    *    object(self)
    *      inherit Netplex_kit.processor_base hooks as super
    *      method process ~when_done container fd proto_name = ...
    *      method supported_ptypes = ...
    *      method post_start_hook container =
    *        ... (* my action *);
    *        super # post_start_hook container
    *    end
    * ]}
   *)
