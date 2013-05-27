(* $Id$ *)

(** Netplex message boxes *)

(** This plugin implements a message box, following a simple model:
    Receivers wait until messages are put into the box. Senders
    wait until the box is free again.

    This is actually an extension of {!Netplex_sharedvar}, and the
    same caveats apply.
 *)

open Netplex_types

val plugin : plugin
  (** To enable message boxes, call the controller's [add_plugin] method
      with this object as argument. This can e.g. be done in the
      [post_add_hook] of the processor.
   *)

(** How to use this module:

    - Encapsulate the type of the messages:
      {[ module Msg_type = struct type t = <some_type> end ]}

    - Create the box module for this type:
      {[ module Mbox_type = Netplex_mbox.Make_mbox_type(Msg_type) ]}

    - Call functions of this module, e.g.
      {[
let box = Mbox_type.create "my_box"
let msg = Mbox_type.receive box
      ]}
 *)


(** The type of mailboxes [mbox] with messages of type [t] *)
module type MBOX = sig
  type t
    (** The type of messages *)

  type mbox
    (** The type of the mailboxes *)

  val create : string -> mbox
    (** Creates a new mailbox with the passed name, or opens an existing
        mailbox. Names are global to the whole Netplex process system.
     *)

  val send : mbox -> t -> unit
    (** Send a message to this box. If the box is full, it is waited until
        the box is free again. If several senders wait for the box, one
        sender is selected.
     *)

  val receive : mbox -> t
    (** Receive a message: It is waited until a sender puts a message into
        the box. If several receivers wait for the box, one receiver is
        selected.
     *)
end

module Make_mbox_type (T:Netplex_cenv.TYPE) : MBOX with type t = T.t
  (** Create a new mailbox access module for message type [T.t] *)
