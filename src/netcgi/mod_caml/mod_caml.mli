(** mod_caml *)
(*
 * Copyright (C) 2003 Merjis Ltd.
 *
 * $Id: mod_caml.mli,v 1.4 2004/12/12 23:23:55 ChriS Exp $
 *)


val register_handler : Apache.handler_t -> string -> unit
(** Modules may call [register_handler fn name] to register one or more
  * handler functions. The handler functions are then referred to in the
  * [Caml*Handler] configuration commands as [Module_name.name] where
  * [Module_name] is derived from the filename (in [CamlLoad]) and [name]
  * is the string passed here.
  *)
