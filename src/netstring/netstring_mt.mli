(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Initialisation code for multi-threading
 *
 * This module initializes the multi-threading mode of 
 * [Netstring]. You must link it with every application that
 * uses multi-threading.
 *
 * This module must be linked explicitly with the executable,
 * i.e. as [cmo] or [cmx]. If you use [findlib], this will be
 * done automatically.
 *)
