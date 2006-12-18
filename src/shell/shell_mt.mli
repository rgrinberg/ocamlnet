(* $Id: shell_mt.mli 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Multi-threading support *)

(** In multi-threaded applications, make sure that Shell_mt is
 * linked into the final application. Do to so, you _must_
 * mention shell_mt.cmo or shell_mt.cmx on the command line,
 * or use ocamlfind -thread.
 *)
