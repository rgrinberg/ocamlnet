(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


(* Minimal support for TI-RPC over the XTI API
 *
 * This library has been developed for Solaris only. I do not know
 * whether it works on other System V flavors, too.
 *)

val cots_connect : string -> string -> Unix.file_descr
  (* The first parameter is the name of the TLI/XTI device.
   * The second parameter is the address of the endpoint
   * to connect. The own endpoint has always an anonymous
   * address.
   * The "tirdwr" module is pushed onto the STREAM such that
   * the "read" and "write" syscalls work.
   *)

type connector =
    [ `Direct of (Rpc_client.connector * Rpc.protocol)
    | `Keyenvoy of string
    ]
  (* Same as Rpc_key_service.connector *)

val keyserv_connector : connector
  (* Returns a connector that can be used to call the
   * keyserv daemon.
   *)
