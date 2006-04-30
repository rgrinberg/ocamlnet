(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

external cots_connect : string -> string -> Unix.file_descr
  = "xti_cots_connect" ;;

type connector =
    [ `Direct of (Rpc_client.connector * Rpc.protocol)
    | `Keyenvoy of string
    ]

let keyserv_connector =
  `Direct
    (Rpc_client.Dynamic_descriptor
       (fun () ->
	  cots_connect "/dev/ticotsord" (Unix.gethostname() ^ ".keyserv")
       ),
       Rpc.Tcp
    )

;;
