(* $Id$ *)

module C1 = Queues_clnt.QUEUESPROG.QUEUESVERS1 ;;

Qclient.pluggable_auth_module :=
  ( "auth_sys",
    (fun host ->
       let clnt = C1.create_portmapped_client host Rpc.Tcp in
       Rpc_client.set_auth_methods clnt 
	 [ Rpc_auth_sys.client_auth_method() ];
       clnt
    )
  )
;;
