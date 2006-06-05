(* $Id$ *)

let start() =
  let host = ref "localhost" in
  let query = ref None in
  Arg.parse
    [ "-host", Arg.Set_string host,
      "<hostname>  Contact the finder daemon on this host";
    ]
    (fun s -> query := Some s)
    "usage: finder_client [options] <query>";

  let query_string =
    match !query with
      | None -> failwith "Query is missing on the command-line"
      | Some q -> q in

  let rpc_client =
    Finder_service_clnt.Finder.V1.create_portmapped_client !host Rpc.Tcp in

  try
    match Finder_service_clnt.Finder.V1.find rpc_client query_string with
      | `not_found ->
	  print_endline ("Not found: " ^ query_string)
      | `found fullpath ->
	  print_endline fullpath
  with
    | Rpc_client.Communication_error exn ->
	prerr_endline ("RPC: I/O error: " ^ Printexc.to_string exn)
    | Rpc_client.Message_lost ->
	prerr_endline "RPC: Message lost"
    | Rpc.Rpc_server Rpc.Unavailable_program ->
	prerr_endline "RPC: Unavailable program"
    | Rpc.Rpc_server (Rpc.Unavailable_version(_,_)) ->
	prerr_endline "RPC: Unavailable version";
    | Rpc.Rpc_server Rpc.Unavailable_procedure ->
	prerr_endline "RPC: Unavailable procedure";
    | Rpc.Rpc_server Rpc.Garbage ->
	prerr_endline "RPC: Garbage";
    | Rpc.Rpc_server Rpc.System_err ->
	prerr_endline "RPC: System error";
    | Rpc.Rpc_server (Rpc.Rpc_mismatch(_,_)) ->
	prerr_endline "RPC: Mismatch of RPC version";
    | Rpc.Rpc_server Rpc.Auth_bad_cred ->
	prerr_endline "RPC: Bad credentials";
    | Rpc.Rpc_server Rpc.Auth_rejected_cred ->
	prerr_endline "RPC: Rejected credentials";
    | Rpc.Rpc_server Rpc.Auth_bad_verf ->
	prerr_endline "RPC: Bad verifier";
    | Rpc.Rpc_server Rpc.Auth_rejected_verf ->
	prerr_endline "RPC: Rejected verifier";
    | Rpc.Rpc_server Rpc.Auth_too_weak ->
	prerr_endline "RPC: Authentication too weak";
    | Rpc.Rpc_server Rpc.Auth_invalid_resp ->
	prerr_endline "RPC: Invalid authentication response";
    | Rpc.Rpc_server Rpc.Auth_failed ->
	prerr_endline "RPC: Authentication failed";
;;

Sys.set_signal Sys.sigpipe Sys.Signal_ignore;
start();;
