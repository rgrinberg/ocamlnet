(* $Id$ *)

open Printf
open Netplex_ctrl_aux

let main() =
  let sockdir = ref "/tmp/.netplex" in
  let cmds = ref [] in
  let admin_cmd = ref [] in
  let admin_target = ref "*" in
  Arg.parse
    [ "-sockdir",
      Arg.String (fun s -> sockdir := s),
      "<dir>  Set the socket directory of the Netplex to administer";

      "-list",
      Arg.Unit (fun () -> cmds := `List :: !cmds),
      "  List available Netplex services";

      "-enable",
      Arg.String (fun s -> cmds := `Enable s :: !cmds),
      "<name>  Enable service <name>";

      "-disable",
      Arg.String (fun s -> cmds := `Disable s :: !cmds),
      "<name>  Disable service <name>";
      
      "-restart",
      Arg.String (fun s -> cmds := `Restart s :: !cmds),
      "<name>  Restart service <name>";

      "-restart-all",
      Arg.Unit (fun () -> cmds := `Restart_all :: !cmds),
      "  Restart all services";

      "-shutdown",
      Arg.Unit (fun () -> cmds := `Shutdown :: !cmds),
      "  Shutdown the whole Netplex";

      "-reopen-logfiles",
      Arg.Unit (fun () -> cmds := `Reopen_logfiles :: !cmds),
      "  Reopen logfiles (if possible)";

      "-receiver",
      Arg.String (fun pat -> admin_target := pat),
      "<pat>  Restrict receivers of admin messages to services matching <pat>";
    ]
    (fun arg ->
       admin_cmd := arg :: !admin_cmd)
    "Usage: netplex-admin [ options ] [ admin_cmd arg ... ]";

  let socket =
    Filename.concat !sockdir "netplex.controller/admin" in

  let client =
    Netplex_ctrl_clnt.Admin.V1.create_client 
      (Rpc_client.Unix socket)
      Rpc.Tcp in

  let exit_code = ref 0 in

  let check_exn f =
    try f() 
    with
      | Rpc.Rpc_server Rpc.System_err ->
	  prerr_endline "Netplex exception";
	  exit_code := 10
      | error ->
	  prerr_endline ("RPC exception: " ^ Printexc.to_string error);
	  exit_code := 11
  in

  let check_code code =
    match code with
      | `code_ok -> ()
      | `code_error msg ->
	  prerr_endline ("Netplex exception: " ^ msg);
	  exit_code := 10
  in

  let state_list =
    [ state_enabled, "Enabled";
      state_disabled, "Disabled";
      state_restarting, "Restarting";
      state_down, "Down";
    ] in

  let line s proto port =
    printf "%-20s %-10s %-35s %-10s %2d\n"
      s.srv_name
      proto
      port
      ( try List.assoc s.srv_state state_list
	with Not_found -> "?"
      )
      s.srv_nr_containers
  in

  List.iter
    (function
       | `List ->
	   check_exn
	     (fun () ->
		let l = Netplex_ctrl_clnt.Admin.V1.list client () in
		Array.iter
		  (fun s ->
		     if s.srv_protocols = [| |] then
		       line s "-" "-"
		     else
		       Array.iter
			 (fun p ->
			    if p.prot_ports = [| |] then
			      line s p.prot_name "-"
			    else
			      Array.iter
				(fun port ->
				   let port_s =
				     match port with
				       | `pf_unknown -> "unknown"
				       | `pf_unix path ->
					   "unix:" ^ path
				       | `pf_inet inet ->
					   "inet:" ^ 
					     inet.inet_addr ^ ":" ^ 
					     (string_of_int inet.inet_port)
				       | `pf_inet6 inet ->
					   "inet6:" ^ 
					     inet.inet6_addr ^ ":" ^ 
					     (string_of_int inet.inet6_port)
				   in
				   line s p.prot_name port_s
				)
				p.prot_ports
			 )
			 s.srv_protocols
		  )
		  l
	     )
       | `Enable pat ->
	   check_exn
	     (fun () ->
		let code = Netplex_ctrl_clnt.Admin.V1.enable client pat in
		check_code code)
       | `Disable pat ->
	   check_exn
	     (fun () ->
		let code = Netplex_ctrl_clnt.Admin.V1.disable client pat in
		check_code code)
       | `Restart pat ->
	   check_exn
	     (fun () ->
		let code = Netplex_ctrl_clnt.Admin.V1.restart client pat in
		check_code code)
       | `Restart_all ->
	   check_exn
	     (fun () ->
		let code = Netplex_ctrl_clnt.Admin.V1.restart_all client () in
		check_code code)
       | `Shutdown ->
	   check_exn
	     (fun () ->
		let code = Netplex_ctrl_clnt.Admin.V1.shutdown client () in
		check_code code)
       | `Reopen_logfiles ->
	   check_exn
	     (fun () ->
		let code =
		  Netplex_ctrl_clnt.Admin.V1.reopen_logfiles client () in
		check_code code)
    )
    (List.rev !cmds);

  ( match List.rev !admin_cmd with
      | [] -> ()
      | name :: args ->
	  let msg =
	    { Netplex_ctrl_aux.msg_name = name;
	      msg_arguments = Array.of_list args
	    } in
	  check_exn
	    (fun () ->
	       Netplex_ctrl_clnt.Admin.V1.send_admin_message 
		 client
		 (!admin_target, msg))
  );

  Rpc_client.shut_down client;
  
  exit !exit_code
;;


main();;
