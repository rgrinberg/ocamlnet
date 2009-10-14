(* $Id$ *)


let any_file_client_connector_1 fname =
  let st = Unix.stat fname in
  match st.Unix.st_kind with
    | Unix.S_SOCK ->
	(Rpc_client.Unix fname, `UD)
    | Unix.S_REG ->
	let f = open_in fname in
	( try
	    let t = input_line f in
	    ( match t with
		| "socket_file" ->
		    let d = input_line f in
		    let n = int_of_string d in
		    (Rpc_client.Internet(Unix.inet_addr_loopback, n),
		     `Socket_file)
		| "w32_pipe_file" ->
		    let d = input_line f in
		    (Rpc_client.W32_pipe d,
		     `W32_pipe_file)
		| _ ->
		    raise Not_found
	    )
	  with 
	    | _ ->
		close_in f; 
		failwith ("Netplex_sockserv.any_file_connector: Bad file: " ^ 
			    fname)
	)
    | _ ->
	failwith ("Netplex_sockserv.any_file_connector: Bad file type: " ^ 
		    fname)


let any_file_client_connector fname =
  (* reexported by Netplex_sockserv *)
  fst(any_file_client_connector_1 fname)

let client_connector addr =
  (* reexported by Netplex_sockserv *)
  match addr with
    | `Socket s ->
	( match s with
	    | Unix.ADDR_INET(ip,p) ->
		Rpc_client.Internet(ip,p)
	    | Unix.ADDR_UNIX p ->
		Rpc_client.Unix p
	)
    | `Socket_file fname ->
	let (conn, conn_type) = any_file_client_connector_1 fname in
	if conn_type <> `Socket_file then
	  failwith("Netplex_sockserv.client_connector: Unexpected file type: " ^ 
		     fname);
	conn
    | `W32_pipe pname ->
	Rpc_client.W32_pipe pname
    | `W32_pipe_file fname ->
	let (conn, conn_type) = any_file_client_connector_1 fname in
	if conn_type <> `W32_pipe_file then
	  failwith("Netplex_sockserv.client_connector: Unexpected file type: " ^ 
		     fname);
	conn

	
