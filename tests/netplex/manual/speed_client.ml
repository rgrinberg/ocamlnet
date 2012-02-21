open Printf

let produce_load n_max =
  let n = ref 0 in
  let n_lim = ref 2 in
  let count = ref 0 in
  let esys = Unixqueue.create_unix_event_system() in

  let rec next() =
    while !n < !n_lim do
      let client =
	Speed_proto_clnt.P.V1.create_client ~esys 
	  (Rpc_client.Unix "sockdir/socket") Rpc.Tcp in
      Speed_proto_clnt.P.V1.ping'async
	client ()
	(fun _ ->
	   decr n;
	   incr count;
	   Rpc_client.shut_down client;
	   next()
	);
      incr n;
      next()
    done in

  let rec timer() =
    let g = Unixqueue.new_group esys in
    Unixqueue.once esys g 1.0
      (fun _ ->
	 if !n_lim < n_max then
	   n_lim := min n_max (2 * !n_lim);
	 printf "Count: %d\n%!" !count;
	 count := 0;
	 timer()
      ) in

  next();
  timer();
  Unixqueue.run esys


let () =
  let n = ref 10 in
  Arg.parse
    [ "-n", Arg.Set_int n, "<n>  number of parallel clients" ]
    (fun _ -> raise(Arg.Bad "unexpected arg"))
    "usage: speed_client [options]";

  produce_load !n
