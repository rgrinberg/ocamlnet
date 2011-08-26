open Printf

let main() =
  let servers = ref [] in
  let queries = ref [] in
  Arg.parse
    [ "-server", Arg.String (fun s ->
			       servers := !servers @ [Unix.inet_addr_of_string s]
			    ),
      "<IP>   Use this server to resolve hosts";
    ]
    (fun s ->
       queries := !queries @ [s]
    )
    "usage: resolve -server <IP> host host ...";

  if !servers = [] then
    failwith "No servers set. This is not yet implemented";

  let esys =
    Unixqueue.create_unix_event_system() in

  let cache = 
    Netdns_resolver.in_memory_cache () in

  let r =
    Netdns_resolver.stub_resolver
      ~cache
      !servers
      esys in

  List.iter
    (fun query ->
       r # host_query
	 query 
	 (fun u ->
	    match u with
	      | None -> 
		  printf "%s: no resolution\n\n" query;
	      | Some he -> 
		  printf "Name:      %s\n" he.Unix.h_name;
		  printf "Aliases:   %s\n" (String.concat "," 
					      (Array.to_list he.Unix.h_aliases));
		  printf "Addresses: %s\n\n" (String.concat ","
					      (List.map
						 Unix.string_of_inet_addr
						 (Array.to_list he.Unix.h_addr_list)))
	 );
    )
    !queries;

  Unixqueue.run esys;
;;

(* Netdns_resolver.debug := true; *)
main();;


