(* $Id$ *)

open Netdns_message

type name_cache_entry =
    { nc_name : string;
      nc_aliases : string list option;
      nc_addr_list : Unix.inet_addr list option;
      nc_aa : bool;
      nc_expires : float;
    }

type nameserver_cache_entry =
    { nsc_name : string;
      nsc_servers : name_cache_entry list;
      nsc_aa : bool;
      nsc_expires : float;
    }

class type dns_cache =
object
  method lookup_name : aa:bool -> string -> name_cache_entry
  method lookup_addr : aa:bool -> Unix.inet_addr -> name_cache_entry
  method lookup_nameserver : aa:bool -> string -> nameserver_cache_entry
  method add : msg -> unit
  method remove_expired_entries : unit -> unit
  method debug_string : string
end

type 'a result = ('a option -> unit)

class type async_resolver =
object
  method host_query : string -> Unix.host_entry result -> unit
  method addr_query : Unix.inet_addr -> Unix.host_entry result -> unit
(*
  method mx_query : string -> Unix.host_entry list result -> unit
 *)
  method shutdown : unit -> unit
end


class type async_resolver_queue =
object
  inherit async_resolver
  method limit : int
  method queue_length : int
end


let debug = ref false

(**********************************************************************)
(* cluster_client                                                     *)
(**********************************************************************)

let select_by_points rnd points =
  (* Given an array of points, the points are interpreted as likelihood
   * that the array element is selected.
   *)
  let total_points =
    Array.fold_left (fun acc p -> acc + p) 0 points in
  if total_points = 0 then
    None
  else
    let r = Random.State.int rnd total_points in
    Some
      (fst
	 (Array.fold_left
	    (fun (k, acc) p ->
	       (* acc = sum points.(0)..points.(k-1) *)
	       let acc' = acc + p in
	       if acc' <= r then
		 (k+1, acc')
	       else 
		 (k, acc'))
	    (0, 0)
	    points))
      


(* A cluster_client sends DNS requests to a set of name servers instead
 * of a single one. It disables name servers that are unreachable or
 * inresponsive. It automatically falls back to TCP if UDP is not
 * good enough.
 *
 * The name servers [nshosts] are assumed to be equivalent.
 *)
class cluster_client nshosts esys =
  let nshosts = Array.of_list nshosts in
  let numhosts = Array.length nshosts in
  let qlength = 10 in  (* length of quality queues *)
object(self)
  val udp_clients = Array.make numhosts None

  val tcp_clients = Array.make numhosts None
  val tcp_missing = Array.make numhosts 0     
    (* missing responses, if 0 the tcp client is shut down *)


  val failures = Array.make numhosts 0 
    (* Number of failures in the last [qlength] queries *)

  val qqueues = Array.init numhosts (fun _ -> Queue.create())
    (* Queues of bool values. If [true], a query was successful. For every
     * result a bool is added to the queue. If the queue becomes longer
     * than [qlength], the last bool is removed.
     *)

  val rnd = Random.State.make_self_init()

  method private server_probably_down k =
    (* Whether the k-th server is currently non-responsive *)
    failures.(k) >= qlength

  method private count_success k is_successful =
    let q = qqueues.(k) in
    Queue.add is_successful q;
    if not is_successful then 
      failures.(k) <- failures.(k) + 1;
    if Queue.length q > qlength then (
      let b = Queue.take q in
      if not b then
	failures.(k) <- failures.(k) - 1;
    )


  method private next_call remaining_udp_trials remaining_tcp_trials req_tcp
                           msg pass_result =
    (* Selects a name server, and sends the query [msg] to it. If the server
     * responds, [pass_result f] is called back such that [f()] yields the
     * response message. If the server does not respond, the method calls
     * itself recursively to take another server.
     *
     * [remaining_udp_trials] is an int array of length [numhost]. The number
     * is the number of remaining trials. 
     *
     * [remaining_tcp_trials] is a bool array of length [numhost]. If the
     * array element is [true] the corresponding server can still be contacted.
     *
     * When a server fails, the number of remaining trials is reduced by one
     * (in the UDP case) or the element is set to false (in the TCP case).
     * The recursive call will use this information when the server for the
     * next trial is selected.
     *
     * [req_tcp]: forces to use TCP
     *)

    (* First: Determine the UDP server to use, or None, if not possible.
     * For every trial the server gets one point. If the server is likely
     * to be down, the server gets at most one point at all. Then a
     * random server is chosen where points/total_points is the likelihood.
     *)
    let udp_opt =
      ( if req_tcp then
	  None   (* TCP requested *)
	else
	  let points = 
	    Array.init
	      numhosts
	      (fun k ->
		 let trials = remaining_udp_trials.(k) in
		 assert(trials >= 0);
		 if self # server_probably_down k then
		   min trials 1
		 else
		   trials) in
	  select_by_points rnd points
      ) in

    (* Second: Determine the TCP server to use. This is only an option
     * if there is no UDP server left. Servers that are likely to be down
     * are never selected.
     *)
    let tcp_opt = 
      ( if udp_opt <> None then
	  None
	else
	  let points = 
	    Array.init
	      numhosts
	      (fun k ->
		 if self # server_probably_down k then
		   0
		 else
		   if remaining_tcp_trials.(k) then 1 else 0) in
	  select_by_points rnd points
      ) in

    (* Determine the client. If not yet created, do that now *)
    let client_opt =
      match (udp_opt, tcp_opt) with
	| None, None ->
	    None    (* Well, nothing left *)
	| Some udp, None ->
	    (* Use a UDP client *)
	    ( match udp_clients.(udp) with
		| Some cl -> Some (cl, udp)
		| None ->
		    (* Create the client *)
		    let cl =
		      Netdns_client.create 
			(`Socket(Rpc.Udp,
				 `Internet(nshosts.(udp), 53),
				 Netdns_client.default_socket_config))
			esys in
		    Netdns_client.configure cl 0 5.0; (* no retrans! *)
		    udp_clients.(udp) <- Some cl;
		    Some (cl, udp)
	    )
	| None, Some tcp ->
	    (* Use a TCP client *)
	    ( match tcp_clients.(tcp) with
		| Some cl -> Some (cl, tcp)
		| None ->
		    (* Create the client *)
		    let cl =
		      Netdns_client.create 
			(`Socket(Rpc.Tcp,
				 `Internet(nshosts.(tcp), 53),
				 Netdns_client.default_socket_config))
			esys in
		    Netdns_client.configure cl 0 5.0; (* no retrans! *)
		    tcp_clients.(tcp) <- Some cl;
		    tcp_missing.(tcp) <- 0;
		    Some (cl, tcp)
	    )
	| _ ->
	    assert false
    in

(* let qname = String.concat "." ((List.hd msg.msg_question).q_name) in *)

    (* If possible, do the call *)
    match client_opt with
      | None ->
	  pass_result None
      | Some (cl, k) ->
	  let proto = Netdns_client.get_protocol cl in
	  if proto = Rpc.Tcp then
	    tcp_missing.(k) <- tcp_missing.(k) + 1;
	  ( try
	      Netdns_client.add_call
		cl
		msg
		(fun f ->
		   try
		     let result_msg = f() in
		     (* success! *)
		     self # count_success k true;
		     if proto = Rpc.Udp && result_msg.msg_header.msg_is_trunc then
		       raise Netdns_client.Message_too_long;
		     if proto = Rpc.Tcp then self # decr_tcp k;
		     pass_result (Some result_msg)
		   with
		     | Netdns_client.Message_timeout when proto = Rpc.Udp ->
			 (* failure. Try again *)
			 remaining_udp_trials.(k) <-
			   remaining_udp_trials.(k) - 1;
			 self # count_success k false;
			 self # next_call 
			   remaining_udp_trials remaining_tcp_trials req_tcp
                           msg pass_result

		     | Netdns_client.Message_timeout when proto = Rpc.Tcp ->
			 (* failure. Try again *)
			 self # decr_tcp k;
			 remaining_tcp_trials.(k) <- false;
			 self # count_success k false;
			 self # next_call 
			   remaining_udp_trials remaining_tcp_trials req_tcp
                           msg pass_result

		     | Netdns_client.Message_lost 
		     | Netdns_client.Communication_error _ 
		     | Netdns_lexer.Bad_message _ ->
			 (* failure. Try again with new client *)
			 Netdns_client.shut_down cl;
			 ( match proto with
			     | Rpc.Udp ->
				 ( match udp_clients.(k) with
				     | Some cl' when cl' == cl ->
					 udp_clients.(k) <- None
				     | _ -> ()
				 );
				 remaining_udp_trials.(k) <-
				   remaining_udp_trials.(k) - 1;
			     | Rpc.Tcp ->
				 ( match tcp_clients.(k) with
				     | Some cl' when cl' == cl ->
					 tcp_clients.(k) <- None
				     | _ -> ()
				 );
				 remaining_tcp_trials.(k) <- false
			 );
			 self # count_success k false;
			 self # next_call 
			   remaining_udp_trials remaining_tcp_trials req_tcp
                           msg pass_result

		     | Netdns_client.Message_too_long ->  (* resp too long *)
			 self # next_call 
			   remaining_udp_trials remaining_tcp_trials true
                           msg pass_result

		);
	    with
	      | Netdns_client.Message_too_long ->  (* query too long *)
		  (* Switch to TCP if possible *)
		  ( match proto with
		      | Rpc.Udp ->
			  self # next_call 
			    remaining_udp_trials remaining_tcp_trials true
                            msg pass_result
		      | Rpc.Tcp ->
			  (* oops. *)
			  self # decr_tcp k;
			  pass_result None
		  )
	      | Netdns_client.Queue_overflow ->
		  (* We are faster than the servers can respond. For now
                   * respond not found. Better: Throttle ourselves.
                   *)
		  if proto = Rpc.Tcp then
		    self # decr_tcp k;
		  pass_result None
	      | Netdns_message.Cannot_generate_message _ ->
		  (* Cannot even create the query! *)
		  if proto = Rpc.Tcp then
		    self # decr_tcp k;
		  pass_result None
	      | error ->
		  if proto = Rpc.Tcp then
		    self # decr_tcp k;
		  raise error
		    (* Other exceptions are not caught. Want to see them. *)
	  )

  method private decr_tcp k =
    (* Decrement usage count for k-th tcp client. If 0 shut down *)
    if tcp_clients.(k) != None then (
      assert(tcp_missing.(k) >= 1);
      tcp_missing.(k) <- tcp_missing.(k) - 1;
      if tcp_missing.(k) = 0 then (
	match tcp_clients.(k) with
	  | None -> assert false
	  | Some cl ->
	      Netdns_client.shut_down cl;
	      tcp_clients.(k) <- None
      )
    )

  method query msg pass_result = 
    let remaining_udp_trials =
      Array.create numhosts 3 in
    let remaining_tcp_trials =
      Array.create numhosts true in
    self # next_call
      remaining_udp_trials remaining_tcp_trials false msg pass_result


  method shutdown() =
    let udp_l = Array.copy udp_clients in
    let tcp_l = Array.copy tcp_clients in
    for k = 0 to numhosts - 1 do
      udp_clients.(k) <- None;
      tcp_clients.(k) <- None;
      tcp_missing.(k) <- 0;
    done;

    let shut_down =
      function
	| None -> ()
	| Some cl -> Netdns_client.shut_down cl in
    
    Array.iter shut_down udp_l;
    Array.iter shut_down tcp_l;

end


(**********************************************************************)
(* Domain strings                                                     *)
(**********************************************************************)

(* Move that to Netdns_message? *)


let split_re = Pcre.regexp "[.]"

let split_domain name =
  (* Returns the domain name as list of labels *)
  List.filter
    (fun lab -> lab <> "")
    (Pcre.split ~rex:split_re ~max:0 name)


let combine_domain domain =
  String.concat "." domain


module Domain = struct
  type t = string list

  let rec compare dom1 dom2 =
    match dom1,dom2 with
      | (lab1 :: dom1'), (lab2 :: dom2') ->
	  let lab1_lc = String.lowercase lab1 in
	  let lab2_lc = String.lowercase lab2 in
	  ( match Pervasives.compare lab1_lc lab2_lc with
	      | 0 ->
		  compare dom1' dom2'
	      | n -> n
	  )
      | [], [] ->
	  0
      | ([], _) ->
	  (-1)
      | (_, []) ->
	  1
end


module DomainSet = Set.Make(Domain)
module DomainMap = Map.Make(Domain)


let equal_domains dom1 dom2 =
  Domain.compare dom1 dom2 = 0


let equal_rr rr1 rr2 =
  match rr1, rr2 with
    | (`A a1), (`A a2) -> a1 = a2
    | (`CNAME d1), (`CNAME d2) -> equal_domains d1 d2
    | (`HINFO (cpu1,os1)), (`HINFO(cpu2,os2)) -> cpu1=cpu2 && os1=os2
    | (`MX(pref1,d1)), (`MX(pref2,d2)) -> pref1=pref2 && equal_domains d1 d2
    | (`NS d1), (`NS d2) -> equal_domains d1 d2
    | (`PTR d1), (`PTR d2) -> equal_domains d1 d2
    | (`SOA s1), (`SOA s2) ->
	( equal_domains s1.soa_mname s2.soa_mname 
	  && equal_domains s1.soa_rname s2.soa_rname
	  && s1.soa_serial = s2.soa_serial
	  && s1.soa_refresh = s2.soa_refresh
	  && s1.soa_retry = s2.soa_retry
	  && s1.soa_expire = s2.soa_expire
	  && s1.soa_minimum = s2.soa_minimum
	)
    | (`TXT t1), (`TXT t2) -> t1=t2
    | `Unknown, `Unknown -> false  (* For purposes of caching *)
    | _ -> false


let addr_re = Pcre.regexp "^([0-9]+)\\.([0-9]+)\\.([0-9]+)\\.([0-9]+)$"

let to_addr_domain addr =
  let s = Unix.string_of_inet_addr addr in
  match Pcre.extract ~rex:addr_re ~full_match:false s with
    | [| a1; a2; a3; a4 |] ->
	[ a4; a3; a2; a1; "in-addr"; "arpa" ]
    | _ -> assert false


let of_addr_domain dom =
  match dom with
    | [ a4; a3; a2; a1; in_addr; arpa ] 
	when equal_domains [in_addr;arpa] ["in-addr"; "arpa" ] ->
	Unix.inet_addr_of_string
	  (Printf.sprintf "%s.%s.%s.%s" a1 a2 a3 a4)
    | _ ->
	raise Not_found


(**********************************************************************)
(* Caching                                                            *)
(**********************************************************************)

module Float = struct
  type t = float
  let compare x y =
    Pervasives.compare (x:float) y
end


module FloatMap = Map.Make(Float)


let float_map_iter_until f m limit =
  try
    FloatMap.iter
      (fun k v ->
	 if k > limit then raise Exit;
	 f k v
      )
      m
  with
    | Exit -> ()



(* Types for lookup functions: *)

type 'a cache_entry =
    { data : 'a;
      is_aa : bool;    (* Whether this counts as authoritative *)
      expires : float; (* When this will expire *)
    }


type fwd_lookup =
    domain -> qtype -> rr_entry cache_entry list
	(* Returns all rr_entries [rr] for which [rr.rr_name] is the
         * passed domain and for which [rr.rr_type] matches the passed
         * qtype. [rr.rr] must also match qtype.
         *)

type rev_cname_lookup =
    domain -> rr_entry cache_entry list
	(* Returns all rr_entries [rr] for which
         * [rr.rr_type] is `CNAME and [rr.rr] is a `CNAME matching the passed
         * domain
         *)

(*
type rev_ptr_lookup =
    domain -> rr_entry cache_entry list
	(* Returns all rr_entries [rr] for which
         * [rr.rr_type] is `PTR and [rr.rr] is a `PTR matching the passed
         * domain
         *)
 *)

(* Aliases *)

let resolve_alias qname (fwd : fwd_lookup) : domain cache_entry list =
  (* Return a list of names. The last element in the list is the
   * canonical name. The other names are aliases. If the list is empty
   * there is no resolution.
   *
   * Caution: If qname is already the canonical name, the returned
   * cache entry will say it is authoritative although there is no
   * evidence!
   *)

  let rec resolve is_aa expires followed_names qname =
    let rr_list = fwd qname `CNAME in
    match List.rev rr_list with
      | [] ->
	  (* No alias: qname is the final name *)
	  [ { data = qname;
	      is_aa = is_aa;
	      expires = expires;
	    }
	  ]

      | rev_rr_list ->
	  (* One or more alias definition: search *)
	  search_list is_aa expires followed_names qname rev_rr_list

  and search_list is_aa expires followed_names qname rr_list =
    match rr_list with
      | [] ->
	  []
      | cname_rr_ce :: rr_list' ->
	  (* One alias definition: Another round of resolution. Note that it is
             possible to get here more than one alias definition as a result
             of aggregating several answers in the cache. We search the first
             one that yields a result.
	   *)
	  try
	    let cname = 
	      match cname_rr_ce.data.rr with
		| `CNAME n -> n
		| _ -> assert false in
	    if DomainSet.mem cname followed_names then
	      (* loop! *)
	      raise Not_found;
	    let ce =
	      { data = qname;
		is_aa = is_aa;
		expires = expires;
	      } in
	    let is_aa' = is_aa && cname_rr_ce.is_aa in
	    let expires' = min expires cname_rr_ce.expires in
	    let followed_names' = DomainSet.add cname followed_names in
	    ce :: resolve is_aa' expires' followed_names' cname
	  with
	    | Not_found ->
		search_list is_aa expires followed_names qname rr_list'
  in

  resolve true max_float DomainSet.empty qname
  

(*
let all_aliases cname (rev_cname : rev_cname_lookup) : domain list cache_entry =
  (* Return all aliases that resolve to [cname] *)

  let rec search is_aa expires final_aliases found_aliases =
    (* final_aliases: Collects the aliases found in the search loop
     * found_aliases: Aliases that need to be checked whether other
     *    aliases point to them
     * This is a breadth first search algorithm.
     *)
    if DomainSet.is_empty found_aliases then
      { data = DomainSet.elements final_aliases;
	is_aa = is_aa;
	expires = expires;
      }
    else
      let (followed_aliases, is_aa', expires') =
	DomainSet.fold
	  (fun name (set, is_aa', expires') ->
	     let cname_rr_ce_pointing_to_name = rev_cname name in
	     List.fold_left
	       (fun (set, is_aa', expires') rr_ce ->
		  ( DomainSet.add rr_ce.data.rr_name set,
		    is_aa' && rr_ce.is_aa,
		    min expires' rr_ce.expires
		  )
	       )
	       (set, is_aa', expires')
	       cname_rr_ce_pointing_to_name)
	  found_aliases
	  (DomainSet.empty, is_aa, expires) in
      let final_aliases' =
	DomainSet.union final_aliases followed_aliases in
      let found_aliases' =  (* only new ones! *)
	DomainSet.diff followed_aliases final_aliases in
      search is_aa' expires' final_aliases' found_aliases'
  in
  
  search
    true
    max_float
    DomainSet.empty
    (DomainSet.singleton cname)
 *)


let nc_entry_of_A qname (fwd : fwd_lookup) =
  (* Construct a name_cache_entry for an A query. *)

  if !debug then
    prerr_endline "nc_entry_of_A: start";

  let ce_resolutions = resolve_alias qname fwd in

  let (main_name_ce, cname_list, cname_aa, cname_expires) =
    match List.rev ce_resolutions with
      | [] ->
	  ( { data = qname;
	      is_aa = true;
	      expires = max_float
	    },
	    [],
	    true,
	    max_float
	  )
      | main_name_ce :: cname_ce_list ->
	  let cname_list =
	    List.map (fun ce -> combine_domain ce.data) cname_ce_list in
	  let cname_aa =
	    List.for_all (fun ce -> ce.is_aa) cname_ce_list in
	  let cname_expires =
	    List.fold_left
	      (fun acc ce -> min acc ce.expires)
	      max_float 
	      cname_ce_list in
	  (main_name_ce, cname_list, cname_aa, cname_expires) in
  
  let main_name = main_name_ce.data in

  if !debug then
    prerr_endline("nc_entry_of_A: main_name=" ^ combine_domain main_name);

  if !debug then
    prerr_endline("nc_entry_of_A: cname_list=" ^ 
		    String.concat ", " cname_list);

  let addr_rr_ce_list = fwd main_name `A in

  let addr_list =
    List.map
      (fun ce ->
	 match ce.data.rr with 
	   | `A straddr ->
	       Unix.inet_addr_of_string straddr
	   | _ -> assert false
      )
      addr_rr_ce_list in

  if !debug then
    prerr_endline("nc_entry_of_A: addr_list=" ^ 
		    String.concat ", "
		    (List.map Unix.string_of_inet_addr addr_list));

  let addr_list_is_aa =
    List.for_all (fun ce -> ce.is_aa) addr_rr_ce_list in

  let addr_list_expires =
    List.fold_left (fun m ce -> min m ce.expires) max_float addr_rr_ce_list in
      
  { nc_name = 
      combine_domain main_name;
    nc_aliases = 
      Some cname_list;
    nc_addr_list = 
      if addr_list = [] then None else Some addr_list;
      (* map [] to None because we don't have here access to neg cache data *)
    nc_aa = 
      main_name_ce.is_aa && addr_list_is_aa && cname_aa;
    nc_expires = 
      min (min main_name_ce.expires addr_list_expires) cname_expires
  }


let nc_entry_of_PTR qname (fwd : fwd_lookup) =
  (* Construct a name_cache_entry from a complete answer message to a PTR 
   * query. Raises Not_found if not possible.
   *)

  let ptr_name, ptr_is_aa, ptr_expires =  (* or Not_found *)
    match fwd qname `PTR with
      | [] -> raise Not_found
      | ce :: _ -> 
	  (* Note: If there are several `PTR entries, we take only the
           * first one
           *)
	  ( match ce.data.rr with
	      | `PTR n -> n, ce.is_aa, ce.expires
	      | _ -> assert false 
	  )
  in

  let ce_resolutions = resolve_alias qname fwd in

  let (main_name_ce, cname_list, cname_aa, cname_expires) =
    match List.rev ce_resolutions with
      | [] ->
	  ( { data = qname;
	      is_aa = true;
	      expires = max_float
	    },
	    [],
	    true,
	    max_float
	  )
      | main_name_ce :: cname_ce_list ->
	  let cname_list =
	    List.map (fun ce -> combine_domain ce.data) cname_ce_list in
	  let cname_aa =
	    List.for_all (fun ce -> ce.is_aa) cname_ce_list in
	  let cname_expires =
	    List.fold_left
	      (fun acc ce -> min acc ce.expires)
	      max_float 
	      cname_ce_list in
	  (main_name_ce, cname_list, cname_aa, cname_expires) in

  let main_name = main_name_ce.data in

  let addr =
    of_addr_domain qname in

  { nc_name = 
      combine_domain main_name;
    nc_aliases = 
      Some cname_list;
    nc_addr_list = 
      Some [ addr ];
    nc_aa = 
      main_name_ce.is_aa && ptr_is_aa && cname_aa;
    nc_expires = 
      min (min main_name_ce.expires ptr_expires) cname_expires
  }


let host_entry_of_nc nc =
  { Unix.h_name = nc.nc_name;
    h_aliases = ( match nc.nc_aliases with
		    | Some l -> Array.of_list l
		    | None -> [| |]
		);
    h_addrtype = Unix.PF_INET;
    h_addr_list = ( match nc.nc_addr_list with
		      | Some [] -> raise Not_found
		      | Some l -> Array.of_list l
		      | None -> raise Not_found
		  )
  }


class in_memory_cache ?(max_ttl = 86400.0) () : dns_cache =
object(self)
  val mutable fwd_map = DomainMap.empty
  val mutable exp_map = FloatMap.empty


  method debug_string =
    let fwd_size = DomainMap.fold (fun _ _ n -> n+1) fwd_map 0 in
    let exp_size = FloatMap.fold (fun _ _ n -> n+1) exp_map 0 in
    Printf.sprintf "fwd_map.size=%d exp_map.size=%d"
      fwd_size exp_size

  method private fwd_lookup qname qtype =
    let ce_list =
      try DomainMap.find qname fwd_map 
      with Not_found -> ref [] in
    let r = 
      List.filter
	(fun ce ->
	   match qtype with
	     | `ANY -> true
	     | `MAILA -> failwith "MAILA queries not implemented"
	     | `MAILB -> failwith "MAILB queries not implemented"
	   | `AXFR ->  false   (* we cannot do this! *)
	   | #type_ as qtype' -> qtype' = ce.data.rr_type
	)
	!ce_list in
    ( match !ce_list with  (* rotate after each cache access *)
	| [] -> ()
	| first :: rest -> 
	    ce_list := rest @ [first]
    );
    r

  method lookup_name ~aa name = 
    (* TODO: implement [aa] (by separating fwd_map in a aa and a non-aa part) *)
    let qname = split_domain name in
    let nc = nc_entry_of_A qname self#fwd_lookup in
    if nc.nc_addr_list = None then (
      if !debug then
	prerr_endline ("lookup_name: " ^ name ^ " not found");
      raise Not_found
    )
    else
      nc

  method lookup_addr ~aa addr = 
    (* TODO: implement [aa] (by separating fwd_map in a aa and a non-aa part) *)
    let qname = to_addr_domain addr in
    nc_entry_of_PTR qname self#fwd_lookup 


  method private replace_fwd_mapping expires rr =
    (* Get the entry list or create a new one: *)
    let ce_list =
      try DomainMap.find rr.rr_name fwd_map 
      with
	| Not_found -> 
	    let l = ref [] in
	    fwd_map <- DomainMap.add rr.rr_name l fwd_map;
	    l in
    self # replace_in_ce_list expires rr ce_list


  method private replace_in_ce_list expires rr ce_list =
    (* First check if the RR is already there: *)
    let old_ce_opt =
      try
	Some
	  (List.find
	     (fun ce -> 
		equal_domains ce.data.rr_name rr.rr_name
		&& ce.data.rr_type = rr.rr_type
		 && equal_rr ce.data.rr rr.rr)
	     !ce_list)
      with
	| Not_found -> None in

    (* Decide whether to replace: *)
    let do_replace =
      match old_ce_opt with
	| None -> true
	| Some ce -> ce.expires < expires in

    if do_replace then (
      let new_ce =
	{ data = rr;
	  is_aa = false; (* TODO *)
	  expires = expires 
	} in
      ce_list := 
	new_ce :: 
	  (List.filter
	     (fun ce -> 
		match old_ce_opt with
		  | None -> true
		  | Some old_ce -> ce != old_ce (* phys. cmp *)
	     )
	     !ce_list);
      ( match old_ce_opt with
	  | None -> ()
	  | Some old_ce ->
	      self # unreg_expiration old_ce
      );
      self # reg_expiration new_ce
    )

  method private unreg_expiration ce =
    let l = 
      try FloatMap.find ce.expires exp_map
      with Not_found -> ref []  (* strange case *) in

    l := List.filter (fun ce' -> ce != ce') !l;

    if !l = [] then
      exp_map <- FloatMap.remove ce.expires exp_map


  method private reg_expiration ce =
    let l = 
      try FloatMap.find ce.expires exp_map
      with Not_found -> 
	let l = ref [] in
	exp_map <- FloatMap.add ce.expires l exp_map;
	l in

    l := ce :: !l


  method add msg =
    (* We only accept messages with `No_error code. In particular, no
     * negative caching for `Name_error for now. Furthermore, opcode
     * must be `QUERY, and the message must not be truncated.
     *)
    if (msg.msg_header.msg_rcode = `No_error && 
	msg.msg_header.msg_opcode = `QUERY &&
	not msg.msg_header.msg_is_trunc)
    then (
      let now = Unix.gettimeofday() in

      (* We interpret the ANSWER and ADDITIONAL sections. AUTHORITY is
       * considered as non-cachable because it points to other authorities
       * (which may be cached once resolved).
       *)
      let all_rr =
	msg.msg_answer @ msg.msg_additional in

      (* Select only sound Internet-related RRs *)
      let good_rr =
	List.filter
	  (fun rr -> rr.rr_class = `IN_ && is_sound rr)
	  all_rr in

      List.iter
	(fun rr ->
	   let ttl =
	     min max_ttl (Int64.to_float (Rtypes.int64_of_uint4 rr.rr_ttl)) in
	   let expires = 
	     now +. ttl in
	   self # replace_fwd_mapping expires rr;
	)
	good_rr
    )


  method lookup_nameserver ~aa name = raise Not_found
    (* TODO *)

  method remove_expired_entries() =
    let now = Unix.gettimeofday() in
    let l = ref [] in

    float_map_iter_until
      (fun _ e ->
	 l := !e @ !l)
      exp_map
      now;

    List.iter
      (fun ce ->
	 (* Remove ce from fwd_map: *)
	 ( try
	     let ce_list = DomainMap.find ce.data.rr_name fwd_map in
	     ce_list :=
	       List.filter
		 (fun ce' -> ce' != ce)
		 !ce_list;
	     if !ce_list = [] then
	       fwd_map <- DomainMap.remove ce.data.rr_name fwd_map
	   with
	     | Not_found -> ()
	 );
	 (* Remove ce from exp_map: *)
	 self # unreg_expiration ce
      )
      !l;
    
end


let in_memory_cache = new in_memory_cache


(**********************************************************************)
(* The stub resolver                                                  *)
(**********************************************************************)


class stub_resolver ?cache smarthosts esys : async_resolver =
  let ccl = new cluster_client smarthosts esys in
object(self)

  method private query_for qt domain pass_result =
    (* Sets up the question and performs a query *)
    (* qt can be `A or `PTR for now *)
    let hdr =
      { msg_id = 0;   (* later replaced *)
	msg_is_query = true;
	msg_opcode = `QUERY;
	msg_is_aa = false;
	msg_is_trunc = false;
	msg_rd = true;   (* want recursion *)
	msg_ra = false;
	msg_rcode = `No_error 
      } in
    let q =
      { q_name = domain;
	q_type = (qt :> qtype);
	q_class = `IN_
      } in
    let msg =
      { msg_header = hdr;
	msg_question = [ q ];
	msg_answer = [];
	msg_authority = [];
	msg_additional = []
      } in
    
    ccl # query
      msg
      pass_result

  method private query cache_lookup qtype qname pass_result =
    (* Note that this method does intentionally not check whether the
       cached result has expired or not. This is better done for the
       whole cache in one step by calling remove_expired_entries.
     *)

    let query_cache =
      match cache with
	| Some ca -> ca 
	| None -> new in_memory_cache() in

    let he_opt =
      try
	(* Try to reconstruct result from cache: *)
	if !debug then prerr_endline "Looking into cache";
	let nc = cache_lookup query_cache in (* or Not_found *)
	if !debug then prerr_endline "Found in cache";
	Some (host_entry_of_nc nc)  (* or Not_found *)
      with
	| Not_found ->
	    None in

    ( match he_opt with
	| Some he ->
	    if !debug then prerr_endline "Returning result";
	    pass_result (Some he)
	| None ->
	    if !debug then prerr_endline "Starting remote query";
	    self # query_for
	      qtype
	      qname
	      (function 
		 | Some msg ->
		     if !debug then prerr_endline "Got message";
		     query_cache # add msg;
		     let he_opt =
		       try
			 if !debug then prerr_endline "Looking into cache/2";
			 let nc = cache_lookup query_cache in
			 if !debug then prerr_endline "Found in cache/2";
			 Some (host_entry_of_nc nc)
		       with
			 | Not_found -> None in
		     if !debug then (
		       if he_opt = None then
			 prerr_endline "Missing result/2"
		       else
			 prerr_endline "Returning result/2";
		     );
		     pass_result he_opt
		 | None ->
		     if !debug then prerr_endline "Timeout";
		     pass_result None
	      )
    )

  method host_query name pass_result =
    let qname = split_domain name in
    self # query 
      (fun cache -> cache # lookup_name ~aa:false name) 
      `A
      qname pass_result

  method addr_query addr pass_result =
    let qname = to_addr_domain addr in
    self # query 
      (fun cache -> cache # lookup_addr ~aa:false addr) 
      `PTR
      qname pass_result


  method shutdown () =
    ccl # shutdown()
end


let stub_resolver = new stub_resolver


(**********************************************************************)
(* Intelligent request queueing                                       *)
(**********************************************************************)

class queue ~limit (res : async_resolver) : async_resolver_queue =
object(self)
  val mutable n = 0
    (* number of unresponded queries that have already been sent to [res] *)
    
  val mutable q = Queue.create()
    (* Additional queries if [n = limit] *)

  val mutable host_notify = Hashtbl.create 100
  val mutable addr_notify = Hashtbl.create 100
    (* Map queries to lists of pass_result functions *)


  method limit = limit
  method queue_length = n + Queue.length q


  method host_query name pass_result =
    (* First check whether the same query is already in the queue. If so,
     * simply add the pass_result callback to the notify table, so we
     * get notified when the query is done.
     *)
    try
      let l = Hashtbl.find host_notify name  in
      Hashtbl.replace host_notify name (pass_result :: l)
    with
      | Not_found ->
	  (* No such query in the queue: Add it *)
	  Hashtbl.replace host_notify name [pass_result];
	  self # do_host_query name


  method addr_query addr pass_result =
    (* Same for address lookups *)
    try
      let l = Hashtbl.find addr_notify addr  in
      Hashtbl.replace addr_notify addr (pass_result :: l)
    with
      | Not_found ->
	  Hashtbl.replace addr_notify addr [pass_result];
	  self # do_addr_query addr


  method private check_queue() =
    (* Perform queries taken from the queue of waiting queries *)
    if not(Queue.is_empty q) then (
      let action = Queue.take q in
      match action with
	| `Host_query name ->
	    self # do_host_query name
	| `Addr_query addr ->
	    self # do_addr_query addr
    ) 

  method private do_host_query name =
    if n < limit then (
      (* Less than [limit] queries are performed by the resolver. It is
       * allowed to add another one.
       *)
      res # host_query
	name
	(fun result_opt ->
	   n <- n - 1;
	   self # do_pass_host_result name result_opt;
	   if n < limit then 
	     self # check_queue();
	);
      n <- n + 1
    )
    else
      (* Too many queries are being performed by the resolver. Add the
       * query to the wait queue
       *)
      Queue.add (`Host_query name) q

  method private do_pass_host_result name result_opt =
    (* We have the [result_opt], so pass it to all callbacks that want to
     * have it.
     *)
    let l =
      try Hashtbl.find host_notify name
      with Not_found -> assert false in
    Hashtbl.remove host_notify name;
    List.iter
      (fun pass_result -> pass_result result_opt)
      (List.rev l)

  method private do_addr_query addr =
    if n < limit then (
      res # addr_query
	addr
	(fun result_opt ->
	   n <- n - 1;
	   self # do_pass_addr_result addr result_opt;
	   if n < limit then 
	     self # check_queue();
	);
      n <- n + 1
    )
    else
      Queue.add (`Addr_query addr) q

  method private do_pass_addr_result addr result_opt =
    let l =
      try Hashtbl.find addr_notify addr
      with Not_found -> assert false in
    Hashtbl.remove addr_notify addr;
    List.iter
      (fun pass_result -> pass_result result_opt)
      (List.rev l)

  method shutdown () =
    res # shutdown()
end


let queue =
  new queue
