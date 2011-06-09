(* $Id$ *)

type 't engine_state =
  [ `Working of int
  | `Done of 't
  | `Error of exn
  | `Aborted
  ]

class type [ 't ] engine = object
  method state : 't engine_state
  method abort : unit -> unit
  method request_notification : (unit -> bool) -> unit
  method event_system : Unixqueue.event_system
end

exception Host_not_found of string

class type resolver =
object
  method host_by_name : 
           string -> Unixqueue.event_system -> Unix.host_entry engine
end

let default_resolver() : resolver =
object (self)
  method host_by_name host esys =
    let state =
      try
	let addr = Unix.inet_addr_of_string host in
	`Done
	  { Unix.h_name = host;
	    h_aliases = [| |];
	    h_addrtype = Netsys.domain_of_inet_addr addr;
	    h_addr_list = [| addr |]
	  }
      with
	| Failure _ ->
	    try
	      let he = Unix.gethostbyname host in
	      (`Done he)
	    with Not_found ->
	      (`Error(Host_not_found host)) in
    ( object
	method state = state
	method abort() = ()
	method request_notification _ = ()
	method event_system = esys
      end
    )
end


let cur_resolver = ref(default_resolver())

let current_resolver() = !cur_resolver

let set_current_resolver r = cur_resolver := r


let get_host_by_name ?(resolver = !cur_resolver) host =
  let esys = Unixqueue.create_unix_event_system() in
  let eng = resolver # host_by_name host esys in
  let eng_final = ref false in
  try
    Unixqueue.run esys;
    ( match eng#state with
	| `Done he ->
	    he
	| `Error e ->
	    eng_final := true;
	    raise e
	| _ ->
	    assert false
    )
  with
    | e when not !eng_final ->
	eng # abort();
	raise e
