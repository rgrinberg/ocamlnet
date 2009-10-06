(* $Id$ *)

open Netplex_types

let release = ref (fun () -> ())


let plugin : plugin =
  ( object (self)
      val mutable variables = Hashtbl.create 50
      val mutable owns = Hashtbl.create 50

      initializer (
	release :=
	  (fun () -> 
	     variables <- Hashtbl.create 1;
	     owns <- Hashtbl.create 1
	  )
      )

      method program =
	Netplex_ctrl_aux.program_Sharedvar'V1

      method ctrl_added _ = ()

      method ctrl_unplugged ctrl =
	List.iter
	  (fun cid ->
	     self # ctrl_container_finished ctrl cid true
	  )
	  ctrl#containers

      method ctrl_receive_call ctrl cid procname arg reply =
	match procname with
	  | "ping" ->
	      reply(Some(Netplex_ctrl_aux._of_Sharedvar'V1'ping'res ()))

	  | "create_var" ->
	      let (var_name, own_flag, ro_flag) =
		Netplex_ctrl_aux._to_Sharedvar'V1'create_var'arg arg in
	      let success =
		self # create_var ctrl cid var_name own_flag ro_flag in
	      reply(
		Some(Netplex_ctrl_aux._of_Sharedvar'V1'create_var'res success))

	  | "set_value" ->
	      let (var_name, var_value) =
		Netplex_ctrl_aux._to_Sharedvar'V1'set_value'arg arg in
	      let success =
		self # set_value ctrl cid var_name var_value in
	      reply(
		Some(Netplex_ctrl_aux._of_Sharedvar'V1'set_value'res success))

	  | "get_value" ->
	      let (var_name) =
		Netplex_ctrl_aux._to_Sharedvar'V1'get_value'arg arg in
	      let valopt =
		self # get_value ctrl cid var_name in
	      reply(
		Some(Netplex_ctrl_aux._of_Sharedvar'V1'get_value'res valopt))

	  | "delete_var" ->
	      let (var_name) =
		Netplex_ctrl_aux._to_Sharedvar'V1'delete_var'arg arg in
	      let success =
		self # delete_var ctrl cid var_name in
	      reply(
		Some(Netplex_ctrl_aux._of_Sharedvar'V1'delete_var'res success))

	  | "wait_for_value" ->
	      let (var_name) =
		Netplex_ctrl_aux._to_Sharedvar'V1'wait_for_value'arg arg in
	      self # wait_for_value ctrl cid var_name
		(fun r -> 
		   reply
		     (Some
			(Netplex_ctrl_aux._of_Sharedvar'V1'wait_for_value'res 
			   r)))

	  | _ ->
	      failwith ("Netplex_sharedvar: unknown proc " ^ procname)

      method ctrl_container_finished ctrl cid is_last =
	if is_last then (
	  let ssn = cid#socket_service_name in
	  let vars = try Hashtbl.find owns (ctrl,ssn) with Not_found -> [] in
	  Hashtbl.remove owns (ctrl,ssn);
	  List.iter
	    (fun var_name ->
	       ignore(self # delete_var ctrl cid var_name)
	    )
	    vars
	)


      method private create_var ctrl cid var_name own_flag ro_flag =
	let ssn = cid#socket_service_name in
	not (Hashtbl.mem variables (ctrl,var_name)) && (
	  Hashtbl.add 
	    variables 
	    (ctrl,var_name)
	    ("",
	     (if own_flag then Some ssn else None),
	     ro_flag,
	     false,
	     Queue.create());
	  if own_flag then (
	    let ovars =
	      try Hashtbl.find owns (ctrl,ssn) with Not_found -> [] in
	    Hashtbl.replace owns (ctrl,ssn) (var_name :: ovars)
	  );
	  true
	)
	  

      method private delete_var ctrl cid var_name =
	let ssn = cid#socket_service_name in
	try
	  let (_, owner, _, _, q) = Hashtbl.find variables (ctrl,var_name) in
	  ( match owner with
	      | None -> ()
	      | Some ssn' -> if ssn <> ssn' then raise Not_found
	  );
	  Hashtbl.remove variables (ctrl,var_name);
	  if owner <> None then (
	    let ovars =
	       try Hashtbl.find owns (ctrl,ssn) with Not_found -> [] in
	    let nvars =
	      List.filter (fun n -> n <> var_name) ovars in
	    Hashtbl.replace owns (ctrl,ssn) nvars
	  );
	  Queue.iter
	    (fun f ->
	       self # schedule_callback ctrl f None
	    )
	    q;
	  true
	with
	  | Not_found ->
	      false


      method private set_value ctrl cid var_name var_value =
	let ssn = cid#socket_service_name in
	try
	  let (_, owner, ro, _, q) = Hashtbl.find variables (ctrl,var_name) in
	  ( match owner with
	      | None -> ()
	      | Some ssn' -> if ssn <> ssn' && ro then raise Not_found
	  );
	  let q' = Queue.create() in
	  Queue.transfer q q';
	  Hashtbl.replace
	    variables (ctrl,var_name) (var_value, owner, ro, true, q);
	  Queue.iter
	    (fun f ->
	       self # schedule_callback ctrl f (Some var_value)
	    )
	    q;
	  true
	with
	  | Not_found ->
	      false


      method private get_value ctrl cid var_name =
	try
	  let (v, _, _, _, _) = Hashtbl.find variables (ctrl,var_name) in
	  Some v
	with
	  | Not_found -> 
	      None

      method private wait_for_value ctrl cid var_name emit =
	try
	  let (v, _, _, is_set, q) = Hashtbl.find variables (ctrl,var_name) in
	  if is_set then
	    emit (Some v)
	  else
	    Queue.push emit q
	with
	  | Not_found -> 
	      emit None


      method private schedule_callback ctrl f arg =
	let g = Unixqueue.new_group ctrl#event_system in
	Unixqueue.once ctrl#event_system g 0.0 (fun () -> f arg)

    end
  )

let () =
  (* Release memory after [fork]: *)
  Netsys_posix.register_post_fork_handler
    (object
       method name = "Netplex_sharedvar"
       method run () = !release()
     end
    )

let create_var ?(own=false) ?(ro=false) var_name =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Sharedvar'V1'create_var'res
    (cont # call_plugin plugin "create_var"
       (Netplex_ctrl_aux._of_Sharedvar'V1'create_var'arg (var_name,own,ro)))
  
let delete_var var_name =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Sharedvar'V1'delete_var'res
    (cont # call_plugin plugin "delete_var"
       (Netplex_ctrl_aux._of_Sharedvar'V1'delete_var'arg var_name))

let set_value var_name var_value =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Sharedvar'V1'set_value'res
    (cont # call_plugin plugin "set_value"
       (Netplex_ctrl_aux._of_Sharedvar'V1'set_value'arg (var_name,var_value)))

let get_value var_name =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Sharedvar'V1'get_value'res
    (cont # call_plugin plugin "get_value"
       (Netplex_ctrl_aux._of_Sharedvar'V1'get_value'arg var_name))

let wait_for_value var_name =
  let cont = Netplex_cenv.self_cont() in
  Netplex_ctrl_aux._to_Sharedvar'V1'wait_for_value'res
    (cont # call_plugin plugin "wait_for_value"
       (Netplex_ctrl_aux._of_Sharedvar'V1'wait_for_value'arg var_name))

let get_lazily var_name f =
  if create_var var_name then (
    let v_opt =
      try Some(f()) with _ -> None in
    ( match v_opt with
	| None -> 
	    let ok = delete_var var_name in assert ok; ()
	| Some v -> 
	    let ok = set_value var_name v in assert ok; ()
    );
    v_opt
  )
  else
    wait_for_value var_name

