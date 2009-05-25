(* $Id$ *)

open Netplex_types

exception Not_in_container_thread

let cont_of_thread = Hashtbl.create 10

let register_par par =
  if not (Hashtbl.mem cont_of_thread par#ptype) then (
    let (lock, unlock) = par # create_mem_mutex() in
    let m = Hashtbl.create 10 in
    Hashtbl.add cont_of_thread par#ptype (lock, unlock, par, m)
  )
;;


let register_cont cont thread =
  if thread#ptype <> `Controller_attached then (
    let (lock, unlock, par, m) =
      try Hashtbl.find cont_of_thread thread#ptype
      with Not_found -> 
	failwith "Netplex_cenv.register_cont: Unknown parallelizer type" in
    
    lock();
    Hashtbl.replace m thread#sys_id cont;
    unlock()
  )
;;


let unregister_cont cont thread =
  if thread#ptype <> `Controller_attached then (
    let (lock, unlock, par, m) =
      try Hashtbl.find cont_of_thread thread#ptype
      with Not_found -> 
	failwith "Netplex_cenv.unregister_cont: Unknown parallelizer type" in
    
    lock();
    Hashtbl.remove m thread#sys_id;
    unlock()
  )
;;


exception Found of container

let self_cont() =
  (* We do not know the parallelizer, so simply try them one after the other *)
  try
    Hashtbl.iter
      (fun ptype (lock, unlock, par, m) ->
	 let my_sys_id = par # current_sys_id in
	 lock();
	 try
	   let cont = Hashtbl.find m my_sys_id in
	   unlock();
	   raise(Found cont)
	 with
	   | Not_found ->
	       unlock();
      )
      cont_of_thread;
    raise Not_in_container_thread
  with
    | Found cont -> cont
;;


let log level msg =
  let cont = self_cont() in
  cont # log level msg

let logf level fmt =
  Printf.ksprintf (log level) fmt

type timer = < f : timer -> bool; tmo : float; cont : container > ;;

let timer_table = Hashtbl.create 50
let timer_mutex = ( !Netsys_oothr.provider ) # create_mutex()


let cancel_timer_int do_clear tobj =
  let cont = self_cont() in
  let esys = cont#event_system in
  timer_mutex # lock();
  let g_opt =
    try Some(Hashtbl.find timer_table tobj) with Not_found -> None in
  Hashtbl.remove timer_table tobj;
  timer_mutex # unlock();
  if do_clear then
    match g_opt with
      | None -> ()
      | Some g -> 
	  Unixqueue.clear esys g


let cancel_timer = cancel_timer_int true

let rec restart_timer tobj g =
  let cont = self_cont() in
  let esys = cont#event_system in
  timer_mutex # lock();
  Hashtbl.add timer_table tobj g;
  timer_mutex # unlock();
  Unixqueue.once esys g tobj#tmo 
    (fun () ->
       cancel_timer_int false tobj;
       (* We let exceptions fall through to Netplex_container.run *)
       let flag = tobj#f tobj in
       if flag then restart_timer tobj g
    )


let create_timer f tmo =
  let cont = self_cont() in
  let esys = cont#event_system in
  let g = Unixqueue.new_group esys in
  let tobj =
    ( object
	method f = f
	method tmo = tmo
	method cont = cont
      end
    ) in
  restart_timer tobj g;
  tobj
  

let cancel_all_timers() =
  let cont = self_cont() in
  let esys = cont#event_system in
  timer_mutex # lock();
  Hashtbl.iter
    (fun tobj g ->
       if tobj # cont = cont then
	 Unixqueue.clear esys g
    )
    timer_table;
  Hashtbl.clear timer_table;
  timer_mutex # unlock()


let timer_id tobj =
  Oo.id tobj


exception Container_variable_not_found of string
exception Container_variable_type_mismatch of string

let get_var name =
  let cont = self_cont() in
  try cont # var name 
  with Not_found -> raise(Container_variable_not_found name)

let int_var name =
  match get_var name with
    | `Int i -> i
    | _ -> raise(Container_variable_type_mismatch name)

let string_var name =
  match get_var name with
    | `String i -> i
    | _ -> raise(Container_variable_type_mismatch name)

let float_var name =
  match get_var name with
    | `Float i -> i
    | _ -> raise(Container_variable_type_mismatch name)

let bool_var name =
  match get_var name with
    | `Bool i -> i
    | _ -> raise(Container_variable_type_mismatch name)

let set_int_var name i =
  let cont = self_cont() in
  cont # set_var name (`Int i)

let set_string_var name i =
  let cont = self_cont() in
  cont # set_var name (`String i)

let set_float_var name i =
  let cont = self_cont() in
  cont # set_var name (`Float i)

let set_bool_var name i =
  let cont = self_cont() in
  cont # set_var name (`Bool i)

let make_var_type to_exn from_exn =
  let get name =
    match get_var name with
      | `Any x ->
	  ( try from_exn x
	    with
	      | Not_found
	      | Match_failure(_,_,_) -> 
		  raise(Container_variable_type_mismatch name)
	  )
      | _ ->
	  raise(Container_variable_type_mismatch name) in
  let set name x =
    let cont = self_cont() in
    cont # set_var name (`Any (to_exn x)) in
  (get, set)

module type TYPE = sig type t end

module type VAR_TYPE = sig
  type t 
  val get : string -> t
  val set : string -> t -> unit
end

module Make_var_type(T:TYPE) = struct
  type t = T.t
  exception X of t
  let (get, set) =
    make_var_type
      (fun x -> X x)
      (function (X x) -> x | _ -> raise Not_found)
end


let admin_connector() =
  let cont = self_cont() in
  match cont#lookup "netplex.controller" "admin" with
    | None ->
	failwith "Netplex_cenv.admin_connector: Socket not found"
    | Some path ->
	`Socket(Rpc.Tcp,
		Rpc_client.Unix path,
		Rpc_client.default_socket_config)

let admin_call f =
  let conn = admin_connector() in
  let client = Netplex_ctrl_clnt.Admin.V2.create_client2 conn in
  try
    f client ();
    Rpc_client.shut_down client
  with
    | err ->
	Rpc_client.shut_down client;
	raise err

let system_shutdown() =
  admin_call Netplex_ctrl_clnt.Admin.V2.system_shutdown

let system_restart() =
  admin_call Netplex_ctrl_clnt.Admin.V2.restart_all

let run_in_controller_context ctrl f =
  let cont = self_cont() in
  if cont#ptype <> `Multi_threading then
    failwith "Netplex_cenv.run_in_controller_context: only possible for multi-threaded environments";
  let mutex = !Netsys_oothr.provider # create_mutex() in
  let cond = !Netsys_oothr.provider # create_condition() in
  let esys = ctrl # event_system in
  let g = Unixqueue.new_group esys in
  let r = ref (fun () -> assert false) in
  Unixqueue.once esys g 0.0
    (fun () ->
       ( try
	   f();
	   mutex # lock();
	   r := (fun () -> ());
	   mutex # unlock();
	 with
	   | e -> 
	       mutex # lock();
	       r := (fun () -> raise e);
	       mutex # unlock();
       );
       cond # signal()
    );
  mutex # lock();
  cond # wait mutex;
  mutex # unlock();
  !r()
