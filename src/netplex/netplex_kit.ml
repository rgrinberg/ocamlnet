(* $Id$ *)

open Netplex_types


class type virtual v_processor =
object
  inherit processor_hooks

  method virtual process : 
           when_done:(unit -> unit) ->
           container -> Unix.file_descr -> string -> unit
  method virtual supported_ptypes : parallelization_type list
end


class empty_processor_hooks() : processor_hooks =
object
  method post_add_hook _ _ = ()
  method post_rm_hook _ _ = ()
  method pre_start_hook _ _ _ = ()
  method post_start_hook _ = ()
  method pre_finish_hook _ = ()
  method post_finish_hook _ _ _ = ()
  method receive_message _ _ _ = ()
  method receive_admin_message _ _ _ = ()
  method shutdown () = ()
  method system_shutdown () = ()
  method global_exception_handler _ = true
    (* i.e. we continue running by default *)
end


class processor_hooks_delegation (hooks : processor_hooks) : processor_hooks =
object(self)
  method post_add_hook socksrv ctrl = 
    hooks # post_add_hook socksrv ctrl
  method post_rm_hook socksrv ctrl = 
    hooks # post_rm_hook socksrv ctrl
  method pre_start_hook socksrv ctrl cont = 
    hooks # pre_start_hook socksrv ctrl cont
  method post_start_hook cont =
    hooks # post_start_hook cont
  method pre_finish_hook cont =
    hooks # pre_finish_hook cont
  method post_finish_hook socksrv ctrl cont = 
    hooks # post_finish_hook socksrv ctrl cont
  method receive_message cont cmd cmdargs =
    hooks # receive_message cont cmd cmdargs
  method receive_admin_message cont cmd cmdargs =
    hooks # receive_admin_message cont cmd cmdargs
  method shutdown () = 
    hooks # shutdown()
  method system_shutdown () = 
    hooks # system_shutdown()
  method global_exception_handler e = 
    hooks # global_exception_handler e
end


class virtual processor_base (hooks : processor_hooks) : v_processor =
object(self)
  inherit processor_hooks_delegation hooks

  method virtual process : 
           when_done:(unit -> unit) ->
           container -> Unix.file_descr -> string -> unit
  method virtual supported_ptypes : parallelization_type list
end


module PTYPE = struct
  type t = parallelization_type
  let compare (x:t) (y:t) = Pervasives.compare x y
end

module PSet = Set.Make(PTYPE)

let to_set l =
  List.fold_left
    (fun acc x -> PSet.add x acc) PSet.empty l

let of_set s =
  PSet.fold
    (fun x acc -> x :: acc) s []


class protocol_switch_processor
         (merge_list : (string * processor) list) : processor =
  let r_merge_list = List.rev merge_list in
  let supported_ptypes =
    match merge_list with
      | (_,hook1) :: mtl ->
	  of_set
	    (List.fold_left
	       (fun acc (_,hooks) ->
		  PSet.inter (to_set hooks#supported_ptypes) acc)
	       (to_set hook1#supported_ptypes)
	       mtl)
      | [] ->
	  failwith "Netplex_kit.protocol_switch: list is empty" in

object(self)
  method process ~when_done container fd proto_name =
    let p_opt =
      try Some(List.assoc proto_name merge_list) with Not_found -> None in
    match p_opt with
      | Some p ->
	  p # process ~when_done container fd proto_name
      | None ->
	  failwith("Netplex_kit.protocol_switch: Unknown protocol: " ^
		     proto_name)

  method supported_ptypes =
    supported_ptypes

  method post_add_hook socksrv ctrl = 
    List.iter (fun (_,hooks) -> hooks # post_add_hook socksrv ctrl) merge_list
  method post_rm_hook socksrv ctrl = 
    List.iter (fun (_,hooks) -> hooks # post_rm_hook socksrv ctrl) r_merge_list
  method pre_start_hook socksrv ctrl cont = 
    List.iter
      (fun (_,hooks) -> hooks # pre_start_hook socksrv ctrl cont) merge_list
  method post_start_hook cont =
    List.iter (fun (_,hooks) -> hooks # post_start_hook cont) merge_list
  method pre_finish_hook cont =
    List.iter (fun (_,hooks) -> hooks # pre_finish_hook cont) r_merge_list
  method post_finish_hook socksrv ctrl cont = 
    List.iter
      (fun (_,hooks) -> hooks # post_finish_hook socksrv ctrl cont) 
      r_merge_list
  method receive_message cont cmd cmdargs =
    List.iter
      (fun (_,hooks) -> hooks # receive_message cont cmd cmdargs) merge_list
  method receive_admin_message cont cmd cmdargs =
    List.iter
      (fun (_,hooks) -> hooks # receive_admin_message cont cmd cmdargs) 
      merge_list
  method shutdown () = 
    List.iter (fun (_,hooks) -> hooks # shutdown()) merge_list
  method system_shutdown () = 
    List.iter (fun (_,hooks) -> hooks # system_shutdown()) merge_list
  method global_exception_handler e = 
    List.for_all
      (fun (_,hooks) -> hooks # global_exception_handler e) merge_list

end


class protocol_switch_factory 
         name (merge_list : (string*processor_factory) list)
       : processor_factory =
object(self)
  method name = name
  method create_processor ctrl_cfg cfg addr =
    let named_processors =
      List.map
	(fun (proto_name, fac) ->
	   let sub_addrs =
	     cfg # resolve_section addr proto_name in
	   match sub_addrs with
	     | [ sub_addr ] ->
		 let p = fac # create_processor ctrl_cfg cfg sub_addr in
		 (proto_name, p)
	     | [] ->
		 failwith("Missing section: " ^ cfg#print addr ^ "." ^ 
			    proto_name)
	     | _ ->
		 failwith("Section must only occur once: " ^ 
			    cfg#print addr ^ "." ^ proto_name)
	) 
	merge_list in
    new protocol_switch_processor named_processors
end


let add_helper_service ctrl name hooks =
  let helper_sockserv_cfg =
    ( object
	method name = name
	method protocols = []
	method change_user_to = None
	method startup_timeout = (-1.0)
	method controller_config = ctrl#controller_config
      end
    ) in
  let helper_processor =
    ( object
	inherit processor_base hooks
	method process ~when_done _ _ _ = assert false  (* never called *)
	method supported_ptypes = [ `Multi_processing; `Multi_threading ]
      end
    ) in
  let helper_service = 
    Netplex_sockserv.create_socket_service 
      helper_processor helper_sockserv_cfg in
  let helper_wload_mng =
    Netplex_workload.create_constant_workload_manager 1 in
  ctrl # add_service helper_service helper_wload_mng

