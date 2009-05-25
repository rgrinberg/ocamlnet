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
  method global_exception_handler _ = false
end


class virtual processor_base (hooks : processor_hooks) : v_processor =
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

  method virtual process : 
           when_done:(unit -> unit) ->
           container -> Unix.file_descr -> string -> unit
  method virtual supported_ptypes : parallelization_type list
end


let add_helper_service ctrl name hooks =
  let helper_sockserv_cfg =
    ( object
	method name = name
	method protocols = []
	method change_user_to = None
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

