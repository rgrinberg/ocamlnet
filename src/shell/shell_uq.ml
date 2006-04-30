(* $Id: shell_uq.ml 50 2004-10-03 17:06:28Z gerd $ *)

open Shell_sys
open Uq_engines

class type ['t] system_handler_engine_type = object
  inherit ['t] engine
  method system_handler : system_handler
end


class type ['t] job_handler_engine_type = object
  inherit ['t] system_handler_engine_type
  method job : Shell_sys.job
  method job_instance : Shell_sys.job_instance
end


class [ 't ] engine_mixin (init_state : 't engine_state) =
object(self)
  val mutable notify_list = []
  val mutable notify_list_new = []
  val mutable state = init_state
 
  method state = state
 
  method request_notification f =
    notify_list_new <- f :: notify_list_new
     
  method private set_state s =
    if s <> state then (
      state <- s;
      self # notify();
    )
 
  method private notify() =
    notify_list <- notify_list @ notify_list_new;
    notify_list_new <- [];
    notify_list <- List.filter (fun f -> f()) notify_list
end ;;



class system_handler_engine ues : [unit] system_handler_engine_type =
object(self)

  inherit [unit] engine_mixin (`Working 0)

  val mutable group = Unixqueue.new_group ues
  val mutable iv_id = Unixqueue.new_wait_id ues

  val mutable cur_read = []
  val mutable cur_write = []
  val mutable cur_except = []

  val mutable cur_interval = -1.0
  val mutable cur_processes = []
  val mutable cur_wuntraced = false

  val mutable cur_callback = (fun _ -> ())

  initializer
    Unixqueue.add_handler ues group (fun _ _ -> self # handle_event);
    Unixqueue.add_abort_action ues group (fun _ -> self # handle_exception);


  method private sys_register 
                   ?(wuntraced=false) ?(check_interval=0.1) 
		   ?(read=[]) ?(write=[]) ?(except=[]) pl cb =

    (* Get the processes without status: *)
    let pl' =
      List.filter
	(fun process ->
	   try
	     ignore(status process);
	     false                 (* Have status, do not watch *)
	   with
	       Not_found -> true   (* No status yet, so watch process *)
	)
	pl
    in

    cur_callback <- cb;
    cur_wuntraced <- wuntraced;

    (* Update the resources for file descriptors: *)

    let update_res make_op old_list new_list =
      List.iter
	(fun old_descr ->
	   if not(List.mem old_descr new_list) then
	     Unixqueue.remove_resource ues group (make_op old_descr)
	)
	old_list;
      List.iter
	(fun new_descr ->
	   if not(List.mem new_descr old_list) then
	     Unixqueue.add_resource ues group ((make_op new_descr),-1.0)
	)
	new_list
    in

    update_res (fun op -> Unixqueue.Wait_in op)  cur_read read;
    update_res (fun op -> Unixqueue.Wait_out op) cur_write write;
    update_res (fun op -> Unixqueue.Wait_oob op) cur_except except;

    cur_read <- read;
    cur_write <- write;
    cur_except <- except;

    (* Update the timer to watch the processes: *)
    
    let have_timer = 
      if cur_interval >= 0.0 && cur_processes <> [] then
	Some cur_interval
      else
	None in

    let need_timer = 
      if check_interval >= 0.0 && pl' <> [] then
	Some check_interval
      else
	None in

    if have_timer <> need_timer then (
      if have_timer <> None then
	Unixqueue.remove_resource ues group (Unixqueue.Wait iv_id);

      if need_timer <> None then
	Unixqueue.add_resource ues group (Unixqueue.Wait iv_id, 
					  check_interval);

      cur_interval <- check_interval;
    );
    cur_processes <- pl';

    (* No resources? *)
    if cur_read=[] && cur_write=[] && cur_except=[] && cur_processes=[] then (
      self # callback [];
      (* Still nothing to watch? *)
      if cur_read=[] && cur_write=[] && cur_except=[] && cur_processes=[] then
	self # set_state(`Done ())
    );


  method private handle_event ev =
    match ev with
	Unixqueue.Input_arrived(_,fd) -> 
	  if List.mem fd cur_read then
	    self # handle_fd_event (File_read fd)
      | Unixqueue.Output_readiness(_,fd) -> 
	  if List.mem fd cur_write then
	    self # handle_fd_event (File_write fd)
      | Unixqueue.Out_of_band(_,fd) -> 
	  if List.mem fd cur_except then
	    self # handle_fd_event (File_except fd)
      | Unixqueue.Timeout(_,_) ->
	  self # handle_process_event()
      | Unixqueue.Signal ->
	  self # handle_process_event();
	  raise Equeue.Reject
      | _ ->
	  raise Equeue.Reject


  method private handle_exception exn =
    self # set_state (`Error exn)


  method private handle_fd_event ev =
    self # callback [ev];
    self # count();


  method private handle_process_event () =
    (* Check status of processes: *)
    let ev_list = Shell_sys.wait 
		    ~wnohang:true ~wuntraced:cur_wuntraced ~restart:true 
		    cur_processes in
    if ev_list <> [] then (
      self # callback ev_list;
      self # count();
    )


  method private sys_wait() =
    Unixqueue.run ues


  method private count() =
    match state with
	`Working n ->
	  self # set_state (`Working (n+1))
      | _ ->
	  ()


  method private callback evlist =
    if true then (
      try cur_callback evlist 
      with
	  error ->
	    self # set_state(`Error error);
	    Unixqueue.clear ues group;
    )
    else (
      cur_callback evlist
    )


  method abort() =
    match state with
	`Working _ ->
	  self # set_state `Aborted;
	  Unixqueue.clear ues group
      | _ ->
	  ()


  method event_system = ues


  method system_handler =
    { sys_register = self#sys_register;
      sys_wait = self#sys_wait;
    }

end;;


class call_engine ?ignore_error_code ?mode ?stdin ?stdout ?stderr cmds ues =
  let (job, fdlist) = Shell.setup_job ?stdin ?stdout ?stderr cmds in
  let ji = run_job ?mode job in
  let close_fdlist() = List.iter Unix.close fdlist in
  let eng = new system_handler_engine ues in
  let () = register_job eng#system_handler ji in
object(self)
  inherit
    [unit, job_status] map_engine
      ~map_done:(fun _ ->
		   close_fdlist();
		   try
		     Shell.postprocess_job ?ignore_error_code ji;
		     `Done (job_status ji)
		   with
		       Shell.Subprocess_error _ as error ->
			 `Error error
		)
      ~map_error:(fun error ->
		    close_fdlist();
		    Shell_sys.abandon_job ji;
		    `Error error)
      ~map_aborted:(fun () ->
		      close_fdlist();
		      Shell_sys.abandon_job ji;
		      `Aborted)
      eng

  method job = job
  method job_instance = ji
  method system_handler = eng#system_handler

end
;;
