(* $Id$ *)

exception Ssl_error of Ssl.ssl_error

type ssl_socket_state = [ `Unset | `Client | `Server | `Unclean | `Clean ]

class type ssl_multiplex_controller =
object
  inherit Uq_engines.multiplex_controller
  method ssl_socket : Ssl.socket
  method ssl_socket_state : ssl_socket_state
  method ssl_connecting : bool
  method ssl_accepting : bool
  method start_ssl_connecting : 
    when_done:(exn option -> unit) -> unit -> unit
  method start_ssl_accepting :
    when_done:(exn option -> unit) -> unit -> unit
end


class ssl_mplex_ctrl ?(close_inactive_descr=true)
                     fd ssl_sock esys : ssl_multiplex_controller =
  let () = Unix.set_nonblock fd in
object(self)
  val mutable alive = true    (* if false => state in { `Clean, `Unclean } *)
  val mutable read_eof = false
  val mutable wrote_eof = false

  val mutable state = (`Unset : ssl_socket_state)

  val mutable connecting = false   (* true only in state `Unset *)
  val mutable accepting = false    (* true only in state `Unset *)
  val mutable reading = None       (* <> None only in states `Client/`Server *)
  val mutable writing = None       (* <> None only in states `Client/`Server *)
  val mutable shutting_down = None (* <> None only in states `Client/`Server *)
  val mutable disconnecting = None

  val mutable have_handler = false

  val mutable pending = []
         (* list of pending socket operations *)

  val mutable expecting_input = false
  val mutable expecting_output = false

  val group = Unixqueue.new_group esys

  method alive = alive
  method ssl_socket = ssl_sock
  method ssl_socket_state = state

  method ssl_connecting = connecting
  method ssl_accepting = accepting
  method reading = reading <> None
  method writing = writing <> None
  method shutting_down = shutting_down <> None
  method read_eof = read_eof
  method wrote_eof = wrote_eof

  method supports_half_open_connection = false


  method start_ssl_connecting ~when_done () =
    if state <> `Unset then
      failwith "#start_connecting: no longer possible in this state";
    if connecting || accepting then
      failwith "#start_connecting: handshake already in progress";
    self # nonblock_operation
      (ref false)
      `Connecting
      (fun () ->
	 try
	   Ssl.connect ssl_sock;
	   state <- `Client;
	   connecting <- false;
	   (false, false, fun () -> when_done None)
	 with
	   | Ssl.Connection_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl.Connection_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl.Connection_error ssl_err ->
	       state <- `Unclean;
	       connecting <- false;
	       (false, false, fun () -> when_done (Some (Ssl_error ssl_err)))
	   | err ->
	       state <- `Unclean;
	       connecting <- false;
	       (false, false, fun () -> when_done (Some err))
      );
    connecting <- true


  method start_ssl_accepting ~when_done () =
    if state <> `Unset then
      failwith "#start_accepting: no longer possible in this state";
    if connecting || accepting then
      failwith "#start_accepting: handshake already in progress";
    self # nonblock_operation
      (ref false)
      `Accepting
      (fun () ->
	 try
	   Ssl.accept ssl_sock;
	   state <- `Server;
	   accepting <- false;
	   (false, false, fun () -> when_done None)
	 with
	   | Ssl.Accept_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl.Accept_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl.Accept_error ssl_err ->
	       state <- `Unclean;
	       accepting <- false;
	       (false, false, fun () -> when_done (Some (Ssl_error ssl_err)))
	   | err ->
	       state <- `Unclean;
	       accepting <- false;
	       (false, false, fun () -> when_done (Some err);)
      );
    accepting <- true;


  method start_reading ?(peek = fun() -> ()) ~when_done s pos len =
    if pos < 0 || len < 0 || pos + len > String.length s then
      invalid_arg "#start_reading";
    if state <> `Client && state <> `Server then
      failwith "#start_reading: bad state";
    if reading <> None then
      failwith "#start_reading: already reading";
    if shutting_down <> None then
      failwith "#start_reading: already shutting down";
    let cancel_flag = ref false in
    self # nonblock_operation
      cancel_flag
      `Reading
      (fun () ->
	 try
	   (* peek(); *)
	   (* [peek] is used by auth-local. It does not work for SSL. *)
	   let n = Ssl_exts.single_read ssl_sock s pos len in
	   reading <- None;
	   assert(n > 0);
	   (false, false, fun () -> when_done None n)
	 with
	   | Ssl.Read_error Ssl.Error_zero_return ->
	       (* Read EOF *)
	       read_eof <- true;  
	       (* Note: read_eof should be consistent with Ssl.read *)
	       (false, false, fun () -> when_done (Some End_of_file) 0)
	   | Ssl.Read_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl.Read_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl.Read_error ssl_err ->
	       state <- `Unclean;
	       reading <- None;
	       (false, false, fun () -> when_done (Some (Ssl_error ssl_err)) 0)
	   | err ->
	       state <- `Unclean;
	       reading <- None;
	       (false, false, fun () -> when_done (Some err) 0)
      );
    reading <- Some (when_done, cancel_flag)


  method cancel_reading () =
    match reading with
      | None ->
	  ()
      | Some (f_when_done, cancel_flag) ->
	  assert(not !cancel_flag);
	  self # cancel_operation `Reading;
	  cancel_flag := true;
	  reading <- None;
	  f_when_done (Some Uq_engines.Cancelled) 0


  method start_writing ~when_done s pos len =
    if pos < 0 || len < 0 || pos + len > String.length s then
      invalid_arg "#start_writing";
    if state <> `Client && state <> `Server then
      failwith "#start_writing: bad state";
    if writing <> None then
      failwith "#start_writing: already reading";
    if shutting_down <> None then
      failwith "#start_writing: already shutting down";
    if wrote_eof then
      failwith "#start_writing: already past EOF";
    let cancel_flag = ref false in
    self # nonblock_operation
      cancel_flag
      `Writing
      (fun () ->
	 try
	   let n = Ssl_exts.single_write ssl_sock s pos len in
	   writing <- None;
	   (false, false, fun () -> when_done None n)
	 with
	   | Ssl.Write_error Ssl.Error_zero_return ->
	       (false, true, fun () -> ())
	   | Ssl.Write_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl.Write_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl.Write_error ssl_err ->
	       state <- `Unclean;
	       writing <- None;
	       (false, false, fun () -> when_done (Some (Ssl_error ssl_err)) 0)
	   | err ->
	       state <- `Unclean;
	       writing <- None;
	       (false, false, fun () -> when_done (Some err) 0)
      );
    writing <- Some (when_done, cancel_flag)


  method start_writing_eof ~when_done () =
    failwith "#start_writing_eof: operation not supported";
    

  method cancel_writing () =
    match writing with
      | None ->
	  ()
      | Some (f_when_done, cancel_flag) ->
	  assert(not !cancel_flag);
	  self # cancel_operation `Writing;
	  cancel_flag := true;
	  writing <- None;
	  f_when_done (Some Uq_engines.Cancelled) 0


  method start_shutting_down ?(linger = 60.0) ~when_done () =
    if state <> `Client && state <> `Server then
      failwith "#start_shutting_down: bad state";
    if reading <> None || writing <> None then
      failwith "#start_shutting_down: still reading or writing";
    if shutting_down <> None then
      failwith "#start_shutting_down: already shutting down";
    let n = ref 0 in
    let cancel_flag = ref false in
    self # nonblock_operation
      cancel_flag
      `Shutting_down
      (fun () ->
	 try
	   Ssl_exts.single_shutdown ssl_sock;
	   incr n;

	   let (rcvd_shutdown, sent_shutdown) =
	     Ssl_exts.get_shutdown ssl_sock in
	   if rcvd_shutdown then
	     read_eof <- true;
	   if sent_shutdown then
	     wrote_eof <- true;

	   if !n=2 && not (rcvd_shutdown && sent_shutdown) then (
	     (* Unclean crash *)
	     shutting_down <- None;
	     state <- `Unclean;
	     if rcvd_shutdown || sent_shutdown then
	       (false, false, fun () -> when_done None)
	     else
	       (false, false, 
		fun () -> when_done(Some(Failure "Unclean SSL shutdown")))

	   )
	   else
	     match (rcvd_shutdown, sent_shutdown) with
	       | (false, false) ->
		   (* strange *)
		   (false, true, fun () -> ())
	       | (true, false) ->
		   (false, true, fun () -> ())
	       | (false, true) ->
		   (true, false, fun () -> ())
	       | (true, true) ->
		   shutting_down <- None;
		   state <- `Clean;
		   (false, false, fun () -> when_done None)
	 with
	   | Ssl_exts.Shutdown_error Ssl.Error_want_read ->
	       (true, false, fun () -> ())
	   | Ssl_exts.Shutdown_error Ssl.Error_want_write ->
	       (false, true, fun () -> ())
	   | Ssl_exts.Shutdown_error ssl_err ->
	       state <- `Unclean;
	       shutting_down <- None;
	       (false, false, fun () -> when_done (Some (Ssl_error ssl_err)))
	   | err ->
	       state <- `Unclean;
	       shutting_down <- None;
	       (false, false, fun () -> when_done (Some err))
      );
    shutting_down <- Some(when_done, cancel_flag)

  method cancel_shutting_down () =
    match shutting_down with
      | None ->
	  ()
      | Some (f_when_done, cancel_flag) ->
	  assert(not !cancel_flag);
	  self # cancel_operation `Shutting_down;
	  cancel_flag := true;
	  shutting_down <- None;
	  f_when_done (Some Uq_engines.Cancelled)


  method private nonblock_operation cancel_flag tag f =
    Unixqueue.once
      esys
      group
      0.0
      (fun () ->
	 if not !cancel_flag then (
	   let (want_rd, want_wr, action) = f() in
	   if want_rd || want_wr then
	     pending <- (tag, want_rd, want_wr, f) :: pending;
	   ( try
	       action();
	       self # setup_queue();
	     with
	       | error ->
		   self # setup_queue(); raise error
	   )
	 )
      )


  method private cancel_operation tag =
    pending <-
      List.filter (fun (t, _, _, _) -> t <> tag) pending;
    self # setup_queue()


  method private retry_nonblock_operations can_read can_write =
    let cur_pending = pending in
    pending <- [];    (* maybe new operations are added! *)
    let actions = ref [] in
    let pending' =
      List.flatten
	(List.map
	   (fun (tag, want_rd, want_wr, f) ->
	      if (want_rd && can_read) || (want_wr && can_write)  then (
		let (want_rd', want_wr', action) = f() in  (* must not fail! *)
		actions := action :: !actions;
		if want_rd' || want_wr' then
		  [ tag, want_rd', want_wr', f ]   (* try again later *)
		else
		  []
	      )		      
	      else
		[ tag, want_rd, want_wr, f ]   (* just keep *)
	   )
	   cur_pending
	) in
    pending <- pending @ pending';

    (* Be careful: We can only return the first error *)
    let first_error = ref None in
    List.iter
      (fun f ->
	 try f()
	 with
	   | e ->
	       ( match !first_error with
		   | None -> first_error := Some e
		   | Some _ ->
		       Unixqueue.exn_log esys ~suppressed:true 
			 ~label:"Uq_ssl hidden exception" e
			 
	       )
      )
      (List.rev !actions);

    self # setup_queue();

    ( match !first_error with
	| None -> ()
	| Some e -> raise e
    )


  method private setup_queue() =
    if alive then (
      let expecting_input' = 
	List.exists (fun (_, want_rd, _, _) -> want_rd) pending in
      let expecting_output' =
	List.exists (fun (_, _, want_wr, _) -> want_wr) pending in
      
      if expecting_input' || expecting_output' then (
	if not have_handler then (
	  Unixqueue.add_handler esys group (fun _ _ -> self # handle_event);
	  have_handler <- true;
	);
	disconnecting <- None;
      )
      else
	if have_handler && disconnecting = None then (
	  (* It makes only sense to disconnect if all callbacks are cancelled *)
	  if not(accepting || connecting || reading <> None ||
		   writing <> None || shutting_down <> None) then (
	    let wid = Unixqueue.new_wait_id esys in
	    let disconnector = Unixqueue.Wait wid in
	    Unixqueue.add_event esys (Unixqueue.Timeout(group,disconnector));
	    disconnecting <- Some disconnector
	  )
	);
      
      ( match expecting_input, expecting_input' with
	  | (false, true) ->
	      Unixqueue.add_resource esys group (Unixqueue.Wait_in fd, (-1.0))
	  | (true, false) ->
	      Unixqueue.remove_resource esys group (Unixqueue.Wait_in fd)
	  | _ ->
	      ()
      );
	
      ( match expecting_output, expecting_output' with
	  | (false, true) ->
	      Unixqueue.add_resource esys group (Unixqueue.Wait_out fd, (-1.0))
	  | (true, false) ->
	      Unixqueue.remove_resource esys group (Unixqueue.Wait_out fd)
	  | _ ->
	      ()
      );

      expecting_input  <- expecting_input';
      expecting_output <- expecting_output';
    )


  method private handle_event ev =
    match ev with
      | Unixqueue.Input_arrived(g, _) when g = group ->
	  self # retry_nonblock_operations true false

      | Unixqueue.Output_readiness(g, _) when g = group ->
	  self # retry_nonblock_operations false true

      | Unixqueue.Timeout (g, op) when g = group ->
	  ( match disconnecting with
	      | Some op' when op = op' ->
		  disconnecting <- None;
		  have_handler <- false;
		  raise Equeue.Terminate

	      | _ -> raise Equeue.Reject
		  (* Can also be a timeout event from a "once" handler *)
	  )

      | _ ->
	  raise Equeue.Reject


  method inactivate() =
    if alive then (
      alive <- false;
      pending <- [];
      disconnecting <- None;
      have_handler <- false;
      Unixqueue.clear esys group;
      if close_inactive_descr then
	Unix.close fd
    )

  method event_system = esys

end
;;


let create_ssl_multiplex_controller ?close_inactive_descr fd ctx esys =
  let () = Unix.set_nonblock fd in
  let s = Ssl.embed_socket fd ctx in
  let m = Ssl_exts.get_mode s in
  let () = Ssl_exts.set_mode s 
    { m with
	Ssl_exts.enable_partial_write = true; 
	accept_moving_write_buffer = true } in
  new ssl_mplex_ctrl ?close_inactive_descr fd s esys
;;


class ssl_connect_engine (mplex : ssl_multiplex_controller) =
object(self)
  inherit [ unit ] Uq_engines.engine_mixin (`Working 0)

  initializer
    mplex # start_ssl_connecting
      ~when_done:(fun exn_opt ->
		    match exn_opt with
		      | None ->
			  self # set_state (`Done())
		      | Some err ->
			  self # set_state (`Error err)
		 )
      ()

  method event_system = mplex # event_system

  method abort() =
    match self#state with
      | `Working _ ->
	  mplex # inactivate();
	  self # set_state `Aborted
      | _ ->
	  ()

end


let ssl_connect_engine = new ssl_connect_engine


class ssl_accept_engine (mplex : ssl_multiplex_controller) =
object(self)
  inherit [ unit ] Uq_engines.engine_mixin (`Working 0)

  initializer
    mplex # start_ssl_accepting
      ~when_done:(fun exn_opt ->
		    match exn_opt with
		      | None ->
			  self # set_state (`Done())
		      | Some err ->
			  self # set_state (`Error err)
		 )
      ()

  method event_system = mplex # event_system

  method abort() =
    match self#state with
      | `Working _ ->
	  mplex # inactivate();
	  self # set_state `Aborted
      | _ ->
	  ()

end


let ssl_accept_engine = new ssl_accept_engine
