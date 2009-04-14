(* $Id$
 *
 *)

(*
 * Copyright 2005 Baretta s.r.l. and Gerd Stolpmann
 *
 * This file is part of Nethttpd.
 *
 * Nethttpd is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Nethttpd is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with WDialog; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)


open Nethttp
open Nethttp.Header
open Nethttpd_types
open Nethttpd_kernel

type engine_req_state =
    [ `Received_header
    | `Receiving_body
    | `Received_request 
    | `Finishing
    ]


class type http_engine_config =
object
  inherit Nethttpd_reactor.http_processor_config
  method config_input_flow_control : bool
  method config_output_flow_control : bool
end


class type extended_async_environment =
object
  inherit extended_environment
  method input_ch_async : Uq_engines.async_in_channel
  method output_ch_async : Uq_engines.async_out_channel
end


class type http_request_header_notification =
object
  method req_state : engine_req_state
  method environment : extended_async_environment
  method schedule_accept_body : on_request:(http_request_notification -> unit) ->
                               ?on_error:(unit -> unit) -> unit -> unit
  method schedule_reject_body : on_request:(http_request_notification -> unit) ->
                               ?on_error:(unit -> unit) -> unit -> unit
  method schedule_finish : unit -> unit
end

and http_request_notification =
object
  method req_state : engine_req_state
  method environment : extended_async_environment
  method schedule_finish : unit -> unit
end


(* TODO: Export Uq_engines.engine_mixin *)
class [ 't ] engine_mixin (init_state : 't Uq_engines.engine_state) =
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


type conn_state =
    [ `Active of http_protocol
    | `Closing of lingering_close
    | `Closed
    ]


exception Ev_output_filled of Unixqueue.group
  (** Event: The output channel filled data into the [http_response] object, and
    * notifies now the [http_engine] 
   *)

exception Ev_output_empty of ((unit -> unit) * Unixqueue.group)
  (** Event: The [http_response] object just got empty, and the function must
    * be called for further notification.
   *)

exception Ev_input_empty of Unixqueue.group
  (** Event: The input channel became empty, and the engine must be notified 
    * (used for input flow control)
   *)


class http_engine_input config ues group in_cnt =
object(self)
  (* The input channel is fed with data by the main event handler that invokes
   * [add_data] and [add_eof] to forward new input data to this channel.
   *)

  val mutable front = None
  val mutable data_queue = Queue.create()
  val mutable eof = false
  val mutable pos_in = 0
  val mutable closed = false
  val mutable locked = true
  val mutable aborted = false
  val mutable notify_list = []
  val mutable notify_list_new = []


  method input s spos slen =
    if closed then raise Netchannels.Closed_channel;
    if locked then failwith "Nethttpd_engine: channel is locked";
    if aborted then failwith "Nethttpd_engine: channel aborted";
    (match front with
       | None ->
	   ( try
	       front <- Some(Queue.take data_queue)
	     with
		 Queue.Empty -> ()
	   )
       | Some _ -> ()
    );
    (match front with
       | None ->
	   if eof then 
	     raise End_of_file
	   else
	     0    (* buffer underrun *)
       | Some (u,upos,ulen) ->
	   let len = min slen ulen in
	   String.blit u upos s spos len;
	   if len = ulen then
	     front <- None
	   else
	     front <- Some(u,upos+len,ulen-len);
	   pos_in <- pos_in + len;
	   if not (self # can_input) then (
	     if config#config_input_flow_control then
	       Unixqueue.add_event ues (Unixqueue.Extra(Ev_input_empty group));
	     self # notify();
	   );
	   len
    )

  method pos_in =
    pos_in

  method close_in() =
    if closed then raise Netchannels.Closed_channel;
    if locked then failwith "Nethttpd_engine: channel is locked";
    front <- None;
    Queue.clear data_queue;
    closed <- true

  method can_input =
    not closed && (front <> None || not(Queue.is_empty data_queue) || eof)

  method request_notification f =
    notify_list_new <- f :: notify_list_new

  method private notify() =
    notify_list <- notify_list @ notify_list_new;
    notify_list_new <- [];
    notify_list <- List.filter (fun f -> f()) notify_list

  method add_data ((_,_,len) as data_chunk) =
    assert(len > 0);
    if not eof then (
      let old_can_input = self # can_input in
      Queue.push data_chunk data_queue;
      in_cnt := Int64.add !in_cnt (Int64.of_int len);
      if not old_can_input && not closed then self # notify()
    )
      (* else: we are probably dropping all data! *)

  method add_eof() =
    if not eof then (
      let old_can_input = self # can_input in
      eof <- true;
      if not old_can_input && not closed then self # notify()
    )
    
  method unlock() =
    locked <- false

  method drop() =
    locked <- false;
    eof <- true

  method abort() =
    aborted <- true

end


class http_engine_output config ues group resp (f_access : unit->unit) =
object(self)
  (* The output channel adds the incoming data to the [http_response] object
   * [resp]. The main [http_engine] is notified about new data by a
   * [Ev_output_filled] event. This gives the [http_engine] has the chance to
   * check whether it again enables output because new data is to be output.
   *
   * Note that [resp] is setup such that it calls [resp_notify] whenever the state
   * of [resp] changes, or the [resp] queue becomes empty. We do not immediately
   * forward this notification, but generate another event [Ev_output_empty].
   * When the event is processed, [notify] is finally called. This indirection
   * is necessary because the moment of the [resp_notify] invocation is quite
   * hairy, and failures must not be risked.
   *)

  val mutable pos_out = 0
  val mutable closed = false
  val mutable locked = true
  val mutable aborted = false
  val mutable notify_list = []
  val mutable notify_list_new = []

  initializer (
    resp # set_callback self#resp_notify
  )

  method output s spos slen =
    (* In principle we always accept any amount of output data. For practical reasons,
     * the length of an individual chunk is limited to 8K - it just prevents
     * some dumb programming errors.
     *)
    if closed then raise Netchannels.Closed_channel;
    if locked then failwith "Nethttpd_engine: channel is locked";
    if aborted then failwith "Nethttpd_engine: channel aborted";
    let len = min slen 8192 in
    if len > 0 then (
      let old_can_output = self # can_output in    
      let u = String.sub s spos len in
      resp # send (`Resp_body(u,0,String.length u));
      Unixqueue.add_event ues (Unixqueue.Extra(Ev_output_filled group));
      (* CHECK: Maybe we should also add a mechanism to detect duplicate events,
       * just to be sure that the event queue does not get jammed.
       *)
      pos_out <- pos_out + len;
      if old_can_output <> self # can_output then self # notify();
    );
    len

  method pos_out =
    pos_out

  method flush() =
    ()

  method close_out() =
    if closed then raise Netchannels.Closed_channel;
    if locked then failwith "Nethttpd_engine: channel is locked";
    let old_can_output = self # can_output in
    resp # send `Resp_end;
    closed <- true;
    f_access();
    Unixqueue.add_event ues (Unixqueue.Extra(Ev_output_filled group));
    if old_can_output <> self # can_output then self # notify();

  method close_after_send_file() =
    closed <- true;
    f_access();
    
  method can_output =
    (not config#config_output_flow_control) ||
    ((resp # state = `Active) && resp # send_queue_empty)

  method unlock() =
    locked <- false

  method resp_notify() =
    Unixqueue.add_event ues (Unixqueue.Extra(Ev_output_empty(self # notify, group)));
    (* CHECK: It is assumed that this event arrives before it is again invalid.
     * If this turns out to be wrong, we have to implement a quicker way of
     * notification. It is known that [resp_notify] is called back from the
     * current [cycle]. One could check after every [cycle] whether the
     * notification is still valid.
     *)
    
  method request_notification f =
    notify_list_new <- f :: notify_list_new

  method private notify() =
    notify_list <- notify_list @ notify_list_new;
    notify_list_new <- [];
    notify_list <- List.filter (fun f -> f()) notify_list

  method abort() =
    aborted <- true

end


class http_async_environment config ues group
                             ((req_meth, req_uri), req_version) req_hdr 
                             fd_addr peer_addr
                             in_ch_async in_cnt out_ch_async resp reqrej =
  let in_ch = 
    Netchannels.lift_in ~buffered:false 
                        (`Raw (in_ch_async :> Netchannels.raw_in_channel)) in

  let out_ch =
    Netchannels.lift_out ~buffered:false
                         (`Raw (out_ch_async :> Netchannels.raw_out_channel)) in

  (* [in_ch] and [out_ch] are standard channels corresponding to [in_ch_async] and
   * [out_ch_async]. Note that there is no buffering. A buffer would introduce
   * a delay between the standard and the asynchronous channels - a very surprising
   * effect. Furthermore, there is already a lot of buffering in [http_protocol],
   * so buffers are probably not needed here.
   *)

object (self)
  inherit Nethttpd_reactor.http_environment 
                             config
                             req_meth req_uri req_version req_hdr 
  			     fd_addr peer_addr
                             in_ch in_cnt out_ch resp 
			     out_ch_async#close_after_send_file reqrej
			     as super

  method input_ch_async = (in_ch_async : Uq_engines.async_in_channel)
  method output_ch_async = (out_ch_async :> Uq_engines.async_out_channel)

  method send_output_header() =
    super # send_output_header();
    Unixqueue.add_event ues (Unixqueue.Extra(Ev_output_filled group));

  method send_file fd length =
    super # send_file fd length;
      (* does not block, because [synch] is now [ fun () -> () ] *)
    Unixqueue.add_event ues (Unixqueue.Extra(Ev_output_filled group));
    

end


class http_request_manager config ues group req_line req_hdr expect_100_continue 
                           fd_addr peer_addr resp =
  let f_access = ref (fun () -> ()) in  (* set below *)
  let in_cnt = ref 0L in
  let reqrej = ref false in

  let in_ch = new http_engine_input config ues group in_cnt in
  let out_ch = new http_engine_output config ues group resp 
                 (fun () -> !f_access()) in

  let env = new http_async_environment 
	      config ues group req_line req_hdr fd_addr peer_addr 
	      (in_ch :> Uq_engines.async_in_channel) in_cnt
	      out_ch resp reqrej in
     (* may raise Standard_response! *)

  let () =
    f_access := env # log_access in


object(self)
  (* This class also satisfies the types [http_request_notification] and
   * [http_request_header_notification]
   *)

  val mutable req_state = ( `Received_header : engine_req_state )
    (* When this object is created, the header has just been received *)  

  val mutable req_handler = (fun _ -> failwith "Nethttpd_engine: No [on_request] function")

  val mutable error_handler = (fun () -> ())

  method real_input_ch = in_ch    (* full type! *)
  method real_output_ch = out_ch  (* full type! *)
  method environment = (env :> extended_async_environment)
  method req_state = req_state
  method set_req_state s = req_state <- s
  method req_handler = (req_handler : http_request_notification -> unit)
  method error_handler = error_handler

  method log_access = env#log_access

  method abort() =
    in_ch # abort();
    out_ch # abort();

  method schedule_accept_body ~on_request ?(on_error = fun ()->()) () =
    (* Send the "100 Continue" response if requested: *)
    if expect_100_continue then (
      resp # send resp_100_continue;
      Unixqueue.add_event ues (Unixqueue.Extra(Ev_output_filled group));
    );
    (* Unlock everything: *)
    in_ch # unlock();
    out_ch # unlock();
    env # unlock();
    (* Remember the callback functions: *)
    req_handler <- on_request;
    error_handler <- on_error
      

  method schedule_reject_body ~on_request ?(on_error = fun ()->()) () =
    (* Unlock everything: *)
    in_ch # drop();
    out_ch # unlock();
    env # unlock();
    reqrej := true;
    (* Remember the callback functions: *)
    req_handler <- on_request;
    error_handler <- on_error

  method schedule_finish() =
    (* This is quite tricky:
     * - Any remaining data in the input channel is dropped. The [drop] method
     *   does this. This has also the effect that any additional data still
     *   arriving is thrown away.
     * - We have to check the output state for the response. If it is still `Start,
     *   the whole response is missing. We generate a "Server Error" in this case.
     *   Otherwise we just close the output channel and hope we are done.
     * - We also set [req_state] to `Finishing to inform all other parts of the
     *   engine what is going on.
     *)
    in_ch # drop();
    out_ch # unlock();
    env # unlock();
    req_state <- `Finishing;
    match env # output_state with
      | `Start ->
	  (* The whole response is missing! Generate a "Server Error": *)
	  output_std_response config env `Internal_server_error None
	    (Some "Nethttpd: Missing response, replying 'Server Error'");
	  env # set_output_state `End;
      | `Sent_header
      | `Sending_body ->
	  (* The response body is probably incomplete or missing. Try to close
	   * the channel.
	   *)
	  ( try env # output_ch # close_out() with Netchannels.Closed_channel -> () );
	  env # set_output_state `End;
      | `Sent_body 
      | `End ->
	  (* Everything ok, just to be sure... *)
	  ( try env # output_ch # close_out() with Netchannels.Closed_channel -> () );
	  env # set_output_state `End;
      | _ ->
	  (* These states must not happen! *)
	  assert false
end


let ensure_not_reentrant name lock f arg =
  if !lock then (
    prerr_endline ("Illegal reentrancy: " ^ name);
    assert false
  );
  lock := true;
  try 
    let r = f arg in
    lock := false;
    r
  with err ->
    lock := false;
    raise err


class http_engine ~on_request_header () config fd ues =
  let _proto = new http_protocol config fd in
  let handle_event_lock = ref false in
object(self)
  inherit [unit] engine_mixin (`Working 0)

  val fd_addr = Unix.getsockname fd
  val peer_addr = Unix.getpeername fd

  val mutable conn_state = (`Active _proto : conn_state)
  val mutable group = Unixqueue.new_group ues

  val mutable enable_in = false
  val mutable in_timeout = 0.0

  val mutable enable_out = false
  (* - When to listen for input events? In principle, when [proto # do_input]
   *   indicates this. This flag can only change after every [proto # cycle].
   * - When to listen for output events? In principle, when [proto # do_output]
   *   indicates this. This flag can change after [proto # cycle], but also
   *   after new data have been output. Our output channel will tell us, and
   *   sends [Ev_output_filled] or [Ev_output_empty] events to us.
   *)

  val mutable cur_request_manager = None
  val mutable eof_seen = false

  initializer 
    self # start()

  method private start() =
    Unixqueue.add_handler ues group (fun _ _ -> self # handle_event);
    Unixqueue.add_abort_action ues group (fun _ -> self # handle_exception);
    self # enable_input true

  method private enable_input flag =
    let timeout =
      match conn_state with
	| `Active proto ->
	    ( match proto # input_timeout_class with
		| `None         -> (-1.0)
		| `Normal       -> config#config_timeout
		| `Next_message -> config#config_timeout_next_request
	    )
	| `Closing lc ->
	    1.0  (* i.e. check every second whether the lingering phase is over *)
	| `Closed ->
	    assert false 
    in
    ( match (flag, enable_in) with
	| (true, false) ->
	    Unixqueue.add_resource ues group (Unixqueue.Wait_in fd, timeout);
	| (false, true) ->
	    Unixqueue.remove_resource ues group (Unixqueue.Wait_in fd);
	| (true, true) when timeout <> in_timeout ->
	    Unixqueue.remove_resource ues group (Unixqueue.Wait_in fd);
	    Unixqueue.add_resource ues group (Unixqueue.Wait_in fd, timeout);
	| _ -> ()
    );
    enable_in <- flag;
    in_timeout <- timeout;

  method private enable_output flag =
    if flag && not enable_out then
      Unixqueue.add_resource ues group (Unixqueue.Wait_out fd, config#config_timeout);
    if not flag && enable_out then
      Unixqueue.remove_resource ues group (Unixqueue.Wait_out fd);
    enable_out <- flag

  method private handle_event ev =
    ensure_not_reentrant
      "Nethttpd_engine.http_engine#handle_event"
      handle_event_lock
      (fun () ->
	 match ev with
	   | Unixqueue.Input_arrived(g,_) when g = group ->
	       (* Input data from the HTTP client *)
	       ( match conn_state with
		   | `Active proto -> self # cycle_proto proto
		   | `Closing lc   -> self # cycle_lc lc
		   | `Closed       -> ()   (* drop this event *)
	       );
	       self # count()
	   | Unixqueue.Output_readiness(g,_) when g = group ->
	       (* The HTTP client accepts new data *)
	       ( match conn_state with
		   | `Active proto -> self # cycle_proto proto
		   | `Closing lc   -> ()  (* Strange. Ignore for now. *)
		   | `Closed       -> ()  (* drop this event *)
	       );
	       self # count()
	   | Unixqueue.Timeout(g,_) when g = group ->
	       (* Register a timeout *)
	       ( match conn_state with
		   | `Active proto -> self # timeout_proto proto
		   | `Closing lc   -> self # cycle_lc lc
		   | `Closed       -> ()  (* drop this event *)
	       )
	   | Unixqueue.Extra (Ev_output_filled g) when g = group ->
	       (* The output channel is filled with fresh data *)
	       ( match conn_state with
		   | `Active proto -> (* Check whether to enable output now: *)
		       self # enable_output proto#do_output
		   | `Closing lc   -> ()  (* drop this event *)
		   | `Closed       -> ()  (* drop this event *)
	       )
	   | Unixqueue.Extra (Ev_output_empty(f,g)) when g = group ->
	       (* The current response object is (probably?) empty. Notify [f] *)
	       ( match conn_state with
		   | `Active proto -> f()
		   | `Closing lc   -> ()  (* drop this event *)
		   | `Closed       -> ()  (* drop this event *)
	       )
	   | _ ->
	       raise Equeue.Reject   (* Other engines may want to see this event *)
      )
      ()

  method private count() =
    match state with
      | `Working n -> self # set_state(`Working(n+1))
      | _ -> ()

  method private cycle_proto proto =
    (* Do a HTTP protocol cycle, and check whether I/O is still enabled *)
    proto # cycle();   (* do not block! *)
    self # goon_proto proto

  method private timeout_proto proto =
    (* Either input or output has timed out. *)
    proto # timeout();
    self # goon_proto proto

  method private goon_proto proto =
    let enabled_input_flow =
      (not config#config_input_flow_control) || (
	(* Input flow control: We stop reading from the descriptor when there are
	 * unprocessed request tokens.
	 *)
	match cur_request_manager with
	  | None ->
	      true
		(* CHECK: This might be nonsense. It is possible that in the
		 * last request manager the input channel has unprocessed data.
		 * Don't know how to handle this. (I don't think it is possible
		 * to attack the server because of this issue, because the
		 * pipeline length option limits the number of unresponded
		 * requests anyway.)
		 *)
	  | Some rm ->
	      (* If the input channel is empty, we enable input, and vice versa: *)
	      not(rm # environment # input_ch_async # can_input)
      )
    in
    self # forward_input_tokens proto;
    self # enable_input (enabled_input_flow && proto#do_input);
    self # enable_output proto#do_output;
    (* Check whether the HTTP connection is processed and can be closed: *)
    if eof_seen && not proto#do_output then (
      if proto # need_linger then (
	let lc = new lingering_close fd in
	conn_state <- `Closing lc;
	self # enable_input true;
	self # enable_output false;
      )
      else (
	(* Just close the descriptor and shut down the engine: *)
	conn_state <- `Closed;
	Unix.close fd;
	Unixqueue.clear ues group;    (* Stop in Unixqueue terms *)
	self # set_state (`Done());   (* Report the new state *)
      )
    )

  method private forward_input_tokens proto =
    (* Interpret all available input tokens: *)
    while proto # recv_queue_len > 0 do
      match proto # receive() with
	| `Req_header(req_line, req_hdr, resp) ->
	    (* The next request starts. *)
	    assert(cur_request_manager = None);
	    let expect_100_continue =
	      try
		proto # peek_recv() = `Req_expect_100_continue
	      with
		  Recv_queue_empty -> false in
	    if expect_100_continue then
	      ignore(proto # receive());

	    let f_access = ref (fun () -> ()) in

	    ( try
		let rm = new http_request_manager    (* or Standard_response *)
			   config ues group
			   req_line req_hdr expect_100_continue 
			   fd_addr peer_addr resp in
	    
		f_access := rm # log_access;
		cur_request_manager <- Some rm;
	    
		(* Notify the user that we have received the header: *)
		on_request_header (rm :> http_request_header_notification);
  	           (* Note: the callback may raise an arbitrary exception *)
	      with
		| Standard_response(status, hdr_opt, msg_opt) ->
		    (* Probably a problem when decoding a header field! *)
                    ( match msg_opt with
			| Some msg ->
			    let (req_meth, req_uri) = fst req_line in
                                config # config_log_error
                                  (Some fd_addr) (Some peer_addr) (Some(req_meth,req_uri))
                                  (Some req_hdr) msg
			| None -> ()
                    );
                    (* CHECK: Also log to access log? *)
                    let body = config # config_error_response 400 in
		  Nethttpd_kernel.send_static_response resp status hdr_opt body;
		  Unixqueue.add_event ues (Unixqueue.Extra(Ev_output_filled group));
		  (* Now [cur_request_manager = None]. This has the effect that 
		   * all further input tokens for this request are silently
		   * dropped.
		   *)
	    )
	    
	| `Req_expect_100_continue ->
	    assert false   (* already processed *)

	| `Req_body data_chunk ->
	    (* Just forward data to the current request manager: *)
	    ( match cur_request_manager with
		| Some rm ->
		    if rm # req_state <> `Finishing then
		      rm # set_req_state `Receiving_body;
		    rm # real_input_ch # add_data data_chunk
		| None -> ()  (* drop it *)
	    )

	| `Req_trailer _ ->
	    (* Don't know what to do with the trailer. *)
	    ()

	| `Req_end ->
	    (* Just forward this event to the current request manager: *)
	    ( match cur_request_manager with
		| Some rm ->
		    cur_request_manager <- None;
		    ( match rm # req_state with
			| `Finishing ->
			    (* The request has been dropped, so call the error
			     * handler.
			     *)
			    rm # error_handler()
			| _ ->
			    (* Normal end of request: *)
			    rm # set_req_state `Received_request;
			    rm # real_input_ch # add_eof ();
			    (* Now call the function given by the [on_request] argument: *)
			    rm # req_handler (rm :> http_request_notification);
		            (* Note: the callback may raise an arbitrary exception *)
		    )
		| None -> ()   (* drop *)
	    );

	| `Eof ->
	    (* If there is still a request manager, finish the current request. *)
	    ( match cur_request_manager with
		| Some rm ->
		    cur_request_manager <- None;
		    ( match rm # req_state with
			| `Received_request -> 
			    (* This is impossible! *)
			    assert false
			| `Finishing ->
			    (* The current request has not been arrived completely.
			     * It has been dropped, so call the error handler.
			     *)
			    rm # error_handler()
			| _ ->
			    (* Same as before, but the request was not yet properly
			     * finished.
			     *)
			    rm # schedule_finish();
			    rm # error_handler()
		    );
		| None -> ()
	    );
	    (* Record this event. Will be checked in [cylce_proto] *)
	    eof_seen <- true

	| `Fatal_error e ->
	    (* The connection is already down. Just log the incident: *)
	    let msg = Nethttpd_kernel.string_of_fatal_error e in
	    config # config_log_error 
	      (Some fd_addr) (Some peer_addr) None None msg;
	    (* Note: The kernel ensures that the following token will be [`Eof].
	     * Any necessary cleanup will be done when [`Eof] is processed.
	     *)
	    
	| `Bad_request_error (e, resp) ->
	    (* Log the incident, and reply with a 400 response. There isn't any
	     * request manager, because `Bad_request_error replaces `Req_header
	     * when bad requests arrive.
	     *)
	    assert(cur_request_manager = None);
	    let msg = string_of_bad_request_error e in
	    let status = status_of_bad_request_error e in
	    config # config_log_error
	      (Some fd_addr) (Some peer_addr) None None msg;
	    let body = config # config_error_response (int_of_http_status status) in
	    Nethttpd_kernel.send_static_response resp status None body;
	    Unixqueue.add_event ues (Unixqueue.Extra(Ev_output_filled group));
	    (* Note: The kernel ensures that the following token will be [`Eof].
	     * Any necessary cleanup will be done when [`Eof] is processed.
	     *)

	| `Timeout -> 
	    (* A non-fatal timeout. Always followed by [`Eof] *)
	    ()
    done


  method private cycle_lc lc =
    (* Do a cycle of the [lc] engine. *)
    lc # cycle();     (* do not block! *)
    let cont = lc # lingering in
    self # enable_output false;
    self # enable_input cont;
    if not cont then (
      (* Now stop the whole engine! *)
      conn_state <- `Closed;
      Unixqueue.clear ues group;    (* Stop in Unixqueue terms *)
      self # set_state (`Done());   (* Report the new state *)
    )

  method private handle_exception err =
    (* In general this should not happen. The HTTP kernel already handles all kinds
     * of I/O errors. This means all remaining exceptions are programming errors.
     *)
    assert false

  method event_system = ues

  method abort() =
    (* The hard way to stop everything: *)
    match state with
      | `Working _ ->
	  Unixqueue.clear ues group;    (* Stop the queue immediately *)
	  if conn_state <> `Closed then ( try Unix.close fd with _ -> ());
	  ( match conn_state with
	      | `Active proto ->
		  proto # abort `Broken_pipe
		    (* This closes the file descriptors of all files currently
		     * being sent by [send_file_response].
		     *)
	      | _ -> ()
	  );
	  ( match cur_request_manager with
	      | Some rm -> rm # abort()
		  (* The input and output channels are forced to fail when data
		   * is input/output.
		   *)
	      | None -> ()
	  );
	  self # set_state `Aborted
      | _ ->
	  ()   (* already in a final state *)
    
end


class type http_engine_processing_config =
object
  method config_synch_input : 
           (Netchannels.in_obj_channel -> unit) ->
           Uq_engines.async_in_channel ->
           unit
  method config_synch_output : 
           (Netchannels.out_obj_channel -> unit) ->
           Uq_engines.async_out_channel ->
           unit
end


class buffering_engine_processing_config : http_engine_processing_config =
object
  method config_synch_input f (ch : Uq_engines.async_in_channel) =
    let queue = Queue.create() in

    let ch' = object(self)
      val mutable closed = false
      val mutable token = None
      val mutable pos_in = 0

      method input u up ul =
	if closed then raise Netchannels.Closed_channel;
	try
	  let (s,sp,sl) = self # next_token in
	  let n = min ul sl in
	  String.blit s sp u up n;
	  token <- if n = sl then None else Some(s,sp+n,sl-n);
	  pos_in <- pos_in + n;
	  n
	with
	    Queue.Empty -> raise End_of_file

      method close_in() = closed <- true

      method pos_in = pos_in

      method private next_token =
	match token with
	  | None ->
	      let tok = Queue.take queue in
	      token <- Some tok;
	      tok
	  | Some(s,p,l) ->
	      (s,p,l)

    end in

    let s = String.create 8192 in
    let on_data() =
      try
	while ch # can_input do
	  let n = ch # input s 0 8192 in
	  Queue.push (String.sub s 0 n, 0, n) queue
	done;
	true  (* notify again *)
      with
	  End_of_file -> 
	    let ch' = Netchannels.lift_in ~buffered:false (`Raw ch') in
	    f ch';
	    false  (* don't notify any longer *)
    in

    ch # request_notification on_data;
    if ch # can_input then ignore(on_data());

  method config_synch_output f (ch : Uq_engines.async_out_channel) =
    let ch' = 
      Netchannels.lift_out ~buffered:false (`Raw (ch :> Netchannels.raw_out_channel)) in
    f ch'
      (* The output channel of the engine buffers anyway! *)
end


class type http_engine_processing_context =
object
  method engine : unit Uq_engines.engine
end


type x_reaction = 
    [ http_service_reaction
    | `Redirect_request of string * http_header
    ]


exception Ev_stage2_processed of (http_service_generator option * Unixqueue.group)
  (* Event: Stage 2 has been processed. The argument is the follow-up action:
   * - None: Finish request immediately
   * - Some g: Proceed with generator g
   *)

exception Ev_stage3_processed of ((string * http_header) option * Unixqueue.group)
  (* Event: Stage 2 has been processed. The argument is the follow-up action:
   * - None: Finish request
   * - Some(uri,hdr): Redirect response to this location
   *)


class redrained_environment ~out_channel (env : extended_environment) =
object(self)
  inherit redirected_environment env
  method output_ch = out_channel
end


class fake_rhn (req:http_request_notification) : http_request_header_notification =
object
  method req_state = `Received_header
  method environment = req # environment
  method schedule_accept_body ~on_request ?(on_error = fun () -> ()) () =
    on_request req
  method schedule_reject_body ~on_request ?(on_error = fun () -> ()) () =
    on_request req
  method schedule_finish() = req # schedule_finish()
end


let process_connection config pconfig fd ues stage1 : http_engine_processing_context =
  let fd_addr = Unix.getsockname fd in
  let peer_addr = Unix.getpeername fd in

  let on_req_hdr = ref (fun _ -> ()) in

  let eng_config = object
    method config_input_flow_control = true
    method config_output_flow_control = true

    method config_timeout_next_request = config # config_timeout_next_request
    method config_timeout = config # config_timeout
    method config_cgi = config # config_cgi
    method config_error_response = config # config_error_response
    method config_log_error = config # config_log_error
    method config_log_access = config # config_log_access
    method config_max_reqline_length = config # config_max_reqline_length
    method config_max_header_length = config # config_max_header_length
    method config_max_trailer_length = config # config_max_trailer_length
    method config_limit_pipeline_length = config # config_limit_pipeline_length
    method config_limit_pipeline_size = config # config_limit_pipeline_size
    method config_announce_server = config # config_announce_server
  end in

  let engine = 
    new http_engine 
      ~on_request_header:(fun req -> !on_req_hdr req) ()
      eng_config fd ues in

  let log_error req msg =
    let env = req # environment in
    let meth = env # cgi_request_method in
    let uri = env # cgi_request_uri in
    config # config_log_error
      (Some fd_addr) (Some peer_addr) (Some(meth,uri)) (Some env#input_header) msg
  in

  let group = Unixqueue.new_group ues in
     (* for the extra events *)

object(self)

  val mutable watched_groups = []

  initializer (
    on_req_hdr := self # on_request_header;
    Uq_engines.when_state 
      ~is_aborted:(fun _ -> 
		     List.iter (Unixqueue.clear ues) watched_groups;
		     watched_groups <- []
		  )
      engine
  )


  method private do_stage3 (req : http_request_notification) env redir_count stage3 =
    (* We call the response generator with a synchronized output channel. By sending
     * the [Ev_stage3_processed] event, we catch the point in time when the whole
     * request is over, and can be finished.
     *)

    (* This construction just catches the [Ev_stage3_processed] event: *)
    let pe = new Uq_engines.poll_engine 
	       ~extra_match:(function 
			       | Ev_stage3_processed (_,g) -> g = group
			       | _ -> false) 
	       [] ues in
    watched_groups <- pe#group :: watched_groups;
    Uq_engines.when_state
      ~is_done:(function
		  | Unixqueue.Extra(Ev_stage3_processed(redirect_opt,_)) ->
		      (* Maybe we have to perform a redirection: *)
		      ( match redirect_opt with
			  | Some (new_uri,new_hdr) ->
			      if env # output_state <> `Start 
			      then
				log_error req
				  "Nethttpd: Redirect_response is not allowed after output has started"
			      else (
				let (new_script_name, new_query_string) = 
				  decode_query new_uri in
				new_hdr # update_field "Content-length" "0";
				let new_properties =
				  update_alist 
				    [ "REQUEST_URI", new_uri;
				      "SCRIPT_NAME", new_script_name;
				      "QUERY_STRING", new_query_string;
				      "REQUEST_METHOD", "GET"
				    ] 
				    env#cgi_properties in
				let new_env =
				  new redirected_environment 
				    ~properties:new_properties
				    ~in_header:new_hdr
				    env in
				let new_req = new fake_rhn req in
				(* The fake_rhn accepts/rejects the body immediately.
				 * In reality this is already done, but when we
				 * redirect the response we must fake a fresh
				 * request header object
				 *)
				self # process_request new_req new_env (redir_count+1)
			      )
			  | None ->
			      req # schedule_finish())
		  | _ -> assert false)
      pe;

    pconfig # config_synch_output
      (fun out_ch ->
	 let env' =
	   new redrained_environment ~out_channel:out_ch env in
	 let redirect_opt =
	   try
	     stage3 # generate_response env';
	     None
	   with
	     | Redirect_request(_,_) ->
		 log_error req
		   "Nethttpd: Caught Redirect_request in stage 3, but it is only allowed in stage 1";
		 None
	     | Redirect_response(new_uri, new_hdr) ->
		 Some(new_uri, new_hdr)
             | Standard_response(status, hdr_opt, errmsg_opt) 
		 when env'#output_state = `Start ->
		   output_std_response config env' status hdr_opt errmsg_opt;
		   None
	     | err when env#output_state = `Start ->
		 output_std_response config env' `Internal_server_error None 
		   (Some("Nethttpd: Uncaught exception: " ^ Netexn.to_string err));
		 None
	     | err ->
		 log_error req
		   ("Nethttpd: Uncaught exception: " ^ Netexn.to_string err);
		 None
	 in
	 (* Send the event that we are done here: *)
	 ues # add_event (Unixqueue.Extra(Ev_stage3_processed(redirect_opt,group)))
      )
      req # environment # output_ch_async


  method private do_stage2 (req : http_request_header_notification)
                           env redir_count stage2 =
    (* This is quite complicated. First, we schedule the body acceptance. Second,
     * we call the synchronized stage2 processor. In an MT environment, both
     * processes may run in parallel, so we have to synchronize them again (i.e.
     * determine the point in time when the body has accepted due to 
     * [schedule_accept_body], and when the stage2 processor is done). To do so,
     * we send a [Ev_stage2_processed] event, and catch that by the main event loop.
     *)
    let accepted_request = ref None in
    let stage3_opt = ref None in
    (* When both variables are [Some], we are in synch again. *)

    let check_synch() =
      match (!accepted_request, !stage3_opt) with
	| (Some req', Some(Some stage3)) ->
	    (* Synch + stage2 was successful. Continue with stage3: *)
	    self # do_stage3 req' env redir_count stage3
	| (Some req', Some None) ->
	    (* Synch, but stage2 was not successful. Finish the request immediately. *)
	    req' # schedule_finish()
	| _ ->
	    (* All other cases: not yet in synch. Do nothing. *)
	    ()
    in

    req # schedule_accept_body
      ~on_request:(fun req' -> 
		     accepted_request := Some req';
		     check_synch())
      (* ~on_error:XXX *)  (* CHECK: close async in channel? *)
      ();

    (* This construction just catches the [Ev_stage2_processed] event: *)
    let pe = new Uq_engines.poll_engine 
	       ~extra_match:(function 
			       | Ev_stage2_processed(_,g) -> g=group 
			       | _ -> false) 
	       [] ues in
    watched_groups <- pe#group :: watched_groups;
    Uq_engines.when_state
      ~is_done:(function
		  | Unixqueue.Extra(Ev_stage2_processed(st3_opt,_)) ->
		      stage3_opt := Some st3_opt;
		      check_synch()
		  | _ -> assert false)
      pe;

    pconfig # config_synch_input
      (fun in_ch ->
	 let env' =
	   new redirected_environment ~in_channel:in_ch env in
	 let stage3_opt =
	   try
	     Some(stage2 # process_body env')
	   with
	     | Redirect_request(_,_) ->
		 log_error req
		   "Nethttpd: Caught Redirect_request in stage 2, but it is only allowed in stage 1";
		 None
	     | Redirect_response(_,_) ->
		 log_error req
		   "Nethttpd: Caught Redirect_response in stage 2, but it is only allowed in stage 3";
		 None
             | Standard_response(status, hdr_opt, errmsg_opt) 
		 when env'#output_state = `Start ->
		   output_std_response config env' status hdr_opt errmsg_opt;
		   None
	     | err when env#output_state = `Start ->
		 output_std_response config env' `Internal_server_error None 
		   (Some("Nethttpd: Uncaught exception: " ^ Netexn.to_string err));
		 None
	     | err ->
		 log_error req
		   ("Nethttpd: Uncaught exception: " ^ Netexn.to_string err);
		 None
	 in
	 (* Send the event that we are done here: *)
	 ues # add_event (Unixqueue.Extra(Ev_stage2_processed(stage3_opt,group)))
      )
      req # environment # input_ch_async

  method private process_request (req:http_request_header_notification) 
                                 redir_env redir_count =
    (* [redir_env]: The environment of the request, possibly rewritten by redirects.
     * [redir_count]: The number of already performed redirections
     * [req]: Contains always the original environment
     *)
    if redir_count > 10 then
      failwith "Too many redirections";
    let reaction = 
      try (stage1 # process_header redir_env :> x_reaction)
      with 
	| Redirect_request(new_uri, new_hdr) ->
	    `Redirect_request(new_uri, new_hdr)
	| Redirect_response(_,_) ->
	    failwith "Caught Redirect_response in stage 1, but it is only allowed in stage 3"
    in
    ( match reaction with
	| `Accept_body stage2 ->
	    self # do_stage2 req redir_env redir_count stage2
	| `Reject_body stage3 ->
	    req # schedule_reject_body
	      ~on_request:(fun req' -> 
			     self # do_stage3 req' redir_env redir_count stage3)
	      ();
	| `Static(status, resp_hdr_opt, resp_str) ->
	    req # schedule_reject_body
	      ~on_request:(fun req' -> 
			     output_static_response redir_env status 
			       resp_hdr_opt resp_str;
			     req' # schedule_finish())
	      ();
	| `File(status, resp_hdr_opt, resp_filename, pos, length) ->
	    req # schedule_reject_body
	      ~on_request:(fun req' -> 
			     output_file_response redir_env status resp_hdr_opt
			       resp_filename pos length;
			     req' # schedule_finish())
	      ();
	| `Std_response(status, resp_hdr_opt, errlog_opt) ->
	    req # schedule_reject_body
	      ~on_request:(fun req' -> 
			     output_std_response config redir_env status 
			       resp_hdr_opt errlog_opt;
			     req' # schedule_finish())
	      ();
	| `Redirect_request(new_uri, new_hdr) ->
	    let (new_script_name, new_query_string) = decode_query new_uri in
	    new_hdr # update_multiple_field 
	      "Content-length" (redir_env # multiple_input_header_field "Content-length");
	    let new_properties =
	      update_alist 
		[ "REQUEST_URI", new_uri;
		  "SCRIPT_NAME", new_script_name;
		  "QUERY_STRING", new_query_string ] 
		redir_env#cgi_properties in
	    let new_env =
	      new redirected_environment 
		~properties:new_properties
		~in_header:new_hdr
		~in_channel:(redir_env # input_ch) redir_env in
	    self # process_request req new_env (redir_count+1)
    )

  method private on_request_header req =
    try
      self # process_request req (req#environment :> extended_environment) 0
    with
      | err ->
	  log_error req
	    ("Nethttpd: Uncaught exception: " ^ Netexn.to_string err)
	  
  method engine = engine

end
