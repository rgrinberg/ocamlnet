(* $Id$
 *
 * Event queues
 * written by Gerd Stolpmann
 *)

open Printf;;

type 'a t =
 { mutable queue : 'a list;               (* The queue of unprocessed events *)
   mutable new_queue : 'a list;           (* new events *)
   mutable error_queue : 'a list;         (* re-scheduled events*)
   mutable handlers : 'a hdl list;        (* The list of event handlers *)
   mutable new_handlers : 'a hdl list;    (* New handlers *)
   mutable source : 'a t -> unit;         (* The default source of events *)
   string_of_event : 'a -> string;        (* A printer for debugging purposes *)
   mutable running : bool;
 }

and 'a hdl = 'a t -> 'a -> unit           (* The type of handlers *)
;;


exception Reject;;          (* Event handler rejects an event *)
exception Terminate;;       (* Event handler removes itself from the list *)
exception Out_of_handlers;; (* There is an event  but no event handler *)

let create ?(string_of_event = fun _ -> "<abstr>") source =
  { queue        = [];   (* main event queue *)
    new_queue    = []; 
    error_queue  = []; 
    handlers     = [];
    new_handlers = [];
    source       = source;
    string_of_event = string_of_event;
    running      = false;
  }
;;


let debug_mode = ref false;;

let debug_print s =
  if !debug_mode then
    prerr_endline("Equeue debug msg: " ^ Lazy.force s)
;;


let set_debug_mode b =
  debug_mode := b
;;


let add_handler esys h =
  debug_print (lazy "add_handler");
  esys.new_handlers <- esys.new_handlers @ [h]
;;


let add_event esys e =
  debug_print (lazy (sprintf "add_event <event: %s>" (esys.string_of_event e)));
  esys.new_queue <- esys.new_queue @ [e]
;;


let run esys =
  if esys.running then
    failwith "Equeue.run: Already running";
  debug_print (lazy "run <starting>");
  esys.running <- true;
  try
    if esys.queue = [] then begin
      if esys.new_queue = [] && esys.error_queue = [] then (
	(* try to get new events *)
	debug_print (lazy "run <invoking source>");
	esys.source esys;
      );
      (* schedule new events or events again that previously caused errors *)
      debug_print (lazy "run <reloading queue>");
      esys.queue <- esys.new_queue @ esys.error_queue;
      esys.new_queue <- [];
      esys.error_queue <- []
    end;
    while esys.queue <> [] do
      let debug_msg = lazy (
	let n = List.length esys.queue in
	let qs = String.concat "; " (List.map esys.string_of_event esys.queue) in
	sprintf "run <queue has %d events, 1st will be processed: %s>" n qs
      ) in
      debug_print debug_msg;
      match esys.queue with
	| []  -> assert false
	| e :: q' ->
	    let accept = ref false in
	    esys.handlers <- esys.handlers @ esys.new_handlers;
	    esys.new_handlers <- [];
	    (* debug_print (lazy (sprintf "run <%d event handlers>" (List.length esys.handlers))); *)
	    (* Printing the handlers does not make sense; e.g. Unixqueue only adds one global handler *)
	    (* Exceptions occuring in 'h' have to be done: 
             * - The exception is propagated up
             * - The event is moved to the end of the queue
             * - If 'run' is called again, the event is scheduled again
	     *)
	    begin try
	      esys.handlers <-
		List.filter
		(fun h ->
		   if not !accept then
		     try
		       h esys e;
		       accept := true;
		       true
		     with
			 Reject ->
			   true
		       | Terminate ->
			   accept := true;
			   false
		   else
		     true
		)
		esys.handlers;
	      debug_print (lazy (sprintf "run <event %s: %s>" 
				   (esys.string_of_event e) 
				   (if !accept then "accepted" else "dropped")));
	      esys.queue <- q';
	    with
		any ->
		  esys.queue <- q';
		  esys.error_queue <- esys.error_queue @ [e];
		  debug_print (lazy (sprintf "run <event %s: exception %s>"
				       (esys.string_of_event e)
				       (Printexc.to_string any)));
		  raise any
	    end;
	    if esys.queue = [] then begin
	      if esys.new_queue = [] && esys.error_queue = [] then begin
		(* try to get new events (or handlers!) *)
		debug_print (lazy "run <invoking source>");
		esys.source esys;
		if esys.new_queue <> [] && esys.handlers = [] && esys.new_handlers = []
		then (
		  debug_print (lazy "run <out of handlers>");
		  raise Out_of_handlers
		)
		  (* If there are new events there must also be (new) handlers.
   	           * Otherwise the program would loop infinitely.
		   *)
	      end;
	      (* schedule new events or events again that previously caused errors *)
	      debug_print (lazy "run <reloading queue>");
	      esys.queue <- esys.new_queue @ esys.error_queue;
	      esys.new_queue <- [];
	      esys.error_queue <- []
	    end;
    done;
    debug_print (lazy "run <returning normally>");
    esys.running <- false;
  with
      any ->
	debug_print (lazy (sprintf "run <returning with exception %s>"
			     (Printexc.to_string any)));
	esys.running <- false;
	raise any
;;


let is_running esys = esys.running
