(* $Id$
 *
 * Event queues
 * written by Gerd Stolpmann
 *)

open Printf;;

type 'a t =
 { mutable queue : 'a Queue.t;            (* The queue of unprocessed events *)
   mutable new_queue : 'a Queue.t;        (* new events *)
   mutable error_queue : 'a Queue.t;      (* re-scheduled events*)
   mutable handlers : ('a hdl option ref) Queue.t;     (* The list of event handlers - if None, the handler is terminated *)
   mutable live_handlers : int;
   mutable new_handlers : ('a hdl option ref) Queue.t; (* New handlers *)
   mutable source : 'a t -> unit;         (* The default source of events *)
   string_of_event : 'a -> string;        (* A printer for debugging purposes *)
   mutable running : bool;
 }

and 'a hdl = 'a t -> 'a -> unit           (* The type of handlers *)
;;

type debug_target = [ `Any | `Process of int | `Thread of int | `None ]

exception Reject;;          (* Event handler rejects an event *)
exception Terminate;;       (* Event handler removes itself from the list *)
exception Out_of_handlers;; (* There is an event  but no event handler *)

let create ?(string_of_event = fun _ -> "<abstr>") source =
  { queue        = Queue.create();   (* main event queue *)
    new_queue    = Queue.create(); 
    error_queue  = Queue.create(); 
    handlers     = Queue.create();
    new_handlers = Queue.create();
    live_handlers = 0;
    source       = source;
    string_of_event = string_of_event;
    running      = false;
  }
;;


let debug_mode = ref `None;;


let test_debug_target t =
  match t with
    | `Any -> true
    | `Process pid -> Unix.getpid() = pid
    | `Thread id -> 
	!Netsys_oothr.provider # self # id = id
    | `None -> false

let debug_print s =
  if test_debug_target !debug_mode then
    prerr_endline("Equeue debug msg: " ^ Lazy.force s)
;;


let set_debug_mode b =
  debug_mode := (if b then `Any else `None)
;;


let set_debug_target t =
  debug_mode := t
;;


let add_handler esys h =
  debug_print (lazy "add_handler");
  Queue.push (ref (Some h)) esys.new_handlers
;;


let add_event esys e =
  debug_print (lazy (sprintf "add_event <event: %s>" (esys.string_of_event e)));
  Queue.push e esys.new_queue
;;


let run esys =
  if esys.running then
    failwith "Equeue.run: Already running";
  debug_print (lazy "run <starting>");
  esys.running <- true;
  try
    if Queue.is_empty esys.queue then (
      if Queue.is_empty esys.new_queue && Queue.is_empty esys.error_queue then (
	(* try to get new events *)
	debug_print (lazy "run <invoking source>");
	esys.source esys;
      );
      (* schedule new events or events again that previously caused errors *)
      debug_print (lazy "run <reloading queue>");
      Queue.transfer esys.new_queue esys.queue;
      Queue.transfer esys.error_queue esys.queue;
    );
    while not (Queue.is_empty esys.queue) do
      let debug_msg = lazy (
	let n = Queue.length esys.queue in
	let qs = String.concat "; " 
	          (Queue.fold
		     (fun acc e ->
			esys.string_of_event e :: acc)
		     []
		     esys.queue) in
	sprintf "run <queue has %d events, 1st will be processed: %s>" n qs
      ) in
      debug_print debug_msg;

      if 2 * esys.live_handlers < Queue.length esys.handlers then (
	debug_print (lazy "run <garbage collecting handlers>");
	let handlers' = Queue.create() in
	Queue.iter
	  (fun h ->
	     match !h with
	       | None -> ()
	       | Some hf ->
		   Queue.push h handlers'
	  )
	  esys.handlers;
	esys.handlers <- handlers';
	esys.live_handlers <- Queue.length handlers'
      );
      let l_new_handlers = Queue.length esys.new_handlers in
      if l_new_handlers > 0 then (
	Queue.transfer esys.new_handlers esys.handlers;
	esys.live_handlers <- esys.live_handlers + l_new_handlers;
	debug_print (lazy (sprintf "run <considering %d new handlers>" 
			     l_new_handlers))
      );
     
      let e = Queue.take esys.queue in
      let accept = ref false in
      (* debug_print (lazy (sprintf "run <%d event handlers>" (List.length esys.handlers))); *)
      (* Printing the handlers does not make sense; e.g. Unixqueue only adds one global handler *)
      (* Exceptions occuring in 'h' have to be done: 
       * - The exception is propagated up
       * - The event is moved to the end of the queue
       * - If 'run' is called again, the event is scheduled again
       *)
      ( try
	  Queue.iter
	    (fun h ->
	       match !h with
		 | None -> ()   (* terminated handler *)
		 | Some hf ->
		     ( if not !accept then (
			 try
			   hf esys e;
			   accept := true
			 with
			   | Reject ->
			       ()
			   | Terminate ->
			       accept := true;
			       h := None;
			       esys.live_handlers <- esys.live_handlers - 1;
			       debug_print (lazy (sprintf "run <got Terminate #handlers=%d>" esys.live_handlers));
		       )
		     )
	    )
	    esys.handlers;
	  debug_print (lazy (sprintf "run <event %s: %s>" 
			       (esys.string_of_event e) 
			       (if !accept then "accepted" else "dropped")));
	with
	  | error ->
	      Queue.push e esys.error_queue;
	      debug_print (lazy (sprintf "run <event %s: exception %s>"
				   (esys.string_of_event e)
				   (Netexn.to_string error)));
	      raise error
      );
      if Queue.is_empty esys.queue then (
	if (Queue.is_empty esys.new_queue && Queue.is_empty esys.error_queue)
	then (
	  (* try to get new events (or handlers!) *)
	  debug_print (lazy "run <invoking source>");
	  esys.source esys;
	  if (not (Queue.is_empty esys.new_queue) && 
              Queue.is_empty esys.handlers && 
	      Queue.is_empty esys.new_handlers
	     )
	  then (
	    debug_print (lazy "run <out of handlers>");
	    raise Out_of_handlers
	  )
	    (* If there are new events there must also be (new) handlers.
   	     * Otherwise the program would loop infinitely.
             *)
	);

	(* schedule new events or events again that previously caused errors *)
	debug_print (lazy "run <reloading queue>");
	Queue.transfer esys.new_queue esys.queue;
	Queue.transfer esys.error_queue esys.queue;
      )
    done;
    debug_print (lazy "run <returning normally>");
    esys.running <- false;
  with
      any ->
	debug_print (lazy (sprintf "run <returning with exception %s>"
			     (Netexn.to_string any)));
	esys.running <- false;
	raise any
;;


let is_running esys = esys.running
