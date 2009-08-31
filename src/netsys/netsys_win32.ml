(* $Id$ *)

(* Security of named pipes:

   - http://www.blakewatts.com/namedpipepaper.html
   - impersonation: http://msdn.microsoft.com/en-us/library/aa378832(VS.85).aspx
 *)

open Printf

external fill_random : string -> unit = "netsys_fill_random"


type c_event
type c_pipe_helper


(* Note that the proxy descriptor _must not_ be part of the i_* records,
   because this would prevent the proxy descriptors to be garbage-collected
 *)

type i_event = c_event
type w32_event = i_event * Unix.file_descr
    (* The descriptor is the proxy descriptor *)

type pipe_mode = Pipe_in | Pipe_out | Pipe_duplex

type i_pipe =
    { pipe_name : string;
      pipe_mode : pipe_mode;
      pipe_helper : c_pipe_helper;
      (* mutable pipe_signal : w32_event option; *)
      pipe_rd_event : w32_event;
      pipe_wr_event : w32_event;
    }

type w32_pipe = i_pipe * Unix.file_descr
    (* The descriptor is the proxy descriptor *)

type i_pipe_server =
    { psrv_name : string;
      psrv_mode : pipe_mode;
      psrv_max : int;
      mutable psrv_first : bool;
      mutable psrv_queue : c_pipe_helper list;
      (* The queue of pipes waiting for an incoming connection *)
      mutable psrv_listen : int;
      (* The backlog parameter of [listen] (target length of psrv_queue) *)
      psrv_ready : c_pipe_helper Queue.t;
      (* The already accepted but not yet reported connections *)
      psrv_cn_event : w32_event;
      psrv_proxy_handle : c_event;
      psrv_mutex : Netsys_oothr.mutex;
    }

type w32_pipe_server = i_pipe_server * Unix.file_descr
    (* The descriptor is the proxy descriptor *)


type pipe_conn_state = Pipe_deaf | Pipe_listening | Pipe_connected | Pipe_down

type i_object =
  | I_event of i_event
  | I_pipe of i_pipe
  | I_pipe_server of i_pipe_server

type w32_object =
  | W32_event of w32_event
  | W32_pipe of w32_pipe
  | W32_pipe_server of w32_pipe_server


module Int64Map = Map.Make(Int64)


external int64_of_file_descr : Unix.file_descr -> int64
  = "netsys_int64_of_file_descr"
  (* Also occurs in netsys.ml! *)

external netsys_win32_set_debug : bool -> unit
  = "netsys_win32_set_debug"

module Debug = struct
  let enable = ref false

  let debug_c_wrapper = netsys_win32_set_debug
end

let dlog = Netlog.Debug.mk_dlog "Netsys_win32" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netsys_win32" Debug.enable

let () =
  Netlog.Debug.register_module "Netsys_win32" Debug.enable


module FD = struct
  type t = int64
  let equal (fd1) (fd2) =
    fd1=fd2
  let hash fd =
    Hashtbl.hash fd
end

module H = Hashtbl.Make(FD)
  (* Hash table mapping
     proxy file descriptors to the w32_object referenced by the descriptors.
     The keys are the handle values contained in the fd values. As we allow
     that proxies are [Unix.close]d it can happen that several fd's exist
     that have the same handle values. In this case, the address of the
     fd itself is used to distinguish between these same-looking fd's.
   *)

let proxies = ref (H.create 41)
let proxies_mutex = !Netsys_oothr.provider # create_mutex()
let proxies_gc_flag = ref false
let proxies_unreg_count = ref 0


let mk_weak x =
  let w = Weak.create 1 in
  Weak.set w 0 (Some x);
  w

let get_weak w =
  Weak.get w 0


let unregister_proxy cell _ =
  proxies_unreg_count := !proxies_unreg_count + 1;
  cell := None


let register_proxy fd i_obj =
(* prerr_endline ("register_proxy " ^ Int64.to_string (int64_of_file_descr fd)); *)
  let fd_num = int64_of_file_descr fd in
  Netsys_oothr.serialize
    proxies_mutex
    (fun () ->
       if (!proxies_gc_flag && 
	     2 * !proxies_unreg_count > H.length !proxies)
       then (  (* do a GC pass *)
	 let proxies' = H.create 41 in
	 let l = H.length !proxies in
	 H.iter
	   (fun fd_num entry ->
	      let (_, cell) = entry in
	      if !cell <> None then
		H.add proxies' fd_num entry
	   )
	   !proxies;
	 proxies := proxies';
	 proxies_unreg_count := 0;
	 proxies_gc_flag := false;
	 dlogr
	   (fun () ->
	      sprintf "register_proxy: keeping %d/%d entries in proxy tbl"
		(H.length proxies') l
	   )
       );
       let cell = ref (Some i_obj) in
       let fd_weak = mk_weak fd in
       H.add !proxies fd_num (fd_weak, cell);
       Gc.finalise (unregister_proxy cell) fd
    )
    ()


let _ =
  Gc.create_alarm
    (fun () ->
       proxies_gc_flag := true
    )


let lookup fd =
(* prerr_endline ("lookup " ^ Int64.to_string (int64_of_file_descr fd)); *)
  let fd_num = int64_of_file_descr fd in
  Netsys_oothr.serialize
    proxies_mutex
    (fun () ->
       let l = H.find_all !proxies fd_num in
       let (_, cell_opt) =
	 List.find
	   (fun (fd'_weak,cell) ->
	      match get_weak fd'_weak with
		| None -> false
		| Some fd' -> 
		    !cell <> None && fd == fd'  (* phys. cmp! *)
	   )
	   l in
       match !cell_opt with
	 | None ->
	     assert false
	 | Some i_obj ->
	     ( match i_obj with
		 | I_event i_ev -> 
		     W32_event(i_ev, fd)
		 | I_pipe i_pipe -> 
		     W32_pipe(i_pipe, fd)
		 | I_pipe_server i_psrv ->
		     W32_pipe_server(i_psrv, fd)
	     )
    )
    ()


let lookup_pipe fd =
  try
    match lookup fd with
      | W32_pipe ph -> ph
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_pipe: not found"

let lookup_pipe_server fd =
  try
    match lookup fd with
      | W32_pipe_server ph -> ph
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_pipe_server: not found"

let lookup_event fd =
  try
    match lookup fd with
      | W32_event e -> e
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_event: not found"



external netsys_real_select : 
         Unix.file_descr list -> 
         Unix.file_descr list -> 
         Unix.file_descr list -> 
         float ->
           (Unix.file_descr list * Unix.file_descr list * Unix.file_descr list)
  = "netsys_real_select"

let real_select = netsys_real_select



external netsys_create_event : unit -> c_event 
  = "netsys_create_event"

external netsys_event_descr : c_event -> Unix.file_descr
  = "netsys_event_descr"

external netsys_close_event : c_event -> unit
  = "netsys_close_event"

external netsys_set_auto_close_event_proxy : c_event -> bool -> unit
  = "netsys_set_auto_close_event_proxy"

let decorate_event e =
  let e_proxy = netsys_event_descr e in
  let ev = (e, e_proxy) in
  Gc.finalise netsys_close_event e;
  register_proxy e_proxy (I_event e);
  ev

let create_event() =
  let ev = decorate_event(netsys_create_event()) in
  dlogr (fun () -> 
	   sprintf "create_event: descr=%Ld" 
	     (int64_of_file_descr (snd ev)));
  ev


let event_descr (e,e_proxy) = 
  netsys_set_auto_close_event_proxy e false;
  e_proxy

external netsys_set_event : c_event -> unit
  = "netsys_set_event"

external netsys_reset_event : c_event -> unit
  = "netsys_reset_event"

external netsys_test_event : c_event -> bool
  = "netsys_test_event"

external netsys_event_wait : c_event -> int -> bool
  = "netsys_event_wait"

let set_event (e,e_proxy)   = 
  dlogr (fun () -> 
	   sprintf "set_event: descr=%Ld" 
	     (int64_of_file_descr e_proxy));
  netsys_set_event e

let reset_event (e,e_proxy) = 
  dlogr (fun () -> 
	   sprintf "reset_event: descr=%Ld" 
	     (int64_of_file_descr e_proxy));
  netsys_reset_event e

let test_event (e,_)  = netsys_test_event e

let event_wait (e,e_proxy) tmo =
  dlogr (fun () -> 
	   sprintf "event_wait: descr=%Ld tmo=%f" 
	     (int64_of_file_descr e_proxy) tmo);
  let flag =
    Netsys_impl_util.slice_time_ms
      (fun tmo_ms ->
	 if netsys_event_wait e tmo_ms then Some () else None
      )
      tmo
    <> None in
  dlogr (fun () -> 
	   sprintf "event_wait: descr=%Ld returning %b" 
	     (int64_of_file_descr e_proxy) flag);
  flag



external netsys_wsa_event_select :  
  c_event -> Unix.file_descr -> Netsys_posix.poll_req_events -> unit
  = "netsys_wsa_event_select"

external wsa_maximum_wait_events : unit -> int
  = "netsys_wsa_maximum_wait_events"

external netsys_wsa_wait_for_multiple_events : 
  c_event array -> int -> int option
  = "netsys_wsa_wait_for_multiple_events"

external netsys_wsa_enum_network_events : 
  Unix.file_descr -> c_event -> Netsys_posix.poll_act_events
  = "netsys_wsa_enum_network_events"

let wsa_event_select (e,e_proxy) fd pie =
  dlogr (fun () -> 
	   sprintf "wsa_event_select: evdescr=%Ld sockdescr=%Ld bits=%d" 
	     (int64_of_file_descr e_proxy) 
	     (int64_of_file_descr fd)
	     (Netsys_posix.int_of_req_events pie)
	);
  netsys_wsa_event_select e fd pie

let wsa_wait_for_multiple_events ea n =
  dlogr (fun () ->
	   sprintf "wsa_wait_for_multiple_events: descrs=%s tmo=%d"
	     (String.concat ","
		(Array.to_list
		   (Array.map
		      (fun (_,e_proxy) -> 
			 Int64.to_string(int64_of_file_descr e_proxy)) 
		      ea)))
	     n
	);
  let r =
    netsys_wsa_wait_for_multiple_events (Array.map fst ea) n in
  dlogr (fun () ->
	   sprintf "wsa_wait_for_multiple_events: returning %s"
	     (match r with
		| None -> "None"
		| Some k ->
		    let e_proxy = snd(ea.(k)) in
		    sprintf "Some %d (descr %Ld)"
		      k (int64_of_file_descr e_proxy)
	     ));
  r

let wsa_enum_network_events fd (e,e_proxy) =
  let r = netsys_wsa_enum_network_events fd e in
  dlogr (fun () ->
	   sprintf "wsa_enum_network_events: sockdescr=%Ld evdescr=%Ld bits=%d"
	     (int64_of_file_descr fd)
	     (int64_of_file_descr e_proxy)
	     (Netsys_posix.int_of_act_events r)
	);
  r



external netsys_pipe_free : 
  c_pipe_helper -> unit
  = "netsys_pipe_free"

external netsys_create_local_named_pipe :
  string -> pipe_mode -> int -> c_event -> bool -> c_pipe_helper
  = "netsys_create_local_named_pipe"

external netsys_pipe_listen :
  c_pipe_helper -> unit
  = "netsys_pipe_listen"

external netsys_pipe_deafen :
  c_pipe_helper -> unit
  = "netsys_pipe_deafen"

external netsys_pipe_connect :
  string -> pipe_mode -> c_pipe_helper
  = "netsys_pipe_connect"

external netsys_pipe_read :
  c_pipe_helper -> string -> int -> int -> int
  = "netsys_pipe_read"

external netsys_pipe_write :
  c_pipe_helper -> string -> int -> int -> int
  = "netsys_pipe_write"

external netsys_pipe_shutdown :
  c_pipe_helper -> unit
  = "netsys_pipe_shutdown"

external netsys_pipe_rd_event :
  c_pipe_helper -> c_event
  = "netsys_pipe_rd_event"

external netsys_pipe_wr_event :
  c_pipe_helper -> c_event
  = "netsys_pipe_wr_event"

external netsys_pipe_descr :
   c_pipe_helper -> Unix.file_descr
  = "netsys_pipe_descr"

external netsys_pipe_conn_state : 
  c_pipe_helper -> pipe_conn_state
  = "netsys_pipe_conn_state"

external netsys_pipe_signal :
  c_pipe_helper -> c_event -> unit
  = "netsys_pipe_signal"

external netsys_set_auto_close_pipe_proxy : c_pipe_helper -> bool -> unit
  = "netsys_set_auto_close_pipe_proxy"


let rev_mode =
  function
    | Pipe_in -> Pipe_out
    | Pipe_out -> Pipe_in
    | Pipe_duplex -> Pipe_duplex

let create_local_pipe_server name mode n =
  let cn_event = create_event() in
  let p_event = netsys_create_event() in
  let proxy = netsys_event_descr p_event in
  let psrv =
    { psrv_name = name;
      psrv_mode = mode;
      psrv_max = n;
      psrv_first = true;
      psrv_queue = [];
      psrv_listen = 0;
      psrv_ready = Queue.create();
      psrv_cn_event = cn_event;
      psrv_proxy_handle = p_event;
      psrv_mutex = !Netsys_oothr.provider # create_mutex();
    } in
  Gc.finalise netsys_close_event p_event;
  register_proxy proxy (I_pipe_server psrv);
  dlogr (fun () ->
	   sprintf "create_local_pipe_server: \
                    name=%s proxydescr=%Ld cnevdescr=%Ld"
	     name 
	     (int64_of_file_descr proxy) 
	     (int64_of_file_descr (snd cn_event))
	);
  (psrv, proxy)



let decorate_pipe_nogc ph name mode =
  let fd = netsys_pipe_descr ph in
  let pipe =
    { pipe_name = name;
      pipe_mode = mode;
      pipe_helper = ph;
      (* pipe_signal = None; *)
      pipe_rd_event = decorate_event(netsys_pipe_rd_event ph);
      pipe_wr_event = decorate_event(netsys_pipe_wr_event ph);
    } in
  register_proxy fd (I_pipe pipe);
  (pipe, fd)


let decorate_pipe ph name mode =
  Gc.finalise netsys_pipe_free ph;
  decorate_pipe_nogc ph name mode

let prefix = "\\\\.\\pipe\\"
let prefix_len = String.length prefix

let pipe_connect name mode =
  (* Check that name starts with the right prefix, to prevent security
     vulnerabilities:
   *)
  if String.length name < prefix_len ||
     (String.sub name 0 prefix_len <> prefix)
  then
    raise(Unix.Unix_error(Unix.EPERM, "pipe_connect", name));

  dlogr (fun () -> sprintf "pipe_connect: name=%s" name);
  let pipe = decorate_pipe(netsys_pipe_connect name mode) name mode in
  dlogr (fun () -> sprintf "pipe_connect: name=%s returning %Ld" 
	   name (int64_of_file_descr (snd pipe)));
  pipe

let pipe_server_descr (psrv, psrv_proxy) = 
  netsys_set_auto_close_event_proxy psrv.psrv_proxy_handle false;
  psrv_proxy

let pipe_descr (pipe, pipe_proxy) = 
  netsys_set_auto_close_pipe_proxy pipe.pipe_helper false;
  pipe_proxy

let pipe_server_endpoint psrv =
  let ph = 
    netsys_create_local_named_pipe
      psrv.psrv_name psrv.psrv_mode psrv.psrv_max 
      (fst psrv.psrv_cn_event) psrv.psrv_first in
  Gc.finalise netsys_pipe_free ph;
  netsys_pipe_listen ph;
  psrv.psrv_first <- false;
  ph

let pipe_listen_lck psrv n =
  if psrv.psrv_listen < n then (
    let d = n - psrv.psrv_listen in
    for k = 1 to d do
      let ph = pipe_server_endpoint psrv in
      psrv.psrv_queue <- ph :: psrv.psrv_queue
    done
  );
  (* else: we do nothing. You may consider this as a bug, but it is simply
     too risky to shut down server pipes because of race conditions
   *)
  psrv.psrv_listen <- n


let pipe_listen (psrv, psrv_proxy) n =
  dlogr (fun () -> sprintf "pipe_listen: name=%s proxydescr=%Ld n=%d" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy) n);
  Netsys_oothr.serialize
    psrv.psrv_mutex
    (fun () -> pipe_listen_lck psrv n)
    ()


let check_for_connections psrv =
  let rec find_delete l =
    match l with
      | [] -> 
	  []
      | ph :: l' ->
	  let s = netsys_pipe_conn_state ph in
	  if s = Pipe_connected then (
	    Queue.push ph psrv.psrv_ready;
	    find_delete l'
	  )
	  else
	    ph :: find_delete l'
  in

  let queue' = find_delete psrv.psrv_queue in
  let old_listen = psrv.psrv_listen in
  psrv.psrv_listen <- List.length queue';
  psrv.psrv_queue <- queue';
  pipe_listen_lck psrv old_listen
    
(* In rare cases it may happen that cn_event is reset for a short
   period of time, and then set again.
 *)

let rec pipe_accept_1 psrv =
  match Queue.length psrv.psrv_ready with
    | 0 ->
	ignore(event_wait psrv.psrv_cn_event (-1.0));
	reset_event psrv.psrv_cn_event;
	check_for_connections psrv;
	if not(Queue.is_empty psrv.psrv_ready) then
	  set_event psrv.psrv_cn_event;
	pipe_accept_1 psrv
    | 1 ->
	let ph = Queue.take psrv.psrv_ready in
	reset_event psrv.psrv_cn_event;
	check_for_connections psrv;
	if not(Queue.is_empty psrv.psrv_ready) then
	  set_event psrv.psrv_cn_event;
	ignore(netsys_pipe_read ph "" 0 0);     (* check for errors *)
	decorate_pipe_nogc ph psrv.psrv_name psrv.psrv_mode
    | _ ->
	let ph = Queue.take psrv.psrv_ready in
	ignore(netsys_pipe_read ph "" 0 0);     (* check for errors *)
	decorate_pipe_nogc ph psrv.psrv_name psrv.psrv_mode


let pipe_accept (psrv, psrv_proxy) =
  dlogr (fun () -> sprintf "pipe_accept: name=%s proxydescr=%Ld" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy));
  let pipe =
    Netsys_oothr.serialize
      psrv.psrv_mutex
      (fun () -> pipe_accept_1 psrv)
      () in
  dlogr (fun () -> sprintf "pipe_accept: name=%s proxydescr=%Ld returning %Ld" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy)
	   (int64_of_file_descr (snd pipe))
	);
  pipe
  

let pipe_rd_event (pipe,_) =
  pipe.pipe_rd_event

let pipe_wr_event (pipe,_) =
  pipe.pipe_wr_event

let pipe_connect_event (psrv,_) =
  psrv.psrv_cn_event


let pipe_read (pipe,pipe_proxy) s pos len =
  if pos < 0 || len < 0 || pos > String.length s - len then
    invalid_arg "Netsys_win32.pipe_read";
  dlogr (fun () -> sprintf "pipe_read: name=%s proxydescr=%Ld len=%d" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy) len);
  try
    let n = netsys_pipe_read pipe.pipe_helper s pos len in
    dlogr (fun () -> sprintf "pipe_read: name=%s proxydescr=%Ld returning %d" 
	     pipe.pipe_name (int64_of_file_descr pipe_proxy) n);
    n
  with
    | error when !Debug.enable ->
	dlogr (fun () -> 
		 sprintf "pipe_read: name=%s proxydescr=%Ld exception %s" 
		   pipe.pipe_name (int64_of_file_descr pipe_proxy) 
		   (Netexn.to_string error)
	      );
	raise error


let pipe_write (pipe,pipe_proxy) s pos len =
  if pos < 0 || len < 0 || pos > String.length s - len then
    invalid_arg "Netsys_win32.pipe_write";
  dlogr (fun () -> sprintf "pipe_write: name=%s proxydescr=%Ld len=%d" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy) len);
  try
    let n = netsys_pipe_write pipe.pipe_helper s pos len in
    dlogr (fun () -> sprintf "pipe_write: name=%s proxydescr=%Ld returning %d" 
	     pipe.pipe_name (int64_of_file_descr pipe_proxy) n);
    n
  with
    | error when !Debug.enable ->
	dlogr (fun () -> 
		 sprintf "pipe_write: name=%s proxydescr=%Ld exception %s" 
		   pipe.pipe_name (int64_of_file_descr pipe_proxy) 
		   (Netexn.to_string error)
	      );
	raise error


let pipe_shutdown (pipe,pipe_proxy) = 
  dlogr (fun () -> sprintf "pipe_shutdown: name=%s proxydescr=%Ld" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy));
  netsys_pipe_shutdown pipe.pipe_helper

let pipe_shutdown_server (psrv,psrv_proxy) =
  dlogr (fun () -> sprintf "pipe_shutdown_server: name=%s proxydescr=%Ld" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy));
  Netsys_oothr.serialize
    psrv.psrv_mutex
    (fun () ->
       List.iter
	 (fun ph ->
	    netsys_pipe_shutdown ph
	 )
	 psrv.psrv_queue;
       psrv.psrv_queue <- [];
       psrv.psrv_listen <- 0
    )
    ()


let pipe_wait_rd (pipe,pipe_proxy) tmo =
  dlogr (fun () -> sprintf "pipe_wait_rd: name=%s proxydescr=%Ld" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy));
  event_wait pipe.pipe_rd_event tmo

let pipe_wait_wr (pipe,pipe_proxy) tmo =
  dlogr (fun () -> sprintf "pipe_wait_wr: name=%s proxydescr=%Ld" 
	   pipe.pipe_name (int64_of_file_descr pipe_proxy));
  event_wait pipe.pipe_wr_event tmo

let pipe_wait_connect (psrv,psrv_proxy) tmo =
  dlogr (fun () -> sprintf "pipe_wait_connect: name=%s proxydescr=%Ld" 
	   psrv.psrv_name (int64_of_file_descr psrv_proxy));
  event_wait psrv.psrv_cn_event tmo

let pipe_name (pipe,_) =
  pipe.pipe_name

let pipe_server_name (psrv,_) =
  psrv.psrv_name

let pipe_mode (pipe,_) =
  pipe.pipe_mode

let pipe_server_mode (psrv,_) =
  psrv.psrv_mode


let counter = ref 0
let counter_mutex = !Netsys_oothr.provider # create_mutex()

let unpredictable_pipe_name() =
  let n = (
    counter_mutex # lock();
    let n = !counter in
    incr counter;
    counter_mutex # unlock();
    n
  ) in
  let random = String.make 16 ' ' in
  fill_random random;
  let name =
    "\\\\.\\pipe\\ocamlnet" ^ 
      string_of_int (Unix.getpid()) ^ "_" ^ string_of_int n ^ "_" ^ 
      Digest.to_hex random in
  name

let pipe_pair mode =
  (* FIXME: If somebody guesses the pipe name (which is hard),
     it is possible to connect from the outside to lph. We detect
     this problem, and give up on the pipe pair, but external code can 
     make our programs unreliable.
   *)
  dlog "pipe_pair";
  let mode' =
    match mode with
      | Pipe_in -> Pipe_out
      | Pipe_out -> Pipe_in
      | Pipe_duplex -> Pipe_duplex in
  let name = unpredictable_pipe_name() in
  let psrv = create_local_pipe_server name mode 1 in
  pipe_listen psrv 1;
  let rph = pipe_connect name mode' in
  ( try
      pipe_listen psrv 0;
      let lph = pipe_accept psrv in
      ( try
	  let s = String.create 0 in
	  ignore(pipe_write lph s 0 0);
	  dlogr 
	    (fun () -> 
	       sprintf "pipe_pair: returning \
                        name=%s proxydescr1=%Ld proxydescr2=%Ld" 
		 name
		 (int64_of_file_descr (snd lph)) 
		 (int64_of_file_descr (snd rph)) 
	    );
	  (lph, rph)
	with e -> 
	  pipe_shutdown lph; 
	  raise e
      )
    with e -> 
      pipe_shutdown rph; 
      raise e
  )
      
