(* $Id$ *)

external fill_random : string -> unit = "netsys_fill_random"


type c_event
type c_pipe_helper

type w32_event = (c_event * Unix.file_descr)
    (* The descriptor is the proxy descriptor *)

type w32_pipe_helper_pad =
    { pipe_proxy : Unix.file_descr;
      mutable pipe_signal : w32_event option;
      pipe_rd_event : w32_event;
      pipe_wr_event : w32_event;
    }

type w32_pipe_helper = (c_pipe_helper * w32_pipe_helper_pad)


type pipe_mode = Pipe_in | Pipe_out | Pipe_duplex
type pipe_conn_state = Pipe_deaf | Pipe_listening | Pipe_connected | Pipe_down

type w32_object =
    | W32_event of w32_event
    | W32_pipe_helper of w32_pipe_helper


module Int64Map = Map.Make(Int64)


external int64_of_file_descr : Unix.file_descr -> int64
  = "netsys_int64_of_file_descr"
  (* Also occurs in netsys.ml! *)



let proxies = ref Int64Map.empty
let proxies_count = ref 0
let proxies_mutex = !Netsys_oothr.provider # create_mutex()
let proxies_unreg = ref 0


let unregister_proxy fd =
  (* We can only do very simple things here. Changing [proxies] could compete
     with another ongoing change. So the only remaining thing is to look
     up the right proxy, and set it to None.
   *)
  proxies_unreg := !proxies_unreg + 1;  (* this is atomic in ocaml *)
  try
    let fd_num = int64_of_file_descr fd in
    let obj_opt = Int64Map.find fd_num !proxies in
    obj_opt := None
  with
    | Not_found -> ()


let register_proxy fd w32_obj =
  let fd_num = int64_of_file_descr fd in
  assert(not (Int64Map.mem fd_num !proxies));
  (* Registering the same handle twice is not supported, even if the
     handle is wrapped into different file_descr blocks
   *)
(* prerr_endline ("register_proxy " ^ Int64.to_string (int64_of_file_descr fd)); *)
  proxies_mutex # lock();
  if not (Int64Map.mem fd_num !proxies) then (
    proxies := Int64Map.add fd_num (ref (Some w32_obj)) !proxies;
    incr proxies_count;
    if !proxies_unreg > !proxies_count then (
      let p, n = 
	(* Throw out all unregistered proxies. *)
	Int64Map.fold
	  (fun k v (acc, cnt) ->
	     if !v <> None then (Int64Map.add k v acc, cnt+1) else (acc, cnt)
	  )
	  !proxies
	  (Int64Map.empty, 0) in
      proxies := p;
      proxies_count := n;
      proxies_unreg := 0;  (* ":=" is atomic in ocaml *)
    );
  );
  proxies_mutex # unlock();
  Gc.finalise unregister_proxy fd


let lookup fd =
(* prerr_endline ("lookup " ^ Int64.to_string (int64_of_file_descr fd)); *)
  let fd_num = int64_of_file_descr fd in
  let obj_opt = Int64Map.find fd_num !proxies in
  match !obj_opt with
    | Some obj -> 
(* prerr_endline "lookup ok"; *)
	obj
    | None -> raise Not_found
	

let lookup_pipe_helper fd =
  try
    match lookup fd with
      | W32_pipe_helper ph -> ph
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_pipe_helper: not found"

let lookup_event fd =
  try
    match lookup fd with
      | W32_event e -> e
      | _ -> raise Not_found
  with Not_found ->
    failwith "Netsys_win32.lookup_event: not found"





external create_event0 : unit -> c_event 
  = "netsys_create_event"

external event_descr0 : c_event -> Unix.file_descr
  = "netsys_event_descr"

external close_event : c_event -> unit
  = "netsys_close_event"

let decorate_event e =
  let fd = event_descr0 e in
  let ev = (e, fd) in
  Gc.finalise close_event e;
  register_proxy fd (W32_event ev);
  ev

let create_event() =
  decorate_event(create_event0())

let event_descr (_,fd) = fd

external set_event0 : c_event -> unit
  = "netsys_set_event"

external reset_event0 : c_event -> unit
  = "netsys_reset_event"

external test_event0 : c_event -> bool
  = "netsys_test_event"

external event_wait0 : c_event -> int -> bool
  = "netsys_event_wait"

let set_event (e,_)   = set_event0 e
let reset_event (e,_) = reset_event0 e
let test_event (e,_)  = test_event0 e

let event_wait (e,_) tmo =
  Netsys_impl_util.slice_time_ms
    (fun tmo_ms ->
       if event_wait0 e tmo_ms then Some () else None
    )
    tmo
  <> None



external wsa_event_select0 :  
  c_event -> Unix.file_descr -> Netsys_posix.poll_req_events -> unit
  = "netsys_wsa_event_select"

external wsa_maximum_wait_events : unit -> int
  = "netsys_wsa_maximum_wait_events"

external wsa_wait_for_multiple_events0 : 
  c_event array -> int -> int option
  = "netsys_wsa_wait_for_multiple_events"

external wsa_enum_network_events0 : 
  Unix.file_descr -> c_event -> Netsys_posix.poll_act_events
  = "netsys_wsa_enum_network_events"

let wsa_event_select (e,_) fd pie =
   wsa_event_select0 e fd pie

let wsa_wait_for_multiple_events ea n =
  wsa_wait_for_multiple_events0 (Array.map fst ea) n

let wsa_enum_network_events fd (e,_) =
  wsa_enum_network_events0 fd e




external pipe_free0 : 
  c_pipe_helper -> unit
  = "netsys_pipe_free"

external create_local_named_pipe0 :
  string -> pipe_mode -> int -> c_pipe_helper
  = "netsys_create_local_named_pipe"

external pipe_listen0 :
  c_pipe_helper -> unit
  = "netsys_pipe_listen"

external pipe_deafen0 :
  c_pipe_helper -> unit
  = "netsys_pipe_deafen"

external pipe_connect0 :
  string -> pipe_mode -> c_pipe_helper
  = "netsys_pipe_connect"

external pipe_read0 :
  c_pipe_helper -> string -> int -> int -> int
  = "netsys_pipe_read"

external pipe_write0 :
  c_pipe_helper -> string -> int -> int -> int
  = "netsys_pipe_write"

external pipe_shutdown0 :
  c_pipe_helper -> unit
  = "netsys_pipe_shutdown"

external pipe_rd_event0 :
  c_pipe_helper -> c_event
  = "netsys_pipe_rd_event"

external pipe_wr_event0 :
  c_pipe_helper -> c_event
  = "netsys_pipe_wr_event"

external pipe_descr0 :
   c_pipe_helper -> Unix.file_descr
  = "netsys_pipe_descr"

external pipe_conn_state0 : 
  c_pipe_helper -> pipe_conn_state
  = "netsys_pipe_conn_state"

external pipe_signal0 :
  c_pipe_helper -> c_event -> unit
  = "netsys_pipe_signal"


let decorate_pipe_helper ph =
  Gc.finalise pipe_free0 ph;
  let fd = pipe_descr0 ph in
  let pad =
    { pipe_proxy = fd;
      pipe_signal = None;
      pipe_rd_event = decorate_event(pipe_rd_event0 ph);
      pipe_wr_event = decorate_event(pipe_wr_event0 ph);
    } in
  register_proxy fd (W32_pipe_helper(ph,pad));
  (ph,pad)


let create_local_named_pipe name mode n =
  decorate_pipe_helper(create_local_named_pipe0 name mode n)

let pipe_connect name mode =
  decorate_pipe_helper(pipe_connect0 name mode)

let pipe_descr (_,pad) = pad.pipe_proxy

let pipe_listen (ph,_) = pipe_listen0 ph
let pipe_deafen (ph,_) = pipe_deafen0 ph

let pipe_rd_event (_,pad) =
  pad.pipe_rd_event

let pipe_wr_event (_,pad) =
  pad.pipe_wr_event

let pipe_read (ph,_) s pos len =
  if pos < 0 || len < 0 || pos > String.length s - len then
    invalid_arg "Netsys_win32.pipe_read";
  pipe_read0 ph s pos len

let pipe_write (ph,_) s pos len =
  if pos < 0 || len < 0 || pos > String.length s - len then
    invalid_arg "Netsys_win32.pipe_write";
  pipe_write0 ph s pos len

let pipe_shutdown (ph,_) = pipe_shutdown0 ph
let pipe_conn_state (ph,_) = pipe_conn_state0 ph

let pipe_signal (ph,pad) ((e,_) as ev)  =
  match pad.pipe_signal with
    | Some _ ->
	invalid_arg "pipe_signal: signal event already set"
    | None ->
	(* We put the signal event into the pad, so it will survive the
           pipe helper (there must never be an invalid event inside
           the pipe helper)
	 *)
	pad.pipe_signal <- Some ev;
	pipe_signal0 ph e


let pipe_wait_rd (ph,pad) tmo =
  event_wait pad.pipe_rd_event tmo

let pipe_wait_wr (ph,pad) tmo =
  event_wait pad.pipe_wr_event tmo


let counter = ref 0
let counter_mutex = !Netsys_oothr.provider # create_mutex()

let pipe_pair mode =
  (* FIXME: If somebody guesses the pipe name (which is hard),
     it is possible to connect from the outside to lph. We detect
     this problem, and give up on the pipe pair, but external code can 
     make our programs unreliable.
   *)
  let mode' =
    match mode with
      | Pipe_in -> Pipe_out
      | Pipe_out -> Pipe_in
      | Pipe_duplex -> Pipe_duplex in
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
(*prerr_endline "pipe_pair";*)
  let lph = create_local_named_pipe name mode 1 in
  ( try
      pipe_listen lph;
      let rph = pipe_connect name mode' in
      ( try
	  let s = String.create 0 in
	  ignore(pipe_write lph s 0 0);
	  (lph, rph)
	with e -> 
	  pipe_shutdown rph; 
	  raise e
      )
    with e -> 
      pipe_shutdown lph; 
      raise e
  )
      
