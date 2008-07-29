(* $Id$ *)

type w32_event

external create_event0 : unit -> w32_event 
  = "netsys_create_event"

external close_event : w32_event -> unit
  = "netsys_close_event"

let create_event() =
  let e = create_event0() in
  Gc.finalise close_event e;
  e

external set_event : w32_event -> unit
  = "netsys_set_event"

external reset_event : w32_event -> unit
  = "netsys_reset_event"

external wsa_event_select :  
  w32_event -> Unix.file_descr -> Netsys.poll_in_events -> unit
  = "netsys_wsa_event_select"

external wsa_maximum_wait_events : unit -> int
  = "netsys_wsa_maximum_wait_events"

external wsa_wait_for_multiple_events : 
  w32_event array -> int -> int option
  = "netsys_wsa_wait_for_multiple_events"

external wsa_enum_network_events : 
  Unix.file_descr -> w32_event -> Netsys.poll_out_events
  = "netsys_wsa_enum_network_events"


type w32_pipe_helper

type pipe_mode = Pipe_in | Pipe_out | Pipe_duplex

external pipe_free : 
  w32_pipe_helper -> unit
  = "netsys_pipe_free"

external create_local_named_pipe0 :
  string -> pipe_mode -> int -> w32_pipe_helper
  = "netsys_create_local_named_pipe"

external pipe_listen :
  w32_pipe_helper -> unit
  = "netsys_pipe_listen"

external pipe_unlisten :
  w32_pipe_helper -> unit
  = "netsys_pipe_unlisten"

external pipe_connect0 :
  string -> pipe_mode -> w32_pipe_helper
  = "netsys_pipe_connect"

external pipe_read0 :
  w32_pipe_helper -> string -> int -> int -> int
  = "netsys_pipe_read"

external pipe_write0 :
  w32_pipe_helper -> string -> int -> int -> int
  = "netsys_pipe_write"

external pipe_close0 :
  w32_pipe_helper -> unit
  = "netsys_pipe_close"

external pipe_rd_event0 :
  w32_pipe_helper -> w32_event
  = "netsys_pipe_rd_event"

external pipe_wr_event0 :
  w32_pipe_helper -> w32_event
  = "netsys_pipe_wr_event"

external pipe_descr0 :
   w32_pipe_helper -> Unix.file_descr
  = "netsys_pipe_descr"

let create_local_named_pipe name mode n =
  let ph = create_local_named_pipe0 name mode n in
  Gc.finalise pipe_free ph;
  ph

let pipe_connect name mode =
  let ph = pipe_connect0 name mode in
  Gc.finalise pipe_free ph;
  ph

let pipe_rd_event ph =
  let e = pipe_rd_event0 ph in
  Gc.finalise close_event e;
  e

let pipe_wr_event ph =
  let e = pipe_wr_event0 ph in
  Gc.finalise close_event e;
  e

let pipe_read ph s pos len =
  if pos < 0 || len < 0 || pos > String.length s - len then
    invalid_arg "Netsys_win32.pipe_read";
  pipe_read0 ph s pos len

let pipe_write ph s pos len =
  if pos < 0 || len < 0 || pos > String.length s - len then
    invalid_arg "Netsys_win32.pipe_write";
  pipe_write0 ph s pos len



let pipes = Hashtbl.create 10
let mutex = !Netsys_oothr.provider # create_mutex()

let pipe_close ph =
  mutex # lock();
  Hashtbl.remove pipes (pipe_descr0 ph);
  mutex # unlock();
  pipe_close0 ph

let pipe_descr ph =
  mutex # lock();
  let fd = pipe_descr0 ph in
  if not (Hashtbl.mem pipes fd) then
    Hashtbl.add pipes fd ph;
  mutex # unlock();
  fd

let lookup_pipe fd =
  mutex # lock();
  let ph_opt =
    try
      Some(Hashtbl.find pipes fd)
    with Not_found -> None in
  mutex # unlock();
  match ph_opt with
    | Some ph -> ph
    | None -> raise Not_found
