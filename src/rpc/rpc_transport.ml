(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Rpc
open Rpc_packer
open Printf


type 't result =
    [ `Ok of 't
    | `Error of exn
    ]

type 't result_eof =
    [ 't result
    | `End_of_file
    ]


type sockaddr =
    [ `Implied
    | `Sockaddr of Unix.sockaddr
    ]

let string_of_sockaddr =
  function
    | `Implied -> "<implied>"
    | `Sockaddr sa -> Netsys.string_of_sockaddr sa

exception Error of string

type in_rule =
    [ `Deny
    | `Drop
    | `Reject
    | `Reject_with of Rpc.server_error
    | `Accept
    ]

type in_record =
    [ `Deny
    | `Drop
    | `Reject of packed_value
    | `Reject_with of packed_value * Rpc.server_error
    | `Accept of packed_value
    ]

let string_of_in_rule =
  function
    | `Deny -> "Deny"
    | `Drop -> "Drop"
    | `Reject -> "Reject"
    | `Reject_with _ -> "Reject_with"
    | `Accept -> "Accept"


class type rpc_multiplex_controller =
object
  method alive : bool
  method event_system : Unixqueue.event_system
  method getsockname : sockaddr
  method getpeername : sockaddr
  method peer_user_name : string option
  method protocol : protocol
  method reading : bool
  method read_eof : bool
  method start_reading : 
    ?peek: (unit -> unit) ->
    ?before_record:( int -> sockaddr -> in_rule ) ->
    when_done:( (in_record * sockaddr) result_eof -> unit) -> unit -> unit
  method cancel_rd_polling : unit -> unit
  method abort_rw : unit -> unit
  method writing : bool
  method start_writing :
    when_done:(unit result -> unit) -> packed_value -> sockaddr -> unit
  method start_shutting_down :
    when_done:(unit result -> unit) -> unit -> unit
  method cancel_shutting_down : unit -> unit
  method set_timeout : notify:(unit -> unit) -> float -> unit
  method inactivate : unit -> unit
end

module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Rpc_transport" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Rpc_transport" Debug.enable

let () =
  Netlog.Debug.register_module "Rpc_transport" Debug.enable


let mem_size = 65536        (* for allocated bigarrays *)
let fallback_size = 16384   (* for I/O via Unix *)

let mem_alloc() =
  try
    Netsys_mem.alloc_memory_pages mem_size
  with Invalid_argument _ -> (* OS does not support it... *)
    Bigarray.Array1.create
      Bigarray.char Bigarray.c_layout mem_size


let mem_dummy() =
  Bigarray.Array1.create
    Bigarray.char Bigarray.c_layout 0
  


class datagram_rpc_multiplex_controller sockname peername_opt peer_user_name_opt
        (mplex : Uq_engines.datagram_multiplex_controller) esys 
      : rpc_multiplex_controller =
object(self)
  val rd_buffer = 
    if mplex#mem_supported then (
      `Mem(mem_alloc())
    )
    else (
      `String(String.create fallback_size)
    )
    (* Max. size of an Internet datagram is 64 K. See RFC 760. However,
     * the Unix library uses a buffer size of only 16 K. Longer messages
     * can neither be received nor sent without truncation.
     *)

  method alive = mplex # alive
  method event_system = esys
  method getsockname = sockname
  method getpeername = 
    match peername_opt with
      | None -> failwith "#getpeername: not connected"
      | Some a -> a
  method protocol = Udp
  method peer_user_name = peer_user_name_opt
  method reading = mplex # reading
  method read_eof = mplex # read_eof
  method writing = mplex # writing

  val mutable aborted = false


  method private rd_buffer_contents n =  (* first n bytes *)
    match rd_buffer with
      | `String s ->
	  String.sub s 0 n
      | `Mem m ->
	  let s = String.create n in
	  Netsys_mem.blit_memory_to_string m 0 s 0 n;
	  s


  method start_reading ?peek 
                       ?(before_record = fun _ _ -> `Accept)
                       ~when_done () =
    let mplex_when_done exn_opt n =
      self # timer_event `Stop `R;
      match exn_opt with
	| None ->
	    let peer = `Sockaddr (mplex # received_from) in
	    (* TODO: Catch Failure here, and map to `Implied *)

	    let in_rule = before_record n peer in 
	    (* might have called abort_rw, hence we have to test this: *)
	    if not aborted then (
	      let r =
		match in_rule with
		  | `Deny -> `Deny
		  | `Drop -> `Drop
		  | `Reject -> 
		      let pv = 
			packed_value_of_string (self # rd_buffer_contents n) in
		      `Reject pv
		  | `Reject_with (code : Rpc.server_error) -> 
		      let pv = 
			packed_value_of_string (self # rd_buffer_contents n) in
		      `Reject_with(pv,code)
		  | `Accept -> 
		      let pv = 
			packed_value_of_string (self # rd_buffer_contents n) in
		      `Accept pv in
 	      when_done (`Ok(r, peer))
	    )
	| Some End_of_file ->
	    assert false
	| Some Uq_engines.Cancelled ->  (* abort case *)
	    ()   (* Ignore *)
	| Some error ->
	    when_done (`Error error)
    in
    ( match rd_buffer with
	| `String s ->
	    mplex # start_reading ?peek ~when_done:mplex_when_done 
	      s 0 (String.length s)
	| `Mem m ->
	    mplex # start_mem_reading ?peek ~when_done:mplex_when_done 
	      m 0 (Bigarray.Array1.dim m)
	      (* saves us 1 string copy! *)
	      
    );
    self # timer_event `Start `R


  val wr_buffer =
    if mplex#mem_supported then mem_alloc() else mem_dummy()
  

  method start_writing ~when_done pv addr =
    ( match addr with
	| `Implied ->
	    failwith "Rpc_transport.datagram_rpc_multiplex_controller: \
                      Cannot send datagram to implied address"
	| `Sockaddr a ->
	    mplex # send_to a
    );
    let mplex_when_done slen exn_opt n =
      self # timer_event `Stop `W;
      match exn_opt with
	| None ->
	    if n = slen then
	      when_done (`Ok ())
	    else
	      when_done (`Error (Error "Datagram too large"))
	| Some Uq_engines.Cancelled ->
	    ()  (* ignore *)
	| Some error ->
	    when_done (`Error error) in

    let s = string_of_packed_value pv in
    let slen = String.length s in
    if slen > fallback_size && mplex#mem_supported then (
      let mlen = min slen mem_size in
      Netsys_mem.blit_string_to_memory s 0 wr_buffer 0 mlen;
      mplex # start_mem_writing
	~when_done:(mplex_when_done slen) wr_buffer 0 mlen
    )
    else
      mplex # start_writing
	~when_done:(mplex_when_done slen) s 0 slen;
    self # timer_event `Start `W
    (* start_mem_writing is only reasonable for dealing with messages larger
       than 16K that are not supported by [Unix.send].
     *)

  method cancel_rd_polling () =
    if mplex#reading then
      mplex # cancel_reading()

  method abort_rw () =
    aborted <- true;
    mplex # cancel_reading();
    mplex # cancel_writing()
    
  method start_shutting_down ~when_done () =
    mplex # start_shutting_down
      ~when_done:(fun exn_opt ->
		    self # timer_event `Stop `D;
		    match exn_opt with
		      | None -> when_done (`Ok ())
		      | Some error -> when_done (`Error error)
		 )
      ();
    self # timer_event `Start `D

  method cancel_shutting_down () =
    self # timer_event `Stop `D;
    mplex # cancel_shutting_down()

  method inactivate () =
    self # stop_timer();
    mplex # inactivate()

  val mutable timer = None
  val mutable timer_r = `Stop
  val mutable timer_w = `Stop
  val mutable timer_d = `Stop
  val mutable timer_group = None

  method set_timeout ~notify tmo =
    timer <- Some(notify, tmo)

  method private timer_event start_stop which =
    ( match timer with
	| None -> ()
	| Some(notify, tmo) ->
	    ( match which with
		| `R -> timer_r <- start_stop
		| `W -> timer_w <- start_stop
		| `D -> timer_d <- start_stop
	    );
	    self # stop_timer();
	    if timer_r = `Start || timer_w = `Start || timer_d = `Start then (
	      let g = Unixqueue.new_group esys in
	      timer_group <- Some g;
	      Unixqueue.once esys g tmo
		(fun () -> 
		   timer_group <- None;
		   notify()
		)
	    );
    )


  method private stop_timer() =
    ( match timer_group with
	| None -> ()
	| Some g -> Unixqueue.clear esys g
    );
    timer_group <- None;
    timer_r <- `Stop;
    timer_w <- `Stop;
    timer_d <- `Stop


end



let datagram_rpc_multiplex_controller ?(close_inactive_descr=true)
                                      ?(preclose=fun() -> ()) fd esys =
  let sockname, peername_opt = 
    match Netsys.get_fd_style fd with
      | `Recv_send(sockaddr,peeraddr) ->
	  (`Sockaddr sockaddr, Some(`Sockaddr peeraddr))
      | `Recvfrom_sendto ->
	  (* Usually there is a sockname: *)
	  let sockname =
	    try `Sockaddr(Unix.getsockname fd)
	    with _ -> `Implied in
	  (sockname, None)
      | _ ->
	  (`Implied, Some `Implied) in
  let mplex = 
    Uq_engines.create_multiplex_controller_for_datagram_socket
      ~close_inactive_descr ~preclose
      fd esys in
  new datagram_rpc_multiplex_controller sockname peername_opt None mplex esys
;;


class stream_rpc_multiplex_controller sockname peername peer_user_name_opt
        (mplex : Uq_engines.multiplex_controller) esys 
      : rpc_multiplex_controller =
  let () = 
    dlogr (fun () ->
	     sprintf "new stream_rpc_multiplex_controller mplex=%d"
	       (Oo.id mplex))
  in
object(self)
  val mutable rd_buffer = Netpagebuffer.create mem_size
  val mutable rd_buffer_nomem = 
    if mplex#mem_supported then "" else String.create fallback_size

  val mutable rm_buffer = String.create 4
  val mutable rm_buffer_len = 0

  val mutable rd_mode = `RM
  val mutable rd_pos = 0      (* start of record marker or payload section *)

  val mutable rd_queue = Queue.create()
  val mutable rd_queue_len = 0

  val mutable rd_processing = false

  method alive = mplex # alive
  method event_system = esys
  method getsockname = sockname
  method getpeername = peername
  method protocol = Tcp
  method peer_user_name = peer_user_name_opt
  method reading = mplex # reading
  method read_eof = mplex # read_eof
  method writing = mplex # writing

  val mutable aborted = false

  method start_reading ?peek
                       ?(before_record = fun _ _ -> `Accept) 
                       ~when_done () =
    assert(not mplex#reading);

    let rec est_reading() =
      let mplex_when_done exn_opt n =
	self # timer_event `Stop `R;
	match exn_opt with
	  | None ->
	      process `Accept
	  | Some End_of_file ->
	      if rd_mode = `RM && Queue.is_empty rd_queue then
		return_eof()   (* EOF between messages *)
	      else
		return_error (Error "EOF within message")
	  | Some Uq_engines.Cancelled ->
	      ()   (* Ignore *)
	  | Some error ->
	      return_error error 
      in
      
      rd_processing <- false;
      if mplex#mem_supported then (
	let (b, start, len) = Netpagebuffer.page_for_additions rd_buffer in
	mplex # start_mem_reading 
	  ?peek 
	  ~when_done:(fun exn_opt n ->
			Netpagebuffer.advance rd_buffer n;
			mplex_when_done exn_opt n
		     )
	  b
	  start
	  len
      )
      else (
	mplex # start_reading
	  ?peek
	  ~when_done:(fun exn_opt n ->
			Netpagebuffer.add_sub_string 
			  rd_buffer rd_buffer_nomem 0 n;
			mplex_when_done exn_opt n
		     )
	  rd_buffer_nomem
	  0
	  (String.length rd_buffer_nomem)
      );
      self # timer_event `Start `R

    and process (in_rule:in_rule) =
      let len = Netpagebuffer.length rd_buffer - rd_pos in
(* eprintf "rd_pos=%d len=%d in_rule=%s\n%!" rd_pos len (string_of_in_rule in_rule); *)
      if len > 0 then (
	match rd_mode with
	  | `RM ->
(* prerr_endline "RM"; *)
	      (* Read the record marker *)
	      let m = min (4 - rm_buffer_len) len in
	      Netpagebuffer.blit_to_string
		rd_buffer rd_pos rm_buffer rm_buffer_len m;
	      rm_buffer_len <- rm_buffer_len + m;
	      if rm_buffer_len = 4 then (
		rd_pos <- rd_pos + 4;
		rm_buffer_len <- 0;
		let rm_last = (Char.code rm_buffer.[0]) >= 128 in
		let rm_0 = (Char.chr ((Char.code rm_buffer.[0]) land 0x7f)) in
		let rm_opt =
		  try
		    let rm =
		      Rtypes.int_of_uint4
			(Rtypes.mk_uint4 
			   (rm_0,rm_buffer.[1],rm_buffer.[2],rm_buffer.[3])) in
		    if rm > Sys.max_string_length then
		      raise(Rtypes.Cannot_represent "");
		    if rd_queue_len > Sys.max_string_length - rm then
		      raise(Rtypes.Cannot_represent "");
		    Some(rm,rm_last)
		  with
		    | Rtypes.Cannot_represent _ -> None in
		( match rm_opt with
		    | Some(rm,rm_last) ->
(*eprintf "got RM n=%d last=%b\n%!" rm rm_last; *)
			let in_rule' =
			  match in_rule with
			    | `Accept ->
				before_record (rd_queue_len + rm) peername
			    | _ ->
				in_rule in
			if in_rule' = `Drop || in_rule' = `Deny then (
			  Netpagebuffer.delete_hd rd_buffer rd_pos;
			  rd_pos <- 0;
			);
			rd_mode <- `Payload(rm,rm_last);
			process in_rule'
		    | None ->
			return_error (Error "Record too large")
		)
	      )
	      else
		est_reading()
		
	  | `Payload(plen,is_last) ->
	      (* Read payload data *)
(* prerr_endline "payload"; *)
	      if len >= plen then (
(* eprintf "got fragment rd_pos=%d plen=%d\n%!" rd_pos plen; *)
		let fragment = (rd_pos, plen) in
		Queue.push fragment rd_queue;
		rd_queue_len <- rd_queue_len + plen;
		rd_pos <- rd_pos + plen;
		rd_mode <- `RM;
		if in_rule = `Drop || in_rule = `Deny then (
		  Netpagebuffer.delete_hd rd_buffer rd_pos;
		  rd_mode <- `Payload(plen-rd_pos,is_last);
		  rd_pos <- 0;
		);
		if is_last then (
		  let r =
		    match in_rule with
		      | (`Accept | `Reject | (`Reject_with _) as ar) ->
(* eprintf "creating string n=%d\n%!" rd_queue_len; *)
			  let msg = String.create rd_queue_len in
			  let q = ref 0 in
			  Queue.iter
			    (fun (p,l) ->
			       Netpagebuffer.blit_to_string
				 rd_buffer
				 p
				 msg
				 !q
				 l;
			       q := !q + l
			    )
			    rd_queue;
			  let pv = packed_value_of_string msg in
			  ( match ar with
			      | `Accept -> `Accept pv
			      | `Reject -> `Reject pv
			      | `Reject_with (code:Rpc.server_error) -> 
				  `Reject_with(pv,code)
			  ) 
		      | (`Deny | `Drop as dd) ->
			  dd 
		  in
		  Queue.clear rd_queue;
		  rd_queue_len <- 0;
		  Netpagebuffer.delete_hd rd_buffer rd_pos;
		  rd_pos <- 0;
		  rd_processing <- true;(* so [process] will be called again *)
		  return_msg r
		) else 
		  process in_rule
	      )
	      else
		est_reading()
      )
      else
	est_reading()

    and return_msg msg =
      if not aborted then
	when_done (`Ok(msg, peername))

    and return_error e =
      rd_processing <- false;
      if not aborted then
	when_done (`Error e)

    and return_eof () =
      rd_processing <- false;
      if not aborted then
	when_done `End_of_file 

    in
    if rd_processing then
      process `Accept
    else
      est_reading()
	    

  val wr_buffer =
    if mplex#mem_supported then
      mem_alloc()
    else
      Bigarray.Array1.create Bigarray.char Bigarray.c_layout 0


  method start_writing ~when_done pv addr =

    assert(not mplex#writing);

    let rec est_writing s pos mpos mlen =
      let slen = String.length s in
      let len = slen - pos in

      let mplex_when_done exn_opt n =
	self # timer_event `Stop `W;
	match exn_opt with
	  | None ->
	      let pos' = pos + n in
	      assert(pos' <= slen);
	      if pos' = slen then (
		if not aborted then
		  when_done (`Ok ())
	      )
	      else (
		if not aborted then
		  est_writing s pos' (mpos+n) mlen
	      )
	  | Some Uq_engines.Cancelled ->
	      ()  (* ignore *)
	  | Some error ->
	      if not aborted then
		when_done (`Error error)
      in
      if mplex#mem_supported then (
	if mpos < mlen then  (* feels a bit hacky... *)
	  mplex # start_mem_writing
	    ~when_done:mplex_when_done wr_buffer mpos (mlen-mpos)
	else (
	  let mlen = min len mem_size in
	  Netsys_mem.blit_string_to_memory s pos wr_buffer 0 mlen;
	  mplex # start_mem_writing
	    ~when_done:mplex_when_done wr_buffer 0 mlen
	)
      )
      else
	mplex # start_writing
	  ~when_done:mplex_when_done s pos len;
      self # timer_event `Start `W
    in

    ( match addr with
	| `Implied -> ()
	| `Sockaddr a ->
	    if addr <> peername then
	      failwith "Rpc_transport.stream_rpc_multiplex_controller: \
                        cannot send to this address"
    );
    let s = rm_string_of_packed_value pv in
    (* Patch record marker at the beginning of [s] *)
    let rm = Rtypes.uint4_of_int (String.length s - 4) in
    Rtypes.write_uint4 s 0 rm;
    s.[0] <- Char.chr (Char.code s.[0] lor 0x80);
    est_writing s 0 0 0


  method cancel_rd_polling () =
    if mplex#reading then
      mplex # cancel_reading()

  method abort_rw () =
    aborted <- true;
    mplex # cancel_reading();
    mplex # cancel_writing()
    
  method start_shutting_down ~when_done () =
    dlogr (fun () ->
	     sprintf "start_shutting_down mplex=%d"
	       (Oo.id mplex));
    mplex # start_shutting_down
      ~when_done:(fun exn_opt ->
		    dlogr (fun () ->
			     sprintf "done shutting_down mplex=%d"
			       (Oo.id mplex));
		    self # timer_event `Stop `D;
		    match exn_opt with
		      | None -> when_done (`Ok ())
		      | Some error -> when_done (`Error error)
		 )
      ();
    self # timer_event `Start `D

  method cancel_shutting_down () =
    self # timer_event `Stop `D;
    mplex # cancel_shutting_down()

  method inactivate () =
    dlogr (fun () ->
	     sprintf "inactivate mplex=%d"
	       (Oo.id mplex));
    self # stop_timer();
    mplex # inactivate()

  val mutable timer = None
  val mutable timer_r = `Stop
  val mutable timer_w = `Stop
  val mutable timer_d = `Stop
  val mutable timer_group = None

  method set_timeout ~notify tmo =
    timer <- Some(notify, tmo)

  method private timer_event start_stop which =
    ( match timer with
	| None -> ()
	| Some(notify, tmo) ->
	    ( match which with
		| `R -> timer_r <- start_stop
		| `W -> timer_w <- start_stop
		| `D -> timer_d <- start_stop
	    );
	    self # stop_timer();
	    if timer_r = `Start || timer_w = `Start || timer_d = `Start then (
	      let g = Unixqueue.new_group esys in
	      timer_group <- Some g;
	      Unixqueue.once esys g tmo
		(fun () -> 
		   timer_group <- None;
		   notify()
		)
	    );
    )


  method private stop_timer() =
    ( match timer_group with
	| None -> ()
	| Some g -> Unixqueue.clear esys g
    );
    timer_group <- None;
    timer_r <- `Stop;
    timer_w <- `Stop;
    timer_d <- `Stop

end



let stream_rpc_multiplex_controller ?(close_inactive_descr=true)
                                    ?(preclose=fun()->()) fd esys =
  let sockname = 
    try
      `Sockaddr(Unix.getsockname fd) 
    with
      | Unix.Unix_error(_,_,_) -> `Implied in
  let peername = 
    try
      `Sockaddr(Netsys.getpeername fd)
    with
      | Unix.Unix_error(_,_,_) -> `Implied in
  let mplex = 
    Uq_engines.create_multiplex_controller_for_connected_socket
      ~close_inactive_descr ~preclose
      fd esys in
  new stream_rpc_multiplex_controller sockname peername None mplex esys
;;
