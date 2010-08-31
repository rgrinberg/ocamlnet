(* $Id$ *)

open Uq_engines.Operators

type string_like =
    [ `String of string
    | `Memory of Netsys_mem.memory
    ]


class type obj_buffer =
object
  method length : int
  method blit_out : int -> string_like -> int -> int -> unit
  method delete_hd : int -> unit
  method index_from : int -> char -> int
  method add : string_like -> int -> int -> unit
  method advance : int -> unit
  method page_for_additions : string_like * int * int
  method page_for_consumption : string_like * int * int
end


class type in_buffer =
object
  method buffer : obj_buffer
  method eof : bool
  method start_fill_e : unit -> bool Uq_engines.engine
  method fill_e_opt : bool Uq_engines.engine option
    (* The current fill engine, or None *)
  method shutdown_e : unit -> unit Uq_engines.engine
  method inactivate : unit -> unit
  method event_system : Unixqueue.event_system
end


class type out_buffer =
object
  method buffer : obj_buffer
  method eof : bool
  method max : int option
  method start_flush_e : unit -> unit Uq_engines.engine
  method flush_e_opt : unit Uq_engines.engine option
    (* The current flush engine, or None *)
  method write_eof_e : unit -> bool Uq_engines.engine
    (* The buffer must be empty before [write_eof_e] *)
  method shutdown_e : float option -> unit Uq_engines.engine
  method inactivate : unit -> unit
  method event_system : Unixqueue.event_system
end

	  
type in_device =
    [ `Polldescr of Netsys.fd_style * Unix.file_descr * Unixqueue.event_system
    | `Multiplex of Uq_engines.multiplex_controller
    | `Async_in of Uq_engines.async_in_channel * Unixqueue.event_system
    | `Buffer_in of in_buffer
    ]


type out_device =
    [ `Polldescr of Netsys.fd_style * Unix.file_descr * Unixqueue.event_system
    | `Multiplex of Uq_engines.multiplex_controller
    | `Async_out of Uq_engines.async_out_channel * Unixqueue.event_system
    | `Buffer_out of out_buffer
    ]


type in_bdevice =
    [ `Buffer_in of in_buffer ]


let device_esys =
  function
    | `Polldescr(_,_,esys) -> esys
    | `Multiplex mplex -> mplex#event_system
    | `Async_in(_,esys) -> esys
    | `Async_out(_,esys) -> esys
    | `Buffer_in b -> b#event_system
    | `Buffer_out b -> b#event_system

let device_supports_memory =
  function
    | `Polldescr(style,_,_) -> 
	( match style with
	    | `Read_write | `Recv_send _ | `Recv_send_implied ->
		true
	    | _ ->
		false
	)
    | `Multiplex mplex -> 
	mplex # mem_supported
    | `Async_in(_,esys) -> 
	false
    | `Async_out(_,esys) -> 
	false
    | `Buffer_in b -> 
	true
    | `Buffer_out b -> 
	true


let mem_gread style fd m pos len =
  match style with
    | `Read_write ->
	Netsys_mem.mem_read fd m pos len
    | `Recv_send _ | `Recv_send_implied ->
	Netsys_mem.mem_recv fd m pos len []
    | _ ->
	failwith ("Uq_io: This fd style does not support `Memory: " ^ 
		    Netsys.string_of_fd_style style)


let mem_gwrite style fd m pos len =
  match style with
    | `Read_write ->
	Netsys_mem.mem_write fd m pos len
    | `Recv_send _ | `Recv_send_implied ->
	Netsys_mem.mem_send fd m pos len []
    | _ ->
	failwith ("Uq_io: This fd style does not support `Memory: " ^ 
		    Netsys.string_of_fd_style style)


let ach_input_e ch esys s pos len =
  (* case: async channel *)

  let (e, signal) = Uq_engines.signal_engine esys in

  let rec wait_for_input () =
    try
      let n = ch # input s pos len in
      if n > 0 || len = 0 then
	signal (`Done n)
      else (
	ch # request_notification
	  (fun () ->
	     wait_for_input();
	     false
	  )
      )
    with
      | error -> signal (`Error error)
  in

  wait_for_input();
  e


let rec buf_input_e b ms pos len =
  let bl = b#buffer#length in
  if bl > 0 || len = 0 then (
    let n = min len bl in
    b#buffer#blit_out 0 ms pos n;
    b#buffer#delete_hd n;
    eps_e (`Done n) b#event_system
  )
  else if b#eof then (
    eps_e (`Error End_of_file) b#event_system
  )
  else (
    let fe =
      match b#fill_e_opt with
	| None -> b#start_fill_e ()
	| Some fe -> fe in
    fe ++ (fun _ -> buf_input_e b ms pos len)
  )


let input_e d0 ms pos len =
  let d = (d0 :> in_device) in
  match d with
    | `Polldescr(style, fd, esys) ->
	new Uq_engines.input_engine
	  (fun fd -> 
	     match ms with
	       | `String s ->
		   let n = Netsys.gread style fd s pos len in
		   if len > 0 && n = 0 then raise End_of_file;
		   n
	       | `Memory m ->
		   let n = mem_gread style fd m pos len in
		   if len > 0 && n = 0 then raise End_of_file;
		   n
	  )
	  fd (-1.0) esys

    | `Multiplex mplex ->
	let (e, signal) = Uq_engines.signal_engine mplex#event_system in
	let cancel() =
	  if not mplex#reading then mplex # cancel_reading() in
	( match ms with
	    | `String s ->
		mplex # start_reading
		  ~when_done:(fun xopt n ->
				match xopt with
				  | None -> signal (`Done n)
				  | Some Uq_engines.Cancelled ->
				      cancel(); signal `Aborted
				  | Some err -> signal (`Error err)
			     )
		  s pos len;
	    | `Memory m ->
		if mplex#mem_supported then
		  mplex # start_mem_reading
		    ~when_done:(fun xopt n ->
				  match xopt with
				    | None -> signal (`Done n)
				    | Some Uq_engines.Cancelled ->
					cancel(); signal `Aborted
				    | Some err -> signal (`Error err)
			       )
		    m pos len
		else
		  signal
		    (`Error
		       (Failure "Uq_io: This mplex does not support `Memory"));
	);
	e >> (function
		| `Done n -> `Done n
		| `Error e -> `Error e 
		| `Aborted -> cancel(); `Aborted
	     )
		    

    | `Async_in (ch,esys) ->
	( match ms with
	    | `String s ->
		ach_input_e ch esys s pos len
	    | `Memory m ->
		eps_e
		  (`Error
		     (Failure "Uq_io: async channels do not support `Memory"))
		  esys
	)
	
    | `Buffer_in b ->
	buf_input_e b ms pos len


let rec really_input_e d ms pos len =
  if len = 0 then
    eps_e (`Done ()) (device_esys d)
  else
    input_e d ms pos len ++ 
      (fun n -> really_input_e d ms (pos+n) (len-n))


let input_line_e (`Buffer_in b) =
  let consume k1 k2 =
    let s = String.create k1 in
    b#buffer#blit_out 0 (`String s) 0 k1;
    b#buffer#delete_hd k2;
    s in
  let rec look_ahead eof =
    try
      let k = b#buffer#index_from 0 '\n' in
      let s = consume k (k+1) in
      eps_e (`Done s) b#event_system
    with
      | Not_found ->
	  if eof then (
	    let n = b#buffer#length in
	    if n = 0 then
	      eps_e (`Error End_of_file) b#event_system
	    else (
	      let s = consume n n in
	      eps_e (`Done s) b#event_system
	    )
	  )
	  else (
	    assert(not b#eof);
	    let fe =
	      match b#fill_e_opt with
		| None -> b#start_fill_e ()
		| Some fe -> fe in
	    fe ++ look_ahead
	  )
  in
  look_ahead b#eof


let ach_output_e ch esys s pos len =
  (* case: async channel *)

  let (e, signal) = Uq_engines.signal_engine esys in

  let rec wait_for_output () =
    try
      let n = ch # output s pos len in
      if n > 0 || len = 0 then
	signal (`Done n)
      else (
	ch # request_notification
	  (fun () ->
	     wait_for_output();
	     false
	  )
      )
    with
      | error -> signal (`Error error)
  in

  wait_for_output();
  e


let rec buf_output_e b ms pos len =
  if b # eof then
    eps_e
      (`Error (Failure "Uq_io: Buffer already closed for new data"))
      b#event_system
  else (
    let bl = b#buffer#length in
    let n =
      match b # max with
	| None -> len
	| Some m -> max (min len (m - bl)) 0 in
    if n > 0 || len = 0 then (
      b#buffer#add ms pos n;
      eps_e (`Done n) b#event_system
    )
    else (
      let fe =
	match b#flush_e_opt with
	  | None -> b#start_flush_e ()
	  | Some fe -> fe in
      fe ++ (fun _ -> buf_output_e b ms pos len)
    )
  )


let output_e d0 ms pos len =
  let d = (d0 :> out_device) in
  match d with
    | `Polldescr(style, fd, esys) ->
	new Uq_engines.output_engine
	  (fun fd -> 
	     match ms with
	       | `String s ->
		   Netsys.gwrite style fd s pos len
	       | `Memory m ->
		   mem_gwrite style fd m pos len
	  )
	  fd (-1.0) esys

    | `Multiplex mplex ->
	let (e, signal) = Uq_engines.signal_engine mplex#event_system in
	let cancel() =
	  if not mplex#writing then mplex # cancel_writing() in
	( match ms with
	    | `String s ->
		mplex # start_writing
		  ~when_done:(fun xopt n ->
				match xopt with
				  | None -> signal (`Done n)
				  | Some Uq_engines.Cancelled ->
				      cancel(); signal `Aborted
				  | Some err -> signal (`Error err)
			     )
		  s pos len;
	    | `Memory m ->
		if mplex#mem_supported then
		  mplex # start_mem_writing
		    ~when_done:(fun xopt n ->
				  match xopt with
				    | None -> signal (`Done n)
				    | Some Uq_engines.Cancelled ->
					cancel(); signal `Aborted
				    | Some err -> signal (`Error err)
			       )
		    m pos len
		else
		  signal
		    (`Error
		       (Failure "Uq_io: This mplex does not support `Memory"));
	);
	e >> (function
		| `Done n -> `Done n
		| `Error e -> `Error e 
		| `Aborted -> cancel(); `Aborted
	     )

    | `Async_out (ch,esys) ->
	( match ms with
	    | `String s ->
		ach_output_e ch esys s pos len
	    | `Memory m ->
		eps_e
		  (`Error
		     (Failure "Uq_io: async channels do not support `Memory"))
		  esys
	)
	
    | `Buffer_out b ->
	buf_output_e b ms pos len


let rec really_output_e d ms pos len =
  if len = 0 then
    eps_e (`Done ()) (device_esys d)
  else
    output_e d ms pos len ++ 
      (fun n -> really_output_e d ms (pos+n) (len-n))

let output_string_e d s =
  really_output_e d (`String s) 0 (String.length s)

let output_memory_e d m =
  really_output_e d (`Memory m) 0 (Bigarray.Array1.dim m)

let output_netbuffer_e d b =
  let s = Netbuffer.unsafe_buffer b in
  really_output_e d (`String s) 0 (Netbuffer.length b)

let flush_e d =
  match (d :> out_device) with
    | `Buffer_out b ->
	( match b#flush_e_opt with
	    | None -> b#start_flush_e ()
	    | Some fe -> fe
	)
    | _ ->
	eps_e (`Done()) (device_esys d)

let write_eof_e d0 =
  let d = (d0 :> out_device) in
  match d with
    | `Polldescr(style, fd, esys) ->
	eps_e (`Done false) esys
    | `Multiplex mplex ->
	let (e, signal) = Uq_engines.signal_engine mplex#event_system in
	let cancel() =
	  if not mplex#writing then mplex # cancel_writing() in
	if mplex # supports_half_open_connection then
	  mplex # start_writing_eof 
	    ~when_done:(fun xopt ->
			  match xopt with
			    | None -> signal (`Done true)
			    | Some Uq_engines.Cancelled ->
				cancel(); signal `Aborted
			    | Some error -> signal (`Error error)
		       )
	    ()
	else
	  signal (`Done false);
	e >> (function
		| `Done n -> `Done n
		| `Error e -> `Error e 
		| `Aborted -> cancel(); `Aborted
	     )
    | `Async_out (ch,esys) ->
	eps_e (`Done false) esys
    | `Buffer_out b ->
	flush_e d ++
	  (fun () -> b#write_eof_e())

let shutdown_e ?linger d0 =
  let d = (d0 :> [in_device | out_device]) in
  match d with
    | `Polldescr(style, fd, esys) ->
	Netsys.gclose style fd;
	eps_e (`Done()) esys
    | `Multiplex mplex ->
	if mplex#reading then
	  mplex#cancel_reading();
	if mplex#writing then
	  mplex#cancel_writing();
	let (e, signal) = Uq_engines.signal_engine mplex#event_system in
	let cancel() =
	  if not mplex#shutting_down then mplex # cancel_shutting_down() in
	mplex # start_shutting_down
	  ?linger
	  ~when_done:(fun xopt ->
			match xopt with
			  | None ->
			      mplex#inactivate();
			      signal (`Done())
			  | Some Uq_engines.Cancelled ->
			      cancel(); signal `Aborted
			  | Some error ->
			      signal (`Error error)
		     )
	  ();
	e >> (function
		| `Done n -> `Done n
		| `Error e -> `Error e 
		| `Aborted -> cancel(); `Aborted
	     )
    | `Async_in (ch,esys) ->
	ch # close_in();
	eps_e (`Done()) esys
    | `Async_out (ch,esys) ->
	ch # close_out();
	eps_e (`Done()) esys
    | `Buffer_in b ->
	b # shutdown_e ()
    | `Buffer_out b ->
	flush_e (`Buffer_out b) ++ (fun _ -> b # shutdown_e linger)

let inactivate d =
  match d with
    | `Polldescr(style, fd, esys) ->
	Netsys.gclose style fd
    | `Multiplex mplex ->
	mplex#inactivate()
    | `Async_in (ch,esys) ->
	ch # close_in()
    | `Async_out (ch,esys) ->
	ch # close_out()
    | `Buffer_in b ->
	b # inactivate ()
    | `Buffer_out b ->
	b # inactivate ()

let mem_obj_buffer() =
  let psize = 
    try Netsys_mem.getpagesize() with Invalid_argument _ -> 4096 in
  let buf = 
    Netpagebuffer.create psize in
  ( object
      method length = Netpagebuffer.length buf
      method blit_out bpos ms pos len =
	match ms with
	  | `String s -> Netpagebuffer.blit_to_string buf bpos s pos len
	  | `Memory m -> Netpagebuffer.blit_to_memory buf bpos m pos len
      method delete_hd n =
	Netpagebuffer.delete_hd buf n
      method index_from pos c =
	Netpagebuffer.index_from buf pos c
      method add ms pos len =
	match ms with
	  | `String s -> Netpagebuffer.add_sub_string buf s pos len
	  | `Memory m -> Netpagebuffer.add_sub_memory buf m pos len
      method advance n =
	Netpagebuffer.advance buf n
      method page_for_additions =
	let (m,pos,len) = Netpagebuffer.page_for_additions buf in
	(`Memory m, pos, len)
      method page_for_consumption =
	let (m,pos,len) = Netpagebuffer.page_for_consumption buf in
	(`Memory m, pos, len)
    end
  )

let str_obj_buffer() =
  let buf =
    Netbuffer.create 4096 in
  ( object
      method length = Netbuffer.length buf
      method blit_out bpos ms pos len =
	match ms with
	  | `String s -> Netbuffer.blit_to_string buf bpos s pos len
	  | `Memory m -> Netbuffer.blit_to_memory buf bpos m pos len
      method delete_hd n =
	Netbuffer.delete buf 0 n
      method index_from pos c =
	Netbuffer.index_from buf pos c
      method add ms pos len =
	match ms with
	  | `String s -> Netbuffer.add_sub_string buf s pos len
	  | `Memory m -> Netbuffer.add_sub_memory buf m pos len
      method advance n =
	Netbuffer.advance buf n
      method page_for_additions =
	let (s,pos,len) = Netbuffer.area_for_additions buf in
	(`String s, pos, len)
      method page_for_consumption =
	let s = Netbuffer.unsafe_buffer buf in
	(`String s, 0, Netbuffer.length buf)
    end
  )
    

let create_in_buffer d0 =
  let d = (d0 :> in_device) in
  let esys =
    device_esys d in
  let buf =
    if device_supports_memory d then
      mem_obj_buffer()
    else
      str_obj_buffer() in
  let eof = 
    ref false in
  let fill_e_opt =
    ref None in
object
  method buffer = buf
  method eof = !eof

  method start_fill_e () =
    assert(!fill_e_opt = None);
    if !eof then
      eps_e (`Done true) esys
    else (
      let (ms,pos,len) = buf # page_for_additions in
      let e =
	input_e d ms pos len
	++ (fun n ->
	      assert(n > 0);
	      buf # advance n;
	      fill_e_opt := None;
	      eps_e (`Done false) esys
	   )
	>> (function
	      | `Done flag -> `Done flag
	      | `Error End_of_file -> 
		  eof := true; `Done true
	      | `Error error -> `Error error
	      | `Aborted -> `Aborted
	   ) in
      fill_e_opt := Some e;
      e
    )

  method fill_e_opt =
    !fill_e_opt

  method shutdown_e () =
    shutdown_e d

  method inactivate() =
    inactivate d

  method event_system = esys
end


let in_buffer_length (b:in_buffer) =
  b#buffer#length

let in_buffer_blit (b:in_buffer) bpos ms mspos len =
  b#buffer#blit_out bpos ms mspos len

let in_buffer_fill_e (b:in_buffer)  =
  match b#fill_e_opt with
    | None -> b#start_fill_e ()
    | Some fe -> fe


let create_out_buffer ~max d0 =
  let d = (d0 :> out_device) in
  let esys =
    device_esys d in
  let buf =
    if device_supports_memory d then
      mem_obj_buffer()
    else
      str_obj_buffer() in
  let eof = 
    ref false in
  let flush_e_opt =
    ref None in

  let rec flush_e n =
    if n > 0 then (
      let (ms,pos,len) = buf # page_for_consumption in
      let len' = min len n in
      output_e d ms pos len' ++
	(fun k ->
	   buf # delete_hd k;
	   flush_e (n-k)
	)
    )
    else (
      flush_e_opt := None;
      eps_e (`Done ()) esys
    ) in

 object
  method buffer = buf
  method eof = !eof
  method max = max

  method start_flush_e() =
    assert (!flush_e_opt = None);
    let e = flush_e (buf#length) in
    flush_e_opt := Some e;
    e

  method flush_e_opt = !flush_e_opt

  method write_eof_e () =
    if buf#length = 0 then
      write_eof_e d
    else
      eps_e 
	(`Error (Failure "Uq_io: called write_eof_e with non-empty buffer"))
	esys

  method shutdown_e linger =
    shutdown_e ?linger d
    
  method inactivate () =
    inactivate d

  method event_system = esys
end


let copy_e ?len d_in d_out =
  let d_in_esys = device_esys d_in in
  let d_out_esys = device_esys d_out in
  if d_in_esys <> d_out_esys then
    invalid_arg "Uq_io.copy_e: devices must use the same event system";
  let esys = d_in_esys in

  let ms, ms_len =
    if device_supports_memory d_in && device_supports_memory d_out then (
      let m = Netsys_mem.pool_alloc_memory Netsys_mem.default_pool in
      (`Memory m, Bigarray.Array1.dim m)
    )
    else (
      let s = String.create 4096 in
      (`String s, String.length s)
    ) in

  let rec push_data p n =
    if n = 0 then
      eps_e (`Done ()) esys
    else
      output_e d_out ms p n ++ (fun k -> push_data (p+k) (n-k)) in

  let count = ref 0L in

  let rec pull_data() =
    let n =
      match len with
	| None -> ms_len
	| Some l -> min ms_len (l - Int64.to_int !count) in

    let ( >> ) = Uq_engines.fmap_engine in
    (* For a strange reason we need this - somewhere a generalization is
       missing
     *)

    ( input_e d_in ms 0 n
      >> (function
	    | `Done n -> `Done(`Good n)
	    | `Error End_of_file -> `Done `Eof
	    | `Error error -> `Error error
	    | `Aborted -> `Aborted
	 )
      : [`Good of int | `Eof] Uq_engines.engine
    ) ++
    (function
       | `Good n ->
	   count := Int64.add !count (Int64.of_int n);
	   push_data 0 n ++ (fun () -> pull_data())
       | `Eof ->
	   eps_e (`Done !count) esys
    ) in
  pull_data()

  
