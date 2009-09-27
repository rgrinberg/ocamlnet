(* $Id$ *)

open Printf

type entry =
    { sig_number : int;
      sig_library : string option;
      sig_priority : int;
      sig_keep_default : bool;
      sig_name : string;
      sig_callback : int -> unit;
    }


module Debug = struct
  let enable = ref false
end

let dlog = Netlog.Debug.mk_dlog "Netsys_signal" Debug.enable
let dlogr = Netlog.Debug.mk_dlogr "Netsys_signal" Debug.enable

let () =
  Netlog.Debug.register_module "Netsys_signal" Debug.enable


let is_win32 =
  match Sys.os_type with
    | "Win32" -> true
    | _ -> false


let sig_enable = Hashtbl.create 30
  (** Maps signal number to [`Unchanged], [`Enabled], or [`Disabled].
      [`Unchanged] is the same as if there was no entry for the signal 
      number and means that [Sys.set_signal] has not been called yet.
      [`Enabled] measn this function has been called. [`Disabled] means
      that the signal is on the "keep away list".
   *)

let sig_definition = Hashtbl.create 30
  (** Maps signal number to [entry] in priority order. *)

let otp = !Netsys_oothr.provider

let mutex = otp # create_mutex()


let while_locked f =
  Netsys_oothr.serialize mutex f ()

let lethal_default_actions =
  [ Sys.sighup; Sys.sigint; Sys.sigquit; Sys.sigill; Sys.sigabrt;
    Sys.sigfpe; (* Sys.sigkill *) Sys.sigsegv; (* Sys.sigpipe *)
    Sys.sigalrm; Sys.sigterm; Sys.sigusr1; Sys.sigusr2; 
  ]

let handle signo =
  dlogr (fun () -> sprintf "handle %d" signo);
  let entries =
    try Hashtbl.find sig_definition signo
    with Not_found -> [] in
  let emulate_default = ref true in
  List.iter
    (fun entry ->
       emulate_default := !emulate_default && entry.sig_keep_default;
       try
	 dlogr (fun () -> 
		  sprintf "signal %d calling back %s handler '%s'"
		    signo
		    (match entry.sig_library with 
		       | None -> "application"
		       | Some lib -> "library '" ^ lib ^ "'")
		    entry.sig_name
	       );
	 entry.sig_callback signo
       with _ -> ()
	 (* Be hard with exceptions here! *)
    )
    entries;
  if !emulate_default then (
    dlogr (fun () -> sprintf "signal %d emulating default" signo);
    (* If the default signal action is not "ignore", it is an action that
       will terminate the process. So we can emulate it by setting the
       signal handler again, and by sending the signal to the process.
     *)
    if List.mem signo lethal_default_actions then (
      Netsys._exit 126;
      (*  - This does not reliably work in multi-threaded programs: *)
      (*
      ignore(Sys.signal signo Sys.Signal_default);
      Unix.kill (Unix.getpid()) signo;
      (* The signal signo is pending but usually blocked because this function
       * is called from within the signal handler for signo. To force that
       * the signal is delivered we must unblock the signal.
       *)
      ignore(Unix.sigprocmask Unix.SIG_UNBLOCK [ signo ]);
      (* Wait for any signal - at least signo will happen! *)
      while true do Unix.pause() done;
      (* Never return to this point of execution! *)
      assert false
       *)
    )
  )


let manage signo =
  let is_disabled =
    try Hashtbl.find sig_enable signo = `Disabled
    with Not_found -> false in
  if not is_disabled then (
    ( try
	Sys.set_signal signo (Sys.Signal_handle handle)
      with Invalid_argument _ -> ()
    );
    Hashtbl.replace sig_enable signo `Enabled
  )


let restore_management signo =
  while_locked
    (fun () ->
       let is_enabled =
	 try Hashtbl.find sig_enable signo = `Enabled
	 with Not_found -> false in
       if is_enabled then
	 manage signo
    )


let keep_away_from signo =
  while_locked
    (fun () ->
       Hashtbl.replace sig_enable signo `Disabled
    )


let register_handler ?library ?priority ?(keep_default=false) 
                     ~name ~signal ~callback () =
  while_locked
    (fun () ->
       let entry =
	 { sig_number = signal;
	   sig_library = library;
	   sig_priority = if library=None then 100 else 0;
	   sig_keep_default = keep_default;
	   sig_name = name;
	   sig_callback = callback
	 } in
       let old_list =
	 try Hashtbl.find sig_definition signal
	 with Not_found -> [] in
       let same_handler e1 e2 =
	 e1.sig_number = e2.sig_number &&
	 e1.sig_library = e2.sig_library &&
	 e1.sig_name = e2.sig_name in
       let hdl_exists = List.exists (fun e -> same_handler entry e) old_list in
       let rm_list =
	 if hdl_exists then
	   List.filter
	     (fun e -> not(same_handler entry e))
	     old_list
	 else
	   old_list in
       let new_list =
	 List.sort
	   (fun e1 e2 ->
	      match compare e1.sig_priority e2.sig_priority with
		| 0 -> compare e1.sig_name e2.sig_name
	   | n -> n
	   )
	   (entry :: rm_list) in
       Hashtbl.replace sig_definition signal new_list;
       manage signal
    )


let list() =
  while_locked
    (fun () ->
       Hashtbl.fold
	 (fun _ l acc -> l @ acc)
	 sig_definition
	 []
    )

let keep_away_list() =
  while_locked
    (fun () ->
       Hashtbl.fold
	 (fun signo state acc ->
	    if state = `Disabled then signo::acc else acc)
	 sig_enable
	 []
    )

let () =
  if is_win32 then (
    (* Disable all except Sys.sigint: *)
    List.iter
      (fun signo -> Hashtbl.add sig_enable signo `Disabled)
      [ Sys.sigabrt; Sys.sigalrm; Sys.sigfpe; Sys.sighup; Sys.sigill;
	Sys.sigkill; Sys.sigpipe; Sys.sigquit; Sys.sigsegv; Sys.sigterm;
	Sys.sigusr1; Sys.sigusr2; Sys.sigchld; Sys.sigcont; Sys.sigstop;
	Sys.sigtstp; Sys.sigttin; Sys.sigttou; Sys.sigvtalrm; Sys.sigprof
      ]
  )

let () =
  register_handler
    ~library:"netsys"
    ~name:"Sigpipe default handler"
    ~signal:Sys.sigpipe
    ~callback:(fun _ -> ())
    ()

let init() = ()
