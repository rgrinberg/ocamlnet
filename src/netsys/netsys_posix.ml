(* $Id$ *)

open Unix

let int_of_file_descr =
  match Sys.os_type with
    | "Unix" | "Cygwin" ->
	(fun fd -> (Obj.magic (fd:file_descr) : int))
    | _ ->
	(fun fd -> invalid_arg "Netsys.int_of_file_descr")

let file_descr_of_int =
  match Sys.os_type with
    | "Unix" | "Cygwin" ->
	(fun n -> (Obj.magic (n:int) : file_descr))
    | _ ->
	(fun n -> invalid_arg "Netsys.file_descr_of_int")

(* Limits  & resources *)

external sysconf_open_max : unit -> int = "netsys_sysconf_open_max";;

(* Process groups, sessions, terminals *)

external getpgid : int -> int = "netsys_getpgid";;
let getpgrp() = getpgid 0;;
external setpgid : int -> int -> unit = "netsys_setpgid";;
let setpgrp() = setpgid 0 0;;

external tcgetpgrp : file_descr -> int = "netsys_tcgetpgrp";;
external tcsetpgrp : file_descr -> int -> unit = "netsys_tcsetpgrp";;

external ctermid : unit -> string = "netsys_ctermid";;
external ttyname : file_descr -> string = "netsys_ttyname";;

external getsid : int -> int = "netsys_getsid";;

(* Users and groups *)

external setreuid : int -> int -> unit = "netsys_setreuid";;
external setregid : int -> int -> unit = "netsys_setregid";;

(* poll *)

external pollfd_size : unit -> int = "netsys_pollfd_size"

let the_pollfd_size = pollfd_size()

let _have_poll = the_pollfd_size > 0
let have_poll() = _have_poll

type poll_req_events = int
type poll_act_events = int
type poll_cell =
    { mutable poll_fd : Unix.file_descr;
      mutable poll_req_events : poll_req_events;
      mutable poll_act_events : poll_act_events;
    }
type poll_mem
type poll_array =
  | Poll_mem of poll_mem * int (*length*)
  | Poll_emu of poll_cell array

let null_poll_cell =
  { poll_fd = Unix.stdin;
    poll_req_events = 0;
    poll_act_events = 0
  }

external mk_poll_mem : int -> poll_mem
  = "netsys_mk_poll_mem"

external set_poll_mem : poll_mem -> int -> Unix.file_descr -> int -> int -> unit
  = "netsys_set_poll_mem"

external get_poll_mem : poll_mem -> int -> (Unix.file_descr * int * int)
  = "netsys_get_poll_mem"

external blit_poll_mem : poll_mem -> int -> poll_mem -> int -> int -> unit
  = "netsys_blit_poll_mem"

external poll_constants : unit -> int array = "netsys_poll_constants"

let the_poll_constants = poll_constants()

let const_rd_event = the_poll_constants.(0)
let const_pri_event = the_poll_constants.(1)
let const_wr_event = the_poll_constants.(2)
let const_err_event = the_poll_constants.(3)
let const_hup_event = the_poll_constants.(4)
let const_nval_event = the_poll_constants.(5)

let poll_req_events rd wr pri =
  (if rd then const_rd_event else 0) lor
  (if wr then const_wr_event else 0) lor
  (if pri then const_pri_event else 0)

let poll_req_triple p =
  (p land const_rd_event <> 0,
   p land const_wr_event <> 0,
   p land const_pri_event <> 0
  )

let poll_null_events() = 0

let poll_result p = p <> 0
let poll_rd_result p = p land const_rd_event <> 0
let poll_wr_result p = p land const_wr_event <> 0
let poll_pri_result p = p land const_pri_event <> 0
let poll_err_result p = p land const_err_event <> 0
let poll_hup_result p = p land const_hup_event <> 0
let poll_nval_result p = p land const_nval_event <> 0

let poll_array_length =
  function
    | Poll_mem(_,n) -> n
    | Poll_emu e -> Array.length e

let set_poll_cell a k c =
  if k < 0 || k >= poll_array_length a then
    invalid_arg "Netsys.set_poll_cell";
  match a with
    | Poll_mem(s,_) ->
	set_poll_mem s k c.poll_fd c.poll_req_events (* c.poll_revents *) 0 
    | Poll_emu e ->
	e.(k) <- { c with poll_fd = c.poll_fd } (* copy *)

let get_poll_cell a k =
  if k < 0 || k >= poll_array_length a then
    invalid_arg "Netsys.get_poll_cell";
  match a with
    | Poll_mem(s,_) ->
	let (fd, ev, rev) = get_poll_mem s k in
	{ poll_fd = fd; poll_req_events = ev; poll_act_events = rev }
    | Poll_emu e ->
	let c = e.(k) in
	{ c with poll_fd = c.poll_fd }   (* copy *)

let blit_poll_array a1 k1 a2 k2 len =
  let l1 = poll_array_length a1 in
  let l2 = poll_array_length a2 in
  if len < 0 || k1 < 0 || k1+len > l1 || k2 < 0 || k2+len > l2 then
    invalid_arg "Netsys.get_poll_cell";
  match (a1, a2) with
    | (Poll_mem(s1,_), Poll_mem(s2,_)) ->
	blit_poll_mem s1 k1 s2 k2 len
    | (Poll_emu e1, Poll_emu e2) ->
	Array.blit e1 k1 e2 k2 len
    | _ ->
	assert false

let create_poll_array n =
  if _have_poll then (
    let s = mk_poll_mem n in
    Poll_mem(s,n)
  )
  else (
    let e = Array.create n null_poll_cell in
    Poll_emu e
  )

external netsys_poll : poll_mem -> int -> int -> int = "netsys_poll"


let do_poll a k tmo =
  match a with
    | Poll_mem(s,_) ->
	netsys_poll s k tmo
    | Poll_emu e ->
	(* Emulate poll using Unix.select. This is slow! *)
	let tmo' =
	  if tmo < 0 then (-1.0) else float tmo *. 0.001 in
	let l_inp = ref [] in
	let l_out = ref [] in
	let l_pri = ref [] in
	for j = 0 to k-1 do
	  let c = e.(j) in
	  let (f_inp, f_out, f_pri) = poll_req_triple c.poll_req_events in
	  if f_inp then l_inp := c.poll_fd :: !l_inp;
	  if f_out then l_out := c.poll_fd :: !l_out;
	  if f_pri then l_pri := c.poll_fd :: !l_pri;
	done;
	let (o_inp, o_out, o_pri) = 
	  Unix.select !l_inp !l_out !l_pri tmo' in
	let a_inp = Array.of_list o_inp in
	let a_out = Array.of_list o_out in
	let a_pri = Array.of_list o_pri in
	Array.sort Pervasives.compare a_inp;
	Array.sort Pervasives.compare a_out;
	Array.sort Pervasives.compare a_pri;
	let n = ref 0 in
	for j = 0 to k-1 do
	  let c = e.(j) in
	  let g_inp = Netsys_impl_util.mem_sorted_array c.poll_fd a_inp in
	  let g_out = Netsys_impl_util.mem_sorted_array c.poll_fd a_out in
	  let g_pri = Netsys_impl_util.mem_sorted_array c.poll_fd a_pri in
	  let rev =
	    (if g_inp then const_rd_event else 0) lor
	    (if g_out then const_wr_event else 0) lor
	    (if g_pri then const_pri_event else 0) in
	  c.poll_act_events <- rev;
	  if rev <> 0 then incr n
	done;
	!n


let poll a k tmo =
  if k < 0 || k > poll_array_length a then
    invalid_arg "Netsys.poll";
  let r =
    Netsys_impl_util.slice_time_ms
      (fun tmo0 ->
	 (* tmo0 is now an int in milliseconds *)
	 let n = do_poll a k tmo0 in
	 if n = 0 then None else Some n
      )
      tmo in
  match r with
    | None -> 0
    | Some n -> n


let restarting_poll a k tmo =
  Netsys_impl_util.restart_tmo (poll a k) tmo

let poll_single fd r w pri tmo =
  let a = create_poll_array 1 in
  set_poll_cell a 0 { poll_fd = fd; 
		      poll_req_events = poll_req_events r w pri;
		      poll_act_events = poll_null_events()
		    };
  poll a 1 tmo > 0


let act_events_of_int n = n
let int_of_act_events n = n

let req_events_of_int n = n
let int_of_req_events n = n


(* post fork handlers *)

class type post_fork_handler =
object
  method name : string
  method run : unit -> unit
end

module PFH = struct
  type t = post_fork_handler
  let compare = Pervasives.compare
end

module PFH_Set = Set.Make(PFH)

let post_fork_registry = ref PFH_Set.empty
let post_fork_mutex = !Netsys_oothr.provider # create_mutex()

let register_post_fork_handler pfh =
  post_fork_mutex # lock();
  post_fork_registry := PFH_Set.add pfh !post_fork_registry;
  post_fork_mutex # unlock()

let remove_post_fork_handler pfh =
  post_fork_mutex # lock();
  post_fork_registry := PFH_Set.remove pfh !post_fork_registry;
  post_fork_mutex # unlock()

let run_post_fork_handlers() =
  PFH_Set.iter
    (fun pfh ->
       try pfh#run()
       with
	 | error ->
	     prerr_endline("Netsys_posix: Exception in post fork handler "
			   ^ pfh#name ^ ": " ^ Netexn.to_string error)
    )
    !post_fork_registry


(* Spawn *)

type wd_spec =
  | Wd_keep
  | Wd_chdir of string
  | Wd_fchdir of Unix.file_descr

type pg_spec =
  | Pg_keep
  | Pg_new_bg_group
  | Pg_new_fg_group
  | Pg_join_group of int

type fd_action =
  | Fda_close of Unix.file_descr
  | Fda_close_ignore of Unix.file_descr
  | Fda_close_except of bool array
  | Fda_dup2 of Unix.file_descr * Unix.file_descr

type sig_action =
  | Sig_default of int
  | Sig_ignore of int

external netsys_spawn : wd_spec -> pg_spec -> fd_action list -> 
                        sig_action list -> string array ->
                        string -> string array -> int
  = "netsys_spawn_byte" "netsys_spawn_nat"

let spawn ?(chdir = Wd_keep) ?(pg = Pg_keep) ?(fd_actions = [])
          ?(sig_actions = []) ?(env = Unix.environment()) cmd args =
  (* Fixup: if pg = Pg_new_fg_group, we remove any Sig_default for
     SIGTTOU from sig_actions. Because of special handling, the effect
     of Sig_default is enforced by the implementation, but this must be
     done at [execve] time.
   *)
  let sig_actions =
    if pg = Pg_new_fg_group then
      List.filter 
	(fun spec -> spec <> Sig_default Sys.sigttou)
	sig_actions
    else
      sig_actions in
  netsys_spawn chdir pg fd_actions sig_actions env cmd args


type watched_subprocess = 
    { atom_idx : int;
      mutable alive : bool;
    }

external netsys_watch_subprocess : int -> int -> bool -> Unix.file_descr * int
  = "netsys_watch_subprocess"

external netsys_ignore_subprocess : int -> unit
  = "netsys_ignore_subprocess"

external netsys_forget_subprocess : int -> unit
  = "netsys_forget_subprocess"

external netsys_get_subprocess_status : int -> Unix.process_status option
  = "netsys_get_subprocess_status"

external install_subprocess_handler : unit -> unit
  = "netsys_install_sigchld_handler"

external netsys_kill_subprocess : int -> int -> unit
  = "netsys_kill_subprocess"

external netsys_killpg_subprocess : int -> int -> unit
  = "netsys_killpg_subprocess"

external kill_all_subprocesses : int -> bool -> bool -> unit
  = "netsys_kill_all_subprocesses"

external killpg_all_subprocesses : int -> bool -> unit
  = "netsys_killpg_all_subprocesses"

let watch_subprocess pid pgid kill_flag =
  if pid <= 0 || pgid < 0 then
    invalid_arg "Netsys_posix.watch_subprocess";
  let fd, atom_idx = netsys_watch_subprocess pid pgid kill_flag in
  let ws = { atom_idx = atom_idx; alive = true } in
  (fd, ws)

let ignore_subprocess ws =
  if not ws.alive then
    failwith "Netsys_posix.ignore_subprocess: stale reference";
  netsys_ignore_subprocess ws.atom_idx;
  ws.alive <- false

let forget_subprocess ws =
  if ws.alive then (
    netsys_forget_subprocess ws.atom_idx;
    ws.alive <- false
  )

let get_subprocess_status ws =
  if not ws.alive then
    failwith "Netsys_posix.get_subprocess_status: stale reference";
  netsys_get_subprocess_status ws.atom_idx

let kill_subprocess signal ws =
  if ws.alive then
    netsys_kill_subprocess signal ws.atom_idx

let killpg_subprocess signal ws =
  if ws.alive then
    netsys_killpg_subprocess signal ws.atom_idx


let register_subprocess_handler() =
  Netsys_signal.register_exclusive_handler
    ~name:"Sigchld handler in Netsys_posix"
    ~signal:Sys.sigchld
    ~install:install_subprocess_handler
    ()


(* Optional POSIX functions *)

external have_fadvise : unit -> bool = "netsys_have_posix_fadvise"
type advice =
  | FADV_NORMAL
  | FADV_SEQUENTIAL
  | FADV_RANDOM
  | FADV_NOREUSE
  | FADV_WILLNEED
  | FADV_DONTNEED
external fadvise : Unix.file_descr -> int64 -> int64 -> advice -> unit
                 = "netsys_fadvise"

external have_fallocate : unit -> bool = "netsys_have_posix_fallocate"
external fallocate : Unix.file_descr -> int64 -> int64 -> unit
                   = "netsys_fallocate"

(* POSIX shared memory *)

external have_posix_shm : unit -> bool = "netsys_have_posix_shm"
type shm_open_flag =
  | SHM_O_RDONLY
  | SHM_O_RDWR
  | SHM_O_CREAT
  | SHM_O_EXCL
  | SHM_O_TRUNC
external shm_open : string -> shm_open_flag list -> int -> file_descr
  = "netsys_shm_open"
external shm_unlink : string -> unit = "netsys_shm_unlink"

type ioprio_target =
  | Ioprio_process of int
  | Ioprio_pgrp of int
  | Ioprio_user of int

type ioprio =
  | Noprio
  | Real_time of int
  | Best_effort of int
  | Idle

external ioprio_get : ioprio_target -> ioprio = "netsys_ioprio_get"
external ioprio_set : ioprio_target -> ioprio -> unit = "netsys_ioprio_set"

let have_ioprio() =
  try let _ = ioprio_get(Ioprio_process(Unix.getpid())) in true
  with _ -> false
