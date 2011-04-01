(* $Id$ *)

(** Multi-processing for compute jobs

    This library allows it to develop parallel algorithms that can take
    advantage of multiple CPU cores. It does not use Ocaml's multi-threading
    feature because this would implicitly serialize all computations.
    Instead, independent processes are created, and the communication
    between processes is made as cheap and unproblematic as possible.

    Before using this library, it is required to call {!Netmcore.startup}.
    This turns the current process into the master process. The master
    process has only a supervising function, and is responsible for
    managing global resources, and for starting further worker processes.

    The worker processes can start more workers. This is different from
    creating a new thread, though: The new worker does not share memory
    with its creator. It is also different from calling [Unix.fork],
    because new workers are always subprocesses of the master process.
    This means it is not initialized with a copy of the state of the
    logical creator, but with its real parent process which is always
    the master.

    Compatibility with multi-threading: You may run into big trouble
    when the master process starts further threads (after forking 
    thread-related resources are in an uncontrolled state). There is
    no such problem in the worker processes. In this library,
    however, nothing is done to ease the multi-threaded life, so you
    should carefully check the compatibility first.
 *)


(** {2 Types and exceptions} *)

type res_id =
    [ `Resource of int ]
  (** This tagged integer identifies resources. This variant type will never
      be extended.
   *)

type process_id =
    [ `Process of int ]
  (** This tagged integer identifies processes. This variant type will never
      be extended.
   *)


type compute_resource_type =
    [ `File | `Posix_shm | `Posix_shm_preallocated 
    | `Posix_sem | `Fork_point | `Join_point
    ]

type inherit_request =
    [ `Resources of res_id list
    | `All
    ]

type compute_resource_repr =
    [ `File of string
    | `Posix_shm of string
    | `Posix_shm_preallocated of string * Netsys_mem.memory
    | `Posix_sem of string
    | `Fork_point of (inherit_request * Netplex_encap.encap -> process_id)
    | `Join_point of (process_id -> Netplex_encap.encap option)
    ]
  (** Centrally managed resources include:
      - [`File name]: Files [name] (absolute name)
      - [`Posix_shm name]: Shared memory objects with [name]
      - [`Posix_shm_preallocated(name,m)]: Shared memory objects already
        allocated by the master process. These objects are passed over
        to the worker processes by inheritance, and are always mapped at
        the same address. [m] is the bigarray mapping the object.
      - [`Posix_sem name]: Semaphores with [name]
      - [`Fork_point(inh,f)]: Fork points where [let pid=f arg] fork a new process
        with argument [arg]. [pid] is the process identifier. The list [inh]
        are resources inherited from the master.
      - [`Joint_point f]: Joint points where [let res=f pid] wait until
        the process [pid] terminates. If [res] is non-[None] it is the
        result value. If it is [None], no result was passed back
        (including all pathological cases like crashes)
   *)

exception No_resource of res_id
  (** No such resource, or no resource of the expected type *)


class type compute_resource =
object
  method id : res_id
    (** an ID of the resource that is valid in the whole Netplex system *)

  method typ : compute_resource_type
    (** which type of resource *)

  method repr : compute_resource_repr
    (** the resource detail *)

  method release : unit -> unit
    (** Notify the manager that the resource is not used any longer by this
	process. If all processes release the resource it will be finally
	destroyed.
     *)
end

(** {2 Defining processes} *)

val def_process : 
     (Netplex_encap.encap -> Netplex_encap.encap) -> 
     res_id * res_id
  (** [let fork_point, join_point = def_process f]:
      Defines process types, i.e. ways of starting and finishing processes. 
      The definition must be done in the master process, e.g. before the first
      worker is started. 

      Once the process type is defined, new processes can be started,
      and these processes will run the function [f]. When [f] is 
      finished, the process will terminate. Starting processes is possible
      from other worker processes.

      [f] is supplied with its argument by the process starter [start].
      The result value of [f] can be retrieved with [join] (by any
      process, but only the first [join] for this process will be successful).
      
      If [f] throws exceptions, these will be caught and logged, but not
      be passed back to the caller (which just gets [None] as result
      of [join]). The same happens for any unplanned termination of the
      process.

      It is allowed to immediately release the [join_point] if there is
      no interest in catching the termination of started processes.

      Here is an example how to define a process that takes a [string]
      argument and returns an [int]:

      {[
      module String_encap = Netplex_encap.Make_encap(struct type t=string end)
      module Int_encap = Netplex_encap.Make_encap(struct type t=int end)

      let my_process_fork, my_process_join =
        Netmcore.def_process
          (fun s_encap ->
            let (s:string) = String_encap.unwrap s_encap in
            let (r:int) = ... in
            Int_encap.wrap r
          )
      ]}

      The wrapping and unwrapping is required for ensuring type-safety
      (see {!Netplex_encap} for the details of the idea).

      Calling this process is done with (also see below):

      {[
      let pid = Netmcore.start my_process_fork (String_encap.wrap s) in
      let r_encap_opt = Netmcore.join my_process_join pid in
      match r_encap_opt with
        | None -> failwith "Something went wrong"
        | Some r_encap -> Int_encap.unwrap r_encap
      ]}

   *)

(** The following functions can also be called from worker processes
    (i.e. Netplex containers)
 *)

val start : ?inherit_resources:inherit_request -> 
            res_id -> Netplex_encap.encap -> process_id
  (** [let pid = start fork_point arg]: Starts the process with the
      given [fork_point] and the argument [arg].

      Raises [No_resource] if there is no such resource.

      The function returns a process identifier. This is not the Unix
      PID, but a sequentially generated number that is unique for a
      running program.

      Option [inherit_resources]: Certain resources are only accessible by
      the process when they are inherited to it. This is the case for
      [`Posix_shm_preallocated]. This can be set to [`All] to inherit
      all inheritable resources, or to [`Resources l] to only inherit
      the resources of [l]. By default, no resources are inherited.
   *)

val join : res_id -> process_id -> Netplex_encap.encap option
  (** [let res_opt = join join_point pid]: Waits until the process [pid]
      is done, and returns the result value if any.

      Raises [No_resource] if there is no such resource.
   *)

(** {2 Managing resources from worker processes} *)

(** The following functions can also be called from worker processes
    (i.e. Netplex containers)
 *)

val get_resource : res_id -> compute_resource
  (** Retrieves the resource by ID. This implicitly also marks this resource
      as being used by this process. Don't forget to call [release] when
      your are done with the resource.

      Raises [No_resource] if there is no such resource.
   *)

val release : res_id -> unit
  (** Release the resource with this ID. (Same as calling the [release]
      method on the object.)
   *)

val manage_file : string -> compute_resource
  (** hands over a file to the manager *)

val get_file : res_id -> string
  (** Gets the file with this ID (or raises [No_resource]). As for
      [get_resource] the file is marked as being used by the process.
   *)

val manage_shm : string -> compute_resource
  (** hands over a named shm object to the manager *)

val get_shm : res_id -> string
  (** Gets the shm object with this ID (or raises [No_resource]). As
      for [get_resource] the shm object is marked as being used by the process.
   *)

(** Shared memory objects can be created with {!Netsys_posix.shm_create},
    and opened with {!Netsys_posix.shm_open}. 
 *)

val create_preallocated_shm : 
       ?value_area:bool -> string -> int -> (res_id * string)
  (** [create_preallocated_shm prefix size]: Creates a new preallocated
      shm object with a unique name based on [prefix], and a length of
      [size] bytes. The object is created and mapped into the master
      process, and will be available to any newly started process when
      the resource ID is inherited to the process.

      Returns [(res_id,shm_name)] where [res_id] identifies the new
      resource, and [shm_name] is the name of the POSIX shared memory
      object.

      Note that the process calling this function cannot look up this
      resource (using [get_shm] or [get_resource]) because the shm
      block cannot be mapped at the right address. Nevertheless, the calling
      process counts as a user of the object, and needs to release
      the object.

      Option [value_area]: if set, the new memory is marked as value
      area, so the ocaml runtime allows value comparisons in this
      memory area.
   *)

val manage_sem : string -> compute_resource
  (** hands over a named semaphore to the manager *)

val get_sem : res_id -> string
  (** gets the semaphore with this ID (or raises [No_resource]). As
      for [get_resource] the semaphore is marked as being used by the process.
   *)

(** Semaphores can be opened with {!Netsys_posix.sem_open}, and
    created with {!Netsys_posix.sem_create}.
 *)

val self_process_id : unit -> process_id
  (** Returns the process ID of a worker *)

(** {2 Initialization and system start} *)

(** This module can either be used as Netplex plugin and integrated into
    any existing Netplex program, or it can be started in stand-alone mode
 *)

val add_plugins : Netplex_types.controller -> unit
  (** To enable compute processes for any Netplex program, call this
      function with the controller object as argument. This can e.g. 
      be done in the [post_add_hook] of the processor.
   *)

val startup : socket_directory:string ->
              ?pidfile:string ->
              ?init_ctrl:(Netplex_types.controller -> unit) ->
              first_process:(res_id * Netplex_encap.encap) ->
              unit ->
                unit
  (** This function makes the current process the master process.
      It starts immediately a new worker process, called the 
      [first_process]. The [startup] function returns first when this
      process is finished, in which case the whole Netplex system is
      shut down.

      Passing a [socket_directory] is mandatory. This directory will
      contain helper files. The must be a separate [socket_directory]
      for each running Computeplex instance.

      [pidfile]: If passed, the PID of the master process is written
      to this file.
   *)

val destroy_resources : unit -> unit
  (** Destroys all resources that may be left *)


module Debug : sig
  val enable : bool ref
end
