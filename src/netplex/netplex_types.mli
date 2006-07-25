(* $Id$ *)


type param_value =
    [ `String of string
    | `Int of int
    | `Float of float
    | `Bool of bool
    ]

type param_value_or_any =
    [ param_value
    | `Any of exn
    ]

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]

class type logger =
object
  method log : component:string -> level:level -> message:string -> unit
  method reopen : unit -> unit
  method max_level : level
  method set_max_level : level -> unit
end

type parallelization_type =
    [ `Multi_processing
    | `Multi_threading
    ]
  (** Type of parallelization:
    * - [`Multi_processing] on a single host
    * - [`Multi_threading] on a single host
   *)

type socket_state =
    [ `Enabled | `Disabled | `Restarting of bool | `Down ]
  (** The state of a socket:
    * - [`Enabled]: The controller allows containers to accept connections.
    *   Note that this does not necessarily means that there such containers.
    * - [`Disabled]: It is not allowed to accept new connections. The
    *   socket is kept open, however.
    * - [`Restarting b]: The containers are being restarted. The boolean
    *   argument says whether the socket will be enabled after that.
    * - [`Down]: The socket is down/closed
   *)

type container_id = < >
  (** Identifies a container *)

type container_state =
    [ `Accepting of int * float
    | `Busy
    | `Starting
    | `Shutting_down
    ]
  (** The container state for workload management:
    * - [`Accepting(n,t)]: The container is accepting further connections.
    *   It currently processes [n] connections. The last connection was
    *   accepted at time [t] (seconds since the epoch).
    * - [`Busy]: The container does not accept connections
   *)

class type controller = 
object
  method ptype : parallelization_type
    (** The actually effective parallelization type *)

  method controller_config : controller_config

  method services : (socket_service * socket_controller * workload_manager) list
    (** The list of controlled services *)

  method add_service : socket_service -> workload_manager -> unit
    (** Adds a new service. Containers for these services will be started
      * soon.
     *)

  method add_admin : (Rpc_server.t -> unit) -> unit
    (** [add_admin setup]: Allows to bind another RPC program to the admin
      * socket. The function [setup] will be called whenever a connection
      * to the admin socket is established, and this function can call
      * [Rpc_server.bind] to bind another RPC program. By default, only
      * the [Admin] interface is available as described in [netplex_ctrl.x].
      *
      * Note that this RPC server runs in the scope of the controller! No
      * additional process or thread is created.
     *)

  method logger : logger
    (** The logger *)

  method event_system : Unixqueue.unix_event_system
    (** The event system used by the controller. It {b must not} be used
     * from a container.
     *)

  method restart : unit -> unit
    (** Initiates a restart of all containers: All threads/processes are
      * terminated and replaced by newly initialized ones.
     *)

  method shutdown : unit -> unit
    (** Initiates a shutdown of all containers. It is no longer possible
      * to add new services. When the shutdown has been completed, 
      * the controller will terminate itself.
     *)
end

and controller_config =
object
  method socket_directory : string
    (** The directory where Unix domain sockets are created. For every
      * service a subdirectory is created, and the socket has the name
      * of the protocol.
     *)

  method create_logger : controller -> logger
    (** Create a logger to be used for the whole Netplex system. The
      * controller is already initialized which makes it possible to
      * write the logger as Netplex service. Messages arriving during the
      * creation are queued up and sent afterwards to the new logger.
     *)
end
	  
and socket_service =
object
  method name : string
    (** The name of the [socket_service] is used to identify the service
      * in the whole netplex process cluster. Names are hierarchical;
      * name components are separated by dots (e.g. "company.product.service").
      * The prefix "netplex." is reserved for use by Netplex. The name
      * "netplex.controller" refers to the service provided by the
      * controller.
     *)

  method sockets : (string * Unix.file_descr array) list
    (** A [socket_service] consists of a list of supported protocols
      * which are identified by a name. Every protocol is available 
      * on a list of sockets (which may be bound to different addresses).
     *)

  method socket_service_config : socket_service_config
    (** The configuration *)

  method processor : processor
    (** A user-supplied object to process incoming connections *)

  method create_container : parallelization_type -> socket_service -> container
    (** {b Internal method.} Called by the controller to create a new
      * container. The container must match the parallelization type of
      * the controller. This call is already done in the process/thread
      * provided for the container.
      *)

end

and socket_service_config =
object
  method name : string
    (** The proposed name for the [socket_service] *)

  method protocols : protocol list
    (** This list describes the sockets to create in detail *)

  method change_user_to : (int * int) option
    (** Instructs the container to change the user of the process after
      * starting the service. This is only possible in multi-processing mode.
      * In multi-threading mode, this parameter is ignored.
     *)
end

and protocol =
object
  method name : string
    (** The protocol name is an arbitrary string identifying groups of
      * sockets serving the same protocol for a [socket_service].
     *)
  method addresses : Unix.sockaddr array
    (** The addresses of the master sockets. (The socket type is always
      * SOCK_STREAM.) The list must be non-empty.
     *)
  method lstn_backlog : int
    (** The backlog (argument of Unix.listen) *)
  method lstn_reuseaddr : bool
    (** Whether to reuse ports immediately *)
  method configure_slave_socket : Unix.file_descr -> unit
    (** A user-supplied function to configure slave sockets (after [accept]).
      * The function is called from the process/thread of the container.
     *)
end

and socket_controller =
object
  method state : socket_state
    (** The current state *)
  method enable : unit -> unit
    (** Enables a disabled socket service again *)
  method disable : unit -> unit
    (** Disable a socket service temporarily *)
  method restart : unit -> unit
    (** Restarts the containers for this socket service only *)
  method shutdown : unit -> unit
    (** Closes the socket service forever, and initiates a shutdown of all
      * containers serving this type of service.
     *)
  method container_state : (container_id * container_state) list

  method start_containers : int -> unit

  method stop_containers : container_id list -> unit

end

and processor_hooks =
object
  method post_add_hook : socket_service -> unit
    (** A user-supplied function that is called after the service has been
      * added to the controller 
     *)

  method post_rm_hook : socket_service  -> unit
    (** A user-supplied function that is called after the service has been
      * removed from the controller 
     *)

  method pre_start_hook : socket_service -> controller -> container_id -> unit
    (** A user-supplied function that is called before the container is
      * created and started. It is called from the process/thread of the
      * controller.
     *)

  method post_start_hook : container -> unit
    (** A user-supplied function that is called after the container is
      * created and started, but before the first service request arrives.
      * It is called from the process/thread of the
      * container.
     *)

  method pre_finish_hook : container -> unit
    (** A user-supplied function that is called just before the container is
      * terminated. It is called from the process/thread of the
      * container.
     *)

  method post_finish_hook : socket_service -> controller -> container_id -> unit
    (** A user-supplied function that is called after the container is
      * terminated. It is called from the process/thread of the
      * controller.
     *)

  method receive_message :
            container -> string -> string array -> unit
    (** This function is called when a broadcast message is received.
      * The first string is the name of the message, and the array are
      * the arguments.
     *)

  method receive_admin_message :
            container -> string -> string array -> unit
    (** This function is called when a broadcast admin message is received.
      * The first string is the name of the message, and the array are
      * the arguments.
     *)

  method shutdown : unit -> unit
    (** A user-supplied function that is called when a shutdown notification
      * arrives.
     *)

  method global_exception_handler : exn -> bool
    (** This method is called when an uncaught exception would otherwise
      * terminate the container. It can return [true] to indicate that
      * the container continues running.
     *)

end

and processor =
object
  inherit processor_hooks

  method process : 
           when_done:(unit -> unit) ->
           container -> Unix.file_descr -> string -> unit
    (** A user-supplied function that is called when a new socket connection
      * is established. The function can now process the requests arriving
      * over the connection. It is allowed to use the event system of the
      * container, and to return immediately (multiplexing processor). It is 
      * also allowed to process the requests synchronously and to first return
      * to the caller when the connection is terminated. 
      *
      * The function {b must} call [when_done] to indicate that it processed
      * this connection completely.
      *
      * The string argument is the protocol name.
     *)

  method supported_ptypes : parallelization_type list
    (** The supported parallelization types *)

end

and container =
object
  method socket_service : socket_service

  method ptype : parallelization_type
    (** The parallelization type actually used for this container *)

  method event_system : Unixqueue.unix_event_system
    (** The event system the container uses *)

  method start : Unix.file_descr -> Unix.file_descr -> unit
    (** {b Internal Method.} Called by the controller to start the container.
      * It is the responsibility of the container to call the 
      * [post_start_hook] and the [pre_finish_hook].
      *
      * The file descriptors are endpoints of RPC connections to the
      * controller. The first serves calls of the [Control] program,
      * and the second serves calls of the [System] program.
      *
      * When [start] returns the container will be terminated.
     *)

  method shutdown : unit -> unit
    (** Initiates a shutdown of the container. *)

  method system : Rpc_client.t
    (** An RPC client that can be used to send messages to the controller.
      * Only available while [start] is running. It is bound to 
      * [System.V1].
     *)

  method lookup : string -> string -> string option
    (** [lookup service_name protocol_name] tries to find a Unix domain
      * socket for the service and returns it.
     *)

  method send_message : string -> string -> string array -> unit
    (** [send_message service_pattern msg_name msg_arguments]: Sends
      * a message to all services matching [service_pattern]. The pattern
      * may include the wildcard [*].
     *)

  method log : level -> string -> unit
    (** Sends a log message to the controller. *)

  method var : string -> param_value_or_any
    (** Returns the value of a container variable or [Not_found]. Container
      * variables can be used by the user of a container to store additional
      * values in the container. These values exist once per thread/process.
      *)

  method set_var : string -> param_value_or_any -> unit
    (** Sets the value of a container variable *)

end

and workload_manager =
object
  method hello : controller -> unit
    (** Called by the controller when the service is added *)

  method shutdown : unit -> unit
    (** Called by the controller to notify the manager about a shutdown *)

  method adjust : socket_service -> socket_controller -> unit
    (** This function is called by the controller at certain events to
      * adjust the number of available containers. The manager can
      * call [start_containers] and [stop_containers] to change the
      * system.
      *
      * The function is called right after the startup to ensure
      * that there are containers to serve requests. It is also called:
      * - just after a connection has been accepted and before it is
      *   decided which container will have the chance to accept in the
      *   round
      * - after the shutdown of a container
     *)
end
;;


class type par_thread =
object
  method ptype : parallelization_type

  method info_string : string
    (** Outputs the process or thread ID *)

  method watch_shutdown : Unixqueue.unix_event_system -> unit
    (** Called by the controller if it thinks the container is down *)
end


class type parallelizer =
object
  method ptype : parallelization_type

  method init : unit -> unit
    (** Initializes the main process for usage with this parallelizer *)

  method start_thread : 
         't . ('t -> unit) -> 't -> Unix.file_descr list -> string -> logger -> par_thread
    (** [start_thread f arg l]: Starts a new thread or process and calls
      * [f arg] in that context. The list of file descriptors [l] is ensured
      * to be shared with the main process.
      *
      * There is no way to check when the thread terminates.
     *)
end


type config_tree =
    [ `Section of address * string * config_tree list
	(* (relative_name, contents) *)
    | `Parameter of address * string * param_value
	(* (relative_name, contents) *)
    ]

and address = < >

class type config_file =
object
  method filename : string
  method tree : config_tree
  method root_addr : address
  method root_name : string
  method resolve_section : address -> string -> address list
    (* Fails if the address cannot be found. Returns [] if there is no
     * such section at this address
     *)
  method resolve_parameter : address -> string -> address
    (* Fails if the address cannot be found. Raises Not_found if there is no
     * such parameter at this address
     *)
  method print : address -> string
  method string_param : address -> string
  method int_param : address -> int
  method float_param : address -> float
  method bool_param : address -> bool
  method restrict_subsections : address -> string list -> unit
  method restrict_parameters : address -> string list -> unit
end


class type processor_factory =
object
  method name : string
  method create_processor :
    controller_config -> config_file -> address -> processor
end


class type workload_manager_factory =
object
  method name : string
  method create_workload_manager : 
    controller_config -> config_file -> address -> workload_manager
end


class type logger_factory =
object
  method name : string
  method create_logger : config_file -> address -> controller -> logger
end


class type netplex_config =
object
  method ptype : parallelization_type

  method controller_config : controller_config

  method services : (socket_service_config * 
		       (address * processor_factory) * 
		       (address * workload_manager_factory) ) list
end

