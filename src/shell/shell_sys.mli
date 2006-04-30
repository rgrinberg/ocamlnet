(* $Id: shell_sys.mli 50 2004-10-03 17:06:28Z gerd $
 * ----------------------------------------------------------------------
 *
 *)

(** Calls external programs, creates pipelines, etc. (full interface) *)

(** This module is {b not thread-safe} because of undefined behaviour
 * of some signal related functions in multi-threaded programs. This
 * problem cannot be easily fixed, as the necessary multi-threading
 * primitives are not available in O'Caml. (Maybe there is a solution
 * for bytecode threads...)
 *
 * Nevertheless, [shell] often seems to work in a multi-threaded environment.
 * However, strange things can happen when two threads start new processes
 * at the same time, because they overwrite the global signal mask. As
 * a minimum precaution you should ensure that only one thread uses [shell]
 * at any time. Anyway, you have been warned.
 *)


(** {1 Common exceptions} *)

exception Fatal_error of exn
  (** An error is fatal if it is not possible to recover from it in a
   * predictable manner. In this case, many function wrap such exceptions
   * [x] into [Fatal_error x].
   *)

(* ******************************************************************** *)
(* **                       environments                             ** *)
(* ******************************************************************** *)

(** {1 Environments} *)

type environment
  (** The abstract type of a process environment *)

val create_env  : unit -> environment
  (** Creates an empty environment *)

val current_env : unit -> environment
  (** Returns the environment of the current process as abstract environment
   * value 
   *)

val copy_env    : environment -> environment
  (** Copies an environment *)

val set_env     : environment -> string array -> unit
  (** Sets the contents of the environment to the passed string array *)

val get_env     : environment -> string array
  (** Gets the contents of the environment as string array *)

val iter_env    : f:(string -> unit) -> environment-> unit
  (** Iterates over the strings of the environment, and calls
   * [f s] for every string [s].
   *)

val set_env_var : environment -> string -> string -> unit
  (** [set_env_var env varname varval]: Sets the value of the variable
   * [varname] in the environment [env] to [varval].
   *)

val get_env_var : environment -> string -> string
  (** Returns the value of the variable in the environment *)

val iter_env_vars : f:(string -> string -> unit) -> environment -> unit
  (** Iterates over the variables of the environment, and calls
   * [f name value] for every variable with [name] and [value].
   *)

(* ******************************************************************** *)
(* **                    commands and processes                      ** *)
(* ******************************************************************** *)

(** {1 Commands} *)

type command
  (** A command describes how to start a new process *)

(* A _command_ is the description how to start a new process. A
 * _process_ is the running instance of a command; the same command
 * may be started several times.
 *)

val command :
      ?cmdname:string ->                   (* default: derived from filename *)
      ?arguments:(string array) ->         (* default: empty *)
      ?chdir:string ->                     (* default: current working dir *)
      ?environment:environment ->          (* default: current environment *)
      ?descriptors:(Unix.file_descr list) -> 
                                           (* default: stdin, stdout, stderr *)
      ?assignments:((Unix.file_descr * Unix.file_descr) list) ->    
                                           (* default: empty *)
      filename:string ->
      unit ->
	command
  (** Creates a command from the passed arguments:
   *
   * @param filename The name of the executable to start. The executable
   *   file is not searched, use {!Shell_sys.lookup_executable} for this
   *   purpose.
   * @param cmdname The name of the command passed in [argv[0]]. By
   *   default, this argument is derived from [filename].
   * @param arguments The arguments of the command (starting with the
   *   first real argument, skipping [cmdname]). By default [ [] ].
   * @param chdir Before the command is executed it is changed to
   *   this directory.
   * @param environment The environment of the command. By default, the
   *   current environment
   * @param assignments A list of descriptor pairs [ (fd_from,fd_to) ].
   *   The descriptor [fd_from] in the current process will be assigned
   *   to [fd_to] in the subprocess started for the command. 
   *   The list of assignments is executed sequentially, so
   *   later assignments must take the effect of previous assignments
   *   into account. For example, to make stderr of the subprocess write 
   *   to stdout of the parent process, pass [ [(stdout; stderr)] ]. 
   *   - By default, there are no assignments.
   * @param descriptors The list of file descriptors to share with the
   *   current process. In the subprocess only those descriptors remain open
   *   that are either mentioned in [descriptors], or that are the final target
   *   of assignments. By default, [ [stdin; stdout; stderr] ].
   *
   * Note that only the {b final targets} of assignments remain open in the
   * subprocess (unless they are also listed in [descriptors]). If there
   * are cascaded assignments like [ (fd1, fd2); (fd2, fd3) ] the intermediate
   * descriptors like [fd2] are not considered as final targets; only [fd3]
   * would be a final target in this example. 
   *)

exception Executable_not_found of string;;
  (** Raised when an executable file cannot be found; the argument is the
   *  search name
   *)

val lookup_executable :
      ?path:(string list) ->     (* default: use the PATH variable *)
      string ->
	string
  (** Searches an executable file. If the passed search name contains a
   * slash, it is expected that this name is already the path name of the
   * executable. If the search name does not contain a slash character,
   * it is searched in the directories enumerated by the search path.
   *
   * @param path The search path. By default, the contents of the
   *   variable PATH of the current environment, split by ':', are
   *   used
   *)

val get_cmdname     : command -> string
  (** Returns the name of the command *)

val get_arguments   : command -> string array
  (** Returns the argument array of the command (skipping the command name) *)

val get_chdir       : command -> string option
  (** Returns the [chdir] parameter of the command *)

val get_environment : command -> environment
  (** Returns the designated environment of the command *)

val get_descriptors : command -> Unix.file_descr list
  (** Returns the list of active descriptors *)

val get_assignments : command -> (Unix.file_descr * Unix.file_descr) list
  (** Returns the list of assignments [ (fd_from,fd_to) ] *)

val get_filename    : command -> string
  (** Returns the file name of the executable *)

val set_cmdname     : command -> string          -> unit
  (** Sets the command name *)

val set_arguments   : command -> string array    -> unit
  (** Sets the argument array *)

val set_chdir       : command -> string option   -> unit
  (** Sets the [chdir] parameter of the command *)

val set_environment : command -> environment     -> unit
  (** Sets the environment *)

val set_descriptors : command -> Unix.file_descr list -> unit
  (** Sets the list of active descriptors *)
 
val set_assignments : command -> (Unix.file_descr * Unix.file_descr) list -> unit
  (** Sets the list of assignments [ (fd_from,fd_to) ] *)

val set_filename    : command -> string          -> unit
  (** Sets the file name of the executable to start *)

val copy_command : command -> command
  (** Returns a duplicate of the command description *)

val is_executable : command -> bool
  (** Returns [true] if there is an executable file for the command, and
   * it is permitted to run this file (as stated by the file permissions).
   *
   * [false] means that the command can definitely not be executed. However,
   * even if the function returns [true] there may be still reasons that
   * execution will fail.
   *)

(** {1 Processes} *)

type process
  (** A process is the running instance of a command (a Unix process) *)

type group_action =
    New_bg_group      (** Start process in new background process group *)
  | New_fg_group      (** Start process in new foreground process group *)
  | Join_group of int (** Started process joins this existing process group *)
  | Current_group     (** Started process remains in the current group *)
  (** Determines in which process group the new process will run *)

val run :
      ?group:group_action ->       (* default: Current_group *)
      ?pipe_assignments:((Unix.file_descr * Unix.file_descr) list) ->
                                   (* default: [] *)
      command ->
	process
  (** Executes the command concurrently with the current process. The function
   * does not wait until the process terminates; it returns immediately after
   * the [exec] system call has been successfully performed; errors that
   * occur until [exec] are caught and reported as exception (even errors
   * in the fresh subprocess).
   *
   * On error, one can assume that the process state has been cleaned up:
   * any forked child process has terminated; any modifications of the global
   * process state has been restored. 
   *
   * File descriptor assignments: First, the assignments in [pipe_assignments]
   * are performed, then the assignments contained in the command. The
   * [pipe_assignments] are interpreted as parallel assignment, not
   * as sequential assignment.
   *
   * Note: For users without very special needs, it is recommended to run
   * jobs instead of processes. See below for the job API.
   *
   * @param group Determines in which process group the new process will
   *   run. By default [Current_group].
   * @param pipe_assignments A list of descriptor pairs [(fd_from,fd_to)].
   *   The descriptor [fd_from] in the current process will be assigned
   *   to [fd_to] in the started subprocess. In order to
   *   take effect, [fd_to] must also be passed in the [descriptors]
   *   property of the started command.
   *   Furthermore, [fd_from] may or may not be member of [descriptors];
   *   in the first case it will remain open, in the latter case it will
   *   be closed. The list of assignments is executed in parallel. For
   *   example, to swap the roles of stdout and stderr, pass the list
   *   [ [(stdout,stderr); (stderr,stdout)] ].
   *)

val process_id : process -> int
  (** Returns the process ID of the process *)

val status : process -> Unix.process_status
  (** Reports the status as determined by [wait] (below): If the process 
   * has terminated, the status of the process is returned.
   * If the process is still running, [Not_found] will be raised.
   *
   * Note: This function does {b not} call [Unix.waitpid] to get the status
   * and to release the process ID. This is done by [wait] below.
   *)

val command_of_process : process -> command
  (** Returns the command that is now running as the process *)

type process_event =
    File_read of Unix.file_descr   (** Data can be read from the fd *)
  | File_write of Unix.file_descr  (** Data can be written to the fd *)
  | File_except of Unix.file_descr (** OOB data can be read from the fd *)
  | Process_event of process       (** The process has changed its status *)
  | Signal                         (** A signal happened *)
  (** Events used by [wait] *)

val wait : 
      ?wnohang:bool ->                     (* default: false *)
      ?wuntraced:bool ->                   (* default: false *)
      ?restart:bool ->                     (* default: false *)
      ?check_interval:float ->             (* default: 0.1 *)
      ?read:(Unix.file_descr list) ->      (* default: [] *)
      ?write:(Unix.file_descr list) ->     (* default: [] *)
      ?except:(Unix.file_descr list) ->    (* default: [] *)
      process list ->
        process_event list
(** Watches the given list of processes and the file descriptors [read], 
 *  [write], and [except], and waits until events for these resources
 *  have happened, and reports these. It is allowed that the list of
 *  processes includes stopped and terminated processes.
 *
 * The function returns immediately with [] if it is no longer possible
 * that any event can happen.
 *
 * The passed file descriptors must be open.
 *
 * The function reports events under these conditions:
 * - A process of the list terminates, either regularly, or because of
 *   a signal. This is recorded as [Process_event].
 * - A process of the list stops, and [wuntraced = true]. This is also
 *   recorded as [Process_event].
 * - A file descriptor of the [read] list delivers data. This is a
 *   [File_read] event.
 * - A file descriptor of the [write] list accepts data. This is a
 *   [File_write] event.
 * - A file descriptor of the [except] list delivers data. This is a
 *   [File_except] event.
 *
 * Notes:
 * - The list of processes may contain terminated processes (that no longer
 *   exist) as long as the process status has already been recorded by a
 *   previous [wait] invocation.
 * - If [wait] does not restart automatically on a signal, the function
 *   will raise [Unix.Unix_error(Unix.EINTR,_,_)] when the signal condition
 *   is caught.
 * - If a process causes both process and descriptor events at the same time, 
 *   it is not specified which events are reported first.
 * - Only every [check_interval] seconds it is checked whether there are
 *   process events. File descriptor events are reported without delay.
 *
 * It is suggested to install a signal handler for SIGCHLD to improve
 * the responsiveness for process events. It is sufficient to install
 * an empty handler for this effect.
 *
 * @param wnohang If [true], it is immediately checked whether file or
 *   process events have happend, and if so, the event list is returned.
 *   When there are no events to report, the empty list is immediately
 *   returned. Default: [false]
 * @param wuntraced Whether to report events about stopped processes.
 *   Default: [false]
 * @param restart Whether to restart the event loop when a signal
 *   interrupts the loop. If [true], Unix errors of the type EINTR
 *   cannot happen any longer. Default: [false]
 * @param check_interval How frequently the processes are checked for
 *   events (in seconds). In addition to this, the processes are also
 *   checked when a signal happens. Default: 0.1
 * @param read The file descriptors to check for read events
 * @param write The file descriptors to check for write events
 * @param except The file descriptors to check for out-of-band events
 *)

val call : command -> process
  (** Executes the command and waits until the process terminates
   * (synchronous execution a la [system], but no intermediate shell).
   * [status] is guaranteed to return WEXITED or WSIGNALED.
   *)

val kill :
      ?signal:int ->       (* default: SIGTERM *)
      process ->
        unit
  (** Sends a signal to the passed process.
   *
   * @param signal The signal to send, by default SIGTERM
   *)


(* ******************************************************************** *)
(* **                 system event handler type                      ** *)
(* ******************************************************************** *)

(** {1 Foreign event loops} *)

(** The type [system_handler] can be used to watch the progress of
 *  jobs from a foreign event loop instead of [wait]. This interface
 *  is needed for the integration into the Unixqueue framework.
 *)

type system_handler =
    { sys_register :
        ?wuntraced:bool ->                   (* default: false *)
	?check_interval:float ->             (* default: 0.1 *)
	?read:(Unix.file_descr list) ->      (* default: [] *)
	?write:(Unix.file_descr list) ->     (* default: [] *)
        ?except:(Unix.file_descr list) ->    (* default: [] *)
	process list ->
        (process_event list -> unit) ->   (* callback for events *)
	  unit;        (** Register an event handler *)

      sys_wait :
	unit -> unit;  (** Start the event loop *)
    }
(** There are two record components:
 *
 * [sys_register]: By calling this function a callback function for the 
 *   specified events is registered. The meaning of the arguments is the
 *   same as for [wait], except of the last argument which is the callback
 *   function of type [process_event list -> unit]. Instead of returning
 *   the events like [wait], [sys_register] calls this function back
 *   to deliver the events.
 *
 * [sys_wait]: By calling this function the event loop is started, and
 *   events are delivered to the registered callback. If exceptions are
 *   raised in the callback function these will not be caught, so the 
 *   caller of [sys_wait] will get them. It must be possible to restart
 *   [sys_wait] in this case.
 *
 *   The callback function can change the list of interesting events by
 *   calling [sys_register] again.
 *
 *   If effectively no events are interesting ([sys_register] is called without
 *   file descriptors and no running process) the callback function is called
 *   with an empty [process_event list] once. If it does not register a new
 *   callback, the event loop will stop, and [sys_wait] will return normally.
 *)


(* ******************************************************************** *)
(* **                            jobs                                ** *)
(* ******************************************************************** *)

(** {1 Jobs} *)

(** A [job] is the description of how to run several commands which are
 * linked by pipelines (or which are just a logical unit). A [job_instance]
 * is the running instance of a job.
 *
 * Jobs are implemented on a higher layer than commands; the
 * following means of the operating system are used by job
 * invocations:
 * - Normally a [job_instance] corresponds to a Unix process group. In
 *   this case the last added command will result in the process group
 *   leader.
 * - Controlling the execution of jobs requires that signal
 *   handlers are set in many cases (see [install_job_handlers])
 * - The processes of jobs are often interconnected by pipelines
 *   (see [add_pipeline]).
 * - It is possible to handle pipelines between the current process and
 *   processes of the job (see [add_producer] and [add_consumer])
 *)

(** {b Important:}
 * 
 * In order to run jobs efficiently (without busy waiting) and properly
 * it is strongly recommended to install the signal handlers using
 * [install_job_handlers]
 *)  

type job
type job_instance

val new_job : unit -> job
  (** Creates a new job descriptor. Initially the job is empty, but you can
   * fill it with commands ([add_command]), pipelines ([add_pipeline]), 
   * consumers ([add_consumer]) and producers ([add_producer]).
   * When the job is set up, you can start it ([run_job]/[finish_job] or
   * [call_job]).
   *)

val add_command : command -> job -> unit
  (** Adds a command to a job. 
   *
   * Note that you cannot add the same command twice; however you can
   * add a copy of a command already belonging to the job.
   *)


val add_pipeline :
      ?bidirectional:bool ->           (* default: false *)
      ?src_descr:Unix.file_descr ->    (* default: stdout *)
      ?dest_descr:Unix.file_descr ->   (* default: stdin *)
      src:command ->
      dest:command ->
      job ->
        unit
  (** Adds a pipeline which redirects the output of the command [src] to the
   * input of the command [dest].
   * 
   * @param src_descr determines the file descriptor of the source command
   *    which is redirected. This is by default [stdout].
   * @param dest_descr determines the file descriptor of the destination
   *    command to which the data stream is sent. This is by default [stdin].
   * @param bidirectional if [false] (default), a classical pipe is created
   *    to connect the file descriptors. This normally restricts the data
   *    flow to one direction. If [true], a socketpair is created which is
   *    roughly a bidirectional pipe. In this case, data flow in both
   *    directions is possible.
   *)


val add_producer :
      ?descr:Unix.file_descr ->     (* default: stdin *)
      producer:(Unix.file_descr -> bool) ->
      command ->
      job ->
        unit
  (** Adds a producer to the job. A producer transfers data to the
   * subprocess realizing the passed command. To do so, a pipe is created
   * between the file descriptor [descr] of the subprocess and another
   * descriptor [descr'] which is open in the current process. The
   * function [producer] is called when data can be written into the
   * pipe. The argument of [producer] is the writing end of the pipe
   * [descr']. This file descriptor is in non-blocking mode. The
   * function [producer] must close [descr'] when all data are
   * transferred. The return value of [producer] indicates whether
   * the descriptor is still open.
   *
   * @param descr The descriptor of the subprocess to which the reading
   *    end of the pipe is dup'ed. By default [stdin].
   *)

(* CHECK: Was passiert wenn producer eine exception wirft? *)

val from_string :
      ?pos:int ->                  (* default: 0 *)
      ?len:int ->                  (* default: until end of string *)
      ?epipe:(unit -> unit) ->     (* default: empty function *)
      string ->
        (Unix.file_descr -> bool)
  (** [from_string ?pos ?len ?epipe s] returns a function which can be
   * used as [producer] argument for [add_producer]. The data transferred
   * to the subprocess is taken from the string [s]. After these data
   * are sent, the pipeline is closed.
   *
   * @param pos The position in [s] where the data slice to transfer begins.
   *    By default [0].
   * @param len The length of the data slice to transfer. By default,
   *    all bytes from the start position [pos] to the end of the 
   *    string are taken.
   * @param epipe This function is called when the pipeline breaks
   *    (EPIPE). Default: the empty function. EPIPE exceptions are
   *    always caught, and implicitly handled by closing the pipeline.
   *)

val from_stream :
      ?epipe:(unit -> unit) ->     (* default: empty function *)
      string Stream.t ->
        (Unix.file_descr -> bool)
  (** [from_stream ?epipe s] returns a function which can be
   * used as [producer] argument for [add_producer]. The data transferred
   * to the subprocess is taken from the string stream [s]. After these data
   * are sent, the pipeline is closed.
   *
   * @param epipe This function is called when the pipeline breaks
   *    (EPIPE). Default: the empty function. EPIPE exceptions are
   *    always caught, and implicitly handled by closing the pipeline.
   *)

val add_consumer : 
      ?descr:Unix.file_descr ->     (* default: stdout *)
      consumer:(Unix.file_descr -> bool) ->
      command ->
      job ->
        unit
  (** Adds a consumer to the job. A consumer transfers data from the
   * subprocess realizing the passed command to the current process. 
   * To do so, a pipe is created between the file descriptor [descr]
   * of the subprocess and another descriptor [descr'] which is open
   * in the current process. The function [consumer] is called when 
   * data can be read from the pipe. The argument of [consumer] is 
   * reading end of the pipe [descr']. This file descriptor is in
   * non-blocking mode. The function [consumer] must close [descr'] 
   * after EOF is detected. The return value of [consumer] indicates whether
   * the descriptor is still open.
   *
   * @param descr The descriptor of the subprocess to which the writing
   *    end of the pipe is dup'ed. By default [stdout].
   *)

(* CHECK: Was passiert wenn consumer eine exception wirft? *)

val to_buffer :
      Buffer.t ->
        (Unix.file_descr -> bool)
  (** [to_buffer b] returns a function which can be
   * used as [consumer] argument for [add_consumer]. The data received
   * from the subprocess is added to the buffer [b]. 
   *)

type group_mode = 
    Same_as_caller    (** The job runs in the same process group as the current process *)
  | Foreground        (** The job runs in a new foreground process group *)
  | Background        (** The job runs in a new background process group *)
  (** Specifies how the job instance is related to process groups *) 

val run_job :
      ?mode:group_mode ->                (* default: Same_as_caller *)
      ?forward_signals:bool ->           (* default: true *)
      job ->
        job_instance
  (** Invokes the commands of the job such that they run concurrently
   * with the main process.
   *
   * The function returns a [job_instance], i.e. a value recording which
   * processes are started, and how they are related. Furthermore, the
   * function has the side effect of adding the
   * job to the global list of current jobs.
   *
   * The [mode] argument specifies whether a new Unix process group is
   * created for the job instance. A process group has the advantage that
   * it is possible to send signals to all processes of the group at
   * once. For example, one can terminate a group by sending SIGTERM
   * to it: All member processes get the signal. Usually, these are not only
   * the subprocesses initially created, but also further processes 
   * started by the initial members.
   *
   * So if it is necessary to send signals to the processes of the job,
   * it will be advantegous to run it in a new process group. However,
   * this also means that signals sent to the current process group
   * are not automatically forwarded to the created process group. For
   * example, if the current process group is terminated, the job
   * will continue running, because it is member of a different process
   * group. One has to explicitly catch and forward signals to avoid
   * wild-running jobs.
   *
   * The moral of the story is that one should only create new process
   * groups when it is necessary (e.g. the user must be able to stop
   * an action at any time). Furthermore, signal forwarding must be
   * configured.
   *
   * The Unix shell also allows the programmer to specify process group
   * handling to a certain extent. Normally, commands are executed in the
   * same process group as the caller. The syntax "command &" forces that
   * the command is run in a new background process group. There is another
   * situation when new process groups are created: when a new {b interactive} 
   * shell is started the commands are run in new foreground process groups
   * (so the keyboard signals like CTRL-C work).
   *
   * @param mode Specifies the process group handling. By default, the
   *   job is executed in the same process group as the current process
   *   ([Same_as_caller]). The value [Background] causes that a new
   *   background process group is started. The value [Foreground] causes
   *   that a new foreground process group is started. For the latter,
   *   it is required that there is a controlling terminal (i.e. it
   *   does not work for daemons). Any existing foreground process group
   *   (there is at most one) is put into the background, but this is
   *   not restored when the job is over (the caller must do this).
   *   Foreground process groups should be avoided unless you are
   *   writing an interactive shell interpreter.
   * @param forward_signals If [true], the default, keyboard signals
   *   (SIGINT, SIGQUIT) delivered to the current process are forwarded to 
   *   the job. This has only a meaning if the job is running as 
   *   background process group. Furthermore, it is required that
   *   [install_job_handlers] has been called to enable signal 
   *   forwarding.
   * 
   * The function returns normally if at least one process could be started.
   * If no process was startable (i.e. the first command was not startable), 
   * an exception is raised. If one or more processes could be started but
   * not all, [job_status] will return [Job_partially_running]. The caller 
   * should then discard the job and any intermediate result that might
   * already have been produced by the partial job.
   *
   * When all processes could be started and no other exceptional condition
   * happened, the function sets [job_status] to [Job_running].
   *)


val register_job :
      system_handler ->
      job_instance -> unit
  (** Registers the job at the passed [system_handler]. This is not necessary
   * if you directly call [finish_job].
   *)


val finish_job :
      ?sys:system_handler ->
      job_instance -> unit
  (** Waits until all of the processes of the job have terminated.
   * The function handles all producer/consumer events and calls the
   * producer/consumer functions as necessary.
   *
   * Exceptions raised by the producer/consumer functions are not caught.
   * In this case, [finish_job] is restartable.
   *
   * The [sys] argument determines the system_handler ([standard_system_handler]
   * by default). The job instance is registered at the system handler,
   * and it is waited until the job finishes. Roughly, [finish_job]
   * is equivalent to
   * {[
   *   register_job sys jobinst;
   *   sys.sys_wait()
   * ]}
   *)

val call_job :
      ?mode:group_mode ->                     (* default: Same_as_caller *)
      ?forward_signals:bool ->                (* default: true *)
      ?onerror:(job_instance -> unit) ->      (* default: abandon_job *)
      job ->
        job_instance
  (** Starts the job (see [run_job]) and waits until it finishes (see
   * [finish_job]); i.e. [call_job = run_job + finish_job].
   * The function returns normally if all processes can be started; you can
   * examine [job_status] of the result to get the information whether all
   * processes returned the exit code 0.
   *
   * @param onerror If not all of the processes can be started, the
   *    function passed by [onerror] is invoked. By default, this
   *    function calls [abandon_job] to stop the already running
   *    processes. After the [onerror] function has returned, the original 
   *    exception is raised again. Fatal error conditions are not caught.
   * @param mode See [run_job]
   * @param forward_signals See [run_job]
   *)

val processes : job_instance -> process list
  (** Returns the processes that have actually been started for this job
   * by [run_job]; note that the corresponding Unix process group
   * may have additional processes (e.g. indirectly started processes).
   *)

exception No_Unix_process_group;;
  (** Raised by functions referring to Unix process groups when the
   * job has not been started in its own process group.
   *)

val process_group_leader : job_instance -> process
  (** Returns the process group leader process.
   * This function is not available for jobs in the mode [Same_as_caller].
   *)

val process_group_id : job_instance -> int
  (** Returns the Unix ID of the process group as number > 1.
   * This function is not available for jobs in the mode [Same_as_caller].
   *)

val process_group_expects_signals : job_instance -> bool
  (** [true] iff the group has [mode=Background] and [forward_signals]. *)

type job_status =
    Job_running            (** All commands could be started, and at least
			    * one process is still running
			    *)
  | Job_partially_running  (** Not all commands could be started, and at least
			    * one process is still running
			    *)
  | Job_ok                 (** all processes terminated with exit code 0 *)
  | Job_error              (** all processes terminated but some abnormally *)
  | Job_abandoned          (** the job has been abandoned (see [abandon_job]) *)
  (** Indicates the status of the job *)

val job_status : job_instance -> job_status
  (** Returns the status. The status may only change after [finish_job]
   * has been called:
   *
   * - after [run_job]: status is [Job_running] or [Job_partially_running]
   * - after [finish_job]: if returning normally: status is [Job_ok] or 
   *   [Job_error]. After an exception happened the other states are possible,
   *   too
   *)

val kill_process_group : 
      ?signal:int ->              (* default: SIGTERM *)
      job_instance -> unit
  (** Kills the process group if it is still (at least partially) running.
   * This operation is not available if the mode is [Same_as_caller]
   * (exception [No_Unix_process_group]).
   *
   * Note 1: In the Unix terminology, "killing a job" only means to send
   * a signal to the job. So the job may continue running, or it may
   * terminate; in general we do not know this. Because of this, the job
   * will still have an entry in the job list.
   *
   * Note 2: Because sub-sub-processes are also killed, this function may send
   * the signal to more processes than kill_processes (below). On the other
   * hand, it is possible that sub-processes change their group ID such that
   * it is also possible that this function sends the signal to fewer processes
   * than kill_processes.
   *
   * @param signal The signal number to send (O'Caml signal numbers as
   *    used by the [Sys] module). Default is [Sys.sigterm].
   *)

val kill_processes : 
      ?signal:int ->              (* default: SIGTERM *)
      job_instance -> unit
  (** Kills the individual processes of the job which are still running.
   *
   * @param signal The signal number to send (O'Caml signal numbers as
   *    used by the [Sys] module). Default is [Sys.sigterm].
   *)

val abandon_job :
      ?signal:int ->              (* default: SIGTERM *)
      job_instance -> unit
  (** Tries to get rid of a running job. If the mode is [Same_as_caller], the
   * signal is sent to the processes individually. If the mode is
   * [Foreground] or [Background], the signal is sent to the process group 
   * corresponding to the job.
   *
   * This function removes the job from the job list; i.e. it is no longer
   * watched. Because of some magic spells it is guaranteed that the job dies
   * immediately without becoming a zombie (provided you have a SIGCHLD
   * handler).
   *
   * @param signal The signal number to send (O'Caml signal numbers as
   *    used by the [Sys] module). Default is [Sys.sigterm].
   *)

val iter_job_instances :
      f:(job_instance -> unit) ->
        unit
  (** Iterates over the jobs in the list of active jobs and calls [f] for every
   * [job_instance].
   *)

val watch_for_zombies : unit -> unit
  (** Iterates over the jobs in the list of abandoned jobs, and removes
   * zombie processes. 
   *)



exception Already_installed;;
  (** Raised when the job handlers are already installed *)

val configure_job_handlers :
      ?catch_sigint:bool ->      (* default: true *)
      ?catch_sigquit:bool ->     (* default: true *)
      ?catch_sigterm:bool ->     (* default: true *)
      ?catch_sighup:bool ->      (* default: true *)
      ?catch_sigchld:bool ->     (* default: true *)
      ?set_sigpipe:bool ->       (* default: true *)
      ?at_exit:bool ->           (* default: true *)
      unit ->
      unit
  (** Configures signal and at_exit handlers for jobs:
   * - The keyboard signals SIGINT and SIGQUIT are forwarded to all jobs
   *   which are running in the background (and thus are not
   *   automatically notified) and want to get such signals ([forward_signals]).
   *   After the signals have been forwarded, the previous signal action
   *   is performed.
   * - The signals SIGTERM and SIGHUP are (if the handler is installed) 
   *   forwarded to all dependent processes (regardless whether they are
   *   running in their own Unix process group or not, and regardless of
   *   [forward_signals]).
   *   After the signals have been forwarded, the previous signal action
   *   is performed.
   * - The [at_exit] handler sends a SIGTERM to all dependent processes, too.
   * - the SIGCHLD handler calls [watch_for_zombies].
   *   After this function is called, the previous signal action
   *   is performed; however if the previous action was [Signal_ignore]
   *   this is incorrectly interpreted as empty action (zombies are not
   *   avoided)
   * - The handler for SIGPIPE does nothing; note that a previous action
   *   is overwritten (the parameter is called [set_sigpipe] to stress this)
   *
   * Dependent processes are:
   * - For jobs with mode = [Foreground] or [Background]: the processes
   *   of the corresponding Unix process group
   * - For jobs with mode = [Same_as_caller]: the actually started
   *   children processes
   *
   * Note that if an uncaught exception leads to program termination,
   * this situation will not be detected; any running jobs will
   * not be terminated (sorry, this cannot be fixed).
   *
   * This function sets only which handlers will be installed when
   * [install_job_handlers] (below) is invoked.
   * The function fails if the handlers are already installed.
   *
   * KNOWN BUGS: At least for O'Caml 3.00, the handlers do not call the old
   * signal handlers after their own work has been done; this is due to an
   * error in Sys.signal.
   *
   * @param catch_sigint whether to install a SIGINT handler (default: [true])
   * @param catch_sigquit whether to install a SIGQUIT handler (default: [true])
   * @param catch_sigterm whether to install a SIGTERM handler (default: [true])
   * @param catch_sighup whether to install a SIGHUP handler (default: [true])
   * @param catch_sigchld whether to install a SIGCHLD handler (default: [true])
   * @param set_sigpipe whether to set a SIGPIPE handler (default: [true])
   * @param at_exit whether to set the [at_exit] handler (default: [true])
   *)

val install_job_handlers : unit -> unit
  (** Installs handlers as configured before.
   * Raises [Already_installed] if the handlers are already installed.
   *)

(** {1 Removed functions} *)

(** The functions [add_rd_polling] and [add_wr_polling] have been removed.
 * They were added prior to the merge with the equeue library. Use a 
 * Unixqueue now, which is much more powerful.
 *)


(**/**)

val init_mt : (unit -> ((unit -> unit) * (unit -> unit))) -> unit
  (* private *)
