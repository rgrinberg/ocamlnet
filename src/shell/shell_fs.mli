(* $Id$ *)

(** Shell filesystem *)

(** This module emulates a filesystem by submitting shell commands.
    For example, a directory listing is retrieved via the [ls]
    utility instead of opening the directory directly. This also
    works when logging in to a remote machine, e.g. via [ssh].

    The following standard POSIX commands are used:
    - [dd] with options [if], [of], [bs], [skip], [conv=notrunc], 
      and optionally [excl] (the latter is a GNU extension)
    - [test] with options [-d], [-e], [-f], [-r], [-s], [-w], [-x]
    - [ls] with options [-1], [-n], [-d], [-a], [-L]
    - [rm] with options [-r] and [-f]
    - [mv] with option [-f]
    - [ln] with option [-s]
    - [mkdir] with options [-p]
    - [rmdir]
    - [cp] with option [-p]

    In addition to this, the commands may be embedded in one-line shell
    scripts.

    Filenames with leading minus chars are not supported.

    Error reporting is less accurate than for a local filesystem.

    {b Example.} List a directory on a remote system:

    {[
      let fs = 
        Shell_fs.shell_fs 
          (Shell_fs.ssh_interpreter ~host:"moon" ())
      let files =
        fs # readdir [] "/this/is/a/directory/on/moon"
    ]}
 *)


type command_context =
    { sfs_command : string;          (** The command line *)
      sfs_stdin : Shell.producer;    (** stdin from here *)
      sfs_stdout : Shell.consumer;   (** stdout goes here *)
      sfs_stderr : Shell.consumer;   (** stderr goes here *)
      mutable sfs_status : Unix.process_status option; (** The exit code is put here *)
    }

type command_interpreter
  (** Runs the command, and fills in [sfs_status] *)

val local_interpreter : unit -> command_interpreter
  (** Executes commands on the local machine *)

val cmd_interpreter : (command_context -> Shell_sys.command list) -> 
                       command_interpreter
  (** Creates a command interpreter from a function that creates the
      real command (as pipeline) to execute
   *)

val ssh_interpreter : ?options:string list -> ?user:string -> host:string ->
                      unit -> command_interpreter
  (** Executes commands via ssh on the machine [host] as [user] (defaults
      to current user). [options] are further command-line options.
      By default, only [-o BatchMode yes] is passed.
   *)

class type shell_stream_fs =
object
  inherit Netfs.stream_fs

  method last_stderr : string
    (** The stderr output of the last operation *)
end


class shell_fs : ?encoding:Netconversion.encoding -> ?root:string -> 
                 ?dd_has_excl:bool ->
                 command_interpreter -> shell_stream_fs
val shell_fs : ?encoding:Netconversion.encoding -> ?root:string -> 
               ?dd_has_excl:bool ->
               command_interpreter -> shell_stream_fs
  (** The shell filesystem.

      - [encoding]: the assumed character encoding of the filenames.
        [None] by default.
      - [root]: the root of the file tree that is accessed. This can
        be an absolute path, or a relative path. 
      - [dd_has_excl]: whether the [dd] command support "conv=excl".
        Default is [false]; this is a GNU extension.
      
   *)
