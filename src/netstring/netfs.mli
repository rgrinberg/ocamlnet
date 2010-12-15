(* $Id$ *)

(** Class type [stream_fs] for filesystems with stream access to files *)

(** This is an abstraction for both kernel-level and user-level
    filesystems. It is used as parameter for algorithms (like globbing)
    that operate on filesystems but do not want to assume any particular
    filesystem. Only stream access is provided (no seek).

    The filesystem supports hierarchical file names. File paths use
    Unix conventions, i.e.
    - [/] is the root
    - Path components are separated by slashes. Several consecutive slashes
      are allowed but mean the same as a single slash.
    - [.] is the same directory
    - [..] is the parent directory

    All paths need to be absolute (i.e. start with [/]).

    There can be additional constraints on paths:
    - Character encoding restriction: A certain ASCII-compatible character
      encoding is assumed (including UTF-8)  
    - Character exclusion: Certain characters may be excluded
    - Case insensitivity

    Implementations may impose more constraints that cannot be expressed
    here (path length, exclusion of special names etc.).

    There is no assumption that [/] is the real root of the local filesystem.
    It can actually be anywhere - a local subdirectory, or a remote directory,
    or a fictive root. There needs not to be any protection against "running
    beyond root", e.g. with the path [/..].

    This class type also supports remote filesystems, and thus there is no
    concept of file handle (because this would exclude a number of
    implementations).

    Errors should generally be indicated by raising [Unix_error].
 *)

type read_flag =
    [ `Skip of int64 | `Binary ]

type write_flag =
    [ `Create | `Exclusive | `Truncate | `Binary ]

type size_flag =
    [ `Dummy ]

type test_flag =
    [ `Link ]

type remove_flag =
    [ `Recursive ]

type rename_flag =
    [ `Dummy ]

type symlink_flag =
    [ `Dummy ]

type readdir_flag =
    [ `Dummy ]

type mkdir_flag =
    [ `Path | `Nonexcl ]

type rmdir_flag =
    [ `Dummy ]


type test_type =
    [ `N | `E | `D | `F | `H | `R | `W | `X | `S ]
     (** - [`N]: the file name exists
         - [`E]: the file exists
	 - [`D]: the file exists and is a directory
	 - [`F]: the file exists and is regular
	 - [`H]: the file exists and is a symlink (possibly to a non-existing
           target)
	 - [`R]: the file exists and is readable
	 - [`W]: the file exists and is writable
	 - [`X]: the file exists and is executable
	 - [`S]: the file exists and is non-empty
      *)

class type stream_fs =
object
  method path_encoding : Netconversion.encoding option
    (** The encoding must be ASCII-compatible
	({!Netconversion.is_ascii_compatible}). If [None] the
	ASCII encoding is assumed for codes 0-127, and no meaning is
	defined for byte codes 128-255.
     *)

  method path_exclusions : (int * int) list
    (** Code points that must not occur in path components between slashes.
	This is given as ranges [(from,to)]. The code points are interpreted
	as Unicode code points if an encoding is available, and as byte codes
	otherwise. For example, for Unix the code points 0 and 47 (slash)
	are normally the only excluded code points.
     *)

  method case_insensitive : bool
    (** Whether the filesystem is case-insensitive *)

  method nominal_dot_dot : bool
    (** Whether the effect of [..] can be obtained by stripping off the
	last path component, i.e. whether
	[Filename.dirname path <=> path ^ "/.."]
     *)

  method read : read_flag list -> string -> Netchannels.in_obj_channel
    (** [read flags filename]: Opens the file [filename] for reading,
	and returns the input stream. Flags:
	- [`Skip n]: Skips the first [n] bytes of the file. On many
	  filesystems this is more efficient than reading [n] bytes and
	  dropping them; however, there is no guarantee that this 
	  optimization exists.
	- [`Binary]: Opens the file in binary mode (if there is such
	  a distinction)
     *)

  method write : write_flag list -> string -> Netchannels.out_obj_channel
    (** [write flags filename]: Opens (and optionally creates) the [filename]
	for writing, and returns the output stream. Flags:
	- [`Create]: If the file does not exist, create it
	- [`Truncate]: If the file exists, truncate it to zero before
	  writing
	- [`Exclusive]: The [`Create] is done exclusively
	- [`Binary]: Opens the file in binary mode (if there is such
	  a distinction)

	Many filesystems refuse this operation if neither [`Create] nor
	[`Truncate] is specified because overwriting an existing file
	is not supported. There are also filesystems that cannot even
	modify files by truncating them first, but only allow to write
	to new files.

	It is unspecified whether the file appears in the directory directly
	after calling [write] or first when the stream is closed.
     *)

  method size : size_flag list -> string -> int64
    (** Returns the size of a file *)

  method test : test_flag list -> string -> test_type -> bool
    (** Returns whether the test is true. For filesystems that know
	symbolic links, the test operation normally follows symlinks
	(except for the [`N] and [`H] tests). By specifying the [`Link] flag
	symlinks are not followed.
     *)

  method test_list : test_flag list -> string -> test_type list
    (** Similar to [test] but this function returns all tests that are
	true.
     *)

  method remove : remove_flag list -> string -> unit
    (** Removes the file or symlink. Implementation are free to also
	support the removal of empty directories.
        
        Flags:
	- [`Recursive]: Remove the contents of the non-empty directory
	  recursively. This is an optional feature. There needs not to
	  be any protection against operations done by other processes
	  that affect the directory tree being deleted.
     *)


  method rename : rename_flag list -> string -> string -> unit
    (** Renames the file. There is no guarantee that a rename is atomic
     *)

  method symlink : symlink_flag list -> string -> string -> unit
    (** [symlink flags oldpath newpath]: Creates a symlink. This
	is an exclusive create, i.e. the operation fails if [newpath]
	already exists.
     *)

  method readdir : readdir_flag list -> string -> string list
    (** Reads the contents of a directory. Whether "." and ".." are returned
	is platform-dependent. The entries can be returned in any order.
     *)

  method mkdir : mkdir_flag list -> string -> unit
    (** Creates a new directory. Flags:
	- [`Path]: Creates missing parent directories. This is an
	  optional feature. (If not supported, ENOENT is reported.)
	- [`Nonexcl]: Non-exclusive create.
     *)

  method rmdir : rmdir_flag list -> string -> unit
    (** Removes an empty directory *)
end


val local_fs : ?encoding:Netconversion.encoding -> string -> stream_fs
  (** [local_fs real_root]: Returns a filesystem object where the 
      [root] of the system is the directory [real_root] of the local
      filesystem.

      - [encoding]: Specifies the character encoding of paths. The default
        is system-dependent.
   *)

val copy : ?replace:bool -> stream_fs -> string -> stream_fs -> string -> unit
  (** [copy orig_fs orig_name dest_fs dest_name]: Copies the file [orig_name]
      from [orig_fs] to the file [dest_name] in [dest_fs]. By default,
      the destination file is truncated and overwritten if it already
      exists.

      The copy is done by reading from the original file and writing the
      data to the destination file. For remote filesystems, this might be
      not the best method.

      - [replace]: If set, the destination file is removed and created again
        if it already exists
   *)


(** {2 OS Notes} *)

(** {b Unix} in general: There is no notion of character encoding of
    paths. Paths are just bytes. Because of this, the default encoding
    is [None]. If a different encoding is passed to [local_fs], these
    bytes are just interpreted in this encoding. There is no conversion.

    There is right now no generic way of detecting whether a filesystem
    is case-insensitive (help on this topic is greatly appreciated).
 *)

(** {b Windows}: Each drive letter is considered as a separate
    filesystem, e.g.

    {[let fs = local_fs "c:/"]}

    would make only "c:/" accessible, and

    {[ fs # read [] "/file" ]}

    would actually read the file "c:/file". Windows filesystems are
    case-insensitive. The [encoding] arg defaults to current ANSI codepage, 
    and it is
    not supported to request a different encoding. (The difficulty is
    that the Win32 bindings of the relevant OS functions always assume
    the ANSI encoding.)

    There is no support for backslashes as path separators (such paths
    will be rejected), for better compatibility with other platforms.
 *)
