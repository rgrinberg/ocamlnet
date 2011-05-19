(* Copyright (c) 2001 Patrick Doane.
 * For conditions of distribution and use, see copyright notice in LICENSE. *)

(** FORT provides an environment for testing programs and modules.
    This module contains the primitives used by test scripts.
    Individual test cases are written as functions which return a
    value of type [result].  *)

type result =
  | Pass                    (** test succeeded as expected *)
  | UPass                   (** test succeeded but was expected to fail *)
  | Fail of string          (** test failed but was expected to succeed *)
  | XFail                   (** test failed as expected *)
  | Unresolved              (** manual inspection required to determine
			        outcome *)
  | Untested                (** test is still under development *)
  | Unsupported of string   (** test depends on a feature that is not
			        available in the current environment. *)

(** [Fort.test desc body] executes a test case containing the code in
    [body].  Diagnostic information uses [desc] to identify the test
    case *)
val test : desc:string -> body:(unit -> result) -> unit


(** [Fort.run_script file] inteprets the script designated by [file]. *)
val run_script : string -> unit

(** Expect based testing

    These routines are appropriate for test cases that are composed of
    a sequence of possibly-nested assertions. The functions
    [expect_pass] and [expect_fail] will execute a test and return
    Pass or XFail respectively if there are no failures in the test.
    Failures occur by calling the [fail] routine directly or
    indirectly through [expect_true], [expect_equal] or
    [expect_equal_app]. *)

val expect_pass  : desc:string -> body:(unit -> unit) -> unit
val expect_fail  : desc:string -> body:(unit -> unit) -> unit

val fail         : string -> 'a
val expect_true  : ?msg:string -> bool -> unit
val expect_equal : ?msg:string -> ?printer:('a -> string) -> 'a -> 'a -> unit

(** [Fort.expect_equal_app f x g y] executes [f x] and [g y] an expects
    that they produce the same value or raise the same exception. *)
val expect_equal_app : ?msg:string -> ?printer:('b -> string) ->
                       ('a -> 'b) -> 'a -> ('c -> 'b) -> 'c -> unit

(** Test directory structures *)

(** [Fort.input_filename path] prefixes [path] with the location of the
    input root and the path to the test script. *)
val input_filename : string -> string

(** [Fort.output_filename path] prefixes [path] with the location of the
    output root and the path to the test script. *)
val output_filename : string -> string

(*d [Fort.foreach_file path filter f] executes [f] on each filename located
    in the directory specified by [path] which is accepted by
    [filter].  By default, [filter] accepts all files. *)
val foreach_file :
  string -> ?filter:(string -> bool) -> (string -> unit) -> unit

(** Utilities *)

(** [Fort.bracket before after f init] evaluates the function [f] with a
    resource acquired by [before] and released by [after]. The
    resource is always released, even when an exception is raised. *)
val bracket : ('a -> 'b) -> ('b -> unit) -> ('b -> 'c) -> 'a -> 'c

(** [Fort.read_file path f] safely evaluates the function [f] with the
    [in_channel] returned by [open_in path].  The channel is always
    closed, even when an exception is raised. *)
val read_file : string -> (in_channel -> 'a) -> 'a

(** [Fort.write_file path f] safely evaluates the function [f] with
    the [out_channel] returned by [open_out path].  The channel is
    always closed, even when an exception is raised. *)
val write_file : string -> (out_channel -> 'a) -> 'a

(** [Fort.mkdir path perm] creates a directory with the given
    permissions. It will recursively create any parent directories
    that do not exist. *)
val mkdir : string -> perm:int -> unit

(** For internal use by toplevel. *)
val main : unit -> unit
