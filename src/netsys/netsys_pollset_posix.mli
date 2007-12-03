(* $Id$ *)

(** Pollsets for POSIX operating systems *)

open Netsys_pollset

val poll_based_pollset : int -> pollset
  (** Returns a poll set whose implementation is based on the [poll] system
      call. The passed integer is the minimum size of the poll array. The
      array will grow beyond that if necessary.

      Win32: On Win32 this implementation works, but only for sockets,
      and if not cancellable in multi-threaded programs. (This is a 
      restriction because we have to map it to the [select] call of the
      WinSock layer.)
   *)


(* TODO: Implement cancel_wait *)

(* TODO: pollsets for epoll, kqueue, /dev/poll etc. *)
