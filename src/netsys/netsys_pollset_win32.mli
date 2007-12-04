(* $Id$ *)

(** Pollsets for Win32 *)

open Netsys_pollset

exception Too_many_descriptors

val socket_pollset : unit -> pollset
  (** This is a pollset implementation that works only for sockets.
      Unlike [Netsys_pollset.poll_based_pollset], this implementation
      is cancellable. However, it has another limitation: A (quite low)
      maximum number of descriptors that can be added to the set.
      If the number is exceeded the exception [Too_many_descriptors]
      is raised (by [add]).

      POLLERR, POLLHUP, and POLLNVAL are not detected by this impl.
   *)

(*
val pipe_pollset : unit -> pollset
  (** This is a pollset implementation that works only for pipes and
      consoles. Of course, there are some restrictions:
      - One cannot poll for output. Because of this, pipes are always
        writable.
      - There may be a delay until events are recognized, increasing
        the latency
   *)

val sigchannel_pollset : unit -> pollset
  (** This is a pollset implementation that works only for [sigchannel]
      objects as introduced by [Netsys_signalling]. For Win32, this is
      an abstraction of Win32 event objects. There are some restrictions:
      - One cannot poll for output. Because of this, sigchannels are never
        writable.
      - There is a maximum for the number of descriptors. If the number is 
        exceeded the exception [Too_many_descriptors] is raised (by [add]).
   *)
 *)


val threaded_pollset : unit -> pollset
  (** This implementation unifies [socket_pollset], [pipe_pollset],
      and [sigchannel_pollset],
      and overcomes the limit on the number of descriptors one can add
      to the set. It is, however, only available for multi-threaded
      programs, because it uses helper threads.
   *)
