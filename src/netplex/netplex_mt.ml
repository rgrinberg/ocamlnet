(* $Id$ *)

open Netplex_types

class mt () : Netplex_types.parallelizer =
object
  method ptype = `Multi_threading

  method init() =
    ()

  method start_thread : 't . ('t -> unit) -> 't -> 'x -> string -> logger -> par_thread =
    fun f arg l srv_name logger ->
      let t = Thread.create f arg in
      ( object
	  method ptype = `Multi_threading
	  method info_string = "Thread " ^ string_of_int (Thread.id t)
	  method watch_shutdown _ =
	    (* We cannot do anything here to ensure the thread is really dead *)
	    ()
	end
      )
end


let mt() = new mt()
