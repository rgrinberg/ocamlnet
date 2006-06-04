(* $Id$ *)

class mt () : Netplex_types.parallelizer =
object
  method ptype = `Multi_threading

  method init() =
    ()

  method start_thread : 't . ('t -> unit) -> 't -> 'x -> unit =
    fun f arg l ->
      ignore(Thread.create f arg)
end


let mt() = new mt()
