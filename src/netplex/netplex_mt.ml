(* $Id$ *)

open Netplex_types

class mt () : Netplex_types.parallelizer =
object(self)
  method ptype = `Multi_threading

  method init() =
    ()

  method current_sys_id =
    `Thread (Thread.id(Thread.self()))

  method create_mem_mutex() =
    let m = Mutex.create() in
    (fun () -> Mutex.lock m), (fun () -> Mutex.unlock m)

  method start_thread : (par_thread -> unit) -> 'x -> string -> logger -> par_thread =
    fun f l srv_name logger ->
      let throbj t =
	( object
	    method ptype = `Multi_threading
	    method sys_id = `Thread (Thread.id t)
	    method info_string = "Thread " ^ string_of_int (Thread.id t)
	    method watch_shutdown _ =
	      (* We cannot do anything here to ensure the thread is really dead *)
	      ()
	    method parallelizer = (self : #parallelizer :> parallelizer)
	  end
	) in
      let t = 
	Thread.create
	  (fun () ->
	     let o = throbj (Thread.self()) in
	     f o
	  ) 
	  () in
      throbj t
end


let the_mt = lazy(
  let par = new mt() in
  Netplex_cenv.register_par par;
  par
)

let mt() = Lazy.force the_mt
