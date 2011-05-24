(* $Id$ *)

open Uq_engines.Operators

type channel_binding_id = int

class type transport_channel_type =
object
  method setup_e : Unix.file_descr -> channel_binding_id -> float ->
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller Uq_engines.engine
  method continue : Unix.file_descr -> channel_binding_id -> float ->
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller
end


let https_transport_channel_type ctx : transport_channel_type =
  let ctx_of_fd = Hashtbl.create 12 in
  let preclose fd () =
    Hashtbl.remove ctx_of_fd fd in
  ( object(self)
      method setup_e fd cb tmo host port esys =
	let mplex =
	  Uq_ssl.create_ssl_multiplex_controller
	    ~close_inactive_descr:true
	    ~preclose:(preclose fd)
	    fd ctx esys in
	Uq_ssl.ssl_connect_engine mplex
	++ (fun () ->
	      Hashtbl.replace ctx_of_fd fd mplex;
	      eps_e (`Done (mplex :> Uq_engines.multiplex_controller)) esys
	   )
	    
      method continue fd cb tmo host port esys =
	let mplex =
	  Uq_ssl.create_ssl_multiplex_controller
	    ~close_inactive_descr:true
	    ~preclose:(preclose fd)
	    ~initial_state:`Client
	    fd ctx esys in
	(mplex :> Uq_engines.multiplex_controller)
    end
  )
