(* $Id$ *)

open Uq_engines.Operators

type channel_binding_id = int

class type transport_channel_type =
object
  method setup_e : Unix.file_descr -> channel_binding_id -> float -> exn ->
                   string -> int -> Unixqueue.event_system ->
                   (Uq_engines.multiplex_controller * 
                    exn option)
                   Uq_engines.engine
  method continue : Unix.file_descr -> channel_binding_id -> float -> exn ->
                   string -> int -> Unixqueue.event_system ->
                   exn option ->
                   Uq_engines.multiplex_controller
end


exception HTTPS_client_private_data of Ssl.socket


let https_transport_channel_type ?(verify = fun _ _ _ -> ())
                                 ctx : transport_channel_type =
  let ctx_of_fd = Hashtbl.create 12 in
  let preclose fd () =
    Hashtbl.remove ctx_of_fd fd in
  ( object(self)
      method setup_e fd cb tmo tmo_x host port esys =
	let mplex =
	  Uq_ssl.create_ssl_multiplex_controller
	    ~close_inactive_descr:true
	    ~preclose:(preclose fd)
	    ~timeout:(tmo, tmo_x)
	    fd ctx esys in
	Uq_ssl.ssl_connect_engine mplex
	++ (fun () ->
	      verify ctx mplex#ssl_socket fd;
	      Hashtbl.replace ctx_of_fd fd mplex;
              let base_mplex = (mplex :> Uq_engines.multiplex_controller) in
              let priv_data =
                Some(HTTPS_client_private_data mplex#ssl_socket) in
	      eps_e (`Done (base_mplex, priv_data)) esys
	   )
      (* NB. It is not possible to call here mplex#inactivate in case
	 of an error because this would close fd and violate the interface.
	 Instead, Uq_ssl has been changed so that the state after
	 an erroneous ssl_connect_engine is cleaned up within this class.
       *)


      method continue fd cb tmo tmo_x host port esys priv_data =
        let ssl_socket =
          match priv_data with
            | Some(HTTPS_client_private_data s) -> s
            | _ -> raise Not_found in
	let mplex =
	  Uq_ssl.create_ssl_multiplex_controller
	    ~close_inactive_descr:true
	    ~preclose:(preclose fd)
	    ~initial_state:`Client
	    ~timeout:(tmo, tmo_x)
            ~ssl_socket
	    fd ctx esys in
	(mplex :> Uq_engines.multiplex_controller)
    end
  )
