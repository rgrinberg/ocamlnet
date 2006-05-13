(* $Id$ *)

(** {1 Netplex support} *)

type config_log_error =
    Unix.sockaddr option -> Unix.sockaddr option -> Nethttp.http_method option -> Nethttp.http_header option -> string -> unit

val netplex_processor : 
      (config_log_error -> #Nethttpd_reactor.http_reactor_config) ->
      'a Nethttpd_types.http_service ->
      Netplex_types.processor
  (** [netplex_processor mk_config http_service]: Creates a Netplex processor
    * for Nethttpd.
    *
    * [mk_config] gets a logging function as argument that will log to the
    * Netplex logging service. This function has the same type as the
    * [config_log_error] method of the [http_reactor_config].
    *
    * The resulting processor must be turned into a full Netplex service
    * by [Netplex_sockserv.create_socket_service] which can then be added
    * by calling the controller's method [add_service].
   *)
