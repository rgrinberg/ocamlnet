(* $Id$ *)

(** {1 Netplex support} *)

type config_log_error =
    Unix.sockaddr option -> Unix.sockaddr option -> Nethttp.http_method option -> Nethttp.http_header option -> string -> unit

val nethttpd_processor : 
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

val nethttpd_factory :
      ?config_cgi:Netcgi_compat.Netcgi_env.cgi_config -> 
      ?handlers:(string * 'a Nethttpd_services.dynamic_service) list ->
      unit ->
        Netplex_types.processor_factory
  (** Reads a configuration section like
    * {[
    *    processor {
    *      type = "nethttpd";
    *      timeout = 300.0;
    *      timeout_next_request = 15.0;
    *      host {
    *        pref_name = "myhost";     (* optional *)
    *        pref_port = 80;           (* optional *)
    *        names = "myhost:80 yourhost:81";  (* use *:0 for any name *)
    *        uri {
    *          path = "/the/path";
    *          method {
    *            allow = "GET POST";
    *            (* or: deny = "..." *)
    *            service {
    *              type = "...";
    *              ...
    *            }
    *          }
    *        }
    *        uri {
    *          ...
    *        }
    *      }
    *      host {
    *        ...
    *      }
    *    }
    * ]}
    *
    * The sections [host], [uri] and [method] can be nested to any depth.
    * However, on every nesting level only one of these section types must be
    * used. For example, if a [host] section already contains [uri]
    * subsections, it is not allowed to add [method] subsections.
    * Furthermore, the outermost section must be [host].
    *
    * The [service] section may be one of:
    *
    * {[
    *    service {
    *      type = "file";
    *      docroot = "/a/path/in/the/filesystem";
    *      uri = "/the/uri/prefix/corresponding/to/docroot";
    *      media_types_file = "/etc/mime.types";
    *      media_type {
    *        type = "application/foo";
    *        suffix = "foo"
    *      }
    *      default_media_type = "text/plain";
    *      enable_gzip = true;   (* see doc in nethttpd_services.mli *)
    *      index_files = "index.html";
    *      enable_listings = true;
    *      hide_from_listings = "README";   (* list of PCRE regexps *)
    *    }
    * ]}
    *
    * Note that [uri] is taken from the surrounding [uri] section (or
    * assumed to be "/" if there is none) if omitted.
    *
    * {[
    *    service {
    *      type = "dynamic";
    *      handler = "name_of_handler";
    *    }
    * ]}
    *
    * Binds the passed handler here.
    *
    * Any of [host], [uri], and [method] sections may contain one or several
    * [access] sections (which are AND-connected):
    *
    * {[
    *    access {
    *      type = "host";
    *      allow = "host1 host2 ...";
    *      (* or deny = "host1 host2 ..."; *)
    *    }
    * ]}
    *
    * Other access control methods are not yet available.
   *)
