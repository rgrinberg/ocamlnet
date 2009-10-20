(* $Id$ *)

(** {1 Netplex support} *)

(** The important function is [nethttpd_factory], see below. The
    other functions are only needed for special effects.
 *)

type config_log_error =
    Unix.sockaddr option -> Unix.sockaddr option -> Nethttp.http_method option
     -> Nethttp.http_header option -> string -> unit

type config_log_access =
    Unix.sockaddr -> Unix.sockaddr -> Nethttp.http_method ->
    Nethttp.http_header -> int64 -> (string * string) list -> bool -> 
    int -> Nethttp.http_header -> int64 -> unit


val std_log_error : Netplex_types.container -> config_log_error
  (** Returns a function that logs errors using the [log_subch] method of
      the passed container
   *)

val std_log_access : ?debug:bool -> 
                     Netplex_types.container -> config_log_access
  (** Returns a function that logs accesses using the [log_subch] method of
      the passed container

      If [debug] is set, additional debug log messages are printed that
      dump the whole access (incl. header and all available information)
   *)

val restrict_file_service_config : Netplex_types.config_file ->
                                   Netplex_types.address ->  unit
  (** Restricts the subsections and paremeters in the [service]
      configuration section of type "file" to the allowed ones.
   *)


val read_file_service_config : Netplex_types.config_file ->
                               Netplex_types.address -> 
                               string ->
                                 Nethttpd_services.file_service
  (** [read_file_service_config cfg addr uri_path]: Reads the
      [service] configuration section of type "file" from config file
      [cfg] at address [addr].  [uri_path] is the default value put
      into the [file_uri] component of the returned record if no "uri"
      configuration parameter exists. (In other words, this is the
      path of the enclosing "uri" section, or "/" if there is only
      a "host" section.) All other parameters are only
      taken from the configuration section. 

      See below at [nethttpd_factory] how a file service needs to
      be configured.
   *)

val restrict_dynamic_service_config : Netplex_types.config_file ->
                                      Netplex_types.address ->  unit
  (** Restricts the subsections and paremeters in the [service]
      configuration section of type "dynamic" to the allowed ones.
   *)

val read_dynamic_service_config : 
      (string * (Netplex_types.config_file ->
                 Netplex_types.address -> 
                 string ->
                   'a Nethttpd_services.dynamic_service
                ) ) list ->
      Netplex_types.config_file ->
      Netplex_types.address -> 
      string ->
        'a Nethttpd_services.dynamic_service
  (** [read_dynamic_service_config handlers cfg addr uri_path]:
      Reads the [service] configuration section of type "dynamic" from config 
      file [cfg] at address [addr]. The alist [handlers] defines the
      available handlers. Every handler [h] is called like
      [h cfg addr uri_path]. [uri_path] is like in [read_file_service_config],
      i.e. the path of the enclosing "uri" section, or "/" by default.

      The [h] function has to return the dynamic service to use, which
      is also returned by [read_dynamic_service_config].

      See below at [nethttpd_factory] how a dynamic service needs to
      be configured.
   *)


val nethttpd_processor : 
  (Netplex_types.container -> #Nethttpd_reactor.http_reactor_config) ->
  'a Nethttpd_types.http_service ->
  Netplex_types.processor
    (** [netplex_processor mk_config http_service]: Creates a Netplex processor
    * for Nethttpd.
    *
    * [mk_config] determines the nethttpd config for a container.
    * This is especially useful for setting the logging functions.
    *
    * The resulting processor must be turned into a full Netplex service
    * by [Netplex_sockserv.create_socket_service] which can then be added
    * by calling the controller's method [add_service].
     *)

type ('a,'b) service_factory =
    (string * 'a Nethttpd_services.dynamic_service) list ->
    Netplex_types.config_file ->
    Netplex_types.address -> 
    string ->
      'b Nethttpd_types.http_service
    constraint 'b = [ `Dynamic_service of 'a Nethttpd_services.dynamic_service
                    | `File_service of Nethttpd_services.file_service 
		    ]
  (** The service factory function is called when a [service] configuration
      section of a certain type needs to be read. The function has args
      [handlers], [cfg], [addr], and [uri_path]. It needs to return the
      [http_service].

      Such a function is usually [read_file_service_config], or
      [read_dynamic_service_config], or a derivative, whose return
      value is turned into a [http_service]. This can be done with
      {!Nethttpd_services.file_service} and
      {!Nethttpd_services.dynamic_service}.
   *)

val default_services : (string * ('a,'b) service_factory) list
  (** The default services *)


val nethttpd_factory :
      ?name:string ->
      ?config_cgi:Netcgi.config -> 
      ?handlers:(string * 'a Nethttpd_services.dynamic_service) list ->
      ?services:(string * ('a,'b) service_factory) list ->
      ?log_error:(Netplex_types.container -> config_log_error) ->
      ?log_access:(?debug:bool -> Netplex_types.container -> config_log_access) ->
      unit ->
        Netplex_types.processor_factory
  (** Reads a configuration section like
    * {[
    *    processor {
    *      type = "nethttpd";          (* or what is passed as "name" arg *)
    *      timeout = 300.0;
    *      timeout_next_request = 15.0;
    *      access_log = "enabled";
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
    * The [access_log] parameter can be set to [off], [enabled], or [debug].
    * The default is [off]. Access messages go to the "access" subchannel
    * of the component logger. If [enabled], one line is printed with the
    * most important data. If [debug] is set, all access data are printed.
    *
    * The sections [host], [uri] and [method] can be nested to any depth.
    * However, on every nesting level only one of these section types must be
    * used. For example, if a [host] section already contains [uri]
    * subsections, it is not allowed to add [method] subsections.
    * Furthermore, the outermost section must be [host].
    *
    * The [service] section may be one of (at least if the [services]
    * parameter is not overridden):
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
    *
    * The [services] optional argument can be used to change the service
    * types understood. If not passed, it defaults to [default_services].
    * The default includes "file" and "dynamic".
   *)
