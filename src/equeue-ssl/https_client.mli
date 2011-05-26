(* $Id$ *)

(** HTTPS extension to {!Http_client} *)

type channel_binding_id = int
(** Same as {!Http_client.channel_binding_id} *)	  

class type transport_channel_type =
object
  method setup_e : Unix.file_descr -> channel_binding_id -> float -> exn -> 
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller Uq_engines.engine
  method continue : Unix.file_descr -> channel_binding_id -> float -> exn ->
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller
end
(** Same as {!Http_client.transport_channel_type} *)	  

val https_transport_channel_type : Ssl.context -> transport_channel_type
  (** Configures a TLS tunnel for this context *)

(** {2 How to configure a pipeline for TLS}

    Just follow this recipe:

    1. Create the [Ssl] context:

    {[ Ssl.init() ]}

    {[ let ctx = Ssl.create_context Ssl.TLSv1 Ssl.Client_context ]}

    2. Create the transport channel type:

    {[ let tct = Https_client.https_transport_channel_type ctx ]}

    3. Configure the transport:

    {[ pipeline # configure_transport Http_client.https_cb_id tct ]}

    Now all URLs starting with "https://" will use this transport.
    If you need more control about the type of SSL/TLS channel, you
    can create new channel binding IDs, and configure these in addition.
    For each message needing a specific context, just set the
    channel binding ID (method [set_channel_binding] of the message).
 *)

(** {2 Features and limitations} 

    We only implement RFC 2618, i.e. secure connections on a separate
    port (443 instead of 80). There is no support (yet) for RFC 2617,
    i.e. upgrading an existing insecure connection to a secure one.

    If an HTTP proxy server is configured, the TLS connection is established
    via the CONNECT method (documented in RFC 2617).

    There is a limitation if the CONNECT method needs to be
    authenticated: During authentication the connection must not be
    closed. As a rule of thumb, a proxy with HTTP/1.1 support will act
    fine (like Apache). If the proxy supports only HTTP/1.0 (as the
    whole 2.x line of Squid, for instance), things may go wrong. Many
    versions of Squid seem to always close the connection after sending
    out a 407 status code. The client would have then to reconnect, and
    retry CONNECT with the right authentication data. Because of the
    structure of {!Http_client} this is hard to implement for us,
    and it would only be done to support ancient protocol versions.
    Better upgrade your proxy to a recent version (which will also improve
    speed).
    
    Alternatively, it is also possible to connect via SOCKS version 5
    proxies.

 *)
