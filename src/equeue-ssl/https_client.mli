(* $Id$ *)

(** HTTPS extension to {!Http_client} *)

type channel_binding_id = int
(** Same as {!Http_client.channel_binding_id} *)	  

class type transport_channel_type =
object
  method setup_e : Unix.file_descr -> channel_binding_id -> float ->
                   string -> int -> Unixqueue.event_system ->
                   Uq_engines.multiplex_controller Uq_engines.engine
  method continue : Unix.file_descr -> channel_binding_id -> float ->
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
