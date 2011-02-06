(* $Id$ *)

(** SCRAM mechanism for authentication (RFC 5802) *)

(** This implements SCRAM-SHA-1 for GSSAPI. Other profiles may be added later.

    As we do not implement SASLprep, usernames and passwords are restricted
    to US-ASCII.
 *)

type ptype = [ `GSSAPI ]
  (** Currently only the variant for [`GSSAPI] is supported *)

type mechanism = [ `SHA_1 ]

type profile =
    { ptype : ptype;
      mechanism : mechanism;       (** Which mechanism *)
      return_unknown_user : bool;  (** Whether servers hide the fact that the
				       user is unknown *)
      iteration_count_limit : int; (** Largest supported iteration number *)
    }
  (** Profile *)


type server_error =
    [ `Invalid_encoding
    | `Extensions_not_supported
    | `Invalid_proof
    | `Channel_bindings_dont_match
    | `Server_does_support_channel_binding
    | `Channel_binding_not_supported
    | `Unsupported_channel_binding_type
    | `Unknown_user
    | `Invalid_username_encoding
    | `No_resources
    | `Other_error
    | `Extension of string
    ]
  (** Error codes of this protocol *)

type client_session
  (** Session context for clients *)


type server_session
  (** Session context for servers *)


exception Invalid_encoding of string * string
  (** Raised by clients when something cannot be decoded. First string
      is an error message, the second string the raw message that cannot
      be decoded
   *)

exception Invalid_username_encoding of string * string
  (** Raised by clients when the username does not match the requirements.
      Arguments as for [Invalid_encoding].
   *)

exception Extensions_not_supported of string * string
  (** Raised by clients when the server enables an unsupported extension.
      Arguments as for [Invalid_encoding].
   *)

exception Protocol_error of string
  (** Raised by clients when the server violates the protocol. The argument
      is a message.
   *)

exception Invalid_server_signature
  (** Raised by clients when the signature sent by the server is invalid
      (i.e. the server does not know the client password)
   *)

exception Server_error of server_error
  (** Raised by clients when the server sent an error code *)


val profile : ?return_unknown_user:bool -> ?iteration_count_limit:int ->
              ptype -> profile
  (** Creates a profile *)

val string_of_server_error : server_error -> string
val server_error_of_string : string -> server_error
  (** Conversion *)


(** {2 Clients} *)

(** The idea is to create a client session [s] first. The functions
    [client_emit_flag] and [client_recv_flag] indicate now whether
    the client needs to emit a new message, or whether it needs to
    receive a message, respectively. Emission is done by [client_emit_message],
    reception by [client_recv_message]. If everything goes well, the
    protocol state advances, and finally [client_finish_flag] is true.
    This indicates that the client is authenticated and that the server
    knows the client's password. If an error occurs, an exception is
    raised (see above for possibilities), and [client_error_flag] signals
    [true].
 *)

val create_client_session : profile -> string -> string -> client_session
  (** [create_client_session p username password]: Creates a new client
      session for profile [p] so that the client authenticates as user
      [username], and proves its identify with the given [password].
   *)

val client_emit_flag : client_session -> bool
  (** Whether [client_emit_message] can now be called *)

val client_recv_flag : client_session -> bool
  (** Whether [client_recv_message] can now be called *)

val client_finish_flag : client_session -> bool
  (** Whether the client is authenticated and the server verified *)

val client_error_flag : client_session -> bool
  (** Whether an error occurred, and the protocol cannot advance anymore *)

val client_emit_message : client_session -> string
  (** Emits the next message to be sent to the server *)

val client_recv_message : client_session -> string -> unit
  (** Receives the next message from the server *)


(** {2 Servers} *)

(** The idea is to create a server session [s] first. The functions
    [server_emit_flag] and [server_recv_flag] indicate now whether
    the server needs to emit a new message, or whether it needs to
    receive a message, respectively. Emission is done by [server_emit_message],
    reception by [server_recv_message]. If everything goes well, the
    protocol state advances, and finally [server_finish_flag] is true.
    This indicates that the client could be authenticated.

    If an error occurs, {b no} exception is raised, and the protocol
    advances nevertheless, and finally the server sends an error token
    to the client. After this, [server_error_flag] returns true.
 *)


val create_server_session : 
      profile -> (string -> string * string * int) -> server_session
  (** [create_server_session p auth]: Creates a new server session with
      profile [p] and authenticator function [auth].

      The function is [auth] is called when the credentials of the
      client have been received to check whether the client can be
      authenticated. It is called as

      {[
      let (salted_password, salt, iteration_count) = auth username
      ]}

      where [username] is the user name. The function can now raise
      [Not_found] if the user is unknown, or it can return the
      shown triple. Note that the cleartext password needs not to
      be known. [salt] is a random string, and [iteration_count] a
      security parameter that should be at least 4096. Whereas [salt]
      should be different for each user, the [iteration_count] can be
      chosen as a constant (e.g. 4096). Now [salted_password] can be
      computed from the cleartext password and these two extra parameters.
      See [salt_password] below.
   *)

val create_salt : unit -> string
  (** Creates a random string suited as salt *)

val salt_password : string -> string -> int -> string
  (** [let salted_password = salt_password password salt iteration_count]

      As we do not implement [SASLprep] only passwords consisting of
      US-ASCII characters are accepted ([Invalid_encoding] otherwise).
   *)

val server_emit_flag : server_session -> bool
  (** Whether [server_emit_message] can now be called *)

val server_recv_flag : server_session -> bool
  (** Whether [server_recv_message] can now be called *)

val server_finish_flag : server_session -> bool
  (** Whether the client is authenticated *)

val server_error_flag : server_session -> bool
  (** Whether an error occurred, and the protocol cannot advance anymore *)

val server_emit_message : server_session -> string
  (** Emits the next message to be sent to the client *)

val server_recv_message : server_session -> string -> unit
  (** Receives the next message from the client *)


module Debug : sig
  val enable : bool ref
    (** Enable debugging of this module *)
end
