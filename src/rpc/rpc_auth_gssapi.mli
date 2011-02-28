(* $Id$ *)

(** GSS-API for RPC authentication *)

open Netgssapi

type user_name_format =
    [ `Exported_name
    | `Exported_name_text
    | `Exported_name_no_oid
    ]
  (** What to return as user name:
      - [`Exported_name]: the exported name in binary format (as described
        in RFC 2078, section 3.2)
      - [`Exported_name_text]: the exported name in a text format
        "{<oid>}<namestring>".
      - [`Exported_name_no_oid]: the string part of the exported name
   *)

val server_auth_method : 
      ?require_privacy:bool ->
      ?require_integrity:bool ->
      ?shared_context:bool ->
      ?acceptor_cred:credential ->
      ?user_name_format:user_name_format ->
      gss_api -> Rpc_server.auth_method
  (** Creates an authentication method from a GSS-API interface.

      Options:
      - [require_privacy]: Whether the messages must be
        encrypted
      - [require_integrity]: Whether integrity checksums must be
        included
      - [shared_context]: Whether this method maintains only one
        security context for all server endpoints. By default,
        each endpoint has a security context of its own.
      - [acceptor_cred]: Overrides the credentials of the server.
      - [user_name_format]: Defaults to [`Exported_name_text].
   *)

