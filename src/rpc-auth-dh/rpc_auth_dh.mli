(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


(* Diffie-Hellman authentication (AUTH_DH alias AUTH_DES):
 *
 * This module implements DH authentication, the simplest form of
 * Secure RPC. Despite its name, this form of authentication provides
 * only a medium level of security, see below.
 *
 * To use AUTH_DH you need the public-key infrastructure for Secure
 * RPC. This requires that a special daemon, the so-called keyserv,
 * runs on both the client's system and the server's system. The task
 * of keyserv is to store public and private keys.
 * (Note that keyserv is often distributed together with NIS+. However,
 * it can be used independently of NIS+.)
 *
 * In order to make a remote call, the keyserv daemon of the client must
 * store the private key of the client user, and the public key of the
 * server user. The keyserv daemon of the server must store the public
 * key of the client user and the private key of the server user.
 * Note that you can load a key pair into keyserv with the command keylogin.
 * (This is not necessary for the root user, root's key pair is loaded
 * at daemon startup time.)
 *
 * See the manual pages for keyserv, keylogin, keylogout, and /etc/publickey.
 *
 * Furthermore, it is strictly necessary that time synchronization is
 * possible between the client and the server. The recommended solution
 * is to synchronize both clocks independently with a time normal
 * (with NTP). Alternatively, the server can provide a time service on
 * port 37 ("netdate").
 *
 * To identify users, AUTH_DH uses so-called netnames. These have the form
 * "<osflavor>.<user>@<domain>", where <osflavor> determines the kind of
 * operating system (usually "unix"), <user> is an identifier for the user,
 * and <domain> determines where the user identifiers are valid. In UNIX
 * environments, the netnames are formed like:
 * - For root users: "unix.<hostname>@<nisdomain>"
 * - For non-privileged users: "unix.<uid>@<nisdomain>". Note that <uid>
 *   is the numeric user ID.
 *
 * The keyserv daemon has a service (net_get) that returns the netname of
 * the calling user. AUTH_DH uses this service to determine the netname
 * of the current process, but netnames are still visible:
 * - The client must know the netname of the server
 * - The server must interpret the netname of the client and decide whether
 *   the client user is authorized or not
 *
 * How secure is AUTH_DH? As pointed out, the security is not the best.
 * - The chosen prime for DH is too short
 * - It uses DES (56 bits) to encrypt the verifiers
 * The DES weakness can be avoided by changing the conversation key
 * frequently. This AUTH_DH implementation allows it to specify the maximum
 * lifetime of a DES key.
 *
 * Note that it is hard to attack AUTH_DH without knowing the public key.
 * So it is best not to make it accessible for third parties.
 *)

val domainname : unit -> string
  (* Returns the NIS domain name. The name is determined by calling the
   * external command "domainname".
   * Note: This function refuses to work for setuid or setgid programs.
   *)

val client_auth_method : ?ttl:int ->
                         ?getdeviation:(Unix.inet_addr -> float) ->
			 ?key_lifetime:int ->
			 ?keyserv:Rpc_key_service.connector ->
			 string ->
			   Rpc_client.auth_method
  (* Creates a new authentication method using AUTH_DH. The passed string
   * is the netname of the called server.
   *
   * Pass the resulting auth_method to Rpc_client.set_auth_methods to
   * configure AUTH_DH for a certain RPC client.
   *
   * ~ttl: The "time to live" for the network packets. Effectively, this
   *   number is the maximum time deviation the server will tolerate. It
   *   defaults to 60 seconds meaning that it is acceptable if the server
   *   gets the network packet 60/2 seconds before or after the time the packet
   *   is sent by the client.
   * ~getdeviation: This function is called when the time has to be
   *   resynchronized. The argument is the internet address of the server,
   *   and the expected result is the number of seconds the server is ahead
   *   to the client.
   *   By default, a function is used that connects to the netdate time
   *   service of the server, and compares the time of the client and the
   *   server.
   *   If the clocks can be assumed to always be synchronous, it is safe to
   *   pass (fun _ -> 0.0) as deviation function.
   * ~key_lifetime: After this number of seconds the DES key (conversation
   *   key) expires. Default: 3600
   * ~keyserv: The keyserv daemon to use. Defaults to the same default as
   *   Rpc_key_service.create.
   *)

val server_auth_method :
      ?max_sessions:int ->                       (* default: 1000 *)
      ?max_ttl:int ->                            (* default: 60 *)
      ?key_lifetime:int ->                       (* default: 3600 *)
      ?attack_detector:bool ->                   (* default: true *)
      ?keyserv:Rpc_key_service.connector ->      (* default: see create *)
      unit ->
	Rpc_server.auth_method
  (* Pass the result of this function to Rpc_server.set_auth_methods to
   * configure AUTH_DH for a certain server.
   *
   * ~max_sessions: The maximum number of authenticated connections the
   *   server can manage. If more clients connect, the lifetime of the
   *   conversation keys will decrease, but the server will still be
   *   functional.
   * ~max_ttl: The maximum number for the ttl value. The ttl value is
   *   passed by the client, but if it is bigger than ~max_ttl, the maximum
   *   is used instead.
   * ~key_lifetime: After this number of seconds the conversation key expires
   *   and must be renewed.
   * ~attack_detector: Whether an attack detector is to be installed. It
   *   detects if there are many failed connection attempts for a certain
   *   user (more than 10 failures in 10 seconds). If this criterion matches
   *   no more logins are allowed for this user in the current 10 seconds
   *   period.
   *   The detector contains a heuristics that makes it unlikely that
   *   a TCP connection breaks when a key must be renewed and the server
   *   is currently being attacked.
   * ~keyserv: The keyserv daemon to use. Defaults to the same default as
   *   Rpc_key_service.create.
   *
   * Note that the current implementation of AUTH_DH blocks until the
   * keyserv responds. For most applications, this is not a big problem,
   * as keyserv lookups are seldom. Perhaps I will rewrite the code some
   * day such that keyserv lookups are done in an asynchronous way. (The
   * Rpc_server.auth_method interface allows it already.)
   *)
