(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* TODO:
 * - werden alle Deskriptoren wieder geschlossen?
 * - abort handler einfügen, Abort exceptions erzeugen
 * - beim Schliessen auch clear machen, um handler zu löschen
 * - Testen: Fehlerbehandlung im Server und Client
 * - Testen: Signals
 * - Testen: clear-Funktion
 * - Client: Timeouts behandeln
 * - Portmapper
 * - restl. Module
 * - XDR-Selbstreferenzen
 * - Rest Portmapper
 * - UNIX-Authentifizierung, ggf. Interface um weiter Auths einzuhängen
 *)

open Rtypes

type protocol =
    Tcp          (* means: stream-oriented connection *)
  | Udp;;        (* means: datagram exchange *)

type mode =
    Socket     (* classical server socket *)
  | BiPipe     (* server is endpoint of a bidirectional pipe *)



(* these are error conditions sent to the client: *)

type server_error =
    Unavailable_program                      (* accepted call! *)
  | Unavailable_version of (uint4 * uint4)   (* accepted call  *)
  | Unavailable_procedure                    (* accepted call  *)
  | Garbage                                  (* accepted call  *)
  | System_err
  | Rpc_mismatch of (uint4 * uint4)          (* rejected call  *)
  | Auth_bad_cred                            (* rejected call  *)
  | Auth_rejected_cred                       (* rejected call  *)
  | Auth_bad_verf                            (* rejected call  *)
  | Auth_rejected_verf                       (* rejected call  *)
  | Auth_too_weak                            (* rejected call  *)
  | Auth_invalid_resp                        (* rejected call  *)
  | Auth_failed                              (* rejected call  *)
;;


exception Rpc_server of server_error;;

exception Rpc_cannot_unpack of string;;

