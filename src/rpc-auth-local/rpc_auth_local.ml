(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

external get_peer_credentials : Unix.file_descr -> (int * int)
  = "unix_get_peer_credentials"
;;

external peek_peer_credentials : Unix.file_descr -> (int * int)
  = "unix_peek_peer_credentials"
;;


class server_auth_method : Rpc_server.auth_method =
object
  method name = "AUTH_LOCAL"
  method flavors = [ ]
  method peek =
    `Peek_descriptor
      (fun d ->
	 match Unix.getpeername d with
	     Unix.ADDR_UNIX _ ->
	       (* Try now peek_peer_credentials: *)
	       begin try
		 let uid, gid = peek_peer_credentials d in
		 let username =
		   string_of_int uid ^ "." ^ string_of_int gid ^ "@localhost" in
		 Some username
	       with
		   Invalid_argument _ ->
		     (* peek_peer_credentials is not available for this OS *)
		     None
		 | Not_found ->
		     (* Some other failure *)
		     None
		 | Unix.Unix_error(Unix.EAGAIN,_,_) ->
		     (* peek_peer_credentials expects that there is a message
                      * to read. EAGAIN is raised if we call it in the wrong
                      * moment.
                      *)
		     None
	       end
	   | _ ->
	       None
      )

  method authenticate _ _ _ _ _ _ _ _ _ = ()

end



let server_auth_method() = new server_auth_method
