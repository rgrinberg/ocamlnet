(** FCGI execution.  You could of course copy the code of add.ml here
    but it is not done in order to reuse counter.ml with other
    connectors. *)

open Netcgi

(* Errors of the function [main] (for example if the arguments "x" and
   "y" are not numbers) are handled by the connector.  They will
   rollback the output (i.e. any uncomitted material will be deleted),
   log the error, and generate an error page instead of the current
   page.  If you do not like this behavior, you can define your own
   error handling function and set it via the [exn_handler] optional
   argument of the connector.  *)
let () =
  let port = 1201 in
  Printf.printf "%s (FCGI) listening on port %i.\n%!" Sys.argv.(0) port;
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
   Netcgi_fcgi.run ~output_type:(`Transactional buffered)
    ~sockaddr:(Unix.ADDR_INET(Unix.inet_addr_any, port))
     (fun cgi -> Add.main(cgi :> cgi))
