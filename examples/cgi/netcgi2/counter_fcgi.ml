(** CGI execution.  You could of course copy the code of counter.ml
    here but it is not done in order to reuse counter.ml with other
    connectors. *)

open Netcgi

let () =
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_fcgi.run ~output_type:(`Transactional buffered)
    (fun cgi -> Counter.main(cgi :> cgi))
