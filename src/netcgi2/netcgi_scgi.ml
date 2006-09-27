(* netcgi_scgi.ml

   Copyright (C) 2005-2006

     Christophe Troestler
     email: Christophe.Troestler@umh.ac.be
     WWW: http://math.umh.ac.be/an/

   This library is free software; see the file LICENSE for more information.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the file
   LICENSE for more details.
*)

(* The protocol is described at http://python.ca/nas/scgi/protocol.txt *)

open Netcgi_common
open Printf


(************************************************************************)

let scgi_log_error msg =
  let zone = Netdate.localzone (* log local time *) in
  let date = Netdate.format "%c" (Netdate.create ~zone (Unix.gettimeofday())) in
  prerr_endline ("[" ^ date ^ "] [Netcgi_scgi] " ^ msg)


(* [input_props_inheader in_obj] reads the netstring
   [len]":"[string]"," from the input object [in_obj] and chunk it
   into the key-value pairs representing the properties.  *)
let rec input_props_inheader_loop in_obj len props_inheader =
  if len = 0 then begin
    (* The netstring must finish with a comma *)
    if in_obj#input_char() <> ',' then
      raise(HTTP(`Bad_request, "Netcgi_scgi: The header must end with ','"));
    (* Add some compulsory CGI properties *)
    let (props, inheader) = props_inheader in
    (("GATEWAY_INTERFACE", "CGI/1.1") :: props, inheader)
  end else begin
    let name = in_obj#input_all_till '\000' in
    let value = in_obj#input_all_till '\000' in
    let len = len - String.length name - String.length value - 2 (* \000 *) in
    let props_inheader = update_props_inheader (name, value) props_inheader in
    input_props_inheader_loop in_obj len props_inheader
  end

let input_props_inheader in_obj =
  (* length of the "netstring": *)
  let len =
    try int_of_string(in_obj#input_all_till ':')
    with End_of_file | Failure _ ->
      let msg = "Netcgi_scgi: Incorrect length of netstring header" in
      raise(HTTP(`Bad_request, msg)) in
  input_props_inheader_loop in_obj len ([],[])


class scgi_env ?log_error ~config ~properties ~input_header out_obj
  : Netcgi_common.cgi_environment =
object
  inherit cgi_environment ~config ~properties ~input_header out_obj

  (* Override to use the correct channel *)
  method log_error msg = 
    match log_error with
      | None -> scgi_log_error msg
      | Some f -> f msg
end


let handle_request config output_type arg_store exn_handler f ~log fd =
  let in_obj = new in_obj_of_descr ~buffer:(String.create 8192) fd in
  let (properties, input_header) = input_props_inheader in_obj in

  let out_obj = new out_obj_of_descr ~buffer:(String.create 8192) fd in
  let env = new scgi_env ?log_error:log ~config ~properties ~input_header
    (out_obj :> Netchannels.out_obj_channel) in

  (* Now that one knows the environment, one can warn about exceptions *)
  try
    exn_handler_default env ~exn_handler
      (fun () ->
	 let cgi = cgi_with_args (new cgi) env output_type
           (in_obj :> Netchannels.in_obj_channel) arg_store in
	 (try
            f (cgi:Netcgi.cgi);
            cgi#out_channel#commit_work();
            cgi#finalize()
          with e when config.default_exn_handler ->
            cgi#finalize(); raise e);
	 None (* no "special" internal exception *)
      )
      ~finally:(fun () ->
                  (try env#out_channel#close_out() with _ -> ());
                  (* => flush buffer; it is the user responsability to
                   commit his work. *)
               );
    `Conn_close_linger
  with
    | error ->
	`Conn_error error



(* [handle_connection fd .. f] handle an accept()ed connection,
   reading incoming records on the file descriptor [fd] and running
   [f] for each incoming request. *)
let rec handle_connection fd ~config output_type arg_store exn_handler f =
  let log =
    Some scgi_log_error in
  let cdir =
    handle_request config output_type arg_store exn_handler f ~log fd in
  match cdir with
    | `Conn_close_linger ->
	Unix.setsockopt_optint fd Unix.SO_LINGER (Some 15);
        Unix.shutdown fd Unix.SHUTDOWN_ALL;
        Unix.close fd
    | `Conn_error e ->
	Unix.close fd;
	raise e
    (* Othe cdirs not possible *)

let run
    ?(config=Netcgi.default_config)
    ?(allow=fun _ -> true)
    ?(output_type=(`Direct "":Netcgi.output_type))
    ?(arg_store=(fun _ _ _ -> `Automatic))
    ?(exn_handler=(fun _ f -> f()))
    ~port (* no default in the spec *)
    f =
  (* Socket to listen to *)
  let sockaddr = Unix.ADDR_INET(Unix.inet_addr_any, port) in
  let sock =
    Unix.socket (Unix.domain_of_sockaddr sockaddr) Unix.SOCK_STREAM 0 in
  Unix.setsockopt sock Unix.SO_REUSEADDR true;
  Unix.bind sock sockaddr;
  Unix.listen sock 5;
  while true do
    let (fd, server) = Unix.accept sock in
    try
      if allow server then
	handle_connection fd ~config output_type arg_store exn_handler f;
      Unix.close fd
    with
	(* CHECK: Exceptions are impossible here... *)
    | Unix.Unix_error(Unix.EPIPE, _, _) ->
	(* This error is perfectly normal: the sever or ourselves may
	   have closed the connection (Netcgi_common ignores SIGPIPE).
	   Just wait for the next connection. *)
	(try Unix.close fd with _ -> ())
    | e when config.default_exn_handler ->
	(* Log the error and wait for the next conection. *)
	(try
	   scgi_log_error(Printexc.to_string e);
	   Unix.close fd
	 with _ -> ())
  done
