(* Apache interface for mod_caml programs.
 * Copyright (C) 2003 Merjis Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *)

(* Forked from mod_caml on May 29, 2006. *)


(* Must appear in this order! *)
type result_type = OK | DECLINED | DONE

(* List of methods must match order in <httpd.h>. *)
type method_type = M_GET
                 | M_PUT
		 | M_POST
		 | M_DELETE
		 | M_CONNECT
		 | M_OPTIONS
		 | M_TRACE
		 | M_PATCH
		 | M_PROPFIND
		 | M_PROPPATCH
		 | M_MKCOL
		 | M_COPY
		 | M_MOVE
		 | M_LOCK
		 | M_UNLOCK
		 | M_INVALID

exception HttpError of int		(* Exceptions thrown by API functions*)

module Table = struct			(* Table functions. *)
  type t
  external clear : t -> unit			= "mod_caml_table_clear"
  external get : t -> string -> string		= "mod_caml_table_get"
  external get_all : t -> string -> string list	= "mod_caml_table_get_all"
  external fields : t -> (string * string) list	= "mod_caml_table_fields"
  external set : t -> string -> string -> unit	= "mod_caml_table_set"
  external add : t -> string -> string -> unit	= "mod_caml_table_add"
  external unset : t -> string -> unit		= "mod_caml_table_unset"
(*
  Non-copying version. Not a great idea to allow access to this.
  external setn : t -> string -> string -> unit	= "mod_caml_table_setn"
*)

(* ... etc ... *)
end

module Server = struct			(* Server_rec functions. *)
  type t				(* Actual server_rec structure. *)
  external hostname : t -> string 	= "mod_caml_server_hostname"
  external admin : t -> string		= "mod_caml_server_admin"
  external port : t -> int		= "mod_caml_server_port"
  external is_virtual : t -> bool	= "mod_caml_server_is_virtual"

      (* ... etc ... *)
end

module Connection =			(* Conn_rec functions. *)
struct
  type t				(* Actual conn_rec structure. *)

  external remote_ip : t -> string	= "mod_caml_connection_remote_ip"
  external auth_type : t -> string	= "mod_caml_connection_ap_auth_type"
  external remote_host : t -> string	= "mod_caml_connection_remote_host"
  external user : t -> string		= "mod_caml_connection_user"

  (* ... etc ... *)
end

module Request = struct			(* Request_rec functions. *)
  type t				(* Actual request_rec structure. *)

  external connection : t -> Connection.t = "mod_caml_request_connection"
  external server : t -> Server.t	= "mod_caml_request_server"
  external next : t -> t		= "mod_caml_request_next"
  external prev : t -> t		= "mod_caml_request_prev"
  external main : t -> t		= "mod_caml_request_main"
  external the_request : t -> string	= "mod_caml_request_the_request"
  external assbackwards : t -> bool	= "mod_caml_request_assbackwards"

  external header_only : t -> bool	= "mod_caml_request_header_only"
  external protocol : t -> string	= "mod_caml_request_protocol"
  external proto_num : t -> int		= "mod_caml_request_proto_num"
  external hostname : t -> string	= "mod_caml_request_hostname"
  external request_time : t -> float	= "mod_caml_request_request_time"
  external status_line : t -> string	= "mod_caml_request_status_line"
  external set_status_line : t -> string -> unit
    = "mod_caml_request_set_status_line"
  external status : t -> int		  = "mod_caml_request_status"
  external set_status : t -> int -> unit  = "mod_caml_request_set_status"
  external method_name : t -> string	  = "mod_caml_request_method"
  external method_number : t -> method_type = "mod_caml_request_method_number"

  external headers_in : t -> Table.t	  = "mod_caml_request_headers_in"
  external headers_out : t -> Table.t	  = "mod_caml_request_headers_out"
  external err_headers_out : t -> Table.t = "mod_caml_request_err_headers_out"
  external subprocess_env : t -> Table.t  = "mod_caml_request_subprocess_env"
  external notes : t -> Table.t		  = "mod_caml_request_notes"
  external content_type : t -> string	  = "mod_caml_request_content_type"
  external set_content_type : t -> string -> unit
    = "mod_caml_request_set_content_type"

  external user : t -> string		  = "mod_caml_request_user"

  external uri : t -> string		  = "mod_caml_request_uri"
  external set_uri : t -> string -> unit  = "mod_caml_request_set_uri"
  external filename : t -> string	  = "mod_caml_request_filename"
  external set_filename : t -> string -> unit = "mod_caml_request_set_filename"
  external path_info : t -> string	  = "mod_caml_request_path_info"
  external set_path_info : t -> string -> unit
    = "mod_caml_request_set_path_info"
  external args : t -> string		  = "mod_caml_request_args"
  external set_args : t -> string -> unit = "mod_caml_request_set_args"
  external finfo : t -> Unix.stats option = "mod_caml_request_finfo"

  external send_http_header : t -> unit = "mod_caml_request_send_http_header"

  external rflush : t -> int		= "mod_caml_request_rflush"

  type read_policy = REQUEST_NO_BODY
                   | REQUEST_CHUNKED_ERROR
		   | REQUEST_CHUNKED_DECHUNK
		   | REQUEST_CHUNKED_PASS

  external setup_client_block : t -> read_policy -> unit
      = "mod_caml_request_setup_client_block"
  external should_client_block : t -> bool
      = "mod_caml_request_should_client_block"
  external get_client_block : t -> string
      = "mod_caml_request_get_client_block"
  external get_client_block_buffer : t -> string -> int -> int -> int
    = "mod_caml_request_get_client_block_buffered"
  let get_client_block_buf r buf ofs len =
    if ofs < 0 || ofs + len > String.length buf then
      invalid_arg "Apache.Request.get_client_block_buf";
    get_client_block_buffer r buf ofs len
  external discard_request_body : t -> unit
      = "mod_caml_request_discard_request_body"

  external note_auth_failure : t -> unit
      = "mod_caml_request_note_auth_failure"
  external note_basic_auth_failure : t -> unit
      = "mod_caml_request_note_basic_auth_failure"
  external note_digest_auth_failure : t -> unit
      = "mod_caml_request_note_digest_auth_failure"
  external get_basic_auth_pw : t -> string option
      = "mod_caml_request_get_basic_auth_pw"

  external internal_redirect : string -> t -> unit
      = "mod_caml_request_internal_redirect"
  external internal_redirect_handler : string -> t -> unit
      = "mod_caml_request_internal_redirect_handler"

  external print_char : t -> char -> unit = "mod_caml_request_print_char"

  external unsafe_output : t -> string -> int -> int -> int
    = "mod_caml_request_output"
  let output r s ofs len =
    if ofs < 0 || len < 0 || ofs + len > String.length s then
      invalid_arg "Apache.Request.output";
    unsafe_output r s ofs len

  let print_string r s =
    let n = String.length s in
    let i = ref 0 in
    while !i < n do
      let w = unsafe_output r s !i (n - !i) in
      if w <= 0 then failwith "print_string: end of file or error";
      i := !i + w;
    done

  let print_int r i =     print_string r (string_of_int i)
  let print_float r f =   print_string r (string_of_float f)
  let print_newline r =   print_string r "\r\n"
  let print_endline r s = print_string r s; print_newline r

      (* ... etc ... *)

  external register_cleanup : t -> (unit -> unit) -> unit
      = "mod_caml_request_register_cleanup"
end

type handler_t = Request.t -> result_type

(*----------------------------------------------------------------------*)

let () = Callback.register_exception "mod_caml_http_error" (HttpError 0)

(* Unless we actually reference the external C functions, OCaml doesn't
 * load them into the primitive table and we won't be able to access them.
 * Duh!
 *)
let _table_get = Table.get
let _table_set = Table.set
let _table_add = Table.add
let _table_unset = Table.unset
let _server_hostname = Server.hostname
let _connection_remote_ip = Connection.remote_ip
let _request_connection = Request.connection
let _request_server = Request.server
let _request_next = Request.next
let _request_prev = Request.prev
let _request_main = Request.main
let _request_the_request = Request.the_request
let _request_assbackwards = Request.assbackwards
let _request_header_only = Request.header_only
let _request_protocol = Request.protocol
let _request_proto_num = Request.proto_num
let _request_hostname = Request.hostname
let _request_request_time = Request.request_time
let _request_status_line = Request.status_line
let _request_set_status_line = Request.set_status_line
let _request_status = Request.status
let _request_set_status = Request.set_status
let _request_method_name = Request.method_name
let _request_method_number = Request.method_number
let _request_headers_in = Request.headers_in
let _request_headers_out = Request.headers_out
let _request_err_headers_out = Request.err_headers_out
let _request_subprocess_env = Request.subprocess_env
let _request_notes = Request.notes
let _request_content_type = Request.content_type
let _request_set_content_type = Request.set_content_type
let _request_user = Request.user
let _request_uri = Request.uri
let _request_set_uri = Request.set_uri
let _request_filename = Request.filename
let _request_set_filename = Request.set_filename
let _request_path_info = Request.path_info
let _request_set_path_info = Request.set_path_info
let _request_args = Request.args
let _request_set_args = Request.set_args
let _request_finfo = Request.finfo
let _request_send_http_header = Request.send_http_header
let _request_setup_client_block = Request.setup_client_block
let _request_should_client_block = Request.should_client_block
let _request_get_client_block = Request.get_client_block
let _request_discard_request_body = Request.discard_request_body
let _request_note_auth_failure = Request.note_auth_failure
let _request_note_basic_auth_failure = Request.note_basic_auth_failure
let _request_note_digest_auth_failure = Request.note_digest_auth_failure
let _request_get_basic_auth_pw = Request.get_basic_auth_pw
let _request_internal_redirect = Request.internal_redirect
let _request_internal_redirect_handler = Request.internal_redirect_handler
let _request_register_cleanup = Request.register_cleanup
