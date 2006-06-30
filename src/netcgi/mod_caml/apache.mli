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

(* Based on the mod_caml version but simplified and modified to match
   the OCamlNet conventions. *)

type result_type = OK | DECLINED | DONE

(** Types of request methods. See [Request.method] and
    [Request.method_number].  [M_HEAD] = [M_GET].  *)
type method_type = | M_GET
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

exception HttpError of int
  (** Most Apache API functions which, in C, might return an integer
      indicating an error, have been changed to throw this exception
      which contains the error number or status number.

      It is turned into an INTERNAL SERVER ERROR in
      mod_caml_c.c:exception_in_handler *)

module Table :
sig
  type t (** Apache [table] structure. *)

  external get : t -> string -> string          = "mod_caml_table_get"
      (** [Table.get tbl key] returns the corresponding entry in the
          table.  The [key] is matched case-insensitively.  The first
          key will retrieved in case there are more than one entry in
          the table for the key.
	  @raise Not_found otherwise. *)
  external get_all : t -> string -> string list = "mod_caml_table_get_all"
      (** [Table.get_all tbl key] same as [Table.get tbl key] except
          it returns all values corresponding to the [key]. *)
  external fields : t -> (string * string) list = "mod_caml_table_fields"
      (** [Table.fields tbl] returns a list of [(key, value)] pairs
          consisting of all entries of the table [tbl]. *)

  external clear : t -> unit                    = "mod_caml_table_clear"
      (** [Table.clear tbl] removes all key/value pairs from the table
          [tbl]. *)
  external set : t -> string -> string -> unit  = "mod_caml_table_set"
      (** [Table.set tbl key value] sets the [(key, value)] pair in
          the table [tbl].  If one or more key/value pairs already
          exists with the same key they will be deleted and replaced
          with [(key, value)].  *)
  external add : t -> string -> string -> unit  = "mod_caml_table_add"
      (** [Table.add tbl key value] adds the [(key, value)] pair in
          the table [tbl].  This does not erase any existing pairs. *)
  external unset : t -> string -> unit          = "mod_caml_table_unset"
      (** [Table.unset tbl key] delete every key/value pair associated
          with the [key] from the table [tbl]. *)
end

module Server :
sig
  type t (** Apache [server_rec] structure. *)

  external hostname : t -> string 	= "mod_caml_server_hostname"
      (** [server_rec] [hostname] field (as declared in Apache
	  configuration file).
	  @raise Not_found if NULL. *)
  external admin : t -> string		= "mod_caml_server_admin"
  external port : t -> int		= "mod_caml_server_port"
  external is_virtual : t -> bool	= "mod_caml_server_is_virtual"
end

module Connection :
sig
  type t (** Apache [conn_rec] structure. *)

  external remote_ip : t -> string 	= "mod_caml_connection_remote_ip"
      (** [conn_rec] [remote_ip] field. Throws [Not_found] if NULL. *)
  external auth_type : t -> string	= "mod_caml_connection_ap_auth_type"
  external remote_host : t -> string	= "mod_caml_connection_remote_host"
  external user : t -> string		= "mod_caml_connection_user"
end

module Request :
sig
  type t (** Apache [request_rec] structure. *)

  external connection : t -> Connection.t = "mod_caml_request_connection"
      (** [request_rec] [connection] field. *)
  external server : t -> Server.t 	= "mod_caml_request_server"
      (** [request_rec] [server] field. *)
  external next : t -> t 		= "mod_caml_request_next"
      (** [request_rec] [next] field. Raises [Not_found] if NULL. *)
  external prev : t -> t 		= "mod_caml_request_prev"
      (** [request_rec] [prev] field. Raises [Not_found] if NULL. *)
  external main : t -> t 		= "mod_caml_request_main"
      (** [request_rec] [main] field. Raises [Not_found] if NULL. *)
  external the_request : t -> string 	= "mod_caml_request_the_request"
      (** [request_rec] [the_request] field. Throws [Not_found] if NULL. *)
  external assbackwards : t -> bool	= "mod_caml_request_assbackwards"
      (** [request_rec] [assbackwards] field; [true] if HTTP/0.9,
	  "simple" request. *)

  external header_only : t -> bool 	= "mod_caml_request_header_only"
      (** [request_rec] [header_only] field.  It is [true] when the
	  request method is HEAD. *)
  external protocol : t -> string 	= "mod_caml_request_protocol"
      (** [request_rec] [protocol] field.
	  @raise Not_found if NULL. *)
  external proto_num : t -> int 	= "mod_caml_request_proto_num"
      (** [request_rec] [proto_num] field.  Number version of
	  protocol; 1.1 = 1001 *)
  external hostname : t -> string 	= "mod_caml_request_hostname"
      (** [request_rec] [hostname] field -- hostname to which the
	  request was made.
	  @raise Not_found if NULL. *)
  external request_time : t -> float 	= "mod_caml_request_request_time"
      (** [request_rec] [request_time] field. *)
  external status_line : t -> string 	= "mod_caml_request_status_line"
      (** [request_rec] [status_line] field.
	  @raise Not_found if NULL. *)
  external set_status_line : t -> string -> unit
    					= "mod_caml_request_set_status_line"
      (** Set [request_rec] [status_line] field. *)
  external status : t -> int 		= "mod_caml_request_status"
      (** [request_rec] [status] field. *)
  external set_status : t -> int -> unit = "mod_caml_request_set_status"
      (** Set [request_rec] [status] field. *)

  external method_name : t -> string 	= "mod_caml_request_method"
      (** [request_rec] [method] field. *)
  external method_number : t -> method_type = "mod_caml_request_method_number"
      (** [request_rec] [method_number] field. *)

  external headers_in : t -> Table.t = "mod_caml_request_headers_in"
      (** [request_rec] [headers_in] field. *)
  external headers_out : t -> Table.t = "mod_caml_request_headers_out"
      (** [request_rec] [headers_out] field. *)
  external err_headers_out : t -> Table.t
    = "mod_caml_request_err_headers_out"
      (** [request_rec] [err_headers_out] field. *)
  external subprocess_env : t -> Table.t = "mod_caml_request_subprocess_env"
      (** [request_rec] [subprocess_env] field. *)
  external notes : t -> Table.t = "mod_caml_request_notes"
      (** [request_rec] [notes] field. *)
  external content_type : t -> string
    = "mod_caml_request_content_type"
      (** [request_rec] [content_type] field. Throws [Not_found] if NULL. *)
  external set_content_type : t -> string -> unit
    = "mod_caml_request_set_content_type"
      (** Set [request_rec] [content_type] field. *)

  external user : t -> string = "mod_caml_request_user"
      (** The authenticated user. In Apache 1.3 this field is actually
          in the [conn_rec] structure, and was moved here in Apache
          2.0.  We transparently hide this detail for you.  Throws
          [Not_found] if NULL. *)

  external uri : t -> string = "mod_caml_request_uri"
      (** [request_rec] [uri] field. Throws [Not_found] if NULL. *)
  external set_uri : t -> string -> unit = "mod_caml_request_set_uri"
      (** Set [request_rec] [uri] field. *)
  external filename : t -> string = "mod_caml_request_filename"
      (** [request_rec] [filename] field. Throws [Not_found] if NULL. *)
  external set_filename : t -> string -> unit
    = "mod_caml_request_set_filename"
      (** Set [request_rec] [filename] field. *)
  external path_info : t -> string = "mod_caml_request_path_info"
      (** [request_rec] [path_info] field. Throws [Not_found] if NULL. *)
  external set_path_info : t -> string -> unit
    = "mod_caml_request_set_path_info"
      (** Set [request_rec] [path_info] field. *)
  external args : t -> string = "mod_caml_request_args"
      (** [request_rec] [args] field. Throws [Not_found] if NULL. *)
  external set_args : t -> string -> unit
    = "mod_caml_request_set_args"
      (** Set [request_rec] [args] field. *)
  external finfo : t -> Unix.stats option = "mod_caml_request_finfo"
      (** [request_rec] [finfo] field. *)


  (** Policy to apply by [setup_client_block] if the request message
      indicates a body. *)
  type read_policy =
    | REQUEST_NO_BODY		(** Send 413 error if message has any body *)
    | REQUEST_CHUNKED_ERROR
			(** Send 411 error if body without Content-Length *)
    | REQUEST_CHUNKED_DECHUNK	(** If chunked, remove the chunks for me. *)
    | REQUEST_CHUNKED_PASS	(** Pass the chunks to me without removal. *)

  external setup_client_block : t -> read_policy -> unit
    = "mod_caml_request_setup_client_block"
      (** Setup for reading client request.
	  @raise HttpError in case of problems.  *)
  external should_client_block : t -> bool
    = "mod_caml_request_should_client_block"
      (** Returns true if there is any client request data. *)
  external get_client_block : t -> string
    = "mod_caml_request_get_client_block"
      (** Get client request data. *)
  val get_client_block_buf : t -> string -> int -> int -> int
    (** [get_client_block_buf r buf ofs len] read a chunk of data and
	puts it in [buf.[ofs .. ofs+len-1]].  The return value [i] is
	the number of bytes actually read -- i.e. only [buf.[ofs
	.. ofs+i-1]] is meaningful.
	@raise HttpError in case of reading error. *)
  external discard_request_body : t -> unit
    = "mod_caml_request_discard_request_body"
      (** Discard client request body. *)

  external note_auth_failure : t -> unit
    = "mod_caml_request_note_auth_failure"
      (** Set headers to tell browser that authentication failed. *)
  external note_basic_auth_failure : t -> unit
    = "mod_caml_request_note_basic_auth_failure"
      (** Set headers to tell browser that basic authentication failed. *)
  external note_digest_auth_failure : t -> unit
    = "mod_caml_request_note_digest_auth_failure"
      (** Set headers to tell browser that digest authentication failed. *)
  external get_basic_auth_pw : t -> string option
    = "mod_caml_request_get_basic_auth_pw"
      (** Get the password sent in basic authentication. *)
  external internal_redirect : string -> t -> unit
    = "mod_caml_request_internal_redirect"
      (** Internally redirects immediately to [uri]. *)
  external internal_redirect_handler : string -> t -> unit
    = "mod_caml_request_internal_redirect_handler"
      (** Internally redirects immediately to [uri] using handler specified
          by [r]. *)

  external send_http_header : t -> unit
    = "mod_caml_request_send_http_header"
      (** Send the HTTP headers.  Note that you must set the Status
	  and Content-Type with [set_status] and [set_content_type]
	  respectively. *)

  external rflush : t -> int
    = "mod_caml_request_rflush"
    (** Flush any buffered data waiting to be written to the client.
        Returns [0] on success and [-1] for EOF.  *)

  external print_char : t -> char -> unit = "mod_caml_request_print_char"
    (** Send a character back to the client.  *)
  val print_string : t -> string -> unit
    (** Send a string back to the client. *)
  val output : t -> string -> int -> int -> int
    (** [output r s ofs len] send [s[ofs .. len-1]] back to the
	client.  Returns the number of bytes actually written, which is
	smaller than the number of bytes in the string if there was a
	failure. *)

  val print_int : t -> int -> unit
    (** Send a decimal number back to the client. *)
  val print_float : t -> float -> unit
    (** Send a floating-point number back to the client. *)
  val print_newline : t -> unit
    (** Send a CR LF back to the client. *)
  val print_endline : t -> string -> unit
    (** Send a string followed by CR LF back to the client. *)
  external register_cleanup : t -> (unit -> unit) -> unit
    = "mod_caml_request_register_cleanup"
    (** Register a cleanup function which is called when the current
        request cycle ends.  *)
end

type handler_t = Request.t -> result_type
  (** The type of handler functions. *)
