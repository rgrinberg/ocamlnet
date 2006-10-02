(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(** Low-level FastCGI Wire Protocol *)

(** This code is copyright 2003 Eric Stokes, and may be used under
    either, the GNU GPL, or the same license as ocamlnet *)

(** This module impliments the low level fastcgi wire protocal. It exports
    simple functions to interact with the web server much as a standard cgi 
    script would (eg. fastcgi_write_stdout, fastcgi_write_stderr), this is 
    one of the design goals of the fastcgi protocal. This module is marginally
    usefull for making low level web apps by itself, though you should not 
    mix calls to fcgi_write_* with transactional buffered output, unless 
    you really like your, your helpdesk operators :P *)

exception FCGI_error of string * exn

(** The maximum record size for stdout, and stderr. If you try to send
    a record larger than this value to fcgi_write_stdout, or
    fcgi_write_stderr you will get an exception *)
val max_rec_size: int

(** protocal header *)
type fcgiHeader = {version: int; rtype: int; requestid: int; contentlen: int; padlen: int}

(** begin request *)
type fcgiBeginRequestBody = {role: int; flags: int}

(** end request *)
type fcgiEndRequestBody = {astatus: int; pstatus: int}

(** fcgi params will return an asociation list fcgi stdin will return
    a string a full request record, what you get when you call
    fcgi_accept *)
type fcgiRequest = {id: int; 
		    app_type: int;
		    params: (string * string) list;  (* environment variables *)
		    stdin: string; (* used by all *)
		    data: string; (* used only by filter *)
		    con: Unix.file_descr}


(** accept a fastcgi connection *)
val fcgi_accept: unit -> fcgiRequest

(** tear down a connection after finishing with it *)
val fcgi_destroy: fcgiRequest -> unit

(** write to fcgi stdout *)
val fcgi_write_stdout: fcgiRequest -> string -> unit

(** write to fcgi stderr *)
val fcgi_write_stderr: fcgiRequest -> string -> unit

(** write an end request record *)
val fcgi_write_end_request: fcgiRequest -> fcgiEndRequestBody -> unit
