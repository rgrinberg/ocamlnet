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

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.4  2005/12/02 20:42:33  gremlin43820
 * fixed a bug in the handling of very large pages and fastcgi. netchannel expects to transfer the page all at once (which is fine), however fastcgi can only handle 64Kbytes of data at a time. We handle this by splitting things up into 64Kbyte chunks in the fcgi_out_channel. Thanks to Francois Rouaix for reporting this issue.
 *
 * Revision 1.3  2005/02/02 19:05:47  gremlin43820
 * added the exception to the interface file
 *
 * Revision 1.2  2004/07/24 18:30:49  stolpmann
 * 	ocamldoc
 *
 * Revision 1.1  2003/10/07 17:39:32  stolpmann
 * 	Imported Eric's patch for fastcgi
 *
 * 
 *)
