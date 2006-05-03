(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


let exec s =
  let l = Lexing.from_string s in
  let ph = !Toploop.parse_toplevel_phrase l in
  assert(Toploop.execute_phrase false Format.err_formatter ph)
;;

(* Install the printers: *)

exec "#install_printer Mimestring.print_s_param;;";;
exec "#install_printer Neturl.print_url;;";;
exec "#install_printer Netbuffer.print_buffer;;";;
exec "#install_printer Netstream.print_in_obj_stream;;";;
(* exec "#install_printer Cgi.print_argument;;";; *)

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 2.4  2004/06/30 22:27:42  stolpmann
 * 	Update: Cgi is optional, -enable-compatcgi still possible.
 *
 * Revision 2.3  2004/05/30 21:27:14  stolpmann
 * 	Supporting the standard for class-based I/O. This also means
 * the semantics of [input] and [output] methods have changed (return
 * value 0, End_of_file).
 * 	Removed _rep_in, _rep_out.
 * 	Removed Netstream.Deprecated.
 *
 * Revision 2.2  2002/01/12 18:35:46  stolpmann
 * 	Updated the toploop printers.
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.2  2000/06/25 22:34:43  gerd
 * 	Added labels to arguments.
 *
 * Revision 1.1  2000/06/24 20:20:58  gerd
 * 	Initial revision.
 *
 * 
 *)
