(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(***********************************************************************
 * This example demonstrates a very simple CGI page that contains
 * a counter that is incremented by the submit button.
 *
 * See add.ml for a slightly more complex example with more detailed
 * information.
 ***********************************************************************)

let cgi = new Netcgi.std_activation ()
let out = cgi # output # output_string
let n = cgi # argument_value ~default:"0" "Count"
;;

cgi # set_header ();
out "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\"";
out " \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n";
out "<HTML>\n";
out "<HEAD>\n";
out "<TITLE>Counter</TITLE>\n";
out "</HEAD>\n";
out "<BODY>\n";
out "<H1>Counter</H1>\n";
out "<FORM ACTION=\"counter.cgi\" METHOD=\"GET\"";
out " ENCTYPE=\"application/x-www-form-urlencoded\">\n";
out "<INPUT TYPE=\"SUBMIT\" NAME=\"Count\" VALUE=\"";
out (string_of_int (int_of_string n + 1));
out "\">\n";
out "</FORM>\n";
out "</BODY>\n";
out "</HTML>\n";

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.1  2001/10/19 05:32:42  pdoane
 * Moved to new example location
 *
 *)
