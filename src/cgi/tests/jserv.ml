(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Netcgi_jserv
open Netcgi_jserv_ajp12
open Netcgi

(* ======================================================================
 * SAMPLE CONFIGURATION
 * ======================================================================
 *
 * Add to httpd.conf: (adjust paths to your system)
 *
 * LoadModule jserv_module       /usr/lib/apache/mod_jserv.so
 * AddModule mod_jserv.c
 * <IfModule mod_jserv.c>
 * ApJServManual off
 * ApJServProperties /etc/httpd/jserv.properties
 * ApJServLogFile /var/log/httpd.jserv_log
 * ApJServLogLevel info
 * ApJServDefaultProtocol ajpv12
 * ApJServDefaultHost localhost
 * ApJServDefaultPort 8007
 * ApJServSecretKey /path/to/secret/file/containing/anything
 * ApJServMount /servlets /root
 * APJServMountCopy on
 * </IfModule>
 *
 * jserv.properties:
 *
 * wrapper.bin=/path/to/jserv
 * bindaddress=localhost
 * port=8007
 * security.authentication=true
 * security.secretKey=/path/to/secret/file/containing/anything
 * security.challengeSize=5
 *
 * Note that the wrapper.bin property configures that httpd starts this
 * program on demand. Alternatively, you can set ApJServManual to On
 * and start this program yourself.
 *)


let text = Netencoding.Html.encode_from_latin1;;

let onconnect srv =
  serve_connection 
    (fun zone servlet env ->
       let cgi = new std_activation ~env () in
       prerr_endline "CGI activation!";
       ( match zone with
	     None -> ()
	   | Some s -> prerr_endline("Zone: " ^ s);
       );
       ( match servlet with
	     None -> ()
	   | Some s -> prerr_endline("Servlet: " ^ s);
       );
       cgi # set_header();
       let out = cgi # output # output_string in
       out "<H1>Hello world!</H1>\n";
       out "<H2>Properties</H2>\n";
       out "<UL>\n";
       List.iter
	 (fun name ->
	    try
	      let v = env # cgi_property name in
	      out "<LI>";
	      out (text name ^ "=" ^ v ^ "\n")
	    with
		Not_found -> ()
	 )
	 [ (* Standard: *)
	   "GATEWAY_INTERFACE";
	   "SERVER_SOFTWARE";
	   "SERVER_NAME";
	   "SERVER_PROTOCOL";
	   "SERVER_PORT";
	   "REQUEST_METHOD";
	   "PATH_INFO";
	   "PATH_TRANSLATED";
	   "SCRIPT_NAME";
	   "QUERY_STRING";
	   "REMOTE_HOST";
	   "REMOTE_ADDR";
	   "AUTH_TYPE";
	   "REMOTE_USER";
	   (* Apache: *)
	   "DOCUMENT_ROOT";
	   "REQUEST_URI";
	   "SCRIPT_FILENAME";
	   "JSERV_ZONE";
	   "JSERV_SERVLET";
	   "JSERV_ROUTE";
	   "HOSTNAME";
	 ];
       out "</UL>\n";

       out "<H2>Header</H2>\n";
       out "<UL>\n";
       List.iter
	 (fun (name,v) ->
	    out "<LI>";
	    out (text name ^ "=" ^ v ^ "\n")
	 )
	 (env # input_header_fields);
       out "</UL>\n";

       out "<H2>CGI parameters</H2>\n";
       out "<UL>\n";
       List.iter
	 (fun (name,arg) ->
	    out "<LI>";
	    let mimetype = arg # content_type in
	    out (text name ^ ":" ^ text mimetype ^ "=");
	    ( match arg # filename with
		  None -> ()
		| Some fn -> out ("[filename=" ^ text fn ^ "] ")
	    );
	    out (text arg#value);
	 )
	 (cgi#arguments);

       let action = text (cgi # url()) in
       
       out "</UL>\n";
       out "<h2>GET URL-encoded form</h2>\n";
       out ("<form action=\"" ^ action ^ "\" method=GET>\n");
       out "<input type=text name=line>\n";
       out "<input type=submit name=submit value=\"Submit\">\n";
       out "</form>\n";
       
       out "<h2>POST URL-encoded form</h2>\n";
       out ("<form action=\"" ^ action ^ "\" method=POST>\n");
       out "<input type=text name=line>\n";
       out "<input type=submit name=submit value=\"Submit\">\n";
       out "</form>\n";
       
       out "<h2>POST FORM-encoded form</h2>\n";
       out ("<form action=\"" ^ action ^ "\" method=POST enctype=\"multipart/form-data\">\n");
       out "<input type=text name=line>\n";
       out "<input type=submit name=submit value=\"Submit\">\n";
       out "</form>\n";
       
       out "<h2>File upload</h2>\n";
       out ("<form action=\"" ^ action ^ "\" method=POST enctype=\"multipart/form-data\">\n");
       out "<input type=text name=line>\n";
       out "<input type=file name=file>\n";
       out "<input type=submit name=submit value=\"Submit\">\n";
       out "</form>\n";

       cgi # output # commit_work();
    )
;;


let onrestart srv =
  prerr_endline "Restart"
;;


let onshutdown srv =
  prerr_endline "Shutdown"
;;


prng_init_from_file ~length:8 "/dev/random";;

jvm_emu_main
  (fun props auth addr port ->
     server 
       ~onrestart 
       ~onshutdown
       onconnect auth addr port);;


(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.1  2001/12/09 01:28:39  stolpmann
 * 	Initial revision.
 *
 * 
 *)
