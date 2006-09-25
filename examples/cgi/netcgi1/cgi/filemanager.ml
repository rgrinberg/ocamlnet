(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


(**********************************************************************
 * This is an advanced example: A simple filemanager. Every user
 * has a flat directory where he/she can upload files, download
 * them, delete them (... and perhaps some more actions ...)
 *
 * User authentication can be done by the web server, or by this
 * CGI itself.
 *
 * This program demonstrates the following techniques:
 * - How to design a CGI with many pages
 * - How to use the POST method
 * - How to upload and download files
 * - How to set and read cookies
 * - How to find out which user has been logged in
 * **********************************************************************)


(**********************************************************************
 * CONFIGURATION                                 READ THIS CAREFULLY!!!
 *
 * You MUST change the following variables to your local conventions,
 * and create the appropriate directories etc.
 ***********************************************************************)

let file_store =
  "/var/spool/webfilemanager" ;;

  (* This directory will contain one subdirectory per user, and stores
   * the files.
   *
   * File permissions must be:
   * - file_store must be owned by the user as which the web server runs.
   * - file_store must be rwx for the web server user.
   *   You may add rx for other users if you want.
   *
   * file_store must not reside below the document root of the web server.
   *
   * There must be a directory ".tmp" in file_store (with the same permissions).
   *
   * For every user there must be a subdirectory with the same permissions.
   * By creating the subdirectory, the user will begin to exist for the
   * file manager.
   *
   * EXAMPLE:
   * mkdir /var/spool/webfilemanager
   * mkdir /var/spool/webfilemanager/.tmp
   * mkdir /var/spool/webfilemanager/tim
   * mkdir /var/spool/webfilemanager/tina
   * chown -R www:other /var/spool/webfilemanager
   * chmod -R 755 /var/spool/webfilemanager
   *)

let password_file =
  Some (Filename.concat file_store "passwd") ;;

  (* If you set [password_file] to [Some filename], the filemanager will
   * display its own login page. If set to [None], there will be no
   * login page, and it is required that the web server is configured
   * to do the password dialog.
   *
   * The format of the file is "user:password". 
   *
   * EXAMPLE:
   * touch /var/spool/webfilemanager/passwd
   * chown www:other /var/spool/webfilemanager/passwd
   * chmod 400 /var/spool/webfilemanager/passwd
   * Contents of passwd:
   * tim:Tim's password
   * tina:foo
   *)

let session_timeout = 
  3600.0 ;;
  
  (* After this number of seconds idle users are logged out. (This is the
   * lifetime of the cookie.)
   * This works only if the filemanager uses its own password dialog.
   *)

let secret_key_file =
  Filename.concat file_store "secretkey" ;;
  
  (* This file will contain the keys used to sign cookies. You need not
   * to create the file, this is done automatically.
   *)

let secret_key_timeout =
  0.1 *. session_timeout ;;

  (* After this number of seconds a new secret key is generated. There is
   * no need to change this value.
   *)


(* GENERAL NOTES ABOUT SECURITY:
 *
 * If HTTP is used as transport protocol, passwords are transmitted in an
 * insecure way. That the cookies are signed does not change this weakness.
 * 
 * If HTTPS is used as transport protocol, the application is very
 * secure. Every piece of information is sent in an encrypted way.
 * Furthermore, the signed cookies do make sense, because stolen cookies
 * are hard to abuse. You can login by such a cookie only if you have
 * the same IP address. Furthermore, this must happen before the cookie
 * expires, and this date is contained in the cookie (the browser is not
 * trusted). The password cannot be extracted from a cookie.
 *)

(**********************************************************************)

open Netcgi;;
open Netcgi_types;;
open Netcgi_env;;
open Netchannels;;
open Printf;;

exception User_error of string;;

(**********************************************************************
 * CRYPTOGRAPHICALLY SECURE PSEUDO RANDOM NUMBER GENERATOR 
 **********************************************************************)

(* Bases on the RC4 cipher *)

(* Usage: Call [prng_init] once with the initial key, and call [random_8bits]
 * as often as necessary 
 *)

let prng_s = Array.make 256 0;;     (* S-box *)
let prng_i = ref 0;;
let prng_j = ref 0;;

let random_8bits() =
  prng_i := (!prng_i + 1) land 0xff;
  prng_j := (!prng_j + prng_s.( !prng_i )) land 0xff;
  (* Swap prng_s.(!prng_i) and prng_s(!prng_j): *)
  let h = prng_s.( !prng_i ) in
  prng_s.( !prng_i ) <- prng_s.( !prng_j );
  prng_s.( !prng_j ) <- h;
  prng_s.( (prng_s.( !prng_i ) + prng_s.( !prng_j )) land 0xff )
;;


let prng_init key =
  assert(key <> "");
  let k = Array.make 256 0 in
  for p = 0 to 255 do prng_s.(p) <- p done;
  let q = ref 0 in
  for p = 0 to 255 do
    k.(p) <- Char.code key.[ !q ];
    incr q;
    if (!q >= String.length key) then q := 0
  done;
  prng_j := 0;
  for p = 0 to 255 do
    prng_j := ( !prng_j + prng_s.(p) + k.(p) ) land 0xff;
    (* Swap s.(p) and s(!j): *)
    let h = prng_s.(p) in
    prng_s.( p ) <- prng_s.( !prng_j );
    prng_s.( !prng_j ) <- h;
  done;
  prng_i := 0;
  prng_j := 0;
  ()
;;


(**********************************************************************
 * USER/PASSWORD MANAGEMENT
 *
 * The following functions help to find out which user is logged in,
 * whether the user exists, and which is the workspace of the user.
 ***********************************************************************)

let colon_re = Pcre.regexp ":" ;;

let split s =
  (* Splits the string [s] into the colon-separated parts *)
  Pcre.split ~rex:colon_re ~max:(-1) s
;;


exception Password of string ;;              (* locally used in [user_exists] *)

let user_exists username =
  (* Checks whether there is a directory for this user, and whether the
   * required passwd entry exists.
   * Returns: 
   * - `False: if the user does not exist
   * - `Password pw: if this password is required to log in
   * - `Passes: if no password is required
   *)

  let workspace_dir_exists =
    username <> "" &&
    username.[0] <> '.' &&
    try
      let s = Unix.stat (Filename.concat file_store username) in
      s.Unix.st_kind = Unix.S_DIR
    with
	Unix.Unix_error(_,_,_) -> false
  in

  match password_file with
      None ->
	(* We can assume that the user is already authenticated by the
	 * web server. So return `Passes or `False.
	 *)
	if workspace_dir_exists then `Passes else `False
    | Some pwfile ->
	(* Read the password file line by line, and search the entry for
	 * the user.
	 *)
	if not workspace_dir_exists then
	  `False
	else
	  begin
	    try
	      let f = open_in pwfile in
	      with_in_obj_channel                         (* from Netchannels *)
		(new input_channel f)                     (* from Netchannels *)
		(fun ch ->
		   while true do
		     let line = ch # input_line() in        (* or End_of_file *)
		     match split line with
			 [uname; pword] ->
			   if uname = username then raise (Password pword)
		       | _ -> 
			   ()                    (* => Ignore malformed lines *)
		   done
		);
	      (* This point is never reached: *)
	      assert false
	    with
		End_of_file ->
		  (* The user is unknown because the entry is missing *)
		  `False
	      | Password pw ->
		  (* The user is known and has password [pw]: *)
		  `Password pw
	  end
;;


(* Secret keys: these are used to sign cookies. The secret key file contains
 * usually several secret keys. It is known when every key was generated.
 * If we get a cookie from the browser, we accept it if it was signed with
 * any of the keys, but the key must not be older than [session_timeout]
 * seconds.
 * If we send a cookie to the browser, we use the newest key. If the key
 * is older than [secret_key_timeout], we create a new key, and add it 
 * to the list of secret keys.
 * This way we can exactly control which cookies are accepted and need not
 * to trust the browser implementation.
 *)

let read_secret_keys() =
  (* Read in the file with secret keys. There is one key per line in the format:
   * <creation-time>:<key>
   * The creation time is in seconds since the epoch. If the key is older
   * than [session_timeout], it will not be returned by this function, and
   * will no longer be used to check signed cookies.
   *)

  let rec read_file ch =
    try
      let line = ch # input_line () in                      (* or End_of_file *)
      ( match split line with
	    [ t_string; key ] ->
	      let t = float_of_string t_string in      (* or Invalid_argument *)
	      (t,key) :: read_file ch
	  | _ -> assert false
      )
    with
	End_of_file -> []
      | Invalid_argument _ -> assert false
  in

  if not (Sys.file_exists secret_key_file) then
    (* No file: This is perfectly legal. This file will be created when the
     * modified key list will be saved.
     *)
    []
  else begin
    (* First read the contents of the file: *)
    let file_keylist =
      with_in_obj_channel
	(new input_channel (open_in secret_key_file))
	read_file
    in
    (* Now remove keys that are too old: *)
    let now = Unix.time() in
    List.filter
      (fun (t,key) -> t >= (now -. session_timeout))
      file_keylist
  end
;;


let current_secret_key key_list =
  (* Returns a key that is acceptable to sign cookies. If required, this
   * function makes a new key and updates the [secret_key_file].
   * Argument [key_list]: All known keys as pairs (creation_time, key)
   * - as returned by the function [read_secret_keys].
   *)
  (* Find the newest key [key_max] which was created at [t_max]. *)
  let (t_max, key_max) =
    List.fold_left
      (fun (t_m, key_m) (t,key) ->
	 if t > t_m then (t,key) else (t_m,key_m))
      (0.0, "")                              (* default value: too old anyway *)
      key_list in
  (* Is the key acceptable? *)
  let now = Unix.gettimeofday() in
  if t_max >= now -. secret_key_timeout then
    (* Yes! *)
    key_max
  else begin
    (* Create a new key. *)
    prng_init (key_max ^ string_of_float now);
    (* TODO: This is not good enough. Better we concatenate the passwd file
     * and use this string for PRNG initialization, too.
     *)
    let key = ref "" in
    for i = 0 to 15 do
      key := !key ^ sprintf "%02x" (random_8bits())
    done;
    (* Save the new key. We have to take into account that there might be a
     * parallel CGI process doing the same. Because of this, we open the
     * file in append mode, and add the line. There is no race condition
     * if the line is short enough such that the [write] call is atomic.
     * This is normally true if the line is shorter than 512 bytes.
     *)
    (* TODO: From time to time write a new file and throw all old keys
     * away
     *)
    let f = open_out_gen 
	      [ Open_wronly; Open_append; Open_creat ]
	      0o600 
	      secret_key_file in
    Printf.fprintf f "%.0f:%s\n" now !key;
    close_out f;
    (* Return the new key: *)
    !key
  end
;;


let make_cookie user password ip_address key =
  (* Creates the cookie string for [user] using the [key] to sign the
   * cookie. The [password] and the [ip_address] must be passed, too.
   * The cookie string has the format
   *   <username>:<verifier>
   * where <username> is the contents of the cookie, and <verifier>
   * a piece of information ensuring that nobody has changed the
   * contents (a "signature").
   *)
  let raw_verifier = Digest.string (user ^ ":" ^ password ^ ":" ^ 
				    ip_address ^ ":" ^ key) in
  (* [raw_verifier] is the "signature". *)
  let verifier = Netencoding.Base64.encode raw_verifier in
  Printf.sprintf "%s:%s" user verifier
;;


let dest_cookie s =
  try
    match split s with
	[ u; v ] -> (u, Netencoding.Base64.decode v)
      | _        -> assert false
  with
      Not_found -> assert false
;;


let check_cookie key_list ip_address cookie =
  (* Checks whether the cookie with the string [cookie] is an authenticated
   * user. If so, the function returns [Some(username, password)], otherwise
   * [None] is passed back.
   * The [key_list] are the possible keys. The [ip_address] of the client
   * must be available, too.
   *)

  let rec check_keys user password cookie_verifier keys =
    match keys with
	[] -> (* No valid key found: *) None
      | (_,key) :: keys' ->
	    let test_verifier = Digest.string (user ^ ":" ^ password ^ ":" ^ 
					       ip_address ^ ":" ^ key) in
	    if test_verifier = cookie_verifier then
	      Some(user,password)
	    else
	      check_keys user password cookie_verifier keys'
  in

  if cookie = "" then
    None
  else
    let (user, cookie_verifier) = dest_cookie cookie in
    match user_exists user with
	`False -> 
	  None                           (* maybe the user has been removed *)
      | `Password pw -> 
	  check_keys user pw cookie_verifier key_list
      | `Passes ->
	  assert false                      (* strange *)
;;

(**********************************************************************
 * HTML GENERATORS
 *
 * Some functions helping to generate HTML.
 **********************************************************************)

let text = Netencoding.Html.encode_from_latin1;;
(* This function encodes "<", ">", "&", double quotes, and Latin 1 characters 
 * as character entities. E.g. text "<" = "&lt;", and text "ä" = "&auml;"
 *)


let encode_filename x =
  (* Encodes [x] such that it can be safely used inside the _names_ of
   * CGI arguments. We have to be careful because there might be bugs in
   * browsers.
   * The chosen encoding: Alphanumeric characters are represented by 
   * themselves. The other characters are represented by an underscore
   * followed by the hexadecimal encoding.
   *)
  let q = Netencoding.Q.encode x in               (* Uses '=' instead of '_' *)
  Pcre.qreplace ~pat:"=" ~templ:"_" q
;;


let decode_filename x =
  (* The reverse function to [encode_filename] *)
  let q = Pcre.qreplace ~pat:"_" ~templ:"=" x in
  Netencoding.Q.decode q
;;


let begin_page (cgi : cgi_activation) title =
  (* Output the beginning of the page with the passed [title]. *)
  let out = cgi # output # output_string in
  out "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">\n";
  out "<HTML>\n";
  out "<HEAD>\n";
  out ("<TITLE>" ^ text title ^ "</TITLE>\n");

  (*---------------------------- CSS --------------------------------*)
  out ("<STYLE TYPE=\"text/css\">\n");
  out "body { background: white; color: black; }\n";
  out "h2 { width: 95%; background: black; color: white; padding: 5px; \
            font-size: large; font-weight: normal }\n";

  (* Style of file listing: *)
  (* (1) Rows *)
  out ".list-hdr-row  { background: #ff7777; }\n";
  out ".list-even-row { background: #ffffff; }\n";
  out ".list-odd-row  { background: #cccccc; }\n";
  (* (2) Columns *)
  out ".list-sel  { text-align: center; }\n";
  out ".list-name { text-align: left; }\n";
  out ".list-size { text-align: right; }\n";
  out ".list-date { text-align: right; }\n";
  out ".list-sel, .list-name, .list-size, .list-date { padding-left: 6px; padding-right: 6px }\n";
  out "</STYLE>\n";
  out "</HEAD>\n";
  out "<BODY>\n";
  out ("<H1>" ^ text title ^ "</H1>\n")
;;


let end_page (cgi : cgi_activation) =
  let out = cgi # output # output_string in
  out "</BODY>\n";
  out "</HTML>\n"
;;


let begin_form ?(enctype = "application/x-www-form-urlencoded") 
               (cgi : cgi_activation) =
  let out = cgi # output # output_string in
  out (sprintf "<FORM METHOD=POST ACTION=\"%s\" ENCTYPE=\"%s\">\n"
	 (text (cgi # url()))
	 enctype)
;;


let end_form (cgi : cgi_activation) =
  let out = cgi # output # output_string in
  out "</FORM>\n"
;;


let hidden_field (cgi : cgi_activation) name value =
  let out = cgi # output # output_string in
  out (sprintf "<INPUT TYPE=HIDDEN NAME=\"%s\" VALUE=\"%s\">\n"
	 (text name)
	 (text value))
;;


let pressed_button cgi possible_buttons =
  (* Check whether there is a CGI argument whose name is passed in the
   * list [possible_buttons]. If so, the name of the argument is returned.
   * If not, "no_button" is returned.
   *)
  try
    fst
      (List.find
	 (fun (arg_name, _) -> List.mem arg_name possible_buttons )
	 cgi#arguments)
  with
      Not_found -> "no_button"
;;


(**********************************************************************
 * LOGIN PAGE
 **********************************************************************)

let display_login_page (cgi : cgi_activation) =
  let out = cgi # output # output_string in

  begin_page cgi "Filemanager Login";
  begin_form cgi;

  out "<P>Please enter user name and password:<BR>\n";
  out "<TABLE CELLSPACING=0>\n";
  out "<TR>\n";
  out "<TD>User name:</TD><TD><INPUT TYPE=TEXT NAME=\"cred_user\"></TD>\n";
  out "</TR>\n";
  out "<TR>\n";
  out "<TD>Password:</TD><TD><INPUT TYPE=PASSWORD NAME=\"cred_password\"></TD>\n";
  out "</TR>\n";
  out "<TR>\n";
  out "<TD>&nbsp;</TD>\n";
  out "<TD><INPUT TYPE=SUBMIT NAME=LOGIN VALUE=\"Login\"></TD>\n";
  out "</TR>\n";
  out "</TABLE>\n";

  end_form cgi;
  end_page cgi
;;

(* The login page is handled in [process] below, because the login page
 * can "happen" at any time.
 *)

(**********************************************************************
 * LIST PAGE
 *
 * The list page displays the file listing, with lots of buttons.
 **********************************************************************)

let read_list dir =
  (* Read the directory [dir] and return a list of tuples
   * (name, size, date).
   * The list is sorted by name.
   *)
  let f = Unix.opendir dir in
  let l = ref [] in
  try
    while true do
      let n = Unix.readdir f in      (* or End_of_file *)
      let st = Unix.stat (Filename.concat dir n) in
      if n <> "." && n <> ".." then
	l := (n, st.Unix.st_size, st.Unix.st_mtime) :: !l
    done; 
    assert false
  with
      End_of_file ->
	Unix.closedir f;
	List.sort 
	  (fun (a,_,_) (b,_,_) -> Pervasives.compare a b)
	  !l
    | err ->
	Unix.closedir f; raise err
;;


let display_list_page user (cgi : cgi_activation) =
  (* Just output the listing: *)
  let out = cgi # output # output_string in
  begin_page cgi "Filemanager";
  begin_form cgi;

  hidden_field cgi "page" "list";

  out (sprintf "User: %s&nbsp;&nbsp;&nbsp;\n" (text user));
  out (sprintf "<A HREF=\"%s\">Log out</A>\n"
	 (text (cgi # url 
		  ~with_query_string:(`Args[ new simple_argument 
					       "cred_logout" "yes" ])
	          ()
	       )));

  out "<H2>List of files</H2>\n";

  let files = read_list (Filename.concat file_store user) in
  out "<TABLE CELLSPACING=0 frame=border rules=cols border=1>\n";

  (* COLGROUPs would be helpful for this type of table. No browser seems
   * to support them.
   *)

  out "<THEAD>\n";
  out "<TR class='list-hdr-row'>\n";
  out "<TH class='list-sel'>Select</TH>\n";
  out "<TH class='list-name'>Name</TH>\n";
  out "<TH class='list-size'>Size</TH>\n";
  out "<TH class='list-date'>Last modified</TH>\n";
  out "</TR>\n";

  out "<TBODY>\n";
  let odd = ref false in
  List.iter
    (fun (name, size, date) ->
       let mk_arg = new simple_argument in
       let download_url =
	 cgi # url
	   ~with_query_string: (`Args [ mk_arg "page" "list";
					mk_arg "download_link" "yes";
					mk_arg "name" name
				      ])
	   ()
       in
       let row_class = if !odd then "list-odd-row" else "list-even-row" in
       out (sprintf "<TR class='%s'>\n" row_class);
       out (sprintf 
	      "<TD class='list-sel'>\
               <INPUT TYPE=\"CHECKBOX\" NAME=\"sel_%s\" VALUE=\"ON\"></TD>\n"
	      (encode_filename name)
	   );
       out (sprintf 
	      "<TD class='list-name'><A HREF=\"%s\">%s</A></TD>\
               <TD class='list-size'>%d</TD>\
               <TD class='list-date'>%s</TD>\n"
	      (text download_url)
	      (text name)
	      size
	      (Netdate.format ~fmt:"%Y-%m-%d %H:%m" (Netdate.create date)));
       out "</TR>\n";
       odd := not !odd;
    )
    files;

  out "</TABLE>\n";

  out "<H2>Actions</H2>\n";

  out "Select one or more files and press one of the following buttons:\n";

  out "<P><INPUT TYPE=SUBMIT NAME='delete_button' VALUE='Delete'>";

  end_form cgi;

  out "<H2>Upload</H2>\n";

  (* The upload form is a different form because we need a special enctype. *)

  begin_form ~enctype:"multipart/form-data" cgi;
  hidden_field cgi "page" "list";
  out "<P>You can upload a new file here:\n";
  out "<P><INPUT TYPE=FILE NAME='file'>\n";
  out "<INPUT TYPE=SUBMIT NAME='upload_button' VALUE='Upload this file'>\n";
  end_form cgi;

  end_page cgi
;;


let sel_re = Pcre.regexp "^sel_(.*)$" ;;

let selected_files cgi =
  (* Which of the files of the file list are selected? Returns a list
   * of filenames.
   *)
  let names =
    List.map
       (fun (arg_name, arg) ->
	  try
	    match Pcre.extract ~rex:sel_re arg_name with
		[| _; enc_name |] -> 
		  if arg # value = "ON" then
		    decode_filename enc_name
		  else
		    ""
	      | _ ->
		  ""
	  with
	      Not_found -> ""
       )
       cgi#arguments
  in
  List.filter (fun name -> name <> "") names
;;


let handle_list_page user (cgi : cgi_activation) =
  (* This function is called if one of the buttons from the list page
   * has been pressed. 
   * The function must return the name of the continuation.
   *)

  (* Check which button has been pressed: *)
  let button =
    pressed_button 
      cgi
      [ "upload_button"; "delete_button"; "download_link" ] 
  in

  (* Execute the code for the button: *)
  match button with
    | "delete_button" ->
	(* Find out which files to delete: *)
	let files = selected_files cgi in
	if files = [] then raise (User_error "Please select at least one file!");
	`Confirm_delete files
    | "download_link" ->
	let name = cgi # argument_value "name" in
	`Download name
    | "upload_button" ->
	let file = cgi # argument "file" in
	(* Because the [~processing] parameter of [cgi] specified that the
	 * argument called "file" will be put into a temporary file, we
	 * can assume this now.
	 *)
	let tmp_filename = 
	  match file # store with
	      `File name -> name | _ -> assert false in
	let user_filename =
	  match file # filename with
	      Some name -> name | _ -> assert false in
	(* [tmp_filename]: The name of the temporary file where the uploaded
	 * material is stored now
	 * [user_filename]: The file name entered by the user
	 *)
	(* Normalize the user_filename now: *)
	(* (1) Convert backslashes into slashes *)
	let user_filename' =
	  Pcre.qreplace ~pat:"\\\\" ~templ:"/" user_filename in
	(* (2) Get the part after the last slash: *)
	let user_filename'' =
	  Filename.basename user_filename' in
	if user_filename'' = "" || user_filename'' = "/" then
	  raise(User_error "Bad file");
	(* Move the [tmp_filename] to its place: *)
	Sys.rename 
	  tmp_filename 
	  (Filename.concat 
	     (Filename.concat file_store user)
	     user_filename'');
	(* Show the file list again: *)
	`List
    | _ ->
	(* By default: do nothing, and display the list again *)
	`List
;;

(**********************************************************************
 * CONFIRM DELETE PAGE
 *
 **********************************************************************)

let display_confirm_delete_page files user (cgi : cgi_activation) =
  let out = cgi # output # output_string in
  begin_page cgi "Filemanager";
  begin_form cgi;

  hidden_field cgi "page" "confirm_delete";

  out "<H2>Delete</H2>\n";

  out "Do you really want to delete the following files?\n";

  out "<UL>\n";
  List.iter
    (fun name ->
       out (sprintf "<LI> %s\n" (text name));
       hidden_field cgi ("sel_" ^ encode_filename name) "ON"
    )
    files;
  out "</UL>\n";

  out "<P><INPUT TYPE=SUBMIT NAME='yes_button' VALUE='Yes'>\n";
  out "<INPUT TYPE=SUBMIT NAME='no_button' VALUE='No'>\n";
  
  end_form cgi;
  end_page cgi
;;


let handle_confirm_delete_page user (cgi : cgi_activation) =
  (* Check which button has been pressed: *)
  let button =
    pressed_button 
      cgi
      [ "yes_button"; "no_button" ] 
  in

  if button = "yes_button" then begin
    let files = selected_files cgi in
    List.iter
      (fun name ->
	 Sys.remove
	   (Filename.concat
	      (Filename.concat file_store user)
	      name)
      )
      files;
    `List
  end
  else
    `List
;;

(**********************************************************************
 * DOWNLOAD PAGE
 *
 **********************************************************************)

let display_download_page filename user ( cgi : cgi_activation ) =
  (* Special download code: Because [cgi] has an output buffer, we bypass
   * this buffer. (There is no need for this buffer here.)
   *)
  let env = cgi # environment in

  let f =
    open_in_bin
      (Filename.concat (Filename.concat file_store user) filename) in

  (* Get the size of the file such that we can pass it to the browser
   * as content-length. For many browsers, this improves the download
   * dialog.
   *)
  let size = 
    in_channel_length f in

  (* Remove critical characters (space, double quotes, backslashes)
   * from the filename: 
   *)
  let user_filename =
    Pcre.qreplace ~rex:(Pcre.regexp "[ \\\"\\\\]") ~templ:"_" filename in

  (* Set now the header: *)
  cgi # set_header
    ~content_type: "application/octet-stream"
    ~cache: `No_cache
    ~filename: user_filename
    ~fields:[ "content-length", [string_of_int size] ]
    ();
  env # send_output_header();
  (* [set_header] sets immediately the header of the environment. *)

  (* Send the file to [env # output_ch], and NOT [cgi # output]. The
   * difference is that the latter is buffered, and the first is 
   * unbuffered.
   *)
  let ch = new input_channel f in
  env # output_ch # output_channel ch;
  ch # close_in();

  (* Set the output state such that it is known that the body of the
   * HTTP message has been sent.
   *)
  env # set_output_state `Sent_body
;;
  

(**********************************************************************
 * REQUEST BROKER
 *
 * This function calls the various page-specific handlers.
 **********************************************************************)

let process() =
  let now = Unix.time() in
  let yesterday = now -. 86400.0 in

  (* Our configuration. We use a different [tmp_directory] to ensure that
   * the uploaded files are in the same filesystem as [file_store].
   *)
  let config =
    { default_config with
	tmp_directory = Filename.concat file_store ".tmp"
    } 
  in

  (* The environment gets out configuration *)
  let env =
    try
      new std_environment ~config() 
    with Std_environment_not_found ->
      new test_environment ~config()
  in

  let cgi =
    new std_activation 
      ~env 
      ~processing:(function "file" -> (fun _ -> `File) | _ -> (fun _ -> `Memory))
      ~operating_type:buffered_transactional_optype () 
  in
  (* We use buffered output here. 
   * Note: For downloaded files, the buffer is bypassed. See the function
   * [display_download_page].
   *
   * The [~processing] type is `File only for the CGI argument "file".
   * This argument is used for file uploads.
   *)

  (* Set a default header: This will be overridden later, but if an
   * early error happens, we have a header nethertheless
   *)
  cgi # set_header();
  
  (* The [try] block catches errors during the page generation. *)
  try
    (* Find out the current user: 
     * [user] is either the trusted user name or "" (no user)
     * [next_cookie] is one of:
     *  - None: Do not set a cookie
     *  - Some "": Delete the current cookie
     *  - Some v:  Set the cookie with value v
     *
     * If there is no password file, the web server must authenticate the
     * user. In this case, we believe the web server.
     *
     * If there is a password file, we do authentication ourselves. There
     * are a number of possible cases:
     * - There is a cookie, but the CGI argument "cred_logout" = "yes":
     *   ==> Delete the cookie, and force that the login page is displayed
     * - There is a cookie, but it cannot be verified. This means that the
     *   cookie is too old, and the matching key has already been removed 
     *   from the list of valid keys.
     *   ==> Delete the cookie, and force that the login page is displayed
     * - There is a cookie, and it can be verified. In this case we trust
     *   the cookie. Furthermore, a new cookie is created immediately,
     *   because the current key may have changed.
     * - There is no cookie, but CGI arguments cred_user and cred_password:
     *   Verify the password. If it is right, create a new cookie containing
     *   the user name. Furthermore, the cookie is signed. 
     *   If the password is incorrect, display the login page again.
     * - There is no cookie and no CGI arguments containing the credentials:
     *   Display the login page.
     *)
    let user, next_cookie =
      match password_file with
	  None ->
	    (* The web server authenticates the user. It is available by
	     * the CGI property remote_user: 
	     *)
	    (cgi # environment # cgi_remote_user, None)
	| Some _ ->
	    (* Check whether there is a cookie containing the user name: *)
	    let cl = cgi # environment # cookies in         (* all cookies *)
	    let key_list = read_secret_keys() in
	    let ip_addr = cgi # environment # cgi_remote_addr in
	    try
	      if cgi # argument_value "cred_logout" = "yes" then
		("", Some "")                       (* delete the cookie *)
	      else
		let cookie = List.assoc "credentials" cl in  (* or Not_found *)
		if cookie = "" then raise Not_found;
		( match check_cookie key_list ip_addr cookie with
		      None ->
			(* The cookie has expired. Force that the user must
			 * authenticate again.
			 *)
			("", Some "")                  (* delete the cookie! *)
		    | Some (user,password) ->
			(* Create the new cookie: *)
			let key = current_secret_key key_list in
			let cookie' = make_cookie user password ip_addr key in
			(user, Some cookie')
		)
	    with
		Not_found ->
		  (* If there are "cred_user" and "cred_password" CGI arguments
		   * use these
		   *)
		  let user = cgi # argument_value "cred_user" in
		  let password = cgi # argument_value "cred_password" in
		  ( match user_exists user with
			`False -> ("", None)
		      | `Password pw ->
			  if password = pw then begin
			    (* Create the new (first) cookie: *)
			    let key = current_secret_key key_list in
			    let cookie' = 
			      make_cookie user password ip_addr key in
			    (user, Some cookie')
			  end
			  else ("", None)
		      | `Passes -> assert false
		  )
    in

    (* Set a header that is perfect for HTML pages. *)
    cgi # set_header
      ~cache:`No_cache
      ~content_type: "text/html; charset=iso-8859-1"
      ~set_cookie:( match next_cookie with 
			None -> []
		      | Some v ->
			  [ { cookie_name = "credentials";
			      cookie_value = v;
			      cookie_expires = 
				if v="" then Some yesterday else None;
			        (* Expiration is checked by ourselves *)
			      cookie_domain = None;
			      cookie_path = None;
			      cookie_secure = (cgi # environment # cgi_https);
			    }
			  ]
		  )
      ();
    
    (* If user = "", the user has not yet logged in. Display the login
     * page (or fail, if disabled)
     *)
    if user = "" then begin
      if password_file = None then
	failwith "No remote user available (badly configured web server?)";

      display_login_page cgi
    end

    else begin
      (* Call the function that handles the visible page: *)
      let visible_page = cgi # argument_value ~default:"list" "page" in
      let handle = match visible_page with
	  "list"           -> handle_list_page 
	| "confirm_delete" -> handle_confirm_delete_page
(*	| "confirm_rename" -> handle_confirm_rename_page
*)	| _ ->
	    failwith "Unknown page"
      in
      (* and determine the next page to display: *)
      let next_page = handle user cgi in
      let display = match next_page with
	  `List -> display_list_page
	| `Confirm_delete files -> display_confirm_delete_page files 
(*	| `Confirm_rename file -> ()
*)
	| `Download file -> display_download_page file
      in
      display user cgi
    end;

    (* Commit everything: *)
    cgi # output # commit_work();

    (* Cleanup: *)
    cgi # finalize()
  with
    | User_error s ->
        (* An error has happened. Generate now an error page instead of
         * the current page. By rolling back the output buffer, any 
         * uncomitted material is deleted.
         *)
	let out = cgi # output # output_string in
	cgi # output # rollback_work();
	begin_page cgi "Error";
	out s;
	out "<BR>\n";
	out (sprintf "<A HREF=\"%s\">Restart</A>\n"
	       (text (cgi # url())));
	end_page cgi;
        (* Now commit the error page: *)
	cgi # output # commit_work();
	cgi # finalize()
	
    | error -> raise error
	(* The same for system errors. *)
        cgi # output # rollback_work();
	begin_page cgi "Software error";
        cgi # output # output_string "While processing the request an O'Caml exception has been raised:<BR>";
        cgi # output # output_string ("<TT>" ^ text(Printexc.to_string error) ^ "</TT><BR>");
        end_page cgi;
        cgi # output # commit_work();
	cgi # finalize()
;;


let main() =
  (* Call the function that processes the request, and catch any remaining
   * errors. These include protocol errors, and insufficient memory.
   * Because we cannot do anything else the error is simply logged.
   *)
  try
    process()
  with
      error ->
        prerr_endline ("O'Caml exception: " ^ Printexc.to_string error)
;;


main();;


(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.4  2002/11/01 21:37:00  stolpmann
 * 	Typo
 *
 * Revision 1.3  2002/02/02 19:05:20  stolpmann
 * 	Updated.
 *
 * Revision 1.2  2001/11/18 13:28:28  stolpmann
 * 	Improved some comments.
 *
 * Revision 1.1  2001/10/22 00:31:56  stolpmann
 * 	Initial revision.
 *
 * 
 *)
