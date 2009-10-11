(* $Id$ *)

(* TODO:
 * 
 * - Timeouts
 * - Reasonable exception handling. Currently, exceptions simply fall through
 *   to the caller which is always Unixqueue.run
 * - ftp_connected, ftp_data_conn are probably not correctly set
 *)

(** FTP client
  *
  * Warning: Experimental code! Not all features described in this interface
  * work as described!
  *
  * The client is currently only partially implemented. Especially, the
  * STOR and STOU commands are missing. Only IPv4 is supported.
  *
  * It is intended to implement a large subset of RFC 959 (and later,
  * the newer FTP-related RFCs). This includes support for EBCDIC,
  * record files, and block mode.
 *)

exception FTP_error of exn
  (** Something went wrong, often on socket level *)

exception FTP_protocol_violation of string
  (** The server violates the FTP specification *)


type cmd_state =
    [ `Init
    | `Success
    | `Proto_error
    | `Temp_failure
    | `Perm_failure
    | `Rename_seq
    | `Restart_seq
    | `User_pass_seq
    | `User_acct_seq
    | `Pass_acct_seq
    | `Preliminary
    ]
  (** The command state:
    * - [`Init]: Just connected, no greeting message arrived yet
    * - [`Success]: Got the greeting message/last command was successful
    * - [`Proto_error]: {b currently unused}
    * - [`Temp_failure]: Last command was not successful, and the code
    *   was between 400 and 499
    * - [`Perm_failure]: Last command was not successful, and the code
    *   was between 500 and 599
    * - [`Rename_seq]: Used instead of [`Success] after the RNFR command
    * - [`Restart_seq]: Used instead of [`Success] after the REST command
    * - [`User_pass_seq]: Used instead of [`Success] after the USER command
    *   when a password must be typed in
    * - [`User_acct_seq]: Used instead of [`Success] after the USER command
    *   when an account ID must be typed in
    * - [`Pass_acct_seq]: Used instead of [`Success] after the PASS command
    *   when an account iD must be typed in
    * - [`Preliminary]: a reply with code 100 to 199. There will be another
    *   final reply for the command
   *)

type port =
    [ `Active of string * int * Unix.file_descr
    | `Passive of string * int
    | `Unspecified 
    ]
  (** The port of the data connection: [`Active] means that the server 
    * initiates the data connection to the listening client, and in the
    * case of [`Passive] the client initiates the data connection to the
    * listening server. The string argument is the IP address as dotted
    * quad, the int argument is the port number, and the descriptor 
    * is the listening master socket.
   *)


type form_code =
    [ `Non_print | `Telnet | `ASA ]
  (** The [form_code] has a meaning when FTP is used to print files:
    * - [`Non_print]: This is not the case.
    * - [`Telnet]: Telnet control codes are used for vertical movement
    * - [`ASA]: ASA (Fortran) control codes are used for vertical movement
   *)

type representation =
    [ `ASCII of form_code option
    | `EBCDIC of form_code option
    | `Image
    ]
  (** The representation of the transferred file:
    * - [`ASCII]: An ASCII variant is used, i.e. the server sends files in
    *   ASCII encoding with CR/LF as end-of-line marker. Supported by all
    *   servers.
    * - [`EBCDIC]: An EBCDIC variant is used, i.e. the server sends files in
    *   EBCDIC encoding with NEL as end-of-line marker
    * - [`Image]: The file is transferred in its original binary
    *   representation. Supported by all servers.
    *
    * "Local" representations are not supported.
    *
    * This FTP client does not recode the files such that they match the
    * selected representation. When files are downloaded, they are stored
    * as they are received. When files are uploaded, they are sent as they
    * are. The user of this client must do recodings when necessary
    * (the class {!Ftp_data_endpoint.data_converter} may be useful for this).
    *
    * If no representation is selected, FTP servers assume [`ASCII None]
    * as default.
   *)


type structure =
    [ `File_structure
    | `Record_structure
    ]
  (** The client supports two structures:
   * - [`File_structure]: Files are simply contiguous streams of bytes
   * - [`Record_structure]: Files are sequences of records. FTP does
   *   not make a difference between variable and fixed length records.
   *   It is not forbidden that the records are themselves structured
   *   into lines, in fact it can happen that end-of-line markers are
   *   contained in binary records. Operating systems that support 
   *   record-structured files often store text files in this format, i.e.
   *   every line is stored in its own record, without end-of-line marker.
   *   If record structure is selected by a STRU command, it is recommended
   *   to use the classes {!Ftp_data_endpoint.out_record_channel} and
   *   {!Ftp_data_endpoint.in_record_channel} for the local representation
   *   of the files, otherwise the records may be incorrectly mapped
   *   to the local conventions.
   *
   * Page-structured files (i.e. indexed files) are not supported.
   *
   * If no structure is selected, FTP servers will assume file structure
   * as default.
   *)


type transmission_mode =
    [ `Stream_mode
    | `Block_mode
    ]
  (** The transmission mode selects how the data are encoded in the data
    * connection.
    * - [`Stream_mode]: This is the simple format that is responsible for
    *   all the failed FTP downloads. It is supported by all FTP servers,
    *   actually, you cannot assume a better transmission mode from an
    *   unknown FTP server. It is unreliable because it cannot distinguish
    *   between a transmission failure and the regular end-of-file condition.
    * - [`Block_mode]: This is an improved format using frames to protect
    *   the transmitted data. Unfortunately, almost no FTP server supports
    *   it.
    *
    * Both modes are compatible with both structures, i.e. you can transfer
    * a record-structured file in stream mode and a flat file in block
    * mode. However, in practise this is not the case. Servers that only
    * know flat files are likely to only support stream mode, and servers
    * implementing record structure imply that block transfers base on
    * the record format. So the advise is to use stream mode for flat
    * files, and block mode for record files.
   *)


type ftp_state =
    { cmd_state : cmd_state;        (** the command state *)
      ftp_connected : bool;         (** whether connected with the server *)
      ftp_data_conn : bool;         (** whether there is a clean data conn *)
      ftp_user : string option;     (** successfully sent user identifier *)
      ftp_password : string option; (** successfully sent password *)
      ftp_account : string option;  (** successfully sent account identifier *)
      ftp_logged_in : bool;         (** whether the user is logged in *)
      ftp_port : port;              (** the selected port *)
      ftp_repr : representation;    (** the selected representation *)
      ftp_structure : structure;    (** the selected structure *)
      ftp_trans : transmission_mode; (** the selected trans mode *)
      ftp_dir : string list;
         (** The current directory, expressed as list of CWD changes minus
          * CDUP changes. This is only reasonable if CWD does not include
          * slashes. The list is in reverse order, i.e. deepest directory
          * first.
          *)
      ftp_features : (string * string option) list option;
         (** The list of features returned by the last FEAT command.
           * [None] means that no FEAT command was yet tried. 
           * [Some []] means that there are no features (either FEAT
           * returned an empty list, or the FEAT command is not implemented
           * by the server). Otherwise the list enumerates pairs 
           * [(label,param)] where [label] is the case-sensitive feature
           * label and [param] the optional parameter. There is no
           * defined order for the list of features.
          *)
      ftp_options : (string * string option) list;
         (** Remembers the OPTS commands sent to the server. The list
           * enumerates pairs [(command,optionparam)], where [command]
           * is the uppercase command name the option refers to. Only
           * the last negotiated [optionparam] for the command is remembered.
          *)
    }
  (** The ftp_state reflects the knowledge of the client about what has been
    * agreed upon with the server.
   *)


type cmd =
    [ `Connect
    | `Dummy
    (* RFC 959 - standard FTP *)
    | `USER of string
    | `PASS of string
    | `ACCT of string
    | `CWD of string
    | `CDUP
    | `SMNT of string
    | `QUIT
    | `REIN
    | `PORT   (* port is automatically chosen *)
    | `PASV
    | `TYPE of representation
    | `STRU of structure
    | `MODE of transmission_mode
    | `RETR of string * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `STOR of string * (ftp_state -> Ftp_data_endpoint.local_sender)
    | `STOU of (unit -> Ftp_data_endpoint.local_sender)
    | `APPE of string * (ftp_state -> Ftp_data_endpoint.local_sender)
    | `ALLO of int * int option
    | `REST of string
    | `RNFR of string
    | `RNTO of string
    | `DELE of string
    | `RMD of string
    | `MKD of string
    | `PWD
    | `LIST of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `NLST of string option * (ftp_state -> Ftp_data_endpoint.local_receiver)
    | `SITE of string
    | `SYST
    | `STAT of string option
    | `HELP of string option
    | `NOOP
    (* Extensions *)
    (* RFC 2389 - feature negotiation *)
    | `FEAT
    | `OPTS of string * string option
    (* ExtFTP Working Group *)
    | `MDTM of string
    ]
  (** An FTP command. {b Currently, STOR, STOU and APPE are not supported.} *)

type reply = int * string
  (** Reply code plus text *)



(** The client protocol interpreter...
 *
 * has a queue of commands that are sent to the server in turn.
 *
 * [onempty] is called when the last command of the queue has been processed.
 *
 * [onclose] is called when the control connection is closed by the
 * FTP server (e.g. as reaction of the QUIT command).
 *
 * [onerrorstate] is called when an [`Error] state is reached (serious
 * error condition).
 *
 * [onusererror] is called when callbacks (i.e. [onreply]) raise exceptions.
 *)
class ftp_client_pi : 
        ?event_system:Unixqueue.event_system ->
        ?onempty:(ftp_state -> unit) ->
        ?onclose:(unit -> unit) ->
        ?onerrorstate:(exn -> unit) ->
        ?onusererror:(exn -> unit) ->
        Unix.file_descr ->
object


  method add_cmd : ?onreply:(ftp_state -> reply -> unit) -> cmd -> unit
    (** Add another command to the queue. The protocol interpreter does 
     * not check whether this command is allowed in the current ftp_state
     * or not. For every reply of the server [onreply] is called.
     * Due to the FTP specification there may be several replies for
     * a command: First, zero or more replies with [cmd_state = `Preliminary],
     * and then exactly one reply with a final state.
     *)

  method send_abort : unit -> unit
    (** Sends immediately an [ABOR] command, even when a data transfer is
     * in progress.
     *
     * TODO - not yet implemented
     *)

  method run : unit -> unit
    (** Starts the event system; same as [Unixqueue.run] *)

  method ftp_state : ftp_state
    (** Returns the current ftp_state *)

  method state : unit Uq_engines.engine_state
    (** The state in the sense of [Uq_engines]. Possible values are:
      * - [`Working _]: The control connection is still active. The [int]
      *   argument is currently meaningless.
      * - [`Done()]: The control connection has been terminated.
      * - [`Error e]: A violation of the FTP protocol happened, or another
      *   exception [e] occurred
      * - [`Aborted]: The [abort] method was called
     *)

  method abort : unit -> unit
    (** Shuts any active connection immediately down, and changes the
      * state of the engine to [`Aborted].
     *)

  method event_system : Unixqueue.event_system
    (** The used event system *)

  method is_empty : bool
    (** Whether the queue is empty *)

end


module Action : sig
  type plan
  type action = plan -> unit

  val ftp_state : plan -> ftp_state
    (** Returns the ftp_state at the beginning of the plan *)

  val execute : onreply:(ftp_state -> reply -> unit) -> 
                onerror:(ftp_state -> reply -> unit) -> 
                ftp_client_pi -> 
                action ->
                  unit
    (** Executes the action for the given client PI. If the normal 
      * execution path is taken, finally [onreply] is called once.
      * If the error execution path is taken, finally [onerror] is called
      * once.
     *)

  val empty : action
    (** Do nothing *)

  val command : cmd -> action
    (** Execute the command *)

  val dyn_command : (unit -> cmd) -> action
    (** Execute the computed command *)

  val seq2 : action -> action -> action
    (** Do the two actions in turn *)

  val full_seq2 : action -> (reply -> action) -> action
    (** Do the first action, then get the second action using the reply
      * of the first action.
     *)

  val seq : action list -> action
    (** Do the list of actions in turn *)

  val expect : cmd_state -> action -> action
    (** If the execution of the action yields the passed state, continue
      * normally, otherwise treat the situation as error
     *)

  val seq2_case : action -> (cmd_state * action) list -> action
    (** Do the first action, then check the resulting command state.
      * The matching second action is executed. If no second action
      * matches, the situation is treated as error
     *)

end


(** An [ftp_method] is a small procedure doing some task *)
class type ftp_method =
object
  method connect : (string * int) option
    (** The host and port the FTP method wants to be connected to.
      * If [None] the current connection is used.
     *)

  method execute : Action.action
    (** This method is called when the [ftp_client_pi] is connected and
      * the queue of commands is empty.
      *
      * [onsuccess] must be called when the method has been successfully
      * finished.
      *
      * [onerror] must be called when an exception is caught.
     *)

end


exception FTP_method_temp_failure of int * string
exception FTP_method_perm_failure of int * string
exception FTP_method_unexpected_reply of int * string
  (** These exceptions may be raised during execution by the FTP method.
    * The int is the unexpected FTP control code and the string the
    * corresponding text. A temporary failure has a code between 400 and
    * 499, and a permanent failure has a code between 500 and 599.
    *
    * {b This does not work yet as intended!}
   *)


class connect_method : host:string ->
                       ?port:int ->
                       unit -> ftp_method
  (** This method connects to the [host] which must be an IP address *)


class login_method : user:string ->
                     get_password:(unit -> string) ->
                     get_account:(unit -> string) ->
                     unit ->
                       ftp_method
(** This FTP method logs the [user] in. [get_password] is called when
  * the FTP server asks for the password (may be skipped). [get_account]
  * is called when the server asks for the account ID (may be skipped).
 *)


class walk_method : [ `File of string | `Dir of string | `Stay ] ->
                    ftp_method
(** This FTP method walks to the target directory:
  *
  * - [`File name]: The [name] is interpreted as slash-separated path.
  *   It is always interpreted relative to the home directory of the 
  *   user (i.e. the directory after login), even if it begins with a
  *   slash. The FTP command walks to the directory containing [name].
  * - [`Dir name]: The FTP command walks to the directory [name] (same
  *   syntax as for [`File]).
  * - [`Stay]: The FTP command does nothing (stays in the current directory).
 *)

type filename =
    [ `NVFS of string
    | `Verbatim of string
    ]
  (** There are several methods how to specify filenames:
    * - [`NVFS name]: The "Network Virtual File System" is the normal way
    *   of accessing FTP files. The client walks into the directory containing
    *   the file using [CWD] and [CDUP] commands, and calls the file operation
    *   from this directory. For simplicity, this client interprets slashes
    *   in [name] as path component separators. The FTP server will never
    *   see these slashes.
    * - [`Verbatim name]: The string [name] is passed to the server without
    *   transforming it in any way.
    *
    * In the future, there will be a third way of referring to files:
    * TVFS, the "Trivial Virtual File System".
   *)

class get_method : file:filename ->
                   representation:representation ->
                   store:(ftp_state -> Ftp_data_endpoint.local_receiver) ->
                   unit ->
                     ftp_method
(** This FTP method walks to the right directory and gets [file] from
  * the server. The file is stored in the [local_receiver] that can be
  * obtained by calling the [store] function. The selected [representation]
  * remains unchanged.
 *)

class invoke_method : command:cmd ->
                      process_result:(ftp_state -> (int * string) -> unit) ->
                      unit ->
                        ftp_method
(** This FTP method simply invokes [command], and calls for all kinds of
  * successful replies the function [process_result], passing the ftp_state and
  * the code and the reply text.
 *)

class set_structure_method : structure -> ftp_method
(** Requests a certain structure for future file transfers *)

class set_mode_method : transmission_mode -> ftp_method
(** Requests a certain mode for future file transfers *)

class rename_method : file_from:filename ->
                      file_to:filename ->
                      unit ->
                        ftp_method
(** Renames the [file_from] into [file_to].
  *
  * Both file names must be of the same type, either [`NVFS] or [`Verbatim].
  * If [`NVFS], both names must be in the same directory.
 *)

class mkdir_method : filename -> ftp_method
(** Creates the named directory *)

class rmdir_method : filename -> ftp_method
(** Deletes the named directory *)

class delete_method : filename -> ftp_method
(** Deletes the named file *)

class list_method : dir:filename ->
                    representation:representation ->
                    store:(ftp_state -> Ftp_data_endpoint.local_receiver) ->
                    unit ->
                      ftp_method
(** Lists the contents of the directory [dir] using the LIST command.
  * The [representation] must not be [`Image].
 *)

class nlst_method : dir:filename ->
                    representation:representation ->
                    store:(ftp_state -> Ftp_data_endpoint.local_receiver) ->
                    unit ->
                      ftp_method
(** Lists the contents of the directory [dir] using the NLST command
  * The [representation] must not be [`Image].
 *)

class mdtm_method : file:filename ->
                    process_result:(float -> unit) ->
                    unit ->
                      ftp_method
(** Determines the date and time of the last modification of [file].
  * On success, [process_result] is called.
 *)


(** The ftp client is a user session that may even span several connections.
  * However, only one server is connected at once.
 *)
class ftp_client :
        ?event_system:Unixqueue.event_system ->
        ?onempty:(unit -> unit) ->
        unit ->
object
  inherit [unit] Uq_engines.engine
    (** The FTP client is also an engine. The engine can be in one of four
      * states:
      * - [`Working _]: The client is still active. The [int]
      *   argument is currently meaningless.
      * - [`Done()]: The client has been terminated.
      * - [`Error e]: A violation of the FTP protocol happened, or another
      *   exception [e] occurred
      * - [`Aborted]: The [abort] method was called
     *)

  method add : ?onsuccess:(unit -> unit) ->
               ?onerror:(exn -> unit) ->
               ftp_method -> unit
    (** Adds an FTP method to the queue of methods to execute. It is no
      * problem to add the same method twice.
      *
      * When the method could be executed successfully, the function
      * [onsuccess] is called. (By default, this function does nothing.)
      *
      * If the FTP server indicates an error, the 
      * function [onerror] is called instead. The exception is either
      * [FTP_method_temp_failure] or [FTP_method_perm_failure].  
      * The default for [onerror] is to raise the exception again,
      * which has the effect of setting the engine state to [`Error].
      * This effectively stops the FTP client. (Hard errors
      * like socket problems or protocol violations are not reported this
      * way, but by directly setting the engine state to [`Error].)
     *)
  
  method run : unit -> unit
    (** Starts the event system; same as [Unixqueue.run] *)

end


(** {2 Examples and Discussion}
  *
  * To download a single flat file from a server:
  *
  * {[ 
  *   let buffer = Buffer.create 1000 in
  *   let ch = new Netchannels.output_buffer buffer in
  *   let client = new ftp_client() in
  *   client # add (new connect_method ~host:"127.0.0.1");
  *   client # add (new login_method ~user:"foo"
  *                                  ~get_password:(fun () -> "password")
  *                                  ~get_account:(fun () -> "foo") ());
  *   client # add (new get_method ~file:"path/to/file"
  *                                ~representation:`Image
  *                                ~store:(fun _ -> `File_structure ch) ());
  *   client # run()
  * ]}
  *
  * The file is stored in [buffer]. By using a different netchannel, it
  * can be stored whereever wanted.
  *
  * This piece of code has the disadvantage that the client does not stop
  * when a method fails ({b in the current version, it always stops, but
  * in a disadvantegous manner - this will be changed}). Because of this,
  * it is better to execute the next method only when the previous
  * was successful:
  *
  * {[ 
  *   ...
  *   client # add ~onsuccess:(fun () ->
  *                               client # add ~onsuccess:...
                                               (new login_method 
                                                   ~user:"foo"
  *                                                ~get_password:(fun () -> "password")
  *                                                ~get_account:(fun () -> "foo") ());
  *                           )
  *                (new connect_method ~host:"127.0.0.1")
  * ]}
  *
  * Alternatively, one can also force that the execution stops by raising
  * an exception in the [onerror] callback ({b not implemented in the current
  * version}).
  *
  * To download a record-structured text file, use a [store] like:
  *
  * {[ 
  *    let ch = (as above) in
  *    let rec_ch = new Ftp_data_endpoint.write_out_record_channel
  *                       ~repr:(`ASCII_unix `Enc_iso88591)
  *                       ch
  *    ...
  *    ... ~store:(fun _ -> `Record_structure rec_ch)
  * ]}
  *
  * Here, the end-of-record is transcoded to newline. Note that the ASCII
  * variant ([`Enc_iso88591]) is ignored by [write_out_record_channel].
  * {b Open: How to select record structure using an FTP method.}
  *
  * Character conversions: To convert an EBCDIC file to ASCII, use
  * something like
  *
  * {[ 
  *    let ch = (as above) in
  *    let converter = new Ftp_data_endpoint.data_converter
  *                         ~fromrepr:(`EBCDIC `Enc_cp1047)
  *                         ~torepr:(`ASCII_unix `Enc_iso88591) in
  *    let ch_ebcdic = new Netchannels.output_filter converter ch in
  *    ...
  *    ... ~representation:(`EBCDIC None)
  *    ... ~store:(fun _ -> `File_structure ch_ebcdic)
  * ]}
  *
  * The class [data_converter] also performs the transformation of the 
  * end-of-line convention, unlike the similar class
  * [Netconversion.conversion_pipe].
 *)

(** {1 Debugging} *)

module Debug : sig
  val enable : bool ref
    (** Enables {!Netlog}-style debugging of this module  *)
end
