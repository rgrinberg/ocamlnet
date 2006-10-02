(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Netcgi_env
open Netcgi_types

(** Classical CGI implementation
 *
 * For in introduction, see the guide "Introduction into OcamlNet".
 *)


class simple_argument : 
  ?ro:bool -> string -> string -> cgi_argument
  (** [new simple_argument name value]: Creates an unstructured CGI
   * argument called [name] with contents [value].
   *
   * @param ro If [true], the argument will be read-only. By default,
   *   the argument is enabled for read/write.
   *)

class mime_argument :
  ?work_around_backslash_bug:bool ->  (* default: true *)
  string ->                           (* name *)
  Netmime.mime_message ->
    cgi_argument
  (** [new mime_argument name msg]: Creates a MIME-structured CGI
   * argument called [name] with contents [msg].
   *
   * You can create [msg] by either [Netmime.memory_mime_message] or
   * [Netmime.file_mime_message].
   *
   * @param work_around_backslash_bug Whether to work around a bug
   *   found in almost all browsers regarding the treatment of
   *   backslashes. The browsers do not quote backslashes in file
   *   names. This breaks RFC standards, however. This argument
   *   is [true] by default.
   *)



type argument_processing = 
  [ `Memory
  | `File
  | `Automatic ]
  (** How to process CGI arguments:
   * - [`Memory]: Keep the value of the argument in memory
   * - [`File]: Store the value of the argument in an external file
   * - [`Automatic]: If the argument is structured and carries a file name,
   *   the value will be stored in a file; otherwise it is loaded into
   *   memory. (Note: The meaning of [`Automatic] changed in OcamlNet 0.92.)
   *)


type operating_type = 
  [ `Direct of string (* separator *)
  | `Transactional of 
        (cgi_config -> Netchannels.out_obj_channel -> 
	                                     Netchannels.trans_out_obj_channel)
  ]
  (** The operating type determines how generated data are buffered.
   * - [`Direct sep]: Data written to the output channel of the activation
   *   object is not collected in a transaction buffer, but directly sent to
   *   the browser (the normal I/O buffering is still active, however,
   *   so call [flush] to ensure data are really sent). The method
   *   [commit_work] of the output channel is the same as [flush]. The method
   *   [rollback_work] causes that
   *   the string [sep] is sent, meant as a separator between the already
   *   generated output, and the now following error message.
   * - [`Transactional f]: A transactional channel [tc] is created from the
   *   real output channel [ch] by calling [f cfg ch] (here, [cfg] is
   *   the CGI configuration). The channel [tc] is propagated as the
   *   output channel of the activation object. This means that the
   *   methods [commit_work] and [rollback_work] are implemented by
   *   [tc], and the intended behaviour is that data is buffered in a
   *   special transaction buffer until [commit_work] is called.
   *   This invocation forces the buffered data to be sent to the 
   *   browser. If, however, [rollback_work] is called, the buffer is
   *   cleared.
   *
   * Two important examples for [`Transactional]:
   * - [`Transactional(fun cfg ch -> new Netchannels.buffered_output_channel ch)]:
   *   The transaction buffer is implemented in memory
   * - [`Transactional(fun cfg ch -> new Netchannels.tempfile_output_channel ch)]:
   *   The transaction buffer is implemented as an external file
   *)


class std_activation : 
  ?env:cgi_environment ->
  ?processing:(string -> Netmime.mime_header -> argument_processing) ->   
                                         (* default: (fun _ _ -> `Memory) *)
  ?operating_type:operating_type ->                (* default: `Direct "" *)
  unit ->
    cgi_activation
  (** This class is an implementation of classical CGI. When the object
   * is created, the CGI arguments are read from the input channel, and
   * it is arranged that the generated page is printed to the output channel.
   * If [env] is
   * not explicitly passed, the [stdin] descriptor is used as input,
   * and the [stdout] descriptor is used as output channel (conforming
   * to the CGI standard); depending on the process environment variables,
   * either the real CGI connector is activated, or the class falls back
   * to a test mode where the user can interactively test the CGI
   * application.
   *
   * By passing [env], the class can be configured
   * differently, using other channels for I/O, or other sources for
   * the CGI environment variables.
   *
   * The argument [processing] determines how the CGI argument objects
   * are created (and where). The default is [fun _ _ -> `Memory].
   *
   * The argument [operating_type] determines whether an additional
   * transaction buffer is created. By default, no such buffer is
   * created ([`Direct ""]).
   *
   * Example:
   * {[
   * let cgi = new std_activation() in
   * let foo_arg = cgi # argument_value "foo" in
   * cgi # set_header ~content_type:"text/plain"();
   * cgi # output # output_string ("foo = " ^ foo_arg);
   * cgi # commit_work();    (* implies "flush" *)
   * ]}
   *
   * By default, the class only processes POST data encoded as
   * "application/x-www-form-urlencoded" and "multipart/form-data".
   * If the configuration of the environment permits it, data
   * of other types are accepted, too, and one argument "BODY"
   * is created containing the unparsed data.
   *)

(* THE FOLLOWING OPTION HAS BEEN WITHDRAWN:
 *
 * ~decl: If the list contains [ "*", Optional ], all arguments not
 *    mentioned in the list are treated as optional. If the list contains
 *    [ "*", Default _ ], all arguments not mentioned in the list have
 *    this default value. Without "*" declaration, unknown arguments
 *    are rejected.
 *    The default is [ "*", Optional ].
 *)

val buffered_transactional_optype : operating_type 
  (** A predefined transactional [operating_type] using a [Buffer.t] to store
   * the not yet completed transaction.
   *)

val tempfile_transactional_optype : operating_type
  (** A predefined transactional [operating_type] using a temporary file to store
   * the not yet completed transaction
   *)
  


class custom_activation :
  ?env:cgi_environment ->
  ?args:cgi_argument list ->                    (* default: [] *)
  ?meth:request_method ->                       (* default: `GET *)
  ?operating_type:operating_type ->             (* default: Direct *)
  unit ->
    cgi_activation
  (** This class can be used to implement a non-standard connector that
   * has the same output format as CGI. The CGI arguments, however,
   * are not extracted from the input channel, but simply passed
   * as [args] to this class. The input channel of the environment
   * is never used.
   *
   * The class does not modify any property of the environment, and it does
   * not check whether the environment is compatible with the passed method
   * and arguments. This is up to the user.
   *
   * The purpose of the custom activation class is that CGI environments
   * can be created that do not communicate over stdin/stdout, but use other
   * means to get their input and to deliver their output. You could do
   * that by providing a customized [cgi_environment] as well, but this
   * alternate solution might be ineffective because CGI arguments must be 
   * encoded for the only purpose to be immediately decoded by [std_activation].
   * When using [custom_activation], one can bypass the environment, and
   * set the CGI arguments directly.
   *
   * Note: Unlike [std_activation], this class sets the set of current arguments
   * and the set of initial arguments to the same list. This means: if you
   * modify an argument directly ([set_value], for instace) this will change
   * both sets. It is recommended to pass only read-only arguments to
   * this class in order to avoid this surprising behaviour.
   *
   * @param env The default environment is to first try a classical
   *   CGI environment, and if that fails, to fall back to a test
   *   environment.
   * @param args The list of CGI arguments that will be available in the
   *   CGI activation object
   * @param meth The assumed HTTP method. The method is not extracted from
   *   the environment, but taken from this argument. Defaults to [`GET].
   * @param operating_type See [std_activation].
   *)
