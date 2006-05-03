(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


(** Object-oriented I/O: Basic types and classes 
 *
 * {b Contents}
 * 
 * - {!Netchannels.types}
 * - {!Netchannels.input}
 * - {!Netchannels.output}
 * - {!Netchannels.delegation}
 * - {!Netchannels.lifting}
 * - {!Netchannels.descriptors}
 * - {!Netchannels.transactional}
 * - {!Netchannels.filters}
 *   {ul {- {!Netchannels.filters_notes}}}
 * - {!Netchannels.tutorial}
 *)

(** {1:types Types} *)

(* ***************************** Types ******************************** *)

(** There are three levels of class types for channels:
 *  - [rec_in_channel] and [rec_out_channel]: Primitive, but standardized level
 *  - [raw_in_channel] and [raw_out_channel]: Unix level
 *  - [in_obj_channel] and [out_obj_channel]: Application level
 *
 * The "rec" level has been recently introduced to improve interoperability
 * with other libraries (e.g. camomile). The idea is to standardize the
 * real core methods of I/O, so they have the same meaning in all libraries.
 * Read
 * "{{:http://www.ocaml-programming.de/rec/IO-Classes.html}Basic I/O class types}"
 * for more.
 *
 * The "raw" level represents the level of Unix file descriptors.
 *
 * The application level is what should be used in programs. In addition
 * to the "raw" level one can find a number of convenience methods,
 * e.g. [input_line] to read a line from the channel. The downside is that
 * these methods usually work only for blocking I/O.
 *
 * One can lower the level by coercion, e.g. to turn an [in_obj_channel]
 * into a [rec_in_channel], apply the function
 *
 * [(fun ch -> (ch : in_obj_channel :> rec_in_channel))]
 *
 * To higher the level, apply [lift_in] or [lift_out], defined below.
 *)

(** {b Interface changes:} Since ocamlnet-0.98, the semantics of
 * the methods [input] and [output] has slightly changed. When the end
 * of the channel is reached, [input] raises now [End_of_file]. In previous
 * releases of ocamlnet, the value 0 was returned. When the channel cannot
 * process data, but is in non-blocking mode, both methods now return the
 * value 0. In previous releases of ocamlnet, the behaviour was not
 * defined.
 *)

exception Closed_channel
  (** Raised when channel operations are called when the channel is closed *)

exception Buffer_underrun
  (** Raised by input methods if the internal buffer of the channel is too
   * empty to read even one byte of data.
   * This exception is only used by certain implementations of channel
   * classes.
   *)

exception Command_failure of Unix.process_status
  (** Raised by [close_in] or [close_out] if the channel is connected with
   * another process, and the execution of that process fails.
   *)

(** Recommended input class type for library interoperability. *)
class type rec_in_channel = object
  (** Description
   * 
   * This class type is defined in 
   * "{{:http://www.ocaml-programming.de/rec/IO-Classes.html}Basic I/O class types}"
   * as collaborative effort of several library creators.
   *)

  method input : string -> int -> int -> int
    (** Reads octets from the channel and puts them into the string. The
     * first [int] argument is the position of the substring, and the second
     * [int] argument is the length of the substring where the data are
     * stored. The method returns the number of octets actually read and
     * stored.
     *
     * When the end of the channel is reached and there is no further octet
     * to read, the exception [End_of_file] will be raised. {b This has
     * been changed in ocamlnet-0.97! In previous releases the number 0 
     * was returned at the end of the channel.}
     *
     * When the channel is non-blocking, and there are currently no bytes
     * to read, the number 0 will be returned. {b This has
     * been changed in ocamlnet-0.97! In previous releases this behaviour
     * was undefined.}
     *
     * When the channel is closed, the exception [Closed_channel] will be
     * raised if an ocamlnet implementation is used. For implementations
     * of other libraries there is no standard for this case.
     *)

  method close_in : unit -> unit
    (** Closes the channel for input.
     *
     * When the channel is already closed, the exception [Closed_channel] will
     * be raised if an ocamlnet implementation is used. For implementations
     * of other libraries there is no standard for this case.
     *)
end

(** Basic Unix-level class type for input channels as used by ocamlnet. In addition
   * to the recommended standard, ocamlnet always support a position counter
   *)
class type raw_in_channel = object
  inherit rec_in_channel
  method pos_in : int
    (** Returns the current channel position. This position can be expected
     * to be consistent with the returned number of bytes of [input], i.e.
     * when [input] returns [n], the position is advanced by [n].
     *
     * As seek operations are outside the scope of [Netchannels], 
     * implementations may or may not take seek operations into account.
     *)
end

(** Recommended output class type for library interoperability. *)
class type rec_out_channel = object
  (** Description
   *
   * This class type is defined in 
   * "{{:http://www.ocaml-programming.de/rec/IO-Classes.html}Basic I/O class types}"
   * as collaborative effort of several library creators.
   *)

  method output : string -> int -> int -> int
    (** Takes octets from the string and writes them into the channel. The
     * first [int] argument is the position of the substring, and the second
     * [int] argument is the length of the substring where the data can
     * be found. The method returns the number of octets actually written.
     *
     * The implementation may choose to collect written octets in a buffer
     * before they actually delivered to the underlying resource. 
     *
     * When the channel is non-blocking, and there are currently no bytes
     * to write, the number 0 will be returned. {b This has
     * been changed in ocamlnet-0.97! In previous releases this behaviour
     * was undefined.}
     *
     * When the channel is closed, the exception [Closed_channel] will be
     * raised if an ocamlnet implementation is used. For implementations
     * of other libraries there is no standard for this case.
     *)
  method flush : unit -> unit
    (** If there is a write buffer, it will be flushed. Otherwise, nothing
     * happens
     *)
  method close_out : unit -> unit
    (** Flushes the buffer, if any, and closes the channel for output.
     *
     * When the channel is already closed, the exception [Closed_channel] will
     * be raised if an ocamlnet implementation is used. For implementations
     * of other libraries there is no standard for this case.
     *)
end

(** Basic Unix-level class type for output channels as used by ocamlnet. In addition
 * to the recommended standard, ocamlnet always support a position counter
 *)
class type raw_out_channel = object
  inherit rec_out_channel
  method pos_out : int
    (** Returns the current channel position. This position can be expected
     * to be consistent with the returned number of bytes of [output], i.e.
     * when [output] returns [n], the position is advanced by [n].
     *
     * As seek operations are outside the scope of [Netchannels], 
     * implementations may or may not take seek operations into account.
     *)
end

(** A channel supporting both input and output. The input and output
 * aspects are strictly separated
 *)
class type raw_io_channel = object
  inherit raw_in_channel
  inherit raw_out_channel
end

(** Further methods usually supported by ocamlnet channel implementations.
 * These methods are only reasonable when the channel is of blocking type,
 * i.e. waits for input when not enough data are available to perform an
 * operation. Implementations may choose to fail when they detect the
 * channel is non-blocking.
 *)
class type compl_in_channel = object

  method really_input : string -> int -> int -> unit
    (** Reads exactly as many octets from the channel as the second [int]
     * argument specifies. The octets are placed at the position denoted
     * by the first [int] argument into the string.
     *
     * When the end of the channel is reached before the passed number of
     * octets are read, the exception [End_of_file] is raised.
     *)

  method input_char : unit -> char
    (** Reads exactly one character from the channel, or raises [End_of_file]
     *)

  method input_line : unit -> string
    (** Reads the next line from the channel. When the channel is already
     * at the end before [input_line] is called, the exception [End_of_file]
     * is raised.
     *)

  method input_byte : unit -> int
    (** Reads exactly one octet from the channel and returns its code, 
     * or raises [End_of_file]
     *)

end

(** The application-level input channel supports raw and complemented methods *)
class type in_obj_channel = object
  inherit raw_in_channel
  inherit compl_in_channel
end

(** Further methods usually supported by ocamlnet channel implementations.
 * These methods are only reasonable when the channel is of blocking type,
 * i.e. waits for output readiness when the underlying resource currently
 * cannot process enough data. Implementations may choose to fail when they
 * detect the channel is non-blocking.
 *)
class type compl_out_channel = object
  method really_output : string -> int -> int -> unit
    (** Writes exactly as many octets to the channel as the second [int]
     * argument specifies. The octets are taken from the string position 
     * denoted by the first [int] argument.
     *)
  method output_char : char -> unit
    (** Writes exactly one character *)
  method output_string : string -> unit
    (** Writes exactly the passed string *)
  method output_byte : int -> unit
    (** Writes exactly one byte passed as integer code *)
  method output_buffer : Buffer.t -> unit
    (** Writes exactly the contents of the buffer *)
  method output_channel : ?len:int -> in_obj_channel -> unit
    (** Writes the contents of an [in_obj_channel] until the end of the
     * input channel is reached.
     *
     * @param len If passed, at most this number of octets are read from
     * the input channel and written to this channel.
     *)
end


(** The application-level output channel supports raw and complemented methods *)
class type out_obj_channel = object
  inherit raw_out_channel
  inherit compl_out_channel
end


(** A channel supporting both input and output. The input and output
 * aspects are strictly separated
 *)
class type io_obj_channel = object
  inherit in_obj_channel
  inherit out_obj_channel
end


(** A transactional output channel has a buffer for uncommitted data.
 * This means that all data written to this channel is collected in the
 * buffer until either [commit_work] or [rollback_work] is called.
 *
 * When the channel is closed, the buffer may optionally be committed.
 * This is implementation-defined.
 *
 * The method [flush] does not have any effect on the transaction
 * buffer.
 *)
class type trans_out_obj_channel = object

  inherit out_obj_channel

  method commit_work : unit -> unit
    (** Flushes the transaction buffer, and writes its contents to the
     * underlying resource.
     *)

  method rollback_work : unit -> unit
    (** Empties the transaction buffer *)
end


(* ***************************** Input channels *********************** *)

(** {1:input Input channels} *)

class input_channel :
  in_channel ->
    in_obj_channel
  (** Creates an input channel from an [in_channel], which must be open.
   *
   * The method [pos_in] reflects the real position in the channel as
   * returned by [Pervasives.pos_in]. This works for both seekable and
   * non-seekable channels.
   *
   * The method [close_in] also closes the underlying [in_channel].
   *)


class input_command : 
  string ->
    in_obj_channel
  (** Runs the command with [/bin/sh], and reads the data the command prints
   * to stdout. 
   *
   * The method [pos_in] returns the number of read octets.
   *
   * When [close_in] is invoked, the subprocess is [wait]ed for. If the
   * process exits with code 0, the method returns normally. Otherwise,
   * the exception [Command_failure] is raised.
   *)


class input_string :
  ?pos:int -> ?len:int -> string ->
    in_obj_channel
  (** Creates an input channel from a (constant) string. 
   *
   * The method [pos_in] reflects the real position in the string, i.e.
   * a character read at position [k] can be found at [s.[k]] in the string
   * [s].
   *
   * @param pos The data of the channel begins at this position of the string.
   *   Default: 0
   * @param len The data of the channel consists of this number of bytes.
   *   Default: until the end of the string
   *)


val create_input_netbuffer :
  Netbuffer.t ->
    in_obj_channel   *   (* shutdown: *) (unit -> unit)
  (** Creates an input channel and a shutdown function for a netbuffer. 
   * This is a destructive
   * implementation: Every time data is read, the octets are taken from the
   * beginning of the netbuffer, and they are deleted from the netbuffer
   * (recall that a netbuffer works like a queue of characters).
   *
   * Conversely, the user of this class may add new data to the netbuffer 
   * at any time. When the shutdown function is called, the EOF condition
   * is recorded, and no further data must be added.
   *
   * If the netbuffer becomes empty, the input methods raise [Buffer_underrun]
   * when the EOF condition has not yet been set, and they raise
   * [End_of_file] when the EOF condition has been recorded.
   *)

val lexbuf_of_in_obj_channel : in_obj_channel -> Lexing.lexbuf
  (** Creates a lexical buffer from an input channel. The input channel
   * is not closed when the end is reached
   *
   * This function does not work for non-blocking channels.
   *)

val string_of_in_obj_channel : in_obj_channel -> string
  (** Reads from the input channel until EOF and returns the characters
   * as string. The input channel is not closed.
   *
   * This function does not work for non-blocking channels.
   *)

val with_in_obj_channel : 
  (#in_obj_channel as 'a) -> ('a -> 'b) -> 'b
  (** [with_in_obj_channel ch f]:
   * Computes [f ch] and closes [ch]. If an exception happens, the channel is
   * closed, too.
   *)


(* *************************** Output channels ************************ *)

(** {1:output Output channels} *)

class output_channel :
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  out_channel ->
    out_obj_channel
  (** Creates an output channel writing into an [out_channel].
   *
   * The method [pos_out] reflects the real position in the channel as
   * returned by [Pervasives.pos_out]. This works for both seekable and
   * non-seekable channels.
   *
   * The method [close_out] also closes the underlying [out_channel].
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying [out_channel] has been closed.
   *)


class output_command : 
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  string ->
    out_obj_channel
  (** Runs the command with [/bin/sh], and data written to the channel is
   * piped to stdin of the command.
   *
   * The method [pos_out] returns the number of written octets.
   *
   * When [close_out] is invoked, the subprocess is [wait]ed for. If the
   * process exits with code 0, the method returns normally. Otherwise,
   * the exception [Command_failure] is raised.
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying descriptor has been closed.
   *)


class output_buffer :
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  Buffer.t ->
    out_obj_channel
  (** This output channel writes the data into the passed buffer.
   *
   * The method [pos_out] returns the number of written octets.
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying descriptor has been closed.
   *)

class output_netbuffer :
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  Netbuffer.t ->
    out_obj_channel
  (** This output channel writes the data into the passed netbuffer.
   *
   * The method [pos_out] returns the number of written octets.
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying descriptor has been closed.
   *)

class output_null :
  ?onclose:(unit -> unit) ->              (* default: fun _ -> () *)
  unit ->
    out_obj_channel
  (** This output channel discards all written data. 
   *
   * The method [pos_out] returns the number of discarded bytes.
   *
   * @param onclose this function is called when the [close_out] method is
   * invoked, just after the underlying descriptor has been closed.
   *)

val with_out_obj_channel : 
  (#out_obj_channel as 'a) -> ('a -> 'b) -> 'b
  (** [with_out_obj_channel ch f]:
   * Computes [f ch] and closes [ch]. If an exception happens, the channel is
   * closed, too.
   *)


(* ********************* Delegation *********************************** *)

(** {1:delegation Delegation classes} *)

(** Delegation classes just forward method calls to an parameter
 * object, i.e. when method [m] of the delegation class is called,
 * the definition of [m] is just to call the method with the same
 * name [m] of the parameter object. This is very useful in order
 * to redefine methods individually.
 *
 * For example, to redefine the method [pos_in] of an [in_obj_channel],
 * use
 * {[
 * class my_channel = object(self)
 *   inherit in_obj_channel_delegation ...
 *   method pos_in = ...
 * end
 * ]}
 *
 * As a special feature, the following delegation classes can suppress
 * the delegation of [close_in] or [close_out], whatever applies.
 * Just pass [close:false] to get this effect, e.g.
 * {[
 * class input_channel_don't_close c =
 *   in_obj_channel_delegation ~close:false (new input_channel c)
 * ]}
 * This class does not close [c : in_channel] when the [close_in]
 * method is called.
 *)

class rec_in_channel_delegation : ?close:bool -> rec_in_channel ->
  rec_in_channel

class raw_in_channel_delegation : ?close:bool -> raw_in_channel ->
  raw_in_channel

class in_obj_channel_delegation : ?close:bool -> in_obj_channel ->
  in_obj_channel

class rec_out_channel_delegation : ?close:bool -> rec_out_channel ->
  rec_out_channel

class raw_out_channel_delegation : ?close:bool -> raw_out_channel ->
  raw_out_channel

class out_obj_channel_delegation : ?close:bool -> out_obj_channel ->
  out_obj_channel


(* ********************* Raw channels ********************************* *)

(** {1:lifting Lifting channels} *)

(** The following classes and functions add missing methods to reach
 * a higher level in the hierarchy of channel class types. For most
 * uses, the [lift_in] and [lift_out] functions work best.
 *)

val lift_in :
      ?eol:string list ->
      ?buffered:bool ->
      ?buffer_size:int ->
      [ `Rec of rec_in_channel | `Raw of raw_in_channel ] ->
      in_obj_channel
  (** Turns a [rec_in_channel] or [raw_in_channel], depending on the passed
   * variant, into a full [in_obj_channel] object. (This is a convenience
   * function, you can also use the classes below directly.) If you
   * want to define a class for the lifted object, use
   * {[
   * class lifted_ch ... =
   *   in_obj_channel_delegation (lift_in ...)
   * ]}
   *
   * @param eol The accepted end-of-line delimiters. The method 
   *   [input_line] recognizes any of the passed strings as EOL
   *   delimiters. When more than one delimiter matches, the longest
   *   is taken. Defaults to [ ["\n"] ]. The default cannot be
   *   changed when [buffered=false] (would raise [Invalid_argument]).
   *   The delimiter strings must neither be empty, nor longer than
   *   [buffer_size].
   * @param buffered Whether a buffer is added, by default {b true}
   * @param buffer_size The size of the buffer, if any, by default 4096
   *)

val lift_out :
      ?buffered:bool ->
      ?buffer_size:int ->
      [ `Rec of rec_out_channel | `Raw of raw_out_channel ] ->
      out_obj_channel
  (** Turns a [rec_out_channel] or [raw_out_channel], depending on the passed
   * variant, into a full [out_obj_channel] object. (This is a convenience
   * function, you can also use the classes below directly.) If you
   * want to define a class for the lifted object, use
   * {[
   * class lifted_ch ... =
   *   out_obj_channel_delegation (lift_out ...)
   * ]}
   *
   * @param buffered Whether a buffer is added, by default {b true}
   * @param buffer_size The size of the buffer, if any, by default 4096
   *)

(** This class implements the methods from [compl_in_channel] by calling
 * the methods of [raw_in_channel]. There is no additional buffering.
 * The performance of the method [input_line] is very bad (consider
 * to override it, e.g. by [enhanced_input_line] as defined below).
 *)
class virtual augment_raw_in_channel :
object
  inherit compl_in_channel
  method virtual input : string -> int -> int -> int
    (** As in [raw_in_channel] *)
  method virtual close_in : unit -> unit
    (** As in [raw_in_channel] *)
  method virtual pos_in : int
    (** As in [raw_in_channel] *)
end


class lift_rec_in_channel : ?start_pos_in:int -> rec_in_channel -> in_obj_channel
(** This class implements [pos_in] and the methods from [compl_in_channel] 
 * by calling the methods of [rec_in_channel]. 
 * There is no additional buffering.
 *
 * The performance of the method [input_line] is very bad (consider
 * to override it, e.g. by [enhanced_input_line] as defined below).
 *
 * The method [pos_in] is implemented by counting the number of octets
 * read by the [input] method.
 *
 * @param start_pos_in The initial value of the counter for [pos_in].
 *   Defaults to 0.
 *)

(** This class implements the methods from [compl_out_channel] by calling
 * the methods of [raw_out_channel]. There is no additional buffering.
 *)
class virtual augment_raw_out_channel :
object
  inherit compl_out_channel
  method virtual output : string -> int -> int -> int
    (** As in [raw_out_channel] *)
  method virtual close_out : unit -> unit
    (** As in [raw_out_channel] *)
  method virtual flush : unit -> unit
    (** As in [raw_out_channel] *)
  method virtual pos_out : int
    (** As in [raw_out_channel] *)
end


class lift_raw_out_channel : raw_out_channel -> out_obj_channel
(** This class implements the methods from [compl_out_channel] by calling
 * the methods of [raw_out_channel]. There is no additional buffering.
 *)


class lift_rec_out_channel : 
         ?start_pos_out:int -> rec_out_channel -> out_obj_channel
(** This class implements [pos_out] and the methods from [compl_out_channel] 
 * by calling the methods of [rec_out_channel]. 
 * There is no additional buffering.
 *
 * The method [pos_out] is implemented by counting the number of octets
 * read by the [output] method.
 *
 * @param start_pos_out The initial value of the counter for [pos_out].
 *   Defaults to 0.
 *)


type input_result =
    [ `Data of int
    | `Separator of string
    ]
(** This type is for the method [enhanced_input] of [enhanced_raw_in_channel].
 * - [`Data n] means that [n] bytes have been copied to the target string
 * - [`Separator s] means that no bytes have been copied, but that an
 *   end-of-line separator [s] has been found
 *)


(** Defines private methods reading text line by line *)
class type enhanced_raw_in_channel =
object 
  inherit raw_in_channel
  method private enhanced_input_line : unit -> string
    (** An improved implementation of [input_line] that uses the buffer *)
  method private enhanced_input : string -> int -> int -> input_result
    (** Works similar to [input], but distinguishes between normal data
     * and end-of-line separators. The latter are returned as
     * [`Separator s]. When normal data is found, it is copied to the
     * string, and [`Data n] is returned to indicate that [n] bytes
     * were copied.
     *)
end


class buffered_raw_in_channel : 
        ?eol:string list ->
        ?buffer_size:int ->     (* default: 4096 *)
	raw_in_channel ->
	  enhanced_raw_in_channel
  (** This class adds a buffer to the underlying [raw_in_channel].
   * As additional feature, the method [enhanced_input_line] is a fast
   * version of [input_line] that profits from the buffer.
   *
   * @param eol The accepted end-of-line delimiters. The method 
   *   [enhanced_input_line] recognizes any of the passed strings as EOL
   *   delimiters. When more than one delimiter matches, the longest
   *   is taken. Defaults to [ ["\n"] ]. Note that [input_line]
   *   always only recognizes ["\n"] as EOL character, this cannot
   *   be changed.
   *   The delimiter strings must neither be empty, nor longer than
   *   [buffer_size].
   * @param buffer_size The size of the buffer, by default 4096.
   *)

class buffered_raw_out_channel : 
        ?buffer_size:int ->     (* default: 4096 *)
	raw_out_channel ->
	  raw_out_channel
  (** This class adds a buffer to the underlying [raw_out_channel]. 
   *
   * @param buffer_size The size of the buffer, by default 4096.
   *)



(* ********************** Channels over descriptors ******************* *)

(** {1:descriptors Channels over descriptors} *)

class input_descr :
  ?start_pos_in:int ->
  Unix.file_descr ->
    raw_in_channel
  (** Creates a [raw_in_channel] for the passed file descriptor, which must
   * be open for reading. 
   *
   * The [pos_in] method returns logical positions, i.e. it counts the number
   * of read octets. It is not tried to determine the real file position.
   *
   * The method [close_in] also closes the file descriptor.
   *
   * @param start_pos_in The position to which [pos_in] is initialized when
   * the channel is created, by default 0
   *)


class output_descr :
  ?start_pos_out:int ->
  Unix.file_descr ->
    raw_out_channel
  (** Creates a [raw_out_channel] for the passed file descriptor, which must
   * be open for writing. 
   *
   * The [pos_out] method returns logical positions, i.e. it counts the number
   * of written octets. It is not tried to determine the real file position.
   *
   * The method [close_out] also closes the file descriptor.
   *
   * @param start_pos_out The position to which [pos_out] is initialized when
   * the channel is created, by default 0
   *)

class socket_descr :
  ?start_pos_in:int ->
  ?start_pos_out:int ->
  Unix.file_descr ->
    raw_io_channel
  (** Creates a [raw_io_channel] for the passed socket descriptor, which must
   * be open for reading and writing, and not yet shut down in either
   * direction. The [raw_io_channel] is used to represent a bidirectional
   * channel: [close_out] shuts the socket down for sending, [close_in]
   * shuts the socket down for reading, and when both directions are down,
   * the descriptor is closed.
   *
   * The [pos_in] and [pos_out] methods returns logical positions.
   *
   * @param start_pos_in The position to which [pos_in] is initialized when
   * the channel is created, by default 0
   * @param start_pos_out The position to which [pos_out] is initialized when
   * the channel is created, by default 0
   *)


(* ********************* Transactional output channels **************** *)

(** {1:transactional Transactional channels} *)

type close_mode = [ `Commit | `Rollback ]
  (** Whether a [close_out] implies a commit or rollback operation *)

class buffered_trans_channel :
  ?close_mode:close_mode ->
  out_obj_channel ->
    trans_out_obj_channel
  (** A transactional output channel with a transaction buffer implemented
   * in memory
   *
   * @param close_mode Specifies the semantics of [close_out], by default
   * [`Commit]
   *)

val make_temporary_file : 
  ?mode:int -> ?limit:int -> ?tmp_directory:string -> ?tmp_prefix:string -> 
  unit ->
    (string * in_channel * out_channel)
  (** Creates a temporary file in the directory [tmp_directory] with a name
   * prefix [tmp_prefix] and a unique suffix. The function returns 
   * the triple (name, inch, outch) containing the file [name],
   * the file opened as in_channel [inch] and as out_channel [outch].
   *
   * @param tmp_directory By default the current directory
   * @param tmp_prefix By default ["netstring"]. It is better to have a prefix
   *   that is likely to be unique, e.g. the process ID, or the current time.
   * @param mode The creation mask of the file; defaults to 0o600, i.e. the
   *   file is private for the current user
   * @param limit Limits the number of trials to find the unique suffix.
   *   Defaults to 1000.
   *)

class tempfile_trans_channel :
  ?close_mode:close_mode ->
  ?tmp_directory:string ->
  ?tmp_prefix:string ->
  out_obj_channel ->
    trans_out_obj_channel
  (** A transactional output channel with a transaction buffer implemented
   * as temporary file
   *
   * @param close_mode Specifies the semantics of [close_out], by default
   *   [`Commit]
   * @param tmp_directory See [make_temporary_file]
   * @param tmp_prefix See [make_temporary_file]
   *)



(* ************************ Pipes and filters ************************* *)

(** {1:filters Pipes and Filters} *)

(** Note that this has nothing to do with "pipes" on the Unix level.
 * It is, however, the same idea: Connecting two I/O resources with an
 * intermediate buffer.
 *)

class pipe :
  ?conv:(Netbuffer.t -> bool -> Netbuffer.t -> unit) ->
  unit ->
    io_obj_channel
  (** A [pipe] has two internal buffers (realized by Netbuffer). The
   * output methods of the class write to the incoming buffer. When
   * new data are appended to the incoming buffer, the conversion function
   * [conv] is called; the arguments are the incoming buffer and the outgoing
   * buffer. The conversion function must convert the data available in the
   * incoming buffer and append the result to the outgoing buffer. Finally,
   * the input methods of the class return the data found in the outgoing
   * buffer.
   *
   * The conversion function is called as follows:
   * [conv incoming_buffer at_eof outgoing_buffer]
   *
   * The conversion function is allowed to do nothing if the incoming data
   * are not complete enough to be converted. It is also allowed to convert
   * only the beginning of the incoming buffer.
   *
   * If the outgoing buffer is empty, the input methods will raise
   * [Buffer_underrun].
   *
   * If [close_out] is invoked, the end of the data stream will be recorded.
   * In this case, the conversion function is called with [at_eof = true],
   * and it is expected that this function converts the whole data found
   * in the incoming buffer.
   *
   * [close_in] implies [close_out].
   *
   * The conversion function may raise exceptions. The exceptions will
   * fall through to the caller of the input methods. (The output methods
   * and [close_in], [close_out] never fail because of such exceptions.)
   *
   * The default conversion function copies everything from the incoming
   * buffer to the outgoing buffer without modification.
   *)

class output_filter : io_obj_channel -> out_obj_channel -> out_obj_channel
  (** An [output_filter] filters the data written to it through the
   * [io_obj_channel] (usually a [pipe]), and writes the filtered data
   * to the passed [out_obj_channel].
   *
   * If the filter is closed, the [io_obj_channel] will be closed, too,
   * but not the destination [out_obj_channel] (so you can still append
   * further data).
   *)

class input_filter : in_obj_channel -> io_obj_channel -> in_obj_channel
  (** An [input_filter] filters the data read from it through the
   * [io_obj_channel] (usually a [pipe] after the data have been 
   * retrieved from the passed [in_obj_channel].
   *
   * An [input_filter] object never generates [Buffer_underrun] exceptions.
   * However, if the passed [in_obj_channel] or [io_obj_channel] raises such
   * an exception, the exception will fall through the calling chain.
   *
   * If the filter is closed, the [io_obj_channel] will be closed, too,
   * but not the source [in_obj_channel] (so you can still read further
   * data from it).
   *)

(** {2:filters_notes Notes, Examples} *)

(** If you have the choice, prefer [output_filter] over [input_filter].
 * The latter is slower.
 *
 * The primary application of filters is to encode or decode a channel
 * on the fly. For example, the following lines write a BASE64-encoded file:
 *
 * {[let ch = new output_channel (open_out "file.b64") in
 * let encoder = new Netencoding.Base64.encoding_pipe ~linelength:76 () in
 * let ch' = new output_filter encoder ch in
 * ... (* write to ch' *)
 * ch' # close_out();
 * ch  # close_out();  (* you must close both channels! *)
 * ]}
 *
 * All bytes written to [ch'] are BASE64-encoded and the encoded bytes are
 * written to [ch].
 *
 * There are also pipes to decode BASE64, and to encode and decode the
 * "Quoted printable" format. Encoding and decoding work even if the
 * data is delivered in disadvantageous chunks, because the data is
 * "re-chunked" if needed. For example, BASE64 would require that data
 * arrive in multiples of three bytes, and to cope with that, the BASE64 pipe
 * only processes the prefix of the input buffer that is a multiple of three,
 * and defers the encoding of the extra bytes till the next opportunity.
 *)


(** {1:tutorial Tutorial} 
 *
 * [Netchannels] is one of the basic modules of this library, because it
 * provides some very basic abstractions needed for many other functions
 * of the library. The key abstractions [Netchannels] defines are the types
 * [in_obj_channel] and [out_obj_channel]. Both are class types providing
 * sequential access to byte streams, one for input, one for output.
 * They are comparable to the types [in_channel] and [out_channel] of the
 * standard library that
 * allow access to files. However, there is one fundamental difference:
 * [in_channel] and [out_channel] are restricted to resources that are
 * available through file descriptors, whereas [in_obj_channel] and
 * [out_obj_channel] are just class types, and by providing implementations
 * for them any kind of resources can be accessed.
 *
 * {b Motivation}
 *
 * In some respect, [Netchannels] fixes a deficiency of the standard
 * library. Look at the module [Printf] which defines six variants
 * of the [printf] function:
 * {[
 * val fprintf : out_channel -> ('a, out_channel, unit) format -> 'a
 * val printf : ('a, out_channel, unit) format -> 'a
 * val eprintf : ('a, out_channel, unit) format -> 'a
 * val sprintf : ('a, unit, string) format -> 'a
 * val bprintf : Buffer.t -> ('a, Buffer.t, unit) format -> 'a
 * val kprintf : (string -> string) -> ('a, unit, string) format -> 'a
 * ]}
 * It is possible to write into six different kinds of print targets.
 * The basic problem of this style is that the provider of a service 
 * function like [printf] must define it for every commonly used
 * print target. The other solution is that the provider defines only
 * one version of the service function, but that the caller of the
 * function arranges the polymorphism. A [Netchannels]-aware [Printf]
 * would have only one variant of [printf]:
 * {[
 * val printf : out_obj_channel -> ('a, out_obj_channel, unit) format -> 'a
 * ]}
 * The caller would create the right [out_obj_channel] object for the
 * real print target:
 * {[
 * let file_ch = new output_file (file : out_channel) in
 * printf file_ch ...
 * ]}
 * (printing into files), or:
 * {[
 * let buffer_ch = new output_buffer (buf : Buffer.t) in
 * printf buffer_ch ...
 * ]}
 * (printing into buffers).
 * Of course, this is only a hypothetical example. The point is that
 * this library defines many parsers and printers, and that it is really
 * a simplification for both the library and the user of the library
 * to have this object encapsulation of I/O resources.
 *
 * {b Programming with [in_obj_channel] }
 *
 * For example, let us program a function reading a data source
 * line by line, and returning the sum of all lines which must be integer
 * numbers. The argument [ch] is an open {!Netchannels.in_obj_channel},
 * and the return value is the sum:
 * {[
 * let sum_up (ch : in_obj_channel) =
 *   let sum = ref 0 in
 *   try
 *     while true do
 *       let line = ch # input_line() in
 *       sum := !sum + int_of_string line
 *     done;
 *     assert false
 *   with
 *     End_of_file ->
 *       !sum
 * ]}
 * The interesting point is that the data source can be anything: a channel,
 * a string, or any other class that implements the class type
 * [in_obj_channel]. 
 *
 * This expression opens the file ["data"] and returns the sum of this file:
 * {[
 * let ch = new input_channel (open_in "data") in
 * sum_up ch
 * ]}
 * The class {!Netchannels.input_channel} is an implementation of the type
 * [in_obj_channel] where every method of the class simply calls the
 * corresponding function of the module [Pervasives]. (By the way, it would
 * be a good idea to close the channel afterwards: [ch#close_in()].
 * We will discuss that below.)
 *
 * This expression sums up the contents of a constant string:
 * {[
 * let s = "1\n2\n3\n4" in
 * let ch = new input_string s in
 * sum_up ch
 * ]} 
 * The class {!Netchannels.input_string} is an implementation of the type
 * [in_obj_channel] that reads from a string that is treated
 * like a channel.
 *
 * The effect of using the [Netchannels] module is that the same
 * implementation [sum_up] can be used to read from multiple
 * data sources, as it is sufficient to call the function with different
 * implementations of [in_obj_channel].
 *
 * {b The details of [in_obj_channel] }
 *
 * The properties of any class that implements [in_obj_channel]
 * can be summarized as follows:
 *
 * - After the object has been created ([new]), the
 *   netchannel is open. The netchannel remains open until it is
 *   explicitly closed (method [close_in : unit -> unit]). When you call a
 *   method of a closed netchannel, the exception
 *   [Closed_channel] is raised (even if you try to close the channel again).
 * - The methods
 *   {[ 
 *   really_input : string -> int -> int -> unit
 *   input_char : unit -> char
 *   input_byte : unit -> int
 *   input_line : unit -> string
 *   ]}
 *    work like their counterparts of the standard library. In particular,
 *    the end of file condition is signaled by rasising [End_of_file].
 * - The method
 *   {[
 *   input : string -> int -> int -> int
 *   ]}
 *   works like its counterpart of the standard library, except that the
 *   end of the file is also signaled by [End_of_file], and not by the
 *   return value 0.
 * - The method [pos_in : int] returns the current byte position of 
 *   the channel in a way that is logically consistent with the 
 *   input methods: After reading [n] bytes, the method
 *   must return a position that is increased by [n]. Usually the
 *   position is zero after the object has been created, but this
 *   is not specified. Positions are available even for file
 *   descriptors that are not seekable.
 * - There is intentionally no [seek_in] method.  Seekable channels are
 *   currently out of scope, as netstring focuses on non-seekable channels.
 * 
 * {b Programming with [out_obj_channel] }
 *
 * The following function outputs the numbers of an [int list]
 * sequentially on the passed netchannel:
 * {[ 
 * let print_int_list (ch : out_obj_channel) l =
 *   List.iter
 *     (fun n ->
 *        ch # output_string (string_of_int n);
 *        ch # output_char '\n';
 *     )
 *     l;
 *   ch # flush()
 * ]}
 * The following statements write the output into a file:
 * {[
 * let ch = new output_channel (open_out "data") in
 * print_int_list ch [1;2;3]
 * ]}
 * And these statements write the output into a buffer:
 * {[
 * let b = Buffer.create 16 in
 * let ch = new output_buffer b in
 * print_int_list ch [1;2;3]
 * ]}
 *
 * Again, the caller of the function [print_int_list] determines the
 * type of the output destination, and you do not need several functions
 * for several types of destination.
 *
 * {b The details of [out_obj_channel] }
 *
 * The properties of any class that implements [out_obj_channel]
 * can be summarized as follows:
 *
 * - After the object has been created ([new]), the
 *   netchannel is open. The netchannel remains open until it is
 *   explicitly closed (method [close_out : unit -> unit]). When you call a
 *   method of a closed netchannel, the exception
 *   [Closed_channel] is raised (even if you try to close the channel again).
 * - The methods
 *   {[
 *   output : string -> int -> int -> int
 *   really_output : string -> int -> int -> unit
 *   output_char : char -> unit
 *   output_byte : int -> unit
 *   output_string : string -> unit
 *   ]}
 *   work like their counterparts of the standard library. There is
 *   usually an output buffer, but this is not specified. By calling
 *   [flush : unit -> unit], the contents of the output buffer are
 *   forced to be written to the destination.
 * - The method
 *   {[
 *   output_buffer : Buffer.t -> unit
 *   ]}
 *   works like [Buffer.output_channel], i.e. the contents of the buffer
 *   are printed to the channel.
 * - The method
 *   {[
 *   output_channel : ?len:int -> in_obj_channel -> unit
 *   ]}
 *   reads data from the argument [in_obj_channel] and prints them to
 *   the output channel. By default, the input channel is read until the
 *   EOF position. If the [len] argument is passed, at
 *   most this number of bytes are copied from the input
 *   channel to the output channel. The input channel remains
 *   open in all cases.
 * - The method [pos_out : int] returns byte positions
 *   that are logically consistent: After writing [n] bytes, the method
 *   must return a position that is increased by [n]. Usually the
 *   position is zero after the object has been created, but this
 *   is not specified. Positions are available even for file
 *   descriptors that are not seekable.
 * - There is intentionally no [seek_out] method.
 *   Seekable channels are currently out of scope, as netstring
 *   focuses on non-seekable channels.
 *
 * {b How to close channels}
 *
 * As channels may use file descriptors for their implementation,
 * it is very important that all open channels are closed after they have
 * been used; otherwise the operating system will certainly get out of
 * file descriptors. The simple way,
 * {[
 * let ch = new <channel_class> args ... in
 * ... do something ...
 * ch # close_in() or close_out()
 * ]}
 * is dangerous because an exception may be raised between channel creation
 * and the [close_*] invocation. An elegant solution is to use
 * [with_in_obj_channel] and [with_out_obj_channel], as in:
 * {[
 * with_in_obj_channel             (* or with_out_obj_channel *)
 *   (new <channel_class> ...)
 *   (fun ch ->
 *      ... do something ...
 *   )
 * ]}
 * This programming idiom ensures that the channel is always closed after
 * usage, even in the case of exceptions.
 *
 * Complete examples:
 * 
 * {[
 * let sum = with_in_obj_channel
 *             (new input_channel (open_in "data"))
 *             sum_up ;;
 * ]}
 *
 * {[
 * with_out_obj_channel
 *   (new output_channel (open_out "data"))
 *   (fun ch -> print_int_list ch ["1";"2";"3"]) ;;
 * ]}
 *
 * {b Examples: HTML Parsing and Printing}
 *
 * In the Netstring library there are lots of parsers and printers
 * that accept netchannels as data sources and destinations, respectively. One
 * of them is the {!Nethtml} module providing an HTML parser and printer. A
 * few code snippets how to call them, just to get used to netchannels:
 * {[
 * let html_document =
 *   with_in_obj_channel
 *     (new input_channel (open_in "myfile.html"))
 *     Nethtml.parse ;;
 * with_out_obj_channel
 *   (new output_channel (open_out "otherfile.html"))
 *   (fun ch -> Nethtml.write ch html_document) ;;
 * ]}
 *
 * {b Transactional Output Channels}
 *
 * Sometimes you do not want that generated output is directly sent to the
 * underlying file descriptor, but rather buffered until you know that
 * everything worked fine. Imagine you program a network service, and
 * you want to return the result only when the computations are successful,
 * and an error message otherwise. One way to achieve this effect is
 * to manually program a buffer:
 * {[
 * let network_service ch =
 *   try
 *     let b = Buffer.create 16 in
 *     let ch' = new output_buffer b in
 *     ... computations, write results into ch' ...
 *     ch' # close_out;
 *     ch # output_buffer b
 *   with
 *     error ->
 *       ... write error message to ch ...
 * ]}
 * There is a better way to do this, as there are transactional output
 * channels. This type of netchannels provide a buffer for all written
 * data like the above example, and only if data is explicitly committed
 * it is copied to the real destination. Alternatively, you can also
 * rollback the channel, i.e. delete the internal buffer. The signature
 * of the type [trans_out_obj_channel] is:
 * {[
 * class type trans_out_obj_channel = object
 *   inherit out_obj_channel
 *   method commit_work : unit -> unit
 *   method rollback_work : unit -> unit
 * end
 * ]}
 * They have the same methods as [out_obj_channel] plus
 * [commit_work] and [rollback_work]. There are two
 * implementations, one of them keeping the buffer in memory, and the
 * other using a temporary file:
 * {[
 * let ch' = new buffered_trans_channel ch
 * ]}
 * And:
 * {[
 * let ch' = new tempfile_trans_channel ch
 * ]}
 * In the latter case, there are optional arguments specifiying where the
 * temporary file is created.
 *
 * Now the network service would look like:
 * {[
 * let network_service transaction_provider ch =
 *   try
 *     let ch' = transaction_provider ch in
 *     ... computations, write results into ch' ...
 *     ch' # commit_work();
 *     ch' # close_out()     (* implies ch # close_out() *)
 *   with
 *     error ->
 *       ch' # rollback_work();
 *       ... write error message to ch' ...
 *       ch' # commit_work();
 *       ch' # close_out()   (* implies ch # close_out() *)
 * ]}
 * You can program this function without specifying which of the two
 * implementations is used. Just call this function as
 * {[
 * network_service (new buffered_trans_channel) ch
 * ]}
 * or
 * {[
 * network_service (new tempfile_trans_channel) ch
 * ]}
 * to determine the type of transaction buffer.
 *
 * Some details:
 * - The method [commit_work] copies all uncommitted data
 *   to the underlying channel, and flushes all buffers.
 * - When [rollback_work] is called the uncommitted data are deleted.
 * - The method [flush] does not have any effect.
 * - The reported position adds the committed and the uncommitted
 *   amounts of data. This means that [rollback_work] resets the position
 *   to the value of the last [commit_work] call.
 * - When the transactional channel is closed, the underlying
 *   channel is closed, too. By default, the uncommitted data is deleted, but
 *   the current implementations can optionally commit data in this case.
 *
 * {b Pipes and Filters}
 *
 * The class [pipe] is an [in_obj_channel] and an
 * [out_obj_channel] at the same time (i.e. the class has
 * the type [io_obj_channel]). A pipe has two endpoints, one
 * for reading and one for writing (similar in concept to the pipes provided
 * by the operating system, but note that our pipes have nothing to do
 * with the OS pipes). Of course, you cannot read and write
 * at the same time, so
 * there must be an internal buffer storing the data that have
 * been written but not yet read. How can such a construction be
 * useful? Imagine you have two routines that run alternately,
 * and one is capable of writing into netchannels, and the other
 * can read from a netchannel. Pipes are the missing
 * communication link in this situation, because the writer
 * routine can output into the pipe, and the reader routine can
 * read from the buffer of the pipe. In the following example,
 * the writer outputs numbers from 1 to 100, and the reader sums
 * them up:
 * {[
 * let pipe = new pipe() ;;
 * let k = ref 1 ;;
 * let writer() =
 *   if !k <= 100 then (
 *     pipe # output_string (string_of_int !k);
 *     incr k;
 *     if !k > 100 then pipe # close_out() else pipe # output_char '\n';
 *   ) ;;
 * let sum = ref 0 ;;
 * let reader() =
 *   let line = pipe # input_line() in
 *   sum := !sum + int_of_string line ;;
 * try
 *   while true do
 *     writer();
 *     reader()
 *   done
 * with
 *   End_of_file ->
 *     () ;;
 * ]}
 * The [writer] function prints the numbers into the pipe, and the
 * [reader] function reads them in. By closing only the output end
 * Of the pipe the [writer] signals the end of the stream, and the
 * [input_line] method raises the exception [End_of_file].
 *
 * Of course, this example is very simple. What does happen
 * when more is printed into the pipe than read? The internal
 * buffer grows. What does happen when more is tried to read from
 * the pipe than available? The input methods signal this by
 * raising the special exception
 * [Buffer_underrun]. Unfortunately, handling this exception
 * can be very complicated, as the reader must be able to deal
 * with partial reads.
 *
 * This could be solved by using the {!Netstream} module. A
 * netstream is another extension of [in_obj_channel] that
 * allows one to look ahead, i.e. you can look at the bytes that
 * will be read next, and use this information to decide whether
 * enough data are available or not. Netstreams are explained in
 * another chapter of this manual.
 *
 * Pipes have another feature that makes them useful even for
 * "normal" programming. You can specify a conversion function
 * that is called when data is to be transferred from the writing
 * end to the reading end of the pipe. The module
 * {!Netencoding.Base64} defines such a pipe that converts data: The
 * class [encoding_pipe] automatically encodes all bytes
 * written into it by the Base64 scheme:
 * {[
 * let pipe = new Netencoding.Base64.encoding_pipe() ;;
 * pipe # output_string "Hello World";
 * pipe # close_out() ;;
 * let s = pipe # input_line() ;;
 * ]}
 * [s] has now the value ["SGVsbG8gV29ybGQ="], the encoded
 * form of the input. This kind of pipe has the same interface
 * as the basic pipe class, and the same problems to use it.
 * Fortunately, the Netstring library has another facility
 * simplifying the usage of pipes, namely {b filters}.
 *
 * There are two kinds of filters: The class
 * {!Netchannels.output_filter} redirects data written to an
 * [out_obj_channel] through a pipe, and the class
 * {!Netchannels.input_filter} arranges that data read from an
 * [in_obj_channel] flows through a pipe. An example makes
 * that clearer. Imagine you have a function [write_results]
 * that writes the results of a computation into an
 * [out_obj_channel]. Normally, this channel is simply a
 * file:
 * {[
 * with_out_obj_channel
 *   (new output_channel (open_out "results"))
 *   write_results
 * ]}
 * Now you want that the file is Base64-encoded. This can be
 * arranged by calling [write_results] differently:
 * {[
 * let pipe = new Netencoding.Base64.encoding_pipe() in
 * with_out_obj_channel
 *   (new output_channel (open_out "results"))
 *   (fun ch ->
 *     let ch' = new output_filter pipe ch in
 *     write_results ch';
 *     ch' # close_out()
 *   )
 * ]}
 * Now any invocation of an output method for [ch']
 * actually prints into the filter, which redirects the data
 * through the [pipe], thus encoding them, and finally
 * passing the encoded data to the underlying channel
 * [ch]. Note that you must close [ch'] to ensure
 * that all data are filtered, it is not sufficient to flush
 * output.
 *
 * It is important to understand why filters must be closed to
 * work properly. The problem is that the Base64 encoding
 * converts triples of three bytes into quadruples of four
 * bytes.  Because not every string to convert is a multiple of
 * three, there are special rules how to handle the exceeding
 * one or two bytes at the end. The pipe must know the end of
 * the input data in order to apply these rules correctly. If
 * you only flush the filter, the exceeding bytes would simply
 * remain in the internal buffer, because it is possible that
 * more bytes follow. By closing the filter, you indicate that
 * the definite end is reached, and the special rules for
 * trailing data must be performed. \- Many conversions have
 * similar problems, and because of this it is a good advice to
 * always close output filters after usage.
 *
 * There is not only the class [output_filter] but also
 * [input_filter]. This class can be used to perform
 * conversions while reading from a file. Note that you often do
 * not need to close input filters, because input channels can
 * signal the end by raising [End_of_file], so the mentioned
 * problems usually do not occur.
 *
 * There are a number of predefined conversion pipes:
 * - {!Netencoding.Base64.encoding_pipe}: Performs Base64 encoding
 * - {!Netencoding.Base64.decoding_pipe}: Performs Base64 decoding
 * - {!Netencoding.QuotedPrintable.encoding_pipe}: Performs
 *   QuotedPrintable encoding
 * - {!Netencoding.QuotedPrintable.decoding_pipe}: Performs
 *   QuotedPrintable decoding
 * - {!Netconversion.conversion_pipe}: Converts the character encoding
 *   form charset A to charset B
 * 
 * {b Defining Classes for Object Channels}
 *
 * As subtyping and inheritance are orthogonal in O'Caml, you can
 * simply create your own netchannels by defining classes that match the
 * [in_obj_channel] or [out_obj_channel] types. E.g.
 * {[
 * class my_in_channel : in_obj_channel =
 * object (self)
 *   method input s pos len = ...
 *   method close_in() = ...
 *   method pos_in = ...
 *   method really_input s pos len = ...
 *   method input_char() = ...
 *   method input_line() = ...
 *   method input_byte() = ...
 * end
 * ]}
 *
 * Of course, this is non-trivial, especially for the [in_obj_channel]
 * case. Fortunately, the Netchannels module includes a "construction kit"
 * that allows one to define a channel class from only a few methods.
 * A closer look at [in_obj_channel] and [out_obj_channel]
 * shows that some methods can be derived from more fundamental methods.
 * The following class types include only the fundamental methods:
 *
 * {[
 * class type raw_in_channel = object
 *   method input : string -> int -> int -> int
 *   method close_in : unit -> unit
 *   method pos_in : int
 * end
 * ]}
 * {[
 * class type raw_out_channel = object
 *   method output : string -> int -> int -> int
 *   method close_out : unit -> unit
 *   method pos_out : int
 *   method flush : unit -> unit
 * end
 * ]}
 * 
 * In order to define a new class, it is sufficient to define this
 * raw version of the class, and to lift it to the full functionality.
 * For example, to define [my_in_channel]:
 * {[
 * class my_raw_in_channel : raw_in_channel =
 * object (self)
 *   method input s pos len = ...
 *   method close_in() = ...
 *   method pos_in = ...
 * end
 * class my_in_channel =
 *   in_obj_channel_delegation (lift_in (`Raw(new my_raw_in_channel)))
 * ]}
 *
 * The function {!Netchannels.lift_in} can lift several forms of incomplete
 * channel objects to the full class type [in_obj_channel]. There is also
 * the corresponding function {!Netchannels.lift_out}. Note that lifting
 * adds by default another internal buffer to the channel that must be
 * explicitly turned off when it is not wanted. The rationale for this
 * buffer is that it avoids some cases with extremely poor performance
 * which might be surprising for many users.
 *
 * The class [in_obj_channel_delegation] is just an auxiliary construction
 * to turn the [in_obj_channel] {i object} returned by [lift_in] again
 * into a class.
 *
 * {b Some FAQ}
 *
 * {ul
 * {- {i Netchannels add further layers on top of the
 *    built-in channels or file descriptors. Does this make them
 *    slow?} 
 * 
 *    Of course, Netchannels are slower than the underlying
 *    built-in I/O facilities. There is at least one, but often
 *    even more than one method call until the data is transferred
 *    to or from the final I/O target. This costs time, and it is
 *    a good idea to reduce the number of method calls for maximum
 *    speed. Especially the character- or byte-based method calls
 *    should be avoided, it is better to collect data and pass
 *    them in larger chunks. This reduces the number
 *    of method calls that are needed to transfer a block of
 *    data.
 * 
 *    However, some classes implement buffers themselves, and
 *    data are only transferred when the buffers are full (or
 *    empty). The overhead for the extra method calls is small
 *    for these classes. The classes that implement their own
 *    buffers are the transactional channels, the pipes, and
 *    all the classes with "buffer" in their name.
 *
 *    Netchannels are often stacked, i.e. one netchannel object
 *    transfers data to an underlying object, and this object
 *    passes the data to further objects. Often buffers are
 *    involved, and data are copied between buffers several
 *    times. Of course, these copies can reduce the speed, too.}
 * {- {i Why do Netchannels not support seeking?}
 *
 *    Netchannels were invented to support the implementation of
 *    network protocols. Network endpoints are not seekable.}
 * {- {i What about [printf] and [scanf]?}
 *
 *    In principle, methods for [printf] and [scanf] could be
 *    added to [out_obj_channel] and [in_obj_channel], respectively,
 *    as recent versions of O'Caml added the necessary language
 *    means (polymorphic methods, [kprintf], [kscanf]). However,
 *    polymorphic methods work only well when the type of the
 *    channel object is always annotated (e.g. as 
 *    [(ch : out_obj_channel) # printf ...]), so this is not
 *    that much better than
 *    [ch # output_string (sprintf ...)].}
 * {- {i Can I pass an [in_obj_channel] to an ocamllex-generated
 *    lexer?}
 * 
 *    Yes, just call {!Netchannels.lexbuf_of_in_obj_channel} to turn the
 *    [in_obj_channel] into a [lexbuf].}
 * {- {i Do Netchannels support non-blocking I/O?}
 *
 *    Yes and no. Yes, because you can open a descriptor in
 *    non-blocking mode, and create a netchannel from it. When
 *    the program would block, the [input] and [output] methods return 0
 *    to indicate this. However, the non-raw methods cannot cope
 *    with these situations.}
 * {- {i Do Netchannels support multiplexed I/O?}
 *
 *    No, there is no equivalent to [Unix.select] on the
 *    level of netchannels.}
 * {- {i Can I use Netchannels in multi-threaded programs?}
 *
 *    Yes. However, shared netchannels are not locked, and strange
 *    things can happen when netchannels are used by several threads
 *    at the same time.}
 * {- {i Can I use pipes to communicate between threads?}
 *
 *    This could be made work, but it is currently not the case.
 *    A multithreading-aware wrapper around pipes could do the job.}
 * {- {i Pipes call external programs to do their job, don't they?}
 *
 *    No, they do not call external programs, nor do they need
 *    any parallel execution threads. Pipes are just a tricky way
 *    of organizing buffers.}
 * {- {i How do I define my own conversion pipe?}
 *
 *    Look at the sources [netencoding.ml], it includes several
 *    examples of conversion pipes.}
 * }
 *)


(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.16  2004/07/11 15:40:30  stolpmann
 * 	Added: enhanced_input
 *
 * Revision 1.15  2004/07/10 23:42:34  stolpmann
 * 	New: delegation classes
 * 	New: Arg eol of lift_in
 * 	ocamldoc
 *
 * Revision 1.14  2004/07/04 19:19:01  stolpmann
 * 	Updates ocamldoc
 *
 * Revision 1.13  2004/05/30 21:27:14  stolpmann
 * 	Supporting the standard for class-based I/O. This also means
 * the semantics of [input] and [output] methods have changed (return
 * value 0, End_of_file).
 * 	Removed _rep_in, _rep_out.
 * 	Removed Netstream.Deprecated.
 *
 * Revision 1.12  2002/10/26 20:47:37  stolpmann
 * 	Change: [flush] is now part of [raw_out_channel].
 * 	New: [augment_raw_in_channel], [augment_raw_out-channel],
 * [buffered_raw_in_channel], [buffered_raw_out_channel].
 * 	Fixed: [input_descr], [output_descr], [socket_descr] no longer
 * seek to find out the position.
 *
 * Revision 1.11  2002/10/24 23:33:23  stolpmann
 * 	New class output_null that simply discards any output
 *
 * Revision 1.10  2002/03/15 16:09:31  stolpmann
 * 	Added an example for the filter classes.
 *
 * Revision 1.9  2002/02/02 23:53:06  stolpmann
 * 	New: input_command, output_command
 *
 * Revision 1.8  2002/01/12 18:35:17  stolpmann
 * 	Added ?onclose for some output classes.
 *
 * Revision 1.7  2002/01/06 02:20:13  stolpmann
 * 	New optional arguments ?pos and ?len for class [input_string].
 *
 * Revision 1.6  2002/01/05 22:43:54  stolpmann
 * 	New: classes [input_filter], [output_filter]
 *
 * Revision 1.5  2002/01/02 22:55:09  stolpmann
 * 	Added: [input_netbuffer], [output_netbuffer], [pipe]. New
 * exception [Buffer_underrun].
 *
 * Revision 1.4  2001/12/22 09:17:15  pdoane
 * 	Added raw_*_channel types
 * 	Changed output to really_output for out_obj_channel
 * 	Added file_descr/socket_descr implementations
 * 	Renamed buffered_output_channel and tempfile_output_channel
 *
 * Revision 1.3  2001/09/30 00:01:33  stolpmann
 * 	New function: make_temporary_file.
 * 	For the class tempfile_output_channel: the default tmp_directory
 * is now "." (it is difficult to have a reasonable automatic tmp_directory
 * for the scope of netchannels only)
 * 	with_in/out_obj_channel: It is ok if the channel is already
 * closed.
 *
 * Revision 1.2  2001/09/28 21:20:05  stolpmann
 * 	New functions:
 * 	- string_of_in_obj_channel
 * 	- with_in_obj_channel, with_out_obj_channel
 *
 * Revision 1.1  2001/09/24 21:23:17  stolpmann
 * 	Initial revision.
 *
 * 
 *)
