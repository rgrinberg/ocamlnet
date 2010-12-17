(* $Id$ *)

(** Globbing *)

(** Globbing resolves shell wildcards like "*" and "?". For example,

    {[
    let files = Netglob.glob (`String "*.cm[iox]")
    ]}

    would return all files matching this pattern (e.g. module.cmi,
    module.cmo).

    The main user function is {!Netglob.glob}. Globbing accesses the
    local filesystem by default, but one can also run the globbing
    algorithm on any other filesystem, provided the access primitives
    of {!Netglob.glob_fsys} are available.
 *)

type glob_expr = glob_expr_atom list

and glob_expr_atom =
    [ `Literal of string
    | `Star
    | `Qmark
    | `Bracket of (bool * glob_set)
    | `Brace of glob_expr list
    | `Tilde of string
    ]
(** 
    - [`Literal s]: Matches the string literally. The string must not be empty. 
       The backslash is not an escape character, but matches the
       backslash character. Two adjecent [`Literal] atoms are
       not allowed in a [glob_expr].
    - [`Star]: The "*" operator
    - [`Qmark]: The "?" operator
    - [`Bracket(negated,set)]:  The [[...]] operator. The [set] argument
       describes the characters that are matched. The [negated] argument 
       is true when the expression is negated (i.e. [[^...]]). 
    - [`Brace l]: The [{e1,e2,...}] operator
    - [`Tilde t]: The [~username] operator. If [t=""] the current user
      is meant. The [`Tilde] atom may only occur at the beginning of the list.
      The [`Tilde] atom always matches a directory,
      and must be followed by a literal slash (if anything follows).

    Compatibility: Conforms to POSIX with extensions (braces). Shells often
    implement brace expressions in a slightly different way (braces are
    parsed and expanded in a separate step before the other pattern
    constructors are handled). The cases where this leads to different
    results are quite exotic (e.g. "{~g,~h}1" would mean "~g1 ~h1", but
    this implementation rejects the pattern).
 *)

and glob_set = < set : (int * int) list >
  (** A set of code points is given as a list of ranges [(from,to)]. The ranges
      must be sorted, and between consecutive range pairs there must be
      a hole in the range. The code points are from Unicode.
   *)


type valid_glob_expr
  (** A validated [glob_expr] *)

exception Bad_glob_expr of string
  (** An syntax error in the glob expression; the argument is the bad
      expression 
   *)

exception Unsupported_expr of string
  (** The notations [:class:], [.symbol.], [=eqclass=] inside [...] are
   * not supported by this implementation. If they are found, this exception
   * will be raised, and the argument is the whole glob expression
   *)

val parse_glob_expr : 
           ?encoding:Netconversion.encoding ->
           ?enable_star:bool ->     (* Recognize "*" *)
           ?enable_qmark:bool ->    (* Recognize "?" *)
           ?enable_brackets:bool -> (* Recognize "[set]" *)
           ?enable_braces:bool ->   (* Recognize "{alt,...}" *)
           ?enable_tilde:bool ->    (* recognize ~ *)
           ?enable_escape:bool ->   (* Recognize backslash as escape char *)
           string ->
             valid_glob_expr
  (** Parses the glob expression. By default, all syntax features are enabled.
   * May raise [Bad_glob_expr] or [Unsupported_expr].
   *
   * The glob expressions are POSIX-compliant with the extension of 
   * brace expressions, and tildes, and the omission of internationalized
   * bracket expressions:
   * - [*]: Matches a sequence of zero or more arbitrary characters
   * - [?]: Matches one arbitrary character
   * - [[abc]]: Matches one of the mentioned characters
   * - [[a-z]]: Matches one of the characters of the range. This is here
   *   only permitted when the range falls into the ASCII set. (Otherwise
   *   the interpretation would be dependent on the encoding.) Note that
   *   the ASCII restriction does not comply to POSIX.
   * - [[!expr]] or [[^expr]]: Negates the bracket expression
   * - [{expr,expr,...}]: Generates a string for each of the alternatives.
   *   A brace expression is even recognized if there is no comma, or even
   *   no contents (i.e. "{expr}" and "{}"). The elements of brace expressions
   *   may be again glob expressions; nested brace expressions are allowed.
   * - [~username]: Generates the home directory of this user
   * - [~]: Generates the home directory of the current user
   * - If enabled, the backslash character is the escape character. Within
   *   bracket expressions, the backslash character never escapes.
   * - Not supported: Collating symbols [[.a.]], equivalence classes
   *   [[=a=]], and character classes [[:name:]]. If they are found, the
   *   exception [Unsupported_expr] will be raised.
   *
   * Glob expressions have a character [encoding]. This defaults to
   * [`Enc_iso88591]. Encodings must be ASCII-compatible.
   *)

val validate_glob_expr : Netconversion.encoding -> glob_expr -> valid_glob_expr
  (** Checks whether the passed expression is syntactically valid. If so,
      a validated expression is returned. Otherwise, this function fails.
   *)

val recover_glob_expr : valid_glob_expr -> glob_expr
  (** Returns the explicit representation *)

val encoding_of_glob_expr : valid_glob_expr -> Netconversion.encoding
  (** Returns the encoding *)

val literal_glob_expr : Netconversion.encoding -> string -> valid_glob_expr
  (** Returns an expression that matches literally the passed string *)

val print_glob_expr :  valid_glob_expr -> string
  (** Prints the glob expression as string. Meta characters are always
   * escaped by a backslash. Meta characters are:
   *   "*", "?", "[", "]", "{", "}", ",", "~" and "\\" 
   *)

class type user_info =
object
  method path_encoding : Netconversion.encoding option
    (** Paths of filesystems may be encoded *)

  method home_directory : string -> string
    (** Returns the home directory of the passed user, or the home
	directory of the current user for the empty string. Raises
	[Not_found] if the lookup fails.
     *)
end

val expand_glob_expr : 
      ?user_info:user_info ->
      ?expand_brace:bool ->
      ?expand_tilde:bool ->
      valid_glob_expr -> valid_glob_expr list
  (** Resolve generative sub expressions by expanding them. The returned
   * list of glob expr no longer contains the expanded constructions.
   *
   * - [expand_brace]: Expands [`Brace] subexpressions.
   * - [expand_tilde]: Expands [`Tilde] subexrpessions.
   * - [user_info]: The subset of file system operations needed for tilde
   *   expansion. Defaults to {!Netglob.local_user_info} (see below).
   *
   * Both [expand_*] options are enabled by default.
   *)

val match_glob_expr :
      ?protect_period:bool ->   (* Protect leading dots; default: true *)
      ?protect_slash:bool ->    (* Protect slashes; default: true *)
      valid_glob_expr -> 
      string ->
        bool
  (** Matches the glob_expr against a string.
   *
   * The input must neither contain brace expressions nor tildes (i.e. call
   * [expand_glob_expr] first). The function fails if it encounters such an
   * expression.
   *
   * - [protect_period]: If true, a leading period cannot be not matched by
   *    [*], [?], [[...]], but only by a literal [.]. A leading period is
   *    a [.] at the beginning of the string to be matched against, or
   *    if also [protect_slash] a [.] after a [/]
   * - [protect_slash]: If true, a slash cannot be matched by [*], [?], [[...]],
   *    but only by a literal [/]
   *
   * Both options are enabled by default.
   *)

val split_glob_expr : valid_glob_expr -> valid_glob_expr list
  (** Splits the glob expression into filename components separated by
   * literal [/] characters. For example, for the glob expression
   * ["a*b/c/d?"], the list [["a*b"; "c"; "d?"]] is returned. 
   * 
   * If the first component begins with a slash, the slash is not removed
   * from the first returned list element, e.g. for ["/ab/c*"], the list
   * [[ "/ab"; "c*" ]] is computed. Use [check_rooted_glob_expr] to test this
   * case.
   *
   * Several adjacent slashes are handled like a single slash. E.g.
   * for ["a//b"], the list [["a"; "b"]] is returned.
   *
   * If the last component ends with a slash, it is not removed from the
   * returned list element, e.g. for ["a/b/"], the list [[ "a"; "b/" ]] is
   * returned. Use [check_directory_glob_expr] to test this case.
   *
   * The glob expression passed to this function must not contain brace
   * or tilde expressions.
   *)

val check_rooted_glob_expr : valid_glob_expr -> valid_glob_expr option
  (** If the glob expression matches the root directory (i.e. the expression
   * begins with a literal [/]), the function returns [Some expr'], where
   * [expr'] matches the path relative to the root directory (i.e. the
   * expression without the [/] at the beginning).
   *
   * Otherwise, [None] is returned.
   *
   * Example: For ["/a/b*"], the expression ["a/b*"] is returned.
   *
   * Special case: for ["/"], the expression [""] (only matching the empty
   * string) is returned.
   *
   * The glob expression passed to this function must not contain brace
   * or tilde expressions.
   *)

val check_directory_glob_expr : valid_glob_expr -> valid_glob_expr option
  (** If the last component of the glob expression matches only directories
   * because it ends with a literal [/] character, the value [Some expr'] is
   * returned where [expr'] matches the same path without the trailing [/].
   *
   * Otherwise, [None] is returned.
   *
   * Example: For ["a/b*/"], the expression ["a/b*"] is returned.
   *
   * Special case: for ["/"], the expression [""] (only matching the empty
   * string) is returned.
   *
   * The glob expression passed to this function must not contain brace
   * or tilde expressions.
   *)

class type glob_fsys =
object
  method path_encoding : Netconversion.encoding option
    (** Paths of filesystems may be encoded *)
  method read_dir : string -> string list
    (** Returns the file names contained in the directory, without
        path. The names "." and ".." should be returned. It is acceptable
        to return the empty list for an unreadable directory.
     *)
  method file_is_dir : string -> bool
    (** Whether the file name is valid and a directory, or a symlink to
        a directory.
     *)
  method file_exists : string -> bool
    (** Whether the file name is valid and refers to an existing file,
        or to a symlink pointing to an existing file.
     *)
end

type glob_mode = [ `Existing_paths
                 | `All_paths
                 | `All_words
                 ]

type pattern = [ `String of string | `Expr of valid_glob_expr ]

val glob :
      ?encoding:Netconversion.encoding ->  (* default: `Enc_iso88591 *)
      ?base_dir:string ->       (* default: current directory *)
      ?protect_period:bool ->   (* default: true *)
      ?fsys:glob_fsys ->        (* default: access real file system *)
      ?user_info:user_info ->
      ?mode:glob_mode ->        (* default: `Existing_paths *)
      pattern ->
        string list
  (** Forms a set of filenames as described below, and matches this set
   * against the pattern. The pattern can be given as a [`String s]
   * in which case [s] is parsed (with all features enabled, and
   * it is assumed it has the passed [encoding]). Alternatively,
   * an already parsed [`Expr e] can be given. (Note that [encoding]
   * is ignored in this case.)
   *
   * {b Slashes must be explicitly matched:}
   * "/" must literally occur in order to be a candidate for matching.
   * It is not matched by [*] or [?] or a bracket expression.
   *
   * {b Periods:} The leading period is protected if [protect_period].
   * It must then also literally occur to be matched.
   *
   * {b Anchoring:} If the [glob_expr] begins with a literal "/", the set
   * of filenames is
   * anchored at the root directory; otherwise the set is anchored at
   * the current directory or, if [base_dir] is passed, at this directory.
   * (If [fsys] is passed, it is required to also set [base_dir].)
   * 
   * Initially, the set contains all files of the anchor
   * directory (for the root directory, a "/" is prepended).
   *
   * After that, the set is extended by adding the paths of
   * subdirectories relative to the anchor directory. Note that the
   * constructed set is always infinite, because "." and ".." are not
   * handled specially, and are also regarded as "subdirectories". However,
   * after applying the matching criterion, the returned list is always
   * finite.
   *
   * Note that the anchor directory itself is not part of the generated
   * set. For example, for the expression "/*" the root directory "/" is
   * not returned. As an exception of this rule, for the glob expression
   * "/" the file "/" is returned.
   *
   * {b Braces:} Brace expressions are handled by expanding them first, even
   * before filename generation starts.
   *
   * {b Existence:} If no files match, the empty list is returned.
   *
   * By default, only existing paths are returned. By passing a
   * different [mode], this can be changed:
   * - [`All_paths]: It is allowed that non-existing paths
   *    are returned when the paths do not contain *, ?, or \[
   *    metacharacters after the brace expansion. Path expressions
   *    with these metacharacters are still checked for existence.
   * - [`All_words]: When an expression does not refer to existing
   *    paths, it is returned as such, leaving the metacharacters *, ?, \[
   *    unexpanded (i.e., what the Bourne shell does). Note that
   *    either all metacharacters are resolved, or none, but not
   *    a subset of them.
   *
   * {b Encodings:} Often, only the pattern has an encoding, but not
   * the filesystem (as in Unix). In this case, no conversion is attempted,
   * and the byte representation of the pattern is matched with the
   * byte representation of the filenames. Good luck.
   *
   * If the filesystem has an encoding, however, conversions may
   * be required, and this can cause problems. Usually, network filesystems
   * provide an encoding, and the Win32 local filesystem. (For Unix,
   * one can pass a custom [fsys] with encoding knowledge.) Conversion
   * problems can be avoided if (1) the encoding of the pattern is a superset
   * of the filename encoding. Also, (2) one should not use literals
   * in the pattern that cannot be represented in the filename encoding.
   * If (2) cannot be satisfied, ensure you have at least 
   * [mode=`Existing_paths], i.e. the default mode (this returns the
   * empty list when a conversion problem occurs).
   *
   * The return value of [glob] is encoded in the encoding of the filesystem
   * if the filesystem provides an encoding. (If you want to check this
   * encoding, pass [fsys], e.g. as [local_fsys()], and call the
   * [path_encoding] method of [fsys].)
   *)

(** Example for mixed encodings: (Linux)

    {[
       let fsys = local_fsys ~encoding:`Enc_utf8()
       let l = glob ~fsys (`String "\214*")
    ]}

    The byte 214 is O-umlaut in ISO-8859-1 (the default encoding for
    patterns). By passing an [fsys] argument we change the encoding
    for filenames to UTF-8. For example, if

    "\195\150ffentlich"
  
    was a file in the current directory, it would be found and 
    returned in [l].

    A conversion problem: For example, assume we have a file
    "\226\130\172uro" (EUR-uro in UTF-8). The glob

    {[
       let fsys = local_fsys ~encoding:`Enc_utf8()
       let l = glob ~fsys (`String "*")
    ]}

    does not return this file because the euro sign cannot be represented
    in ISO-8859-1. The glob

    {[
       let fsys = local_fsys ~encoding:`Enc_utf8()
       let l = glob ~encoding:`Enc_iso885915 ~fsys (`String "*")
    ]}

    will find it, however, because ISO-8859-15 includes the euro sign.

    In internationalized environments, it is strongly suggested to
    either use the same encodings for patterns and file names, or
    at least UTF-8 for patterns.
 *)

(** Notes for Win32:

  - Globbing only supports forward slashes, not backslashes as path
    separators
  - Globbing does neither recognize drive letters nor UNC
    paths as special cases. This may lead to subtle bugs. Glob
    expressions like "c:/file.*" may or may not work depending on the
    context.
  - The usually case-insensitive file system is not taken into account.
    (To be fixed.)
 *)

class local_user_info : unit -> user_info
val local_user_info : unit -> user_info
  (** Get the home directory of a user from the local user database. *)


class local_fsys : ?encoding:Netconversion.encoding -> unit -> glob_fsys
val local_fsys : ?encoding:Netconversion.encoding -> unit -> glob_fsys
  (** Accesses the local filesystem *)

class of_stream_fs : Netfs.stream_fs -> glob_fsys
val of_stream_fs : Netfs.stream_fs -> glob_fsys
  (** Use an arbitrary network filesystem for globbing *)


(** {2 Compatibilty}

    This implementation is not fully compatible with the POSIX specs.
    The differences:
    - Missing support for character classes, equivalence classes and
      collating symbols.
    - Ranges in brackets are restricted to ASCII.
    - Unparseable patterns are indicated by exceptions. POSIX, however,
      requires that such patterns are taken literally. E.g. a pattern "\["
      would match a left bracket in POSIX, but this module throws a
      syntax error.
    - If the slash character is protected, it is still allowed inside
      brackets. POSIX, however, requires that the pattern is scanned
      for slashes before brackets. For instance, the pattern "\[a/b*\]"
      is scanned as [ [`Literal "[a/b]"; `Star] ] following the POSIX
      rules while this implementation sees a bracket expression with
      "a", "b", "/" and "*" characters.
    - The "^" character negates the set if used at the beginning of
      bracket expressions. POSIX leaves this unspecified.
 *)
