(* Copyright (c) 2001 Patrick Doane.
 * For conditions of distribution and use, see copyright notice in LICENSE. *)

open Printf

(* global settings *)

let input_root  = ref "."
let output_root = ref (Filename.concat "." "results")
let debug       = ref false
let verbose     = ref 2

(* utilities *)

let bracket
    (before : 'a -> 'b)
    (after : 'b -> unit)
    (f : 'b -> 'c)
    (init : 'a) =
  let x = before init in
  let res =
    try f x with exn -> after x; raise exn
  in
  after x;
  res

let rec mkdir name ~perm =
  try Unix.mkdir name perm
  with
    | Unix.Unix_error (Unix.ENOENT, _, _) ->
	mkdir (Filename.dirname name) ~perm;
	(try Unix.mkdir name perm 
         with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
    | Unix.Unix_error (Unix.EEXIST, _, _) -> ()

(* directory management *)

let input_directory    = ref ""
let output_directory   = ref ""

let input_filename s  = Filename.concat !input_directory s
let output_filename s = Filename.concat !output_directory s

let read_file s f  = bracket open_in close_in f s
let write_file s f = bracket open_out close_out f s

let foreach_file dir ?(filter=(fun _ -> true)) f =
  bracket Unix.opendir Unix.closedir (fun h ->
    try
      while true do
	let basename = Unix.readdir h in
	match basename with
	  | "." | ".." -> ()
	  | _ -> if filter basename then f basename
      done
    with End_of_file -> ()
  ) dir

(* test result codes *)

type result = 
  | Pass
  | UPass
  | Fail of string
  | XFail
  | Unresolved
  | Untested
  | Unsupported of string

let is_expected = function
  | Pass | XFail -> true
  | _ -> false

let needs_review = function
  | UPass | Fail _ | Unresolved | Untested -> true
  | _ -> false

let to_string = function
  | Pass           -> "Pass"
  | UPass          -> "Unexpected Pass"
  | Fail ""        -> "Fail"
  | Fail m         -> "Fail - " ^ m
  | XFail          -> "Expected Fail"
  | Unresolved     -> "Unresolved"
  | Untested       -> "Untested"
  | Unsupported "" -> "Unsupported"
  | Unsupported m  -> "Unsupported - " ^ m

let count_pass         = ref 0
let count_upass        = ref 0
let count_fail         = ref 0
let count_xfail        = ref 0
let count_unresolved   = ref 0
let count_untested     = ref 0
let count_unsupported  = ref 0

let count_var = function
  | Pass          -> count_pass
  | UPass         -> count_upass
  | Fail _        -> count_fail
  | XFail         -> count_xfail
  | Unresolved    -> count_unresolved
  | Untested      -> count_untested
  | Unsupported _ -> count_unsupported

(* Tests *)

let test ~desc ~body =
  let result = 
    try
      body ()
    with exn ->
      printf "  uncaught exception: %s\n" (Printexc.to_string exn);
      Unresolved
  in
  incr (count_var result);
  if not (is_expected result) || !verbose > 2 then begin
    printf "  %s: %s\n" (to_string result) desc;
    flush stdout
  end

(* Expect based testing *)

exception EFail of string

let fail m = raise (EFail m)

let expect_equal ?msg ?printer x y =
  if x=y then ()
  else match printer with
    | None ->
        fail (match msg with
              | None -> "not equal"
              | Some msg -> msg)
    | Some p ->
        let test_msg = match msg with None -> "" | Some msg -> msg ^ " " in
	fail (sprintf "expected %s\n          but got %s\n    in test %s"
	  (p x) (p y) test_msg)

let expect_equal_app ?msg ?printer f x g y =
  let test_msg = match msg with None -> "" | Some msg -> msg ^ " " in
  try
    let x' = f x in
    (try
      let y' = g y in
      expect_equal ?msg ?printer x' y'
    with 
    | EFail _ as exn -> raise exn
    | exn2 ->
      let exn2 = "exception " ^ (Printexc.to_string exn2) in
      match printer with
      | None ->
          fail (sprintf "unexpected %s\n    in test %s"
                 exn2 test_msg)
      | Some p ->
          fail (sprintf
                "expected %s\n          but got %s\n    in test %s"
                  (p x') exn2 test_msg))
  with
    | EFail _ as exn -> raise exn
    | exn1 ->
        let exn1 = "exception " ^ (Printexc.to_string exn1) in
        (try
          let y' = g y in
          match printer with
          | None ->
              fail (sprintf
                 "expected %s\n          but got no exception\n    in test %s"
	         exn1 test_msg)
          | Some p ->
              fail (sprintf
                 "expected %s\n          but got %s\n    in test %s"
	         exn1 (p y') test_msg)
        with 
          | EFail _ as exn -> raise exn
          | exn2 ->
              let exn2 = "exception " ^ (Printexc.to_string exn2) in
              expect_equal ?msg ~printer:(fun s -> s) exn1 exn2)

let expect_true ?msg test =
  if test then ()
  else fail (match msg with None -> "" | Some m -> m)

let expect_pass ~desc ~body =
  test desc (fun () -> try body (); Pass with EFail m -> Fail m)

let expect_fail ~desc ~body =
  test desc (fun () -> try body (); UPass with EFail m -> XFail)

(* Interpretation of scripts *)

let run_script path =
  if !verbose > 0 then
    printf "Running '%s'\n" path;
  flush stdout;

  let path_parent = Filename.dirname path in
  let filename = Filename.concat !input_root path in
  input_directory := Filename.concat !input_root path_parent;
  output_directory := Filename.concat !output_root path_parent;

  let old_env = !Toploop.toplevel_env in
  if not (Toploop.use_silently Format.err_formatter filename) then
    begin
      print_endline "  error running script";
      incr (count_var Unresolved)
    end;
  Toploop.toplevel_env := old_env

(* Front end *)

let day =   [| "Sun"; "Mon"; "Tue"; "Wed"; "Thu"; "Fri"; "Sat" |]
let month = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; 
	       "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]

let date_time () =
  let t = Unix.time () in
  let lt = Unix.localtime t in

  (* Generate date string *)
  Printf.sprintf "%s, %02d %s %d %02d:%02d:%02d"
    day.(lt.Unix.tm_wday)
    lt.Unix.tm_mday
    month.(lt.Unix.tm_mon)
    (1900 + lt.Unix.tm_year)
    lt.Unix.tm_hour
    lt.Unix.tm_min
    lt.Unix.tm_sec

let user () =
  (* [getlogin] can be unreliable on some systems so try to access
   * the password database first. *)
  try
    (Unix.getpwuid (Unix.getuid ())).Unix.pw_name
  with Not_found ->
    try
      Unix.getlogin ()
    with _ -> "unknown"

let header () =
  if !verbose > 1 then
    printf "\n-------------------------------------------------------------\n";
  if !verbose > 0 then
    printf "FORT regression test execution\n";
  if !verbose > 1 then begin
    printf "-------------------------------------------------------------\n";
    printf "        user:  %s\n" (user ());
    printf "        time:  %s\n" (date_time ());
    printf "          os:  %s\n" Sys.os_type;
    printf "  input root:  %s\n" !input_root;
    printf " output root:  %s\n" !output_root;
    printf "-------------------------------------------------------------\n\n";
  end;
  flush stdout

let summary () =
  let count_review =
    !count_upass + !count_fail + !count_unresolved + !count_untested
  in
  if !verbose > 0 then
    begin
      let result count desc =
	printf "%4d %s\n" count desc
      in
      let cond_result count desc =
	if count > 0 || !verbose > 2 then result count desc
      in
      if !verbose > 1 then
	printf
	  "\n-------------------------------------------------------------\n";
      printf "Summary:\n";
      result      !count_pass        "passed";
      cond_result !count_upass       "unexpectedly passed";
      cond_result !count_fail        "failed";
      cond_result !count_xfail       "failed as expected";
      cond_result !count_unresolved  "unresolved";
      cond_result !count_untested    "untested";
      cond_result !count_unsupported "unsupported";
      if !verbose > 1 then begin
	print_char '\n';
	result      count_review       "testcase(s) to review";
	printf
	  "-------------------------------------------------------------\n\n";
      end;
      flush stdout;
    end;
  count_review

let usage =
  "FORT (Framework for Ocaml Regression Testing)
Copyright (c) 2001 Patrick Doane.
For conditions of distribution and use, see copyright notice in LICENSE.

Usage: fort <options> <suite filename>"

let initial_env () =
  (* hack to be able to execute
     Toploop.toplevel_env := Compile.initial_env () *)
  let save_arg = Sys.argv.(0) in
  Sys.argv.(0) <- "__dummy";
  let ignore_formatter =
    Format.make_formatter (fun _ _ _ -> ()) (fun _ -> ()) in
  ignore (Toploop.run_script ignore_formatter "__dummy" Sys.argv);
  Sys.argv.(0) <- save_arg

let add_env name =
  if not (Toploop.use_silently Format.err_formatter name) then
    exit 2  

let process_file name =
  if Filename.check_suffix name ".cmo" || 
     Filename.check_suffix name ".cma" then
    begin 
(*    This doesn't produce good diagnostic information when
      the load fails.

        Topdirs.dir_load Format.err_formatter name
*)
      let filename = Filename.temp_file "fort" ".ml" in
      bracket open_out close_out (fun oc ->
        Printf.fprintf oc "#load \"%s\"\n" name
      ) filename;
      let result = Toploop.use_silently Format.err_formatter filename in
      Sys.remove filename;
      if not result then
        exit 2;
    end
  else
    run_script name
  
let main () = 
  let files = ref [] in
  let add_file f s = 
    files := (f,s) :: !files
  in

  initial_env ();  
  Arg.parse 
	[("-input",
	  Arg.String ((:=) input_root),
	  "<dir>\t\tspecify input root");

	 ("-output",
	  Arg.String ((:=) output_root),
	  "<dir>\t\tspecify output root");

	 ("-debug",
	  Arg.Set debug,
          "\t\tenable debugging");

	 ("-verbose",
	  Arg.Int ((:=) verbose),
	  "<level>\tset verbosity");

	 ("-env",
	  Arg.String (add_file add_env),
	  "<file>\tAdd definitions to initial environment");

         ("-I",
          Arg.String Topdirs.dir_directory,
          "<dir>\tAdd <dir> to the list of include directories");
	]
	(add_file process_file)
	usage;

  header ();
  List.iter (fun (f,s) -> f s) (List.rev !files);
  let count_review = summary () in
  exit (if count_review > 0 then 1 else 0)

