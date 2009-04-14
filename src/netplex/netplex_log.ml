(* $Id$ *)

open Printf

type level =
    [ `Emerg | `Alert | `Crit | `Err | `Warning | `Notice | `Info | `Debug ]

let level_weight =
  function
    | `Emerg   -> 0
    | `Alert   -> 1
    | `Crit    -> 2
    | `Err     -> 3
    | `Warning -> 4
    | `Notice  -> 5
    | `Info    -> 6
    | `Debug   -> 7

let level_names =
  [| "emerg"; "alert"; "crit"; "err"; "warning"; "notice"; "info"; "debug" |]

let level_of_string s =
  let s = String.lowercase s in
  match s with
    | "emerg"   -> `Emerg
    | "alert"   -> `Alert
    | "crit"    -> `Crit
    | "err"     -> `Err
    | "warning" -> `Warning
    | "notice"  -> `Notice
    | "info"    -> `Info
    | "debug"   -> `Debug
    | _         -> failwith ("Unknown level: " ^ s)
;;


class type logger =
object
  method log_subch : component:string -> subchannel:string -> 
                     level:level -> message:string -> unit
  method log : component:string -> level:level -> message:string -> unit
  method reopen : unit -> unit
  method max_level : level
  method set_max_level : level -> unit
end


let format_message fmt component subchannel level_weight message =
  let t = Unix.time() in
  let nd = Netdate.create ~zone:(Netdate.localzone) t in
  let b = Buffer.create 100 in
  Buffer.add_substitute
    b
    (fun var ->
       match var with
	 | "timestamp" ->
	     Netdate.format "%c" nd
	 | "timestamp:unix" ->
	     sprintf "%.0f" t
	 | "component" ->
	     component
	 | "subchannel" ->
	     subchannel
	 | "level" ->
	     level_names.(level_weight)
	 | "message" ->
	     message
	 | _ ->
	     if (String.length var >= 10 && 
		   String.sub var 0 10 = "timestamp:") then
	       let nd_fmt =
		 String.sub var 10 (String.length var - 10) in
	       Netdate.format nd_fmt nd
	     else
	       ""
    )
    fmt;
  Buffer.contents b


let std_fmt =
  "[${timestamp}] [${component}] [${level}] ${message}"


class channel_logger ?(fmt=std_fmt) out : logger =
object(self)
  val mutable max_level = `Info

  method log_subch ~component ~subchannel ~level ~message =
    let w = level_weight level in
    if w <= level_weight max_level then (
      try
	fprintf out "%s\n%!"
	  (format_message fmt component subchannel w message)
      with
	| error ->
	    prerr_endline ("Netplex Catastrophic Error: Unable to write to log channel: " ^ Netexn.to_string error)
    )

  method log =
    self # log_subch ~subchannel:""

  method reopen() = ()

  method max_level = max_level

  method set_max_level l = max_level <- l

end

let channel_logger = new channel_logger

let stderr_logger_factory =
object
  method name = "stderr"
  method create_logger cf addr _ = 
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr [ "type"; "format" ];
    let fmt = 
      try cf # string_param (cf # resolve_parameter addr "format")
      with Not_found -> std_fmt in
    channel_logger ~fmt stderr
end


class file_logger ?(fmt = std_fmt) file : logger =
object(self)

  val mutable out = 
    open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 file

  val mutable max_level = `Info

  initializer
    Netsys_posix.register_post_fork_handler
      (object
	 method name = "Netplex_log.file_logger"
	 method run() = self # post_fork()
       end
      )


  method log_subch ~component ~subchannel ~level ~message =
    let w = level_weight level in
    if w <= level_weight max_level then (
      try
	fprintf out "%s\n%!"
	  (format_message fmt component subchannel w message)
      with
	| error ->
	    prerr_endline ("Netplex Catastrophic Error: Unable to write to log file " ^ file ^ ": " ^ Netexn.to_string error)
    )


  method log =
    self # log_subch ~subchannel:""

  method reopen() =
    close_out out;
    try
      out <-
	open_out_gen [ Open_wronly; Open_append; Open_creat ] 0o666 file
    with
      | error ->
	  prerr_endline ("Netplex Catastrophic Error: Unable to reopen log file " ^ file ^ ": " ^ Netexn.to_string error)


  method max_level = max_level

  method set_max_level l = max_level <- l

  method private post_fork() =
    close_out out

end

let file_logger = new file_logger


let file_logger_factory =
object
  method name = "file"
  method create_logger cf addr _ =
    cf # restrict_subsections addr [];
    cf # restrict_parameters addr [ "type"; "file"; "format" ];
    let fileaddr =
      try
	cf # resolve_parameter addr "file"
      with
	| Not_found ->
	    failwith ("File logger needs parameter 'file'") in
    let file = cf # string_param fileaddr in
    let fmt = 
      try cf # string_param (cf # resolve_parameter addr "format")
      with Not_found -> std_fmt in
    file_logger ~fmt file
end


class type multi_file_config =
object
  method log_directory : string
  method log_files :
    (string * string * [ level | `All ] * string * string) list
end


let no_duplicates l =
  let h = Hashtbl.create 10 in
  List.filter
    (fun p ->
       not(Hashtbl.mem h p) && (
         Hashtbl.add h p ();
         true))
    l
;;


let ast_re = Pcre.regexp "[*]";;

let regexp_of_pattern s =
  let l = Netstring_pcre.split_delim ast_re s in
  Netstring_pcre.regexp
    (String.concat ".*" (List.map (fun u -> Netstring_pcre.quote u) l) ^ "$")


class multi_file_logger (mfc : multi_file_config) : logger =
  let log_files =
    List.map
      (fun (comp_pat, subch_pat, level, file, fmt) ->
	 let comp_re = regexp_of_pattern comp_pat in
	 let subch_re = regexp_of_pattern subch_pat in
	 (comp_re, subch_re, level, file, fmt)
      )
      mfc#log_files in
object(self)
  val channels = Hashtbl.create 10
    (* Maps files to channels *)

  val mutable max_level = `Info

  initializer
    Netsys_posix.register_post_fork_handler
      (object
	 method name = "Netplex_log.multi_file_logger"
	 method run() = self # post_fork()
       end
      )

  method log_subch ~component ~subchannel ~level ~message =
    let w = level_weight level in
    if w <= level_weight max_level then (
      let files =
	List.map
	  (fun (_, _, _, file, fmt) -> (file, fmt))
	  (List.filter
	     (fun (comp_re, subch_re, level_pat, _, _) ->
		match Netstring_pcre.string_match comp_re component 0 with
		  | Some _ ->
		      ( match 
			  Netstring_pcre.string_match subch_re subchannel 0
			with
			  | Some _ ->
			      ( match level_pat with
				  | `All -> true
				  | #level as l ->
				      w <= level_weight l
			      )
			  | None -> false
		      )
		  | None -> false
	     )
	     log_files
	  ) in
      let files = no_duplicates files in
      List.iter
	(fun (file, fmt) ->
	   let full_path =
	     if file <> "/" && file.[0] = '/' then
	       file
	     else
	       Filename.concat mfc#log_directory file
	   in
	   try
	     let ch = 
	       try
		 Hashtbl.find channels full_path
	       with
		 | Not_found ->
		     let ch =
	               open_out_gen 
			 [ Open_wronly; Open_append; Open_creat ] 0o666 full_path in
		     Hashtbl.add channels full_path ch;
		     ch
	     in
	     fprintf ch "%s\n%!"
	       (format_message fmt component subchannel w message)
	   with
	     | error ->
		 prerr_endline ("Netplex Catastrophic Error: Unable to write to log file " ^ full_path ^ ": " ^ Netexn.to_string error)

	)
	files
    )

  method log =
    self # log_subch ~subchannel:""

  method reopen() =
    Hashtbl.iter
      (fun name ch ->
	 close_out ch)
      channels;
    Hashtbl.clear channels

  method max_level = max_level

  method set_max_level l = max_level <- l

  method private post_fork() =
    self # reopen()

end


let multi_file_logger = new multi_file_logger

let multi_file_logger_factory =
object
  method name = "multi_file"
  method create_logger cf addr _ =
    cf # restrict_subsections addr [ "file" ];
    cf # restrict_parameters addr [ "type"; "format"; "directory" ];
    let diraddr =
      try
	cf # resolve_parameter addr "directory"
      with
	| Not_found ->
	    failwith ("Multi-file logger needs parameter 'directory'") in
    let dir = cf # string_param diraddr in
    let def_fmt =
      try cf # string_param (cf # resolve_parameter addr "format")
      with Not_found -> std_fmt in
    let log_files =
      List.map
	(fun addr ->
	   cf # restrict_subsections addr [];
	   cf # restrict_parameters addr
	     [ "component"; "subchannel"; "max_level"; "file"; "format" ];
	   let component =
	     try 
	       cf # string_param (cf # resolve_parameter addr "component")
	     with
	       | Not_found -> "*" in
	   let subchannel =
	     try 
	       cf # string_param (cf # resolve_parameter addr "subchannel")
	     with
	       | Not_found -> "*" in
	   let max_level_str =
	     try
	       cf # string_param (cf # resolve_parameter addr "max_level")
	     with
	       | Not_found -> "all" in
	   let max_level =
	     try
	       if String.lowercase(max_level_str) = "all" then
		 `All
	       else
		 level_of_string max_level_str
	     with
	       | _ ->
		   failwith ("In section " ^ cf # print addr ^ ": Bad max_level parameter value: " ^ max_level_str) in
	   let fmt =
	     try cf # string_param (cf # resolve_parameter addr "format")
	     with Not_found -> def_fmt in
	   let file =
	     try
	       cf # string_param (cf # resolve_parameter addr "file")
	     with
	       | Not_found ->
		   failwith ("In section " ^ cf # print addr ^ ": Parameter 'file' is missing") in
	   (component, subchannel, max_level, file, fmt)
	)
	(cf # resolve_section addr "file") in

    let config =
      ( object
	  method log_directory = dir
	  method log_files = log_files
	end
      ) in

    multi_file_logger config
end


let logger_factories =
  [ file_logger_factory;
    multi_file_logger_factory;
    stderr_logger_factory
  ]

let debug_scheduling = ref false

let debug_containers = ref false
