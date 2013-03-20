(* Convert .x files to ocamldoc .txt files. Structured comments
   are /** ... */. They must be on lines of their own.
 *)

open Printf

let start_re = Str.regexp "^[ \t]*/\\*\\*"
let end_re = Str.regexp "\\*/"
let special_re = Str.regexp "[]{}@[]"
let non_ws_re = Str.regexp "[^ \t\r\n]"

let only_ws s =
  try ignore(Str.search_forward non_ws_re s 0); false
  with Not_found -> true


let convert f_in f_out =
  let mode = ref `Code in
  
  let esc s =
    Str.global_substitute 
      special_re 
      (fun u -> "\\" ^ Str.matched_string u)
      s in
  let code_buf = Buffer.create 80 in
  let output_code s = 
    Buffer.add_string code_buf (esc s) in
  let flush_code() =
    let s = Buffer.contents code_buf in
    Buffer.clear code_buf;    
    if not(only_ws s) then
      fprintf f_out "\n{v %s v}\n" s in
  let output_comment s =
    flush_code();
    output_string f_out s in
  let output_newline () =
    if !mode = `Code then
      Buffer.add_string code_buf "\n"
    else
      output_string f_out "\n" in
      
  try
    while true do
      let line = input_line f_in in
      let len = String.length line in
      let comment_starts =
	if !mode = `Code && Str.string_match start_re line 0 then
	  Some(String.index line '/')
	else
	  None in
      let comment_ends =
	if !mode = `Comment || comment_starts <> None then
	  try Some(Str.search_forward end_re line 0) with Not_found -> None
	else
	  None in
      
      ( match comment_starts, comment_ends with
	  | None, None ->
	      ( match !mode with
		  | `Code -> output_code line
		  | `Comment -> output_comment line
	      )
	  | Some pos, None ->
	      output_comment (String.sub line (pos+3) (len - pos - 3))
	  | None, (Some pos) ->
	      output_comment (String.sub line 0 pos);
	      output_string f_out "\n";
	      output_code (String.sub line (pos+2) (len - pos - 2))
	  | (Some spos), (Some epos) ->
	      output_comment (String.sub line (spos+3) (epos - spos - 3));
	      output_string f_out "\n";
	      output_code (String.sub line (epos+2) (len - epos - 2))
      );

      if comment_starts <> None then mode := `Comment;
      if comment_ends <> None then mode := `Code;
      output_newline()
    done
  with
    | End_of_file ->
	flush_code()
	


let main() =
  let outdir = ref "." in
  let files = ref [] in
  Arg.parse
    [ "-d", Arg.Set_string outdir, "<dir>  Output into this directory" ]
    (fun s -> files := !files @ [s])
    "usage: x_to_odoc file.x ...";

  List.iter
    (fun ifile ->
       let ofile =
	 !outdir ^ "/" ^ 
	   Filename.chop_extension (Filename.basename ifile) ^ ".txt" in
       try
	 let f_ifile = open_in ifile in
	 let f_ofile = open_out ofile in
	 convert f_ifile f_ofile;
	 close_in f_ifile;
	 close_out f_ofile;
       with
	 | error ->
	     Sys.remove ofile;
	     raise error
    )
    !files


let () =  
  main()
