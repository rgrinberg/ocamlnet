(* $Id$
 * ----------------------------------------------------------------------
 *
 *)


let comment_re = Str.regexp "^#";;

let fields_re = Str.regexp "^U\\+\\([0-9A-F]+\\)[ \t]+\\([a-zA-Z0-9_]+\\)[ \t]+\\(.*\\)$";;

let rec read_unihan inch fields_to_extract =
  let line = 
    try
      input_line inch 
    with
	End_of_file -> "_END_"
  in
  if Str.string_match comment_re line 0 then
    read_unihan inch fields_to_extract
  else
    if Str.string_match fields_re line 0 then begin
      let hex_nr = Str.matched_group 1 line in
      let field_name = Str.matched_group 2 line in
      let field_value = Str.matched_group 3 line in
      if List.mem field_name fields_to_extract then begin
	(int_of_string ("0x" ^ hex_nr),
	 field_name,
	 field_value) :: read_unihan inch fields_to_extract
      end
      else
	read_unihan inch fields_to_extract
    end
    else 
      if line = "_END_" then
	[]
      else
	begin
	  prerr_endline ("WARNING: Cannot parse line: " ^ line);
	  read_unihan inch fields_to_extract
	end
;;


let read_jis0208 unihan =
  [ "jis0208",
    List.flatten
      (List.map
	 (function
	      (unicode, "kJis0", lstring) ->
		let lcode = int_of_string lstring in
		let row = lcode / 100 in
		let col = lcode mod 100 in
		assert(row >= 1 && row <= 94 && col >= 1 && col <= 94);
		[ row * 96 + col, unicode ]
	    | _ ->
		[]
	 )
	 unihan
      )
  ]
;;


let read_jis0212 unihan =
  [ "jis0212",
    List.flatten
      (List.map
	 (function
	      (unicode, "kJis1", lstring) ->
		let lcode = int_of_string lstring in
		let row = lcode / 100 in
		let col = lcode mod 100 in
		assert(row >= 1 && row <= 94 && col >= 1 && col <= 94);
		[ row * 96 + col, unicode ]
	    | _ ->
		[]
	 )
	 unihan
      )
  ]
;;


let write_portable_file out unimaps =
  List.iter
    (fun (name,unimap) ->
       output_string out (name ^ "\n");
       List.iter
	 (fun (localcode,unicode) ->
	    output_string out (string_of_int localcode ^ "\n");
	    output_string out (string_of_int unicode ^ "\n");
	 )
	 unimap;
       output_string out "\n";
    )
    unimaps
;;


let main() =
  let reader = ref (fun _ -> failwith "No output format selected") in
  let outch = ref (lazy stdout) in

  Arg.parse
      [ "-o", Arg.String (fun s -> outch := lazy (open_out s)),
           " <file>   Redirect stdout to this file";
	"-jis0208", Arg.Unit (fun _ -> reader := read_jis0208),
	         "    Create jis0208 pmap table";
	"-jis0212", Arg.Unit (fun _ -> reader := read_jis0212),
	         "    Create jis0212 pmap table";
      ]
      (fun s -> raise(Arg.Bad ("Don't know what to do with: " ^ s)))
      "usage: unihan_extract <options>  < Unihan.txt";

  let unihan = read_unihan stdin [ "kJis0"; "kJis1" ] in
  let unimaps = !reader unihan in
  let out = Lazy.force !outch in
  write_portable_file out unimaps;
  close_out out
;;

main();;


(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.1  2003/06/03 18:49:39  stolpmann
 * 	Initial revision
 *
 * 
 *)
