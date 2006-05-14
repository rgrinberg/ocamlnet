(* $Id$ *)

open Netplex_types
open Genlex

exception Config_error of string

class address = object end

let parse_config_file filename =
  let rec parse_tree =
    parser 
      | [< 'Ident id;
	   v = parse_rhs
	>] ->
	  ( match v with
	      | `Section tl -> `Section(new address, id, tl)
	      | `Parameter p -> `Parameter(new address, id, p)
	  )
  and parse_tree_list =
    parser
      | [< t = parse_tree;
	   r = semi_parse_tree_list
	>] ->
	  t :: r
      | [< >] ->
	  []

  and semi_parse_tree_list =
    parser
      | [< 'Kwd ";";
	   tl = parse_tree_list;
	>] -> tl
      | [< >] -> []
	  
  and parse_tree_semi =
    parser
      | [< t = parse_tree;
	   _ = semi_list
	>] -> t

  and semi_list =
    parser
      | [< 'Kwd ";"; _ = semi_list >] -> ()
      | [< >] -> ()

  and parse_rhs =
    parser
      | [< 'Kwd "=";
	   v = parse_param_value;
	>] ->
	  `Parameter v
      | [< 'Kwd "{";
	   tl = parse_tree_list;
	   'Kwd "}"
	>] ->
	  `Section tl

  and parse_param_value =
    parser
      | [< 'Int n >] -> `Int n
      | [< 'Float f >] -> `Float f
      | [< 'String s >] -> `String s
  in

  let ch = open_in filename in
  let s = Stream.of_channel ch in
  let lexer = Genlex.make_lexer [ "{"; "}"; "="; ";" ] s in
  try
    let tree =
      parse_tree_semi lexer in
    Stream.empty lexer;
    tree
  with
    (* TODO: would be nice if the error contained the line number *)
    | Stream.Failure ->
	raise(Config_error(filename ^ ": Syntax error"))
    | Stream.Error _ ->
	raise(Config_error(filename ^ ": Syntax error"))
;;


let rec iter_config_tree f prefix cnt (tree : config_tree) =
  match tree with
    | `Section(addr, name, tl) ->
	let n =
	  try 
	    Hashtbl.find cnt name
	  with
	    | Not_found ->
		Hashtbl.add cnt name 0;
		0 in
	Hashtbl.replace cnt name (n+1);
	let fullname = 
	  if prefix <> "" then 
	    prefix ^ "." ^ name ^ "[" ^ string_of_int n ^ "]"
	  else 
	    name in
	f addr fullname tree;
	List.iter (iter_config_tree f fullname (Hashtbl.create 10)) tl
    | `Parameter(addr, name, v) ->
	let fullname = if prefix <> "" then prefix ^ "." ^ name else name in
	if Hashtbl.mem cnt name then
	  raise(Config_error("Parameter defined twice: " ^ fullname));
	Hashtbl.add cnt name 0;
	f addr fullname tree;
;;	


class config_file filename : Netplex_types.config_file =
  let tree = parse_config_file filename in
object(self)
  val addresses = Hashtbl.create 100

  initializer (
    try
      iter_config_tree
	(fun addr fullname subtree ->
	   Hashtbl.add addresses addr (fullname,subtree)
	)
	""
	(Hashtbl.create 10)
	tree
    with
      | Config_error msg -> 
	  raise(Config_error (filename ^ ": " ^ msg))
  )

  method filename = filename
  method tree = tree

  method root_addr =
    match tree with
      | `Section(a,_,_) -> a
      | `Parameter(a,_,_) -> a

  method root_name =
    match tree with
      | `Section(_,n,_) -> n
      | `Parameter(_,n,_) -> n

  method resolve_section addr name =
    let (fullname, subtree) =
      try
	Hashtbl.find addresses addr
      with
	| Not_found ->
	    failwith "#resolve_section" in
    match subtree with
      | `Section(_,_,tl) ->
	  List.map
	    (function
	       | `Section(addr,_,_) -> addr
	       | _ -> assert false
	    )
	    (List.filter
	       (function
		  | `Section(_,n,_) -> n = name
		  | _ -> false)
	       tl
	    )
      | `Parameter _ ->
	  []

  method resolve_parameter addr name =
    let (fullname, subtree) =
      try
	Hashtbl.find addresses addr
      with
	| Not_found ->
	    failwith "#resolve_parameter" in
    match subtree with
      | `Section(_,_,tl) ->
	  let vl =
	    List.map
	      (function
		 | `Parameter(addr,_,_) -> addr
		 | _ -> assert false
	      )
	      (List.filter
		 (function
		    | `Parameter(_,n,_) -> n = name
		    | _ -> false)
		 tl
	      ) in
	  ( match vl with
	      | [] -> raise Not_found
	      | [v] -> v
	      | _ ->
		  raise(Config_error(filename ^ ": Several definitions for parameter " ^ fullname ^ " found"))
	  )
      | `Parameter _ ->
	  raise Not_found

  method print addr =
    let (fullname, subtree) =
      try
	Hashtbl.find addresses addr
      with
	| Not_found ->
	    failwith "#print" in
    fullname

  method string_param addr =
    let (fullname, subtree) =
      try
	Hashtbl.find addresses addr
      with
	| Not_found ->
	    failwith "#string_param" in
    match subtree with
      | `Parameter(_,_,`String s) -> s
      | _ -> 
	  raise(Config_error(filename ^ ": Parameter " ^ fullname ^ 
			       " is not a string"))


  method int_param addr =
    let (fullname, subtree) =
      try
	Hashtbl.find addresses addr
      with
	| Not_found ->
	    failwith "#int_param" in
    match subtree with
      | `Parameter(_,_,`Int s) -> s
      | _ ->
	  raise(Config_error(filename ^ ": Parameter " ^ fullname ^ 
			       " is not an integer"))

  method float_param addr =
    let (fullname, subtree) =
      try
	Hashtbl.find addresses addr
      with
	| Not_found ->
	    failwith "#float_param" in
    match subtree with
      | `Parameter(_,_,`Float s) -> s
      | _ ->
	  raise(Config_error(filename ^ ": Parameter " ^ fullname ^ 
			       " is not a floating-point number"))

end


let read_config_file filename =
  new config_file filename
