(* Our custom HTML generator *)

(* See chtml_ocaml4.ml *)

open Printf
open Odoc_info
open Module

module StringSet = Odoc_html.StringSet


let word_re = Str.regexp "[ \t\r\n]+"

let split_args t =
  match t with
    | [] -> []
    | [Odoc_info.Raw arg] -> Str.split word_re arg
    | _ ->
	failwith "Argument too complicated"



class chtml =
object(self)
  inherit Odoc_html.html as super

  method private html_of_picture b t = 
    let (file, caption) =
      match split_args t with
	| [] ->
	    failwith "{picture ...} needs at least one argument"
	| w ->
	    ( match w with
		| file :: args ->
		    (file, String.concat " " args)
		| [] ->
		    failwith "{picture ...} needs a simple word as first argument"
	    ) in
    bprintf b
      "<div class=\"picture\">\
        <div class=\"picture-caption\">%s</div>\
        <img src=\"%s\">\
       </div>"
      (self#escape caption)
      file

  method private html_of_div tag b t =
    let html_classes =
      match split_args t with
	| [] ->
	     failwith (sprintf "{%s ...} needs at least one argument" tag)
	| w ->
             w in
    bprintf b
      "<%s class=\"%s\">"
      tag
      (String.concat " " (List.map self#escape html_classes));

  method private html_of_divend tag b t =
    bprintf b "</%s>" tag


  val mutable enable_direct_include = false

  method private html_of_direct_include b t =
    match split_args t with
      | ["true"] ->
	  enable_direct_include <- true
      | ["false"] ->
	  enable_direct_include <- false
      | _ ->
	  failwith "{directinclude ...} needs one bool argument"


  method html_of_included_module b im =   (* overridden! *)
    super # html_of_included_module b im;
    if enable_direct_include then (
      match im.im_module with
	| None -> ()    (* case module is unknown *)
	| Some (Mod m) ->
	    bprintf b "<div class=\"included-module\">\n";
	    List.iter
              (self#html_of_module_element b (Name.father m.m_name))
              (Module.module_elements m);
	    bprintf b "</div>\n"
	| Some (Modtype mt) ->
	    bprintf b "<div class=\"included-module-type\">\n";
	    List.iter
              (self#html_of_module_element b (Name.father mt.mt_name))
              (Module.module_type_elements mt);
	    bprintf b "</div>\n"
    )


  method add_known_type t =
    List.iter
      (fun s ->
	 known_types_names <- StringSet.add s known_types_names
      )
      (split_args t)

  method add_known_class t =
    List.iter
      (fun s ->
	 known_classes_names <- StringSet.add s known_classes_names
      )
      (split_args t)

  method html_of_custom_text b s t =
    match s with
	(* left brace: up to ocamldoc-3.11 *)
      | "{picture" | "picture" -> self#html_of_picture b t
      | "{div" | "div" -> self#html_of_div "div" b t
      | "{divend" | "divend" -> self#html_of_divend "div" b t
      | "{table" | "table" -> self#html_of_div "table" b t
      | "{tableend" | "tableend" -> self#html_of_divend "table" b t
      | "{tr" | "tr" -> self#html_of_div "tr" b t
      | "{trend" | "trend" -> self#html_of_divend "tr" b t
      | "{td" | "td" -> self#html_of_div "td" b t
      | "{tdend" | "tdend" -> self#html_of_divend "td" b t
      | "{directinclude" | "directinclude" -> self#html_of_direct_include b t
      | "{knowntype" | "knowntype" -> self#add_known_type t
      | "{knownclass" | "knownclass" -> self#add_known_class t
      | _ -> ()
end

let chtml = new chtml
let () = 
  ignore(
    Odoc_args.set_doc_generator (Some chtml :> Odoc_args.doc_generator option))
