let cookie_re = Pcre.regexp "[ \t]*;[ \t]*"

(* Added "." to support Xanga cookies:  *)
let nv_re = Pcre.regexp "^([a-zA-Z0-9_.]+)(=(.*))?$"

let get_set_cookie s =
  (* Move to netstring: Nethttp.Header *)
  let nv_list =
    List.map
      (fun item ->
	 ( match Netstring_pcre.string_match nv_re item 0 with
	     | None ->
		 failwith ("get_set_cookie: " ^ item)
	     | Some m ->
		 let name = Netstring_pcre.matched_group m 1 item in
		 let value = 
		   try Netstring_pcre.matched_group m 3 item
		   with Not_found -> "" in
		 (name, value)
	 )
      )
      (Pcre.split ~rex:cookie_re s)
  in

  match nv_list with
    | (n,v) :: params ->
	let params = 
	  List.map (fun (n,v) -> (String.lowercase n, v)) params in
	{ Nethttp.cookie_name = Netencoding.Url.decode ~plus:false n;
	  cookie_value = Netencoding.Url.decode ~plus:false v;
	  cookie_expires = (try
			      let exp_str = List.assoc "expires" params in
			      Some(Netdate.since_epoch
				     (Netdate.parse exp_str))
			    with
			      | Not_found -> None);
	  cookie_domain = ( try
			      Some(List.assoc "domain" params)
			    with
			      | Not_found -> None
			  );
	  cookie_path = ( try
			    Some(List.assoc "path" params)
			  with
			    | Not_found -> None
			);
	  cookie_secure = ( try
			      List.mem_assoc "secure" params
			    with
			      | Not_found -> false
			  )
	}
    | _ ->
	failwith "get_set_cookie"

