(* $Id$
 *
 *)

(*
 * Copyright 2005 Baretta s.r.l. and Gerd Stolpmann
 *
 * This file is part of Nethttpd.
 *
 * Nethttpd is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Nethttpd is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with WDialog; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

open Nethttp
open Nethttp.Header

exception Standard_response of http_status * http_header option * string option

class type virtual v_extended_environment =
object
  inherit Netcgi_compat.Netcgi_env.cgi_environment
  method virtual server_socket_addr : Unix.sockaddr
  method virtual remote_socket_addr : Unix.sockaddr

  method cgi_request_uri : string
  method log_error : string -> unit
  method cgi_properties : (string * string) list
  method input_header : http_header
  method output_header : http_header
  method set_status : http_status -> unit
  method send_file : Unix.file_descr -> int64 -> unit
end


class type extended_environment =
object
  inherit v_extended_environment
  method server_socket_addr : Unix.sockaddr
  method remote_socket_addr : Unix.sockaddr
end


class virtual empty_environment =
object(self)
  val mutable config = Netcgi_compat.Netcgi_env.default_config
  val mutable in_state = (`Start : Netcgi_compat.Netcgi_env.input_state)
  val mutable out_state = (`Start : Netcgi_compat.Netcgi_env.output_state)
  val mutable in_header = new Netmime.basic_mime_header []
  val mutable out_header = new Netmime.basic_mime_header []
  val mutable properties = []
  val mutable in_channel = new Netchannels.input_string ""
  val mutable out_channel = new Netchannels.output_null()
  val mutable protocol = `Other

  method virtual server_socket_addr : Unix.sockaddr
  method virtual remote_socket_addr : Unix.sockaddr

  method config = config

  method cgi_properties = properties

  method cgi_property ?default name =
    try
      List.assoc name properties
    with
	Not_found ->
	  ( match default with
	      | None -> raise Not_found
	      | Some d -> d
	  )

  method cgi_gateway_interface = self # cgi_property ~default:"" "GATEWAY_INTERFACE"
  method cgi_server_software   = self # cgi_property ~default:"" "SERVER_SOFTWARE"
  method cgi_server_name       = self # cgi_property ~default:"" "SERVER_NAME"
  method cgi_server_protocol   = self # cgi_property ~default:"" "SERVER_PROTOCOL"
  method cgi_server_port       = ( try 
				Some(int_of_string(self # cgi_property "SERVER_PORT"))
                                   with Not_found -> None )
  method cgi_request_method    = self # cgi_property ~default:"" "REQUEST_METHOD"
  method cgi_path_info         = self # cgi_property ~default:"" "PATH_INFO"
  method cgi_path_translated   = self # cgi_property ~default:"" "PATH_TRANSLATED"
  method cgi_script_name       = self # cgi_property ~default:"" "SCRIPT_NAME"
  method cgi_query_string      = self # cgi_property ~default:"" "QUERY_STRING"
  method cgi_remote_host       = self # cgi_property ~default:"" "REMOTE_HOST"
  method cgi_remote_addr       = self # cgi_property ~default:"" "REMOTE_ADDR"
  method cgi_auth_type         = self # cgi_property ~default:"" "AUTH_TYPE"
  method cgi_remote_user       = self # cgi_property ~default:"" "REMOTE_USER"
  method cgi_remote_ident      = self # cgi_property ~default:"" "REMOTE_IDENT"
  method cgi_https             = match self # cgi_property ~default:"" "HTTPS" with
                                   | "" | "off" -> false | "on" -> true
                                   | _ -> failwith "Cannot interpret HTTPS property"
  method cgi_request_uri       = self # cgi_property ~default:"" "REQUEST_URI"

  method protocol = (protocol : Netcgi_compat.Netcgi_env.protocol)

  method send_output_header() = ()

  method send_file (fd:Unix.file_descr) (n:int64) = ()

  method log_error (s : string) = ()

  method output_state = out_state

  method set_output_state s =
    out_state <- s;
    if s = `End then
      ( try out_channel # close_out() with Netchannels.Closed_channel -> () )

  method input_header = in_header

  method output_header = out_header

  method set_status ( st : http_status ) = 
    out_header # update_field "Status" (string_of_int (int_of_http_status st))

  (* ---- The following is copied from Netcgi_env: ---- *)

  method input_header = in_header

  method input_header_field ?default name =
    try in_header # field name
    with Not_found as nf ->
      match default with
          None -> raise nf
        | Some d -> d

  method multiple_input_header_field name =
    in_header # multiple_field name

  method input_header_fields =
    in_header # fields

  method user_agent =
    self # input_header_field ~default:"" "USER-AGENT"

  method cookies =
    get_cookie self#input_header

  method input_ch = in_channel

  method input_content_length =
    int_of_string (self # input_header_field "CONTENT-LENGTH")

  method input_content_type_string =
    self # input_header_field ~default:"" "CONTENT-TYPE"

  method input_content_type =
    Mimestring.scan_mime_type_ep (self # input_header_field "CONTENT-TYPE") []

  method input_state = in_state

  method set_input_state s = in_state <- s

  method output_header =
    out_header

  method output_header_field ?default name =
    try out_header # field name
    with Not_found as nf ->
      match default with
          None -> raise nf
        | Some d -> d

  method multiple_output_header_field name =
    out_header # multiple_field name

  method output_header_fields =
    out_header # fields

  method output_ch =
    out_channel

  method set_output_header_field name value =
    out_header # update_field name value

  method set_multiple_output_header_field name values =
    out_header # update_multiple_field name values

  method set_output_header_fields h =
    out_header # set_fields h
end


class redirected_environment
        ?in_state:new_in_state 
        ?in_header:new_in_header 
        ?properties:new_properties 
        ?in_channel:(new_in_channel = new Netchannels.input_string "") 
        (env : extended_environment) =
object(self)
  inherit empty_environment
    (* Inherits new containers for both input and output *)

  initializer (
    config <- env # config;
    in_state <- ( match new_in_state with Some s -> s | None -> env # input_state );
    in_header <- ( match new_in_header with 
		     | Some h -> h 
		     | None -> new Netmime.basic_mime_header env#input_header_fields );
    properties <- ( match new_properties with
		      | Some p -> p 
		      | None -> env # cgi_properties );
    in_channel <- new_in_channel;
  )

  (* The following methods are always delegated to [env]: *)

  method server_socket_addr = env # server_socket_addr
  method remote_socket_addr = env # remote_socket_addr

  method protocol = env # protocol
  method send_output_header = env # send_output_header
  method log_error = env # log_error
  method output_state = env # output_state
  method set_output_state = env # set_output_state
  method output_header = env # output_header
  method output_header_field = env # output_header_field
  method multiple_output_header_field = env # multiple_output_header_field
  method output_header_fields = env # output_header_fields
  method output_ch = env # output_ch
  method set_output_header_field = env # set_output_header_field
  method set_multiple_output_header_field = env # set_multiple_output_header_field
  method set_output_header_fields = env # set_output_header_fields
  method set_status = env # set_status
  method send_file = env # send_file
end


let output_static_response (env : #extended_environment) status hdr_opt body =
  ( match hdr_opt with
      | Some hdr -> 
	  env # output_header # set_fields hdr#fields;  (* Replaces any existing fields *)
      | None -> ()
  );
  ( match status with
      | `No_content
      | `Reset_content
      | `Not_modified -> ()
      | _ ->
	  ( try ignore(env # output_header_field "Content-Type")
	    with Not_found -> env # set_output_header_field "Content-type" "text/html";
	  );
  );
  env # set_output_header_field "Content-Length" (string_of_int (String.length body));
  env # set_status status;
  env # send_output_header();
  env # output_ch # output_string body;
  env # output_ch # close_out();
;;


let rec output_channel_large (f_to : Netchannels.out_obj_channel) f_from length =
  if length > 0L then (
    let n = min length (Int64.of_int max_int) in
    f_to # output_channel ~len:(Int64.to_int n) f_from;
    output_channel_large f_to f_from (Int64.sub length n)
  )
  else
    ()

(* -- old implementation:
let output_file_response (env : #extended_environment) status hdr_opt filename pos length =
  Netchannels.with_in_obj_channel 
    ( let f = open_in_bin filename in  (* or Sys_error *)
      LargeFile.seek_in f pos;
      new Netchannels.input_channel f)
    (fun f_ch  ->
       ( match hdr_opt with
	   | Some hdr -> 
	       env # output_header # set_fields hdr#fields; 
	       (* Replaces any existing fields *)
	   | None -> ()
       );
       ( match status with
	   | `No_content
	   | `Reset_content
	   | `Not_modified -> ()
	   | _ ->
	       ( try ignore(env # output_header_field "Content-Type")
		 with Not_found -> env # set_output_header_field "Content-type" "text/html";
	       );
       );
       env # set_output_header_field "Content-Length" (Int64.to_string length);
       env # send_output_header();
       output_channel_large env # output_ch f_ch length;
       env # output_ch # close_out();
    )
 *)


let output_file_response env status 
                         hdr_opt filename pos length =
  let fd = Unix.openfile filename [Unix.O_RDONLY] 0 in
  ignore(Unix.LargeFile.lseek fd pos Unix.SEEK_SET);
  ( match hdr_opt with
      | Some hdr -> 
	  env # output_header # set_fields hdr#fields; 
	  (* Replaces any existing fields *)
      | None -> ()
  );
  env # set_output_header_field "Status" (string_of_int (int_of_http_status status));
  env # send_file fd length
;;


class type min_config =
object
  method config_error_response : int -> string
  method config_log_error : 
    Unix.sockaddr option -> Unix.sockaddr option -> http_method option -> http_header option -> string -> unit
end


let output_std_response config env status hdr_opt msg_opt =
  ( match msg_opt with
      | Some msg ->
	  let req_meth = env # cgi_request_method in
	  let req_uri = env # cgi_request_uri in
	  let req_hdr = new Netmime.basic_mime_header env#input_header_fields in
	  config # config_log_error
	    (Some env#server_socket_addr) (Some env#remote_socket_addr) 
	    (Some(req_meth,req_uri)) (Some req_hdr) msg
      | None -> ()
  );
  let code = int_of_http_status status in
  let body = 
    match status with
      | `No_content
      | `Reset_content
      | `Not_modified -> ""
      | _ -> config # config_error_response code in
  let hdr_opt' =
    match hdr_opt with
      | Some h -> Some h
      | None -> Some (new Netmime.basic_mime_header []) in
  output_static_response env status hdr_opt' body


exception Redirect_request of string * http_header
exception Redirect_response of string * http_header


class type http_service_generator =
object
  method generate_response : extended_environment -> unit
end



class type http_service_receiver =
object
  method process_body : extended_environment -> http_service_generator
end


type http_service_reaction =
    [ `Accept_body of http_service_receiver
    | `Reject_body of http_service_generator
    | `Static of http_status * http_header option * string
    | `File of http_status * http_header option * string * int64 * int64
    | `Std_response of http_status * http_header option * string option
    ]


class type ['a] http_service =
object
  method name : string
  method def_term :'a
  method print : Format.formatter -> unit
  method process_header : extended_environment -> http_service_reaction
end


let update_alist updl l =
  updl @ (List.filter (fun (x,y) -> not (List.mem_assoc x updl)) l)

