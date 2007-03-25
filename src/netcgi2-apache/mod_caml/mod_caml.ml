(* ML part of mod_caml.
 * Copyright (C) 2003 Merjis Ltd.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the Free
 * Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * $Id: mod_caml.ml,v 1.12 2005/08/01 15:40:06 ChriS Exp $
 *)

(* Note a peculiarity of Apache: it runs all initialization TWICE. See this
 * page for an explanation of this feature:
 * http://thingy.kcilink.com/modperlguide/config/
 *   Apache_Restarts_Twice_On_Start.html
 * The upshot is that the configuration code here will be called twice.
 *)

open Mod_caml_config
open Apache

type server_config_t = {
  translate_handler : handler_t option;
}

type dir_config_t = {
  location : string option;
  check_user_id : handler_t option;
  auth_checker : handler_t option;
  access_checker : handler_t option;
  type_checker : handler_t option;
  fixer_upper : handler_t option;
  logger : handler_t option;
  header_parser : handler_t option;
  post_read_request : handler_t option;
  ocaml_bytecode_handler : handler_t option;
}

(* Not very useful - gets called later than you would expect. *)
let module_init () =
  ()

(*----- Initialize Dynlink library. -----*)

let () =
  try
    Dynlink.init ();
    Dynlink.allow_unsafe_modules true
  with
    Dynlink.Error (e) -> failwith (Dynlink.error_message e)

(*----- Configuration. -----*)

let create_dir_config dirname =
  { location = dirname;
    check_user_id = None;
    auth_checker = None;
    access_checker = None;
    type_checker = None;
    fixer_upper = None;
    logger = None;
    header_parser = None;
    post_read_request = None;
    ocaml_bytecode_handler = None }

let merge_dir_config base add =
  let merged = base in
  let merged =
    if add.location <> None then
      { merged with location = add.location } else merged in
  let merged =
    if add.check_user_id <> None then
      { merged with check_user_id = add.check_user_id } else merged in
  let merged =
    if add.auth_checker <> None then
      { merged with auth_checker = add.auth_checker } else merged in
  let merged =
    if add.access_checker <> None then
      { merged with access_checker = add.access_checker } else merged in
  let merged =
    if add.type_checker <> None then
      { merged with type_checker = add.type_checker } else merged in
  let merged =
    if add.fixer_upper <> None then
      { merged with fixer_upper = add.fixer_upper } else merged in
  let merged =
    if add.logger <> None then
      { merged with logger = add.logger } else merged in
  let merged =
    if add.header_parser <> None then
      { merged with header_parser = add.header_parser } else merged in
  let merged =
    if add.post_read_request <> None then
      { merged with post_read_request = add.post_read_request } else merged in
  let merged =
    if add.ocaml_bytecode_handler <> None then
      { merged with ocaml_bytecode_handler = add.ocaml_bytecode_handler }
    else merged in
  merged

let create_server_config s =
  { translate_handler = None; }

let merge_server_config base add =
  let merged = base in
  let merged =
    if add.translate_handler <> None then
      { merged with translate_handler = add.translate_handler } else merged in
  merged

external get_server_config : Request.t -> server_config_t
    = "mod_caml_get_server_config"

external get_dir_config : Request.t -> dir_config_t
    = "mod_caml_get_dir_config"

(*----- Handlers. -----*)

let translate_handler r =
  let config = try Some (get_server_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.translate_handler with
          | Some handler -> handler r
          | None -> DECLINED

let check_user_id r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.check_user_id with
          | Some handler -> handler r
          | None -> DECLINED

let auth_checker r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.auth_checker with
          | Some handler -> handler r
          | None -> DECLINED

let access_checker r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.access_checker with
          | Some handler -> handler r
          | None -> DECLINED

let type_checker r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.type_checker with
          | Some handler -> handler r
          | None -> DECLINED

let fixer_upper r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.fixer_upper with
          | Some handler -> handler r
          | None -> DECLINED

let logger r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.logger with
          | Some handler -> handler r
          | None -> DECLINED

let header_parser r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.header_parser with
          | Some handler -> handler r
          | None -> DECLINED

let post_read_request r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.post_read_request with
          | Some handler -> handler r
          | None -> DECLINED

let ocaml_bytecode_handler r =
  let config = try Some (get_dir_config r) with Not_found -> None in
  match config with
    | None -> DECLINED
    | Some config ->
        match config.ocaml_bytecode_handler with
          | Some handler -> handler r
          | None -> DECLINED

(*----- Handler registration. -----*)

let reg_table = Hashtbl.create 16

let reg_module_name = ref None

(* Register the module's handler. *)
let register_handler handler name =
  match !reg_module_name with
  | None ->
      failwith("Mod_caml.register_handler call outside module initialization")
  | Some module_name ->
      Hashtbl.replace reg_table (module_name ^ "." ^ name) handler

(*----- Commands. -----*)

(* CamlLoad [filename].  Preprend the ocaml standard library path
   (ocamlc -where) if [filename] is relative. *)
let cmd_load filename =
  let filename =
    if Filename.is_relative filename then
      Filename.concat ocamllibdir filename
    else filename in
  reg_module_name := Some(String.capitalize(Filename.chop_extension
                                              (Filename.basename filename)));
  try  Dynlink.loadfile filename;
  with Dynlink.Error (e) -> failwith (Dynlink.error_message e)

let cmd_translate_handler sconfig name =
  { sconfig with translate_handler = Some (Hashtbl.find reg_table name) }

let cmd_check_user_id_handler dconfig name =
  { dconfig with check_user_id = Some (Hashtbl.find reg_table name) }

let cmd_auth_checker_handler dconfig name =
  { dconfig with auth_checker = Some (Hashtbl.find reg_table name) }

let cmd_access_checker_handler dconfig name =
  { dconfig with access_checker = Some (Hashtbl.find reg_table name) }

let cmd_type_checker_handler dconfig name =
  { dconfig with type_checker = Some (Hashtbl.find reg_table name) }

let cmd_fixer_upper_handler dconfig name =
  { dconfig with fixer_upper = Some (Hashtbl.find reg_table name) }

let cmd_logger_handler dconfig name =
  { dconfig with logger = Some (Hashtbl.find reg_table name) }

let cmd_header_parser_handler dconfig name =
  { dconfig with header_parser = Some (Hashtbl.find reg_table name) }

let cmd_post_read_request_handler dconfig name =
  { dconfig with post_read_request = Some (Hashtbl.find reg_table name) }

let cmd_handler dconfig name =
  { dconfig with ocaml_bytecode_handler = Some (Hashtbl.find reg_table name) }

(*----- Register functions. -----*)

let () =
  let cb = Callback.register in
  cb "mod_caml_create_dir_config"      create_dir_config;
  cb "mod_caml_merge_dir_config"       merge_dir_config;
  cb "mod_caml_create_server_config"   create_server_config;
  cb "mod_caml_merge_server_config"    merge_server_config;
  cb "mod_caml_module_init"            module_init;

  cb "mod_caml_translate_handler"      translate_handler;
  cb "mod_caml_check_user_id"          check_user_id;
  cb "mod_caml_auth_checker"           auth_checker;
  cb "mod_caml_access_checker"         access_checker;
  cb "mod_caml_type_checker"           type_checker;
  cb "mod_caml_fixer_upper"            fixer_upper;
  cb "mod_caml_logger"                 logger;
  cb "mod_caml_header_parser"          header_parser;
  cb "mod_caml_post_read_request"      post_read_request;
  cb "mod_caml_ocaml_bytecode_handler" ocaml_bytecode_handler;

  cb "mod_caml_cmd_load"                   cmd_load;
  cb "mod_caml_cmd_translate_handler"      cmd_translate_handler;
  cb "mod_caml_cmd_check_user_id_handler"  cmd_check_user_id_handler;
  cb "mod_caml_cmd_auth_checker_handler"   cmd_auth_checker_handler;
  cb "mod_caml_cmd_access_checker_handler" cmd_access_checker_handler;
  cb "mod_caml_cmd_type_checker_handler"   cmd_type_checker_handler;
  cb "mod_caml_cmd_fixer_upper_handler"    cmd_fixer_upper_handler;
  cb "mod_caml_cmd_logger_handler"         cmd_logger_handler;
  cb "mod_caml_cmd_header_parser_handler"  cmd_header_parser_handler;
  cb "mod_caml_cmd_post_read_request_handler" cmd_post_read_request_handler;
  cb "mod_caml_cmd_handler"                cmd_handler
