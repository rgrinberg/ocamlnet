(* $Id$ *)

open Netplex_types

exception Config_error of string

val read_config_file : string -> config_file

(*
val read_netplex_config : 
      processor_config list -> 
      workload_config list ->
      string -> 
        netplex_config
 *)
