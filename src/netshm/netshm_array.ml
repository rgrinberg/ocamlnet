(* $Id$ *)

open Netshm
open Netshm_data

type 'a t =
    { table : shm_table;
      idx_only_manager : int data_manager;
      idx_val_manager : (int * 'a) data_manager;
      idx_hash_fn : int -> int32;
      default_val : 'a;
    }


let manage ?pagesize ?init defval val_manager lm sd =
  (* TODO: init *)
  let t = Netshm.manage ?pagesize ?init lm sd in
  let idx_manager = Netshm_data.int_manager in
  let idx_only_manager =
    left_pair_manager idx_manager in
  let idx_val_manager =
    pair_manager idx_manager val_manager in
  { table = t;
    idx_only_manager = idx_only_manager;
    idx_val_manager = idx_val_manager;
    idx_hash_fn = idx_manager.hash_fn;
    default_val = defval
  }

let shm_table t = t.table

let length t =
  try Int32.to_int (Netshm.find t.table (-1l)).{ 0 }
  with Not_found -> 
    raise(Corrupt_file "Netshm_array: Entry missing for array length")
;;


let get t k = assert false

let set t k x = assert false

let swap t k1 k2 = assert false

let resize t n = assert false

let default_value t = assert false

let shm_table t = assert false
