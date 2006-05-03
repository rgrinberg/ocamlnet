(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

type from_uni_list =
    U_nil
  | U_single of (int*int)
  | U_double of (int*int * int*int)
  | U_array of int array
;;

let to_unicode = Hashtbl.create 50;;
let from_unicode = Hashtbl.create 50;;

let f_lock = ref (fun () -> ());;
let f_unlock = ref (fun () -> ());;

let lock () = !f_lock();;
let unlock () = !f_unlock();;


let get_to_unicode enc_name : int array =
  lock();
  try
    let table =
      try
	Hashtbl.find to_unicode enc_name
      with
	  Not_found ->
	    let t = Netdb.read_db ("cmapf." ^ enc_name) in
	    Hashtbl.add to_unicode enc_name t;
	    t
    in
    unlock();
    table
  with
      error -> 
	unlock();
	raise error
;;


let get_from_unicode enc_name : from_uni_list array =
  lock();
  try
    let table =
      try
	Hashtbl.find from_unicode enc_name
      with
	  Not_found ->
	    let t = Netdb.read_db ("cmapr." ^ enc_name) in
	    Hashtbl.add from_unicode enc_name t;
	    t
    in
    unlock();
    table
  with
      error -> 
	unlock();
	raise error
;;


let init_mt new_f_lock new_f_unlock =
  f_lock := new_f_lock;
  f_unlock := new_f_unlock
;;

(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 2.3  2003/06/03 19:00:14  stolpmann
 * 	new implementation basing on Netdb
 *
 * Revision 2.2  2002/06/23 19:47:29  stolpmann
 * 	Improved representation of character mappings.
 *
 * Revision 2.1  2001/09/14 14:22:34  stolpmann
 * 	Initial revision (sourceforge)
 *
 *
 * ======================================================================
 * Revision 1.1  2000/08/28 23:17:54  gerd
 * 	Initial revision.
 *
 * 
 *)
