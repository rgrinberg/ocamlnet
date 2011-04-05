(*
open Netmcore_mempool;;
let p = create_mempool 65536;;
#spawn;;
 *)

#directory "../netsys";;
open Printf
open Netmcore_heap;;
let p = `Resource 2;;
Debug.enable := true;;
let h = create_heap p 5000 (ref None);;
print_endline(debug_info h);;

let upto n =
  let rec loop k =
    if k < n then
      k :: loop (k+1)
    else
      [] in
  loop 0;;
  
modify h (fun add -> (root h) := add (Some (upto 10)));;

let addr x =
  sprintf "%nx" (Netsys_mem.obj_address (Obj.repr x));;

let unopt =
  function
    | None -> failwith "unopt"
    | Some x -> x ;;

with_value h (fun () -> List.tl (unopt (!(root h)))) (fun tl -> modify h (fun add -> (root h) := add (Some tl)));;

(*
modify h (fun add -> (root h) := add (Some "Gerd"));;
 *)
