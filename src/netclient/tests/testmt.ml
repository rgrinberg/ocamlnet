(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Http_client;;

let work1() =
  let g = new get "http://localhost/" in
  let p = new pipeline in
  p # verbose [ Verbose_status ];
  p # add g;
  p # run();
  let (_,code,_) = g # dest_status() in
  if code = 200 then
    prerr_endline "OK"
  else
    prerr_endline ("ERROR: code " ^ string_of_int code)
;;

open Convenience;;

let work2() =
  let g = http_get_message "http://localhost/" in
  let (_,code,_) = g # dest_status() in
  if code = 200 then
    prerr_endline "OK"
  else
    prerr_endline ("ERROR: code " ^ string_of_int code)
;;


let l = ref [] in
for n = 1 to 20 do
  let t = Thread.create work1 () in
  l := t :: !l
done;

List.iter
  (fun t -> Thread.join t)
  !l
;;


prerr_endline "-------------------------------------------";
let l = ref [] in
for n = 1 to 20 do
  let t = Thread.create work2 () in
  l := t :: !l
done;

List.iter
  (fun t -> Thread.join t)
  !l
;;


