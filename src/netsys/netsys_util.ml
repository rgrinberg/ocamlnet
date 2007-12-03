(* $Id$ *)

(* this & that *)


let mem_sorted_array x a =
  let rec search l h =
    if l < h then (
      let m = (l+h) / 2 in
      let r = Pervasives.compare x a.(m) in
      if r = 0 then
	true
      else
	if r < 0 then
	  search l m
	else
	  search (m+1) h
    )
    else false
  in
  search 0 (Array.length a)
