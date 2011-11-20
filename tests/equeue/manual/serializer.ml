open Uq_engines.Operators

let rec do_something_e n esys =
  if n > 1 then
    eps_e (`Done()) esys
    ++ (fun () ->
	  do_something_e (n-1) esys
       )
  else
    eps_e (`Done()) esys


let do_nothing_e esys =
( object(self)
    inherit [unit] Uq_engines.engine_mixin (`Done()) esys
    method abort() = ()
  end
)


let test() =
  let esys = Unixqueue.create_unix_event_system() in
  let ser = Uq_engines.serializer esys in

  let current = ref None in
  
  let critical_e label n =
    assert(!current = None);
    current := Some label;
    print_endline ("Starting " ^ label);
    (if n > 0 then do_something_e n esys else do_nothing_e esys)
    ++ (fun () ->
	  assert(!current = Some label);
	  current := None;
	  print_endline ("Finished " ^ label);
	  eps_e (`Done()) esys
       ) in

  let e1 =
    ser # serialized
      (fun _ -> critical_e "A" 1) in
  let e2 =
    ser # serialized
      (fun _ -> critical_e "B" 2) in
  let e3 =
    ser # serialized
      (fun _ -> critical_e "C" 3) in
  let e4 = 
    ser # serialized
      (fun _ -> critical_e "D" 10) in
  let e5 = 
    ser # serialized
      (fun _ -> critical_e "E" 0) in
  let e6 =
    do_something_e 2 esys
    ++ (fun () ->
	  ser # serialized
	    (fun _ -> critical_e "G" 2)
       ) in
  let e7 =
    do_something_e 1 esys
    ++ (fun () ->
	  ser # serialized
	    (fun _ -> critical_e "F" 2)
       ) in
  Unixqueue.run esys;
  assert(e1#state = `Done ());
  assert(e2#state = `Done ());
  assert(e3#state = `Done ());
  assert(e4#state = `Done ());
  assert(e5#state = `Done ());
  assert(e6#state = `Done ());
  assert(e7#state = `Done ());
  ()



