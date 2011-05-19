open Http_client

(* Number of iterations required to trigger bug. *)
let max_count = 4

let string_of_status = function
  | `Client_error          -> "client_error"
  | `Http_protocol_error e -> "http_protocol_error(" ^ Netexn.to_string e ^ ")"
  | `Redirection           -> "redirection"
  | `Server_error          -> "server_error"
  | `Successful            -> "successful"
  | `Unserved              -> "unserved"

let rec main esys pipe count url =
  if count < max_count then (
    let call = new get url in
    pipe # add_with_callback call
      (fun call ->
         let status = call # status in
         print_endline (url ^ ": " ^ (string_of_status status));
         main esys pipe (succ count) url
      )
  )

let () =
  let esys  = Unixqueue.create_unix_event_system () in
  let pipe  = new pipeline in
  pipe # set_event_system esys;

  (* Commenting-out these lines fixes the symptom... *)
  let cache = create_aggressive_cache () in
  pipe # set_connection_cache cache;

  (* twitter: sends immediately a close-connection, so no persistency *)
  main esys pipe 0 "http://twitter.com/hhamsters";
  Unixqueue.run esys;

  (* facebook: allows persistency *)
  main esys pipe 0 "http://www.facebook.com";
  Unixqueue.run esys;

  (* So facebook should be still open here *)
  main esys pipe 0 "http://www.facebook.com";
  Unixqueue.run esys
  
