(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

open Netcgi_jserv
open Netcgi_jserv_ajp12
open Netcgi

let text = Netencoding.Html.encode_from_latin1;;

(* Thread pool: *)

let mutex = Mutex.create();;
let n_workers = 10;;
let n_free_workers = ref n_workers;;
let jobs = Queue.create();;
let n_jobs = ref 0;;
let free_workers_exist = Condition.create();;
let jobs_exist = Condition.create();;

  (* mutex: Protects the other variables.
   * n_workers: The total number of workers, both free and busy workers.
   * n_free_workers: The number of free workers.
   * jobs: The queue of jobs. Jobs are functions unit->unit. The free
   *   workers wait until there are jobs in the queue, and will take new
   *   jobs. The queue must not become longer than the number of free
   *   workers.
   * n_jobs: The length of the queue.
   * free_workers_exist: This variable signals that there are now more free
   *   workers than jobs, so it is possible to add new jobs to the queue.
   * jobs_exist: This variable signals that there are now jobs, so the
   *   free workes should look at the queue.
   *)

let request_handler zone servlet env =
  let cgi = new std_activation ~env () in
  cgi # set_header();
  let out = cgi # output # output_string in
  out "<H1>Hello world!</H1>\n";
  out "<H2>Properties</H2>\n";
  out "<UL>\n";
  List.iter
    (fun name ->
       try
	 let v = env # cgi_property name in
	 out "<LI>";
	 out (text name ^ "=" ^ v ^ "\n")
       with
	   Not_found -> ()
    )
    [ (* Standard: *)
      "GATEWAY_INTERFACE";
      "SERVER_SOFTWARE";
      "SERVER_NAME";
      "SERVER_PROTOCOL";
      "SERVER_PORT";
      "REQUEST_METHOD";
      "PATH_INFO";
      "PATH_TRANSLATED";
      "SCRIPT_NAME";
      "QUERY_STRING";
      "REMOTE_HOST";
      "REMOTE_ADDR";
      "AUTH_TYPE";
      "REMOTE_USER";
      (* Apache: *)
      "DOCUMENT_ROOT";
      "REQUEST_URI";
      "SCRIPT_FILENAME";
      "JSERV_ZONE";
      "JSERV_SERVLET";
      "JSERV_ROUTE";
      "HOSTNAME";
    ];
  out "</UL>\n";
  
  out "<H2>Header</H2>\n";
  out "<UL>\n";
  List.iter
    (fun (name,v) ->
       out "<LI>";
       out (text name ^ "=" ^ v ^ "\n")
    )
    (env # input_header_fields);
  out "</UL>\n";
  
  out "<H2>CGI parameters</H2>\n";
  out "<UL>\n";
  List.iter
    (fun (name,arg) ->
       out "<LI>";
       let mimetype = arg # content_type in
       out (text name ^ ":" ^ text mimetype ^ "=");
       ( match arg # filename with
	     None -> ()
	   | Some fn -> out ("[filename=" ^ text fn ^ "] ")
       );
       out (text arg#value);
    )
    (cgi#arguments);
  
  let action = text (cgi # url()) in
  
  out "</UL>\n";
  out "<h2>GET URL-encoded form</h2>\n";
  out ("<form action=\"" ^ action ^ "\" method=GET>\n");
  out "<input type=text name=line>\n";
  out "<input type=submit name=submit value=\"Submit\">\n";
  out "</form>\n";
  
  out "<h2>POST URL-encoded form</h2>\n";
  out ("<form action=\"" ^ action ^ "\" method=POST>\n");
  out "<input type=text name=line>\n";
  out "<input type=submit name=submit value=\"Submit\">\n";
  out "</form>\n";
  
  out "<h2>POST FORM-encoded form</h2>\n";
  out ("<form action=\"" ^ action ^ "\" method=POST enctype=\"multipart/form-data\">\n");
  out "<input type=text name=line>\n";
  out "<input type=submit name=submit value=\"Submit\">\n";
  out "</form>\n";
  
  out "<h2>File upload</h2>\n";
  out ("<form action=\"" ^ action ^ "\" method=POST enctype=\"multipart/form-data\">\n");
  out "<input type=text name=line>\n";
  out "<input type=file name=file>\n";
  out "<input type=submit name=submit value=\"Submit\">\n";
  out "</form>\n";
  
  cgi # output # commit_work();
;;


let schedule_job j =
  (* Wait until there is a free worker, and add the job to the queue: *)
  Mutex.lock mutex;
  while !n_free_workers <= !n_jobs do
    Condition.wait free_workers_exist mutex
  done;
  Queue.add j jobs;
  incr n_jobs;
  Condition.signal jobs_exist;
  Mutex.unlock mutex
;;


let onconnect srv auth inch outch =
  let do_job () =
    try
      serve_connection request_handler auth inch outch
    with
	Signal_restart ->
	  signal_restart srv
      | Signal_shutdown ->
	  signal_shutdown srv
  in
  schedule_job do_job
;;

exception Terminate

let onrestart srv =
  prerr_endline "Restart"
;;


let onshutdown srv =
  prerr_endline "Shutdown";
  for i = 1 to n_workers do
    schedule_job (fun () -> raise Terminate)
  done
;;


let work() =
  prerr_endline "Worker thread starts up";
  try
    Mutex.lock mutex;
    while true do
      (* Wait until there are jobs: *)
      while !n_jobs = 0 do
	Condition.wait jobs_exist mutex
      done;
      (* Take the first job: *)
      let job = Queue.take jobs in
      decr n_jobs;
      decr n_free_workers;
      Mutex.unlock mutex;
      (* Do the job: *)
      begin try
	job()
      with 
	  Terminate -> raise Terminate
	| any -> 
	    prerr_endline("Uncaught exception: " ^ Printexc.to_string any);
      end;
      (* Now free again: *)
      Mutex.lock mutex;
      if !n_free_workers = !n_jobs then Condition.signal free_workers_exist;
      incr n_free_workers;
    done
  with
      Terminate ->
	prerr_endline "Worker thread terminates.";
	Mutex.unlock mutex
;;


let m = Mutex.create() in
let lock() = Mutex.lock m in
let unlock() = Mutex.unlock m in
prng_init_from_file ~lock ~unlock ~length:8 "/dev/random";;


jvm_emu_main
  (fun props auth addr port ->
     let tlist = ref [] in
     for i = 1 to n_workers do
       let thr = Thread.create work () in 
       tlist := thr :: !tlist
     done;
     server 
       ~onrestart 
       ~onshutdown
       onconnect auth addr port;
     prerr_endline "Waiting for workers...";
     List.iter (fun thr -> Thread.join thr) !tlist;
     prerr_endline "Successfully terminated.";
     flush stderr;
  );;


(* ======================================================================
 * History:
 * 
 * $Log$
 * Revision 1.1  2001/12/09 20:26:09  stolpmann
 * 	Initial revision.
 *
 *)
