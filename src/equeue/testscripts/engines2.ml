(* Test snippet:
 * Opens a server socket on localhost. Every line sent to the server
 * is immediately replied. The "echo" function is implemented by
 * a subprocess. A tridirectional copier is used to connect stdin/stdout
 * of the subprocess with the server socket.
 *)

open Uq_engines;;

(* Unixqueue.set_debug_mode true;; *)
let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0;;
Unix.bind s (Unix.ADDR_INET(Unix.inet_addr_of_string "127.0.0.1",0));;
Unix.listen s 10;;
Unix.set_close_on_exec s;;
let ues = Unixqueue.create_unix_event_system();;

let e1 = new poll_engine [ Unixqueue.Wait_in s, (-1.0) ] ues;;

let rec observe() =
  when_state 
    ~is_done:(fun _ ->
		let (s',addr) = Unix.accept s in
		prerr_endline "Connected!";
		Unix.set_close_on_exec s';

		(* Restart e1 such that further connections can be
		 * accepted in parallel
		 *)
		e1 # restart();
		observe();

		(* Create two pipes: Input from s' goes to s_in;
		 * Output from s_out goes to s'.
		 *)
		
		let (s_in_sub, s_in) = Unix.pipe() in
		let (s_out, s_out_sub) = Unix.pipe() in
		let e2 = new copier (`Tridirectional(s', s_in, s_out)) ues in

		Unix.set_close_on_exec s_in;
		Unix.set_close_on_exec s_out;

		(* Unlike "cat", this sh script copies line-by-line (for better
		 * effect)
		 *)
		let pid =
		  Unix.create_process 
		    "sh" 
		    [| "sh"; "-c"; "while read line; do echo $line; done" |] 
		    s_in_sub s_out_sub Unix.stderr in

		Unix.close s_in_sub;
		Unix.close s_out_sub;
		
		when_state
		  ~is_done:   (fun _ -> ignore(Unix.waitpid [] pid))
		  ~is_aborted:(fun _ -> ignore(Unix.waitpid [] pid))
		  ~is_error:  (fun _ -> ignore(Unix.waitpid [] pid))
		  e2;

             )
    e1
in
observe()
;;

Unixqueue.run ues;;
