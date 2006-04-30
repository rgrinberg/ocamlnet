(* Open a socket, find out the port, and exit the process.
 * The port is guaranteed to refuse a connection for a 
 * moment.
 *)

let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
Unix.listen s 10;
let s_name = Unix.getsockname s in
let Unix.ADDR_INET(inetaddr,port) = s_name in
print_string (string_of_int port);

