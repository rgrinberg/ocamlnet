(* $Id$ *)

type printer = exn -> string

let registry = 
  (Hashtbl.create 50 : (string, (Obj.t * printer) list) Hashtbl.t)
    (* The hashtable maps the name of the exception to an assoc list
       mapping the exception anchor to the printer.
     *)

let register_printer e f =
  let anchor = Obj.field (Obj.repr e) 0 in
  let name = (Obj.obj (Obj.field anchor 0) : string) in
  
  let alist =
    try Hashtbl.find registry name with Not_found -> [] in

  let alist' =
    (anchor, f) :: (List.remove_assq anchor alist) in
  
  Hashtbl.replace registry name alist'


let to_string e =
  let anchor = Obj.field (Obj.repr e) 0 in
  let name = (Obj.obj (Obj.field anchor 0) : string) in

  let f =
    try
      let alist = Hashtbl.find registry name in
      List.assq anchor alist
    with
      | Not_found ->
	  Printexc.to_string in
  f e


(* Add printers for the core exceptions: *)
    
let string_of_unix_code =
  function
    | Unix.E2BIG -> "E2BIG"   
    | Unix.EACCES -> "EACCES"  
    | Unix.EAGAIN -> "EAGAIN"  
    | Unix.EBADF -> "EBADF"   
    | Unix.EBUSY -> "EBUSY"   
    | Unix.ECHILD -> "ECHILD"  
    | Unix.EDEADLK -> "EDEADLK"         
    | Unix.EDOM -> "EDOM"    
    | Unix.EEXIST -> "EEXIST"  
    | Unix.EFAULT -> "EFAULT"  
    | Unix.EFBIG -> "EFBIG"   
    | Unix.EINTR -> "EINTR"   
    | Unix.EINVAL -> "EINVAL"  
    | Unix.EIO -> "EIO"     
    | Unix.EISDIR -> "EISDIR"  
    | Unix.EMFILE -> "EMFILE"  
    | Unix.EMLINK -> "EMLINK"  
    | Unix.ENAMETOOLONG -> "ENAMETOOLONG"    
    | Unix.ENFILE -> "ENFILE"  
    | Unix.ENODEV -> "ENODEV"  
    | Unix.ENOENT -> "ENOENT"  
    | Unix.ENOEXEC -> "ENOEXEC"         
    | Unix.ENOLCK -> "ENOLCK"  
    | Unix.ENOMEM -> "ENOMEM"  
    | Unix.ENOSPC -> "ENOSPC"  
    | Unix.ENOSYS -> "ENOSYS"  
    | Unix.ENOTDIR -> "ENOTDIR"         
    | Unix.ENOTEMPTY -> "ENOTEMPTY"       
    | Unix.ENOTTY -> "ENOTTY"  
    | Unix.ENXIO -> "ENXIO"   
    | Unix.EPERM -> "EPERM"   
    | Unix.EPIPE -> "EPIPE"   
    | Unix.ERANGE -> "ERANGE"  
    | Unix.EROFS -> "EROFS"   
    | Unix.ESPIPE -> "ESPIPE"  
    | Unix.ESRCH -> "ESRCH"   
    | Unix.EXDEV -> "EXDEV"   
    | Unix.EWOULDBLOCK -> "EWOULDBLOCK"     
    | Unix.EINPROGRESS -> "EINPROGRESS"     
    | Unix.EALREADY -> "EALREADY"        
    | Unix.ENOTSOCK -> "ENOTSOCK"        
    | Unix.EDESTADDRREQ -> "EDESTADDRREQ"    
    | Unix.EMSGSIZE -> "EMSGSIZE"        
    | Unix.EPROTOTYPE -> "EPROTOTYPE"      
    | Unix.ENOPROTOOPT -> "ENOPROTOOPT"     
    | Unix.EPROTONOSUPPORT -> "EPROTONOSUPPORT"         
    | Unix.ESOCKTNOSUPPORT -> "ESOCKTNOSUPPORT"         
    | Unix.EOPNOTSUPP -> "EOPNOTSUPP"      
    | Unix.EPFNOSUPPORT -> "EPFNOSUPPORT"    
    | Unix.EAFNOSUPPORT -> "EAFNOSUPPORT"    
    | Unix.EADDRINUSE -> "EADDRINUSE"      
    | Unix.EADDRNOTAVAIL -> "EADDRNOTAVAIL"   
    | Unix.ENETDOWN -> "ENETDOWN"        
    | Unix.ENETUNREACH -> "ENETUNREACH"     
    | Unix.ENETRESET -> "ENETRESET"       
    | Unix.ECONNABORTED -> "ECONNABORTED"    
    | Unix.ECONNRESET -> "ECONNRESET"      
    | Unix.ENOBUFS -> "ENOBUFS"         
    | Unix.EISCONN -> "EISCONN"         
    | Unix.ENOTCONN -> "ENOTCONN"        
    | Unix.ESHUTDOWN -> "ESHUTDOWN"       
    | Unix.ETOOMANYREFS -> "ETOOMANYREFS"    
    | Unix.ETIMEDOUT -> "ETIMEDOUT"       
    | Unix.ECONNREFUSED -> "ECONNREFUSED"    
    | Unix.EHOSTDOWN -> "EHOSTDOWN"       
    | Unix.EHOSTUNREACH -> "EHOSTUNREACH"    
    | Unix.ELOOP -> "ELOOP"   
    | Unix.EOVERFLOW -> "EOVERFLOW"       
    | Unix.EUNKNOWNERR n -> "EUNKNOWNERR " ^ string_of_int n


let string_literal s =
  "\"" ^ String.escaped s ^ "\""

let string_of_unix_error e =
  match e with
    | Unix.Unix_error(code, fname, arg) ->
	"Unix.Unix_error(" ^ 
	  string_of_unix_code code ^ ", " ^ 
	  string_literal fname ^ ", " ^ 
	  string_literal arg ^ ")"
    | _ ->
	assert false


let () =
  register_printer
    (Unix.Unix_error(Unix.ENOENT,"",""))
    string_of_unix_error

     
