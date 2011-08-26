(* $Id$ *)

open Netdns_message

type name_cache_entry =
    { nc_name : string;
        (** The name *)
      nc_aliases : string list option;  (* CHECK: option *)
        (** If [None] nothing is known about aliases. If [Some l], the list
          * [l] are the known alias names.
         *)
      nc_addr_list : Unix.inet_addr list option;
        (** If [None] nothing is known about addresses. If [Some l], the
          * list [l] are all addresses.
         *)
      nc_aa : bool;
        (** Whether the entry is authoritative *)
      nc_expires : float;
        (** When the cache entry expires *)
    }

(* If only alias data are known, nc_aliases is Some _, and ns_addr_list is
 * None.
 *
 * If it is known that the host does not exist, set ns_addr_list = Some [].
 *)


type nameserver_cache_entry =
    { nsc_name : string;
        (** The domain name for which the name servers are competent *)
      nsc_servers : name_cache_entry list;
        (** The list of name servers *)
      nsc_aa : bool;
        (** Whether the entry is authoritative *)
      nsc_expires : float;
        (** When the cache entry expires *)
    }


class type dns_cache =
object

  method lookup_name : aa:bool -> string -> name_cache_entry
    (** Creates a [name_cache_entry] from the information in the cache, or
      * raises [Not_found]. The string is the name to look for.
      * If [aa] is set, only authoritative answers are acceptable.
      * The cache does not check whether the entry is already expired.
     *)
  
  method lookup_addr : aa:bool -> Unix.inet_addr -> name_cache_entry
    (** Creates a [name_cache_entry] from the information in the cache, or
      * raises [Not_found]. The inet_addr is the address to look for.
      * If [aa] is set, only authoritative answers are acceptable.
      * The cache does not check whether the entry is already expired.
     *)

  method lookup_nameserver : aa:bool -> string -> nameserver_cache_entry
    (** Creates a [nameserver_cache_entry] from the information in the cache,
      * or raises [Not_found]. The string is the domain to look for.
      * If [aa] is set, only authoritative answers are acceptable.
      * The cache does not check whether the entry is already expired.
     *) 

  method add : msg -> unit
    (** [add msg]: Adds the message [msg] to the cache. Messages are added
      * even when they are already expired, but other resource records with
      * older expiration dates may be replaced.
     *)

  method remove_expired_entries : unit -> unit
    (** Iterates through the cache and removes all expired entries *)

  method debug_string : string
    (* Testing *)

end


type 'a result = ('a option -> unit)

class type async_resolver =
object

  method host_query : string -> Unix.host_entry result -> unit
    
  method addr_query : Unix.inet_addr -> Unix.host_entry result -> unit

(*
  method mx_query : string -> Unix.host_entry list result -> unit
 *)

  method shutdown : unit -> unit

end


val in_memory_cache : ?max_ttl:float -> unit -> dns_cache
  (** [max_ttl]: Limits the "time to live" for the cache entries. Defaults to
    * 86400 (one day).
   *)

val stub_resolver : 
  ?cache:dns_cache ->
  Unix.inet_addr list -> Unixqueue.event_system -> async_resolver


class type async_resolver_queue =
object
  inherit async_resolver
  method limit : int
  method queue_length : int
end


val queue : 
  limit:int ->
  async_resolver -> async_resolver_queue


val debug : bool ref
