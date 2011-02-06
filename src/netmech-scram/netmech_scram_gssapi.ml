(* $Id$ *)

open Netgssapi



class scram_name (name_string:string) (name_type:oid) =
object
  method otype = `Name
  method name_string = name_string
  method name_type = name_type
end


(* FIXME: several names possible (one name per mech) *)
class scram_cred (name:name) (cred_usage:cred_usage) =
object
  method otype = `Credential
  method name = name
  method cred_usage = cred_usage
end


class scram_context XXX =
object
  method otype = `Context
  method valid = XXX
end



module type WHT_Hashed = sig
  type t_repr
  type t_full
  val coerce : t_full -> t_repr
end


module WHT(T:WHT_Hashed) = struct
  module E = struct
    type t = (T.t_repr * T.t_full option)
    let equal ((x,_):t) ((y,_):t) = (x=y)
    let hash ((x,_):t) = Hashtbl.hash x
  end

  module W = Weak.Make(T)

  let create() =
    W.create 10

  let store table (x : T.t_full) =
    let x_repr = T.coerce x in
    ignore(W.merge table (x_repr,Some x))

  let find table (x_repr : T.t_repr) : T.t_full =
    try
      let (_,x_opt) = W.find table (x,None) in
      match x_opt with
	| None -> assert false
	| Some x -> x
    with
      | Not_found ->
	  invalid_arg "Netmech_scram_gssapi: Unknown opaque object"
end

module Credential = struct
  type t_repr = credential
  type t_full = scram_cred
  let coerce (x:t_full) = (x :> t_repr)
end

module CredentialWHT = WHT(Credential)

module Name = struct
  type t_repr = name
  type t_full = scram_name
  let coerce (x:t_full) = (x :> t_repr)
end

module NameWHT = WHT(Name)

module Context = struct
  type t_repr = context
  type t_full = scram_context
  let coerce (x:t_full) = (x :> t_repr)
end


module ContextWHT = WHT(Context)


class scram_gss_api () : gss_api =
  let credentials = CredentialWHT.create() in
  let names = NameWHT.create() in
  let contexts = ContextWHT.create() in
  let no_cred = ( object method otype = `Credential end ) in
object(self)
  method provider = "Netmech_scram_gssapi.scap_gss_api"

  method accept_sec_context
           ~context ~acceptor_cred ~input_token ~chan_bindings ~out () =
    XXX

  method acquire_cred 
            ~desired_name ~time_req ~desired_mechs ~cred_usage ~out () =
    XXX

  method add_cred
           ~input_cred ~desired_name ~desired_mech ~cred_usage 
	   ~initiator_time_req ~acceptor_time_req ~out () =
    XXX

  method canonicalize_name
           ~input_name ~mech_type ~out () =
    XXX

  method compare_name
           ~name1 ~name2 ~out () =
    XXX

  method context_time
           ~context ~out () = 
    XXX

  method display_name
           ~input_name ~out () =
    XXX

  method display_status
           ~status ~mech_type ~out () =
    XXX

  method duplicate_name ~src_name ~out () =
    XXX

  method export_name ~name ~out () =
    XXX

  method export_sec_context ~context ~out () =
    XXX

  method get_mic ~context ~qop_req ~message ~out () =
    XXX

  method import_name ~input_name ~input_name_type ~out () =
    XXX

  method import_sec_context ~interprocess_token ~out () =
    XXX

  method indicate_mechs ~out () = 
    XXX

  method init_sec_context 
          ~initiator_cred ~context ~target_name ~mech_type ~req_flags
          ~time_rec ~chan_bindings ~input_token ~out () =
    XXX

  method inquire_context
           ~context ~out () = 
    XXX

  method inquire_cred ~cred ~out () =
    XXX

  method inquire_cred_by_mech ~cred ~mech_type ~out () = 
    XXX

  method inquire_names_for_mech ~mechanism ~out () = 
    XXX

  method procesS_context_token ~context ~token ~out () =
    XXX

  method unwrap ~context ~input_message ~output_message_preferred_type ~out () =
    XXX

  method verify_mic ~context ~message ~token ~out () =
    XXX

  method wrap
          ~context ~conf_req ~qop_req ~input_message 
	  ~output_message_preferred_type ~out () =
    XXX

  method wrap_size_limit
           ~context ~conf_req ~qop_req ~req_output ~out () =
    XXX
    
end
