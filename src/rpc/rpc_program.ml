(* $Id$
 * ----------------------------------------------------------------------
 *
 *)

(* The concept of RPC "programs" *)


open Rtypes
open Xdr


module StringMap = Map.Make(String);;

type t =
    { id_obj : < >;
      prog_nr : uint4;
      vers_nr : uint4;
      spec_ts : xdr_type_system;
      spec_procs :
	(uint4 * xdr_type_term * xdr_type_term) StringMap.t;
      spec_procs_validated :
	(uint4 * xdr_type * xdr_type) StringMap.t ref
    }

let create prognr versnr ts procs =
  { id_obj = (object end);
    prog_nr = prognr;
    vers_nr = versnr;
    spec_ts = ts;
    spec_procs = List.fold_left
		   (fun set (name,proc) -> StringMap.add name proc set)
		   StringMap.empty
		   procs;
    spec_procs_validated = ref StringMap.empty
  }

let id p =
  Oo.id p.id_obj

let update ?program_number ?version_number p =
  { p with
      prog_nr = ( match program_number with Some x -> x | None -> p.prog_nr );
      vers_nr = ( match version_number with Some x -> x | None -> p.vers_nr );
  }


let program_number p = p.prog_nr

let version_number p = p.vers_nr

let mutex = !Netsys_oothr.provider # create_mutex()

let signature p procname =
  Netsys_oothr.serialize
    mutex
    (fun () ->
       try
	 StringMap.find procname !(p.spec_procs_validated)
       with
	   Not_found ->
	     let (m,s,t) = StringMap.find procname p.spec_procs in
	     let s_type = expanded_xdr_type p.spec_ts s in
	     let t_type = expanded_xdr_type p.spec_ts t in
	     p.spec_procs_validated :=
               StringMap.add 
		 procname (m,s_type,t_type) !(p.spec_procs_validated);
	     (m,s_type, t_type)
    )
    ()

let procedure_number p procname =
  let (m,s,t) = StringMap.find procname p.spec_procs in
  m
