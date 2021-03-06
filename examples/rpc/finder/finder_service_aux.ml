(************************************************************
 * WARNING!
 *
 * This file is generated by ocamlrpcgen from the source file
 * finder_service.x
 *
 ************************************************************)
type longstring = 
      string
and location_enum = 
      Rtypes.int4
and location = 
      [ | `not_found | `found of (longstring) ]
and t_Finder'V1'ping'arg = 
      unit
and t_Finder'V1'ping'res = 
      unit
and t_Finder'V1'find'arg = 
      longstring
and t_Finder'V1'find'res = 
      location
and t_Finder'V1'lastquery'arg = 
      unit
and t_Finder'V1'lastquery'res = 
      longstring
and t_Finder'V1'shutdown'arg = 
      unit
and t_Finder'V1'shutdown'res = 
      unit
;;
let not_found = (Rtypes.mk_int4('\000','\000','\000','\000'));;
let found = (Rtypes.mk_int4('\000','\000','\000','\001'));;
let rec _to_longstring (x:Xdr.xdr_value) : longstring =
  (Xdr.dest_xv_string x)
and _of_longstring (x:longstring) : Xdr.xdr_value = (Xdr.XV_string x)
and _to_location_enum (x:Xdr.xdr_value) : location_enum =
  (match Xdr.dest_xv_enum_fast x with
  | 0 -> (Rtypes.mk_int4('\000','\000','\000','\000'))
  | 1 -> (Rtypes.mk_int4('\000','\000','\000','\001'))
  | _ -> assert false
  )
and _of_location_enum (x:location_enum) : Xdr.xdr_value =
  (match Rtypes.dest_int4 x with
  | ('\000','\000','\000','\000') -> Xdr.XV_enum_fast 0
  | ('\000','\000','\000','\001') -> Xdr.XV_enum_fast 1
  | _ -> failwith "RPC/XDR error: invalid enum value for type `location_enum'"
  )
and _to_location (x:Xdr.xdr_value) : location =
  (match Xdr.dest_xv_union_over_enum_fast x with
  | (0, x) -> `not_found 
  | (1, x) -> `found (_to_longstring x)
  | _ -> assert false
  :> [ | `not_found | `found of _ ]
  )
and _of_location (x:location) : Xdr.xdr_value =
  (match x with
  | `not_found -> Xdr.XV_union_over_enum_fast(0, Xdr.XV_void)
  | `found x -> Xdr.XV_union_over_enum_fast(1, (_of_longstring x)))
and _to_Finder'V1'ping'arg (x:Xdr.xdr_value) : t_Finder'V1'ping'arg = ()
and _of_Finder'V1'ping'arg (x:t_Finder'V1'ping'arg) : Xdr.xdr_value =
  Xdr.XV_void
and _to_Finder'V1'ping'res (x:Xdr.xdr_value) : t_Finder'V1'ping'res = ()
and _of_Finder'V1'ping'res (x:t_Finder'V1'ping'res) : Xdr.xdr_value =
  Xdr.XV_void
and _to_Finder'V1'find'arg (x:Xdr.xdr_value) : t_Finder'V1'find'arg =
  (_to_longstring x)
and _of_Finder'V1'find'arg (x:t_Finder'V1'find'arg) : Xdr.xdr_value =
  (_of_longstring x)
and _to_Finder'V1'find'res (x:Xdr.xdr_value) : t_Finder'V1'find'res =
  (_to_location x)
and _of_Finder'V1'find'res (x:t_Finder'V1'find'res) : Xdr.xdr_value =
  (_of_location x)
and _to_Finder'V1'lastquery'arg (x:Xdr.xdr_value) : t_Finder'V1'lastquery'arg =
  ()
and _of_Finder'V1'lastquery'arg (x:t_Finder'V1'lastquery'arg) : Xdr.xdr_value =
  Xdr.XV_void
and _to_Finder'V1'lastquery'res (x:Xdr.xdr_value) : t_Finder'V1'lastquery'res =
  (_to_longstring x)
and _of_Finder'V1'lastquery'res (x:t_Finder'V1'lastquery'res) : Xdr.xdr_value =
  (_of_longstring x)
and _to_Finder'V1'shutdown'arg (x:Xdr.xdr_value) : t_Finder'V1'shutdown'arg =
  ()
and _of_Finder'V1'shutdown'arg (x:t_Finder'V1'shutdown'arg) : Xdr.xdr_value =
  Xdr.XV_void
and _to_Finder'V1'shutdown'res (x:Xdr.xdr_value) : t_Finder'V1'shutdown'res =
  ()
and _of_Finder'V1'shutdown'res (x:t_Finder'V1'shutdown'res) : Xdr.xdr_value =
  Xdr.XV_void
;;
let xdrt_longstring = Xdr.X_rec("longstring", Xdr.x_string_max)
;;
let xdrt_location_enum =
  Xdr.X_rec("location_enum",
    Xdr.X_enum
      [
        ("NOT_FOUND", (Rtypes.mk_int4('\000','\000','\000','\000')));
        ("FOUND", (Rtypes.mk_int4('\000','\000','\000','\001')));
      ])
;;
let xdrt_location =
  Xdr.X_rec("location",
    Xdr.X_union_over_enum(
      (Xdr.X_enum
         [
           ("NOT_FOUND", (Rtypes.mk_int4('\000','\000','\000','\000')));
           ("FOUND", (Rtypes.mk_int4('\000','\000','\000','\001')));
         ]),
      [
        "NOT_FOUND", (Xdr.X_void);
        "FOUND", (xdrt_longstring);
      ],
      None))
;;
let xdrt_Finder'V1'ping'arg = Xdr.X_void
;;
let xdrt_Finder'V1'ping'res = Xdr.X_void
;;
let xdrt_Finder'V1'find'arg = xdrt_longstring
;;
let xdrt_Finder'V1'find'res = xdrt_location
;;
let xdrt_Finder'V1'lastquery'arg = Xdr.X_void
;;
let xdrt_Finder'V1'lastquery'res = xdrt_longstring
;;
let xdrt_Finder'V1'shutdown'arg = Xdr.X_void
;;
let xdrt_Finder'V1'shutdown'res = Xdr.X_void
;;
let program_Finder'V1 =
  Rpc_program.create
    (Rtypes.mk_uint4('\000','\003','\013','\064'))
    (Rtypes.mk_uint4('\000','\000','\000','\001'))
    (Xdr.validate_xdr_type_system [])
    [
      "ping",
        ((Rtypes.mk_uint4('\000','\000','\000','\000')),
        xdrt_Finder'V1'ping'arg,
        xdrt_Finder'V1'ping'res);
      "find",
        ((Rtypes.mk_uint4('\000','\000','\000','\001')),
        xdrt_Finder'V1'find'arg,
        xdrt_Finder'V1'find'res);
      "lastquery",
        ((Rtypes.mk_uint4('\000','\000','\000','\002')),
        xdrt_Finder'V1'lastquery'arg,
        xdrt_Finder'V1'lastquery'res);
      "shutdown",
        ((Rtypes.mk_uint4('\000','\000','\000','\003')),
        xdrt_Finder'V1'shutdown'arg,
        xdrt_Finder'V1'shutdown'res);
    ]
;;

