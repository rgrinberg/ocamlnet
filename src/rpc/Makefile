TOP_DIR=../..

PKGNAME  = rpc
OBJECTS  = rtypes.cmo xdr_mstring.cmo xdr.cmo rpc.cmo rpc_common.cmo \
           rpc_program.cmo rpc_util.cmo rpc_portmapper_aux.cmo \
           rpc_packer.cmo rpc_transport.cmo rpc_client.cmo \
           rpc_simple_client.cmo rpc_portmapper_clnt.cmo rpc_portmapper.cmo \
           rpc_server.cmo rpc_auth_sys.cmo \
	   rpc_proxy.cmo
DOBJECTS = rtypes.mli xdr_mstring.mli xdr.mli rpc.mli \
           rpc_program.mli rpc_util.mli rpc_portmapper_aux.mli \
           rpc_packer.mli rpc_transport.mli rpc_client.mli \
           rpc_simple_client.mli rpc_portmapper_clnt.mli rpc_portmapper.mli \
           rpc_server.mli rpc_auth_sys.mli \
	   rpc_proxy.mli \
	   rpc_intro.txt rpc_mapping_ref.txt

# rpc_packer.ml is full of incomplete matchings, but this is ok:
OCAMLC_OPTIONS_FOR_rpc_packer.ml = -w p
OCAMLOPT_OPTIONS_FOR_rpc_packer.ml = -w p

# rpc_portmapper.ml is full of incomplete matchings, but this is ok:
OCAMLC_OPTIONS_FOR_rpc_portmapper.ml = -w p
OCAMLOPT_OPTIONS_FOR_rpc_portmapper.ml = -w p

# rtypes has some conditional stuff:
RTYPES_DEFS = -ppopt -DWORDSIZE_$(WORD_SIZE) -ppopt -DUSE_NETSYS_XDR
OCAMLC_OPTIONS_FOR_rtypes.ml = \
	-syntax camlp4o -package camlp4.macro $(RTYPES_DEFS)
OCAMLOPT_OPTIONS_FOR_rtypes.ml = \
	-inline 10 \
	-syntax camlp4o -package camlp4.macro $(RTYPES_DEFS)

OCAMLOPT_OPTIONS_FOR_xdr.ml = -inline 5

OCAMLDEP_OPTIONS = -syntax camlp4o -package camlp4.macro

INCLUDES += $(INC_EQUEUE) $(INC_NETSYS) $(INC_NETSTRING)

include $(TOP_DIR)/Makefile.rules

include depend
