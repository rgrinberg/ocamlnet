TOP_DIR=..

PKGNAME  = rpc-auth-dh
ARCHIVE  = rpc_auth_dh
OBJECTS  = rpc_time.cmo \
           rpc_key_aux.cmo rpc_key_clnt.cmo rpc_key_service.cmo rpc_auth_dh.cmo
OCAMLDOC = 

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../rpc
RPCGEN = ../rpc-generator/ocamlrpcgen

ocamldoc.dump: $(OCAMLDOC)
        ocamldoc $(INCLUDES) -v -stars -dump ocamldoc.dump $(OCAMLDOC)

rpc_key_aux.ml: rpc_key.x
	$(RPCGEN) -int int32 -hyper int64 -aux rpc_key.x

rpc_key_clnt.ml: rpc_key.x
	$(RPCGEN) -int int32 -hyper int64 -clnt rpc_key.x

include depend
