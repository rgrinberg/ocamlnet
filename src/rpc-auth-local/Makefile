TOP_DIR=..

PKGNAME  = rpc-auth-local
ARCHIVE  = rpc_auth_local
OBJECTS  = rpc_auth_local.cmo
COBJECTS = rpclocal.o
OCAMLDOC = 

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../rpc

ocamldoc.dump: $(OCAMLDOC)
        ocamldoc $(INCLUDES) -v -stars -dump ocamldoc.dump $(OCAMLDOC)

include depend
