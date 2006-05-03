TOP_DIR=..

PKGNAME  = rpc-xti
ARCHIVE  = rpc_xti
OBJECTS  = rpc_xti_client.cmo
COBJECTS = rpcxti.o
OCAMLDOC = 

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../rpc

ocamldoc.dump: $(OCAMLDOC)
        ocamldoc $(INCLUDES) -v -stars -dump ocamldoc.dump $(OCAMLDOC)

include depend
