TOP_DIR=..

PKGNAME  = rpc-ssl
ARCHIVE  = rpc_ssl
OBJECTS  = rpc_ssl.cmo
OCAMLDOC = rpc_ssl.mli

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../rpc -I ../equeue -I ../equeue-ssl

ocamldoc.dump: $(OCAMLDOC)
        ocamldoc $(INCLUDES) -v -stars -dump ocamldoc.dump $(OCAMLDOC)

include depend
