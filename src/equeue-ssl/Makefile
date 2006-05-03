TOP_DIR=..

PKGNAME  = equeue-ssl
ARCHIVE  = equeue_ssl.cma
XARCHIVE = equeue_ssl.cmxa
OBJECTS  = ssl_exts.cmo uq_ssl.cmo
OCAMLDOC = uq_ssl.mli
COBJECTS = ssl_exts_stubs.o

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../equeue

ocamldoc.dump: $(OCAMLDOC)
        ocamldoc -I ../equeue -v -stars -dump ocamldoc.dump $(OCAMLDOC)

include depend
