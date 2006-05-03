TOP_DIR=..

PKGNAME  = equeue-tcl
ARCHIVE  = equeue_tcl
XARCHIVE = equeue_tcl
OBJECTS  = uq_tcl.cmo
OCAMLDOC = uq_tcl.mli
COBJECTS = tclqueue.o

LINK_OPTIONS = $(EQUEUE_TCL_LIBS)
CC_OPTIONS = $(EQUEUE_TCL_DEFS)

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../equeue

ocamldoc.dump: $(OCAMLDOC)
        ocamldoc -I ../equeue -v -stars -dump ocamldoc.dump $(OCAMLDOC)

include depend
