TOP_DIR=..

PKGNAME  = shell
OBJECTS  = unix_exts.cmo shell_sys.cmo shell.cmo shell_uq.cmo 
COBJECTS = unix_exts_c.o
OCAMLDOC = unix_exts.mli shell_sys.mli shell.mli shell_uq.mli

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../equeue

ocamldoc.dump: $(OCAMLDOC)
        ocamldoc -I ../equeue -v -stars -dump ocamldoc.dump $(OCAMLDOC)

clean::
        rm -f ocamldoc.dump


include depend
