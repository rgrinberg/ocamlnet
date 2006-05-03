TOP_DIR=..

PKGNAME  = pop
OBJECTS  = netpop.cmo
OCAMLDOC = netpop.mli

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../netstring
INSTALL_EXTRA += LICENSE

ocamldoc.dump: $(OCAMLDOC)
	ocamldoc `ocamlfind query -i-format pcre` -I ../netstring -v -stars -dump ocamldoc.dump $(OCAMLDOC)

clean::
	rm -f ocamldoc.dump


include depend
