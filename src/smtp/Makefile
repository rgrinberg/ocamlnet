TOP_DIR=..

PKGNAME  = smtp
OBJECTS  = netsmtp.cmo
OCAMLDOC = netsmtp.mli

include $(TOP_DIR)/Makefile.rules

INCLUDES += -I ../netstring

ocamldoc.dump: $(OCAMLDOC)
	ocamldoc `ocamlfind query -i-format pcre` -I ../netstring -v -stars -dump ocamldoc.dump $(OCAMLDOC)

include depend
