TOP_DIR=../..

include ./Makefile.gtk
# Sets GTK_PKG, GTK_VERSION, ARCHIVE, FL_NAME

PKGNAME = $(FL_NAME)
OBJECTS = uq_gtk.cmo
OCAMLDOC = uq_gtk.mli

include $(TOP_DIR)/Makefile.rules

REQUIRES += camlp4.macro $(GTK_PKG)
GTK_DEFINES += -ppopt -D$(GTK_VERSION)

OCAMLC_OPTIONS = -I ../equeue -syntax camlp4o $(GTK_DEFINES)
OCAMLOPT_OPTIONS = -I ../equeue -syntax camlp4o $(GTK_DEFINES)

ocamldoc.dump: $(OCAMLDOC)
	ocamldoc -dump ocamldoc.dump -stars -I ../equeue $(OCAMLDOC)

include depend
