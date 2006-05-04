TOP_DIR=../..

include ./Makefile.gtk
# Sets GTK_PKG, GTK_VERSION, ARCHIVE, FL_NAME

PKGNAME = $(FL_NAME)
OBJECTS = uq_gtk.cmo
DOBJECTS = uq_gtk.mli

REQUIRES += camlp4.macro $(GTK_PKG)
GTK_DEFINES += -ppopt -D$(GTK_VERSION)

INCLUDES += -I ../equeue

OCAMLC_OPTIONS = -syntax camlp4o $(GTK_DEFINES)
OCAMLOPT_OPTIONS = -syntax camlp4o $(GTK_DEFINES)

include $(TOP_DIR)/Makefile.rules

include depend
