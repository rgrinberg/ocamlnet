TOP_DIR=../..

include ./Makefile.gtk
# Sets GTK_PKG, GTK_VERSION, ARCHIVE, FL_NAME

PKGNAME = $(FL_NAME)
OBJECTS = uq_gtk.cmo
DOBJECTS = uq_gtk.mli

INCLUDES += $(INC_EQUEUE)

PP_OPTIONS = -syntax camlp4o -package camlp4.macro $(GTK_DEFINES)

include $(TOP_DIR)/Makefile.rules

REQUIRES += $(GTK_PKG)
GTK_DEFINES += -ppopt -D$(GTK_VERSION)

include depend
