TOP_DIR=../..

PKGNAME  = equeue-ssl
ARCHIVE  = equeue_ssl
OBJECTS  = ssl_exts.cmo uq_ssl.cmo
DOBJECTS = uq_ssl.mli
COBJECTS = ssl_exts_stubs.o

INCLUDES += -I ../equeue

include $(TOP_DIR)/Makefile.rules

REQUIRES += unix ssl

include depend
