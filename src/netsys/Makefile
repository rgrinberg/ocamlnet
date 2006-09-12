TOP_DIR=../..

include Makefile.conf

PKGNAME  = netsys
OBJECTS  = netsys.cmo
COBJECTS = netsys_c.o
DOBJECTS = netsys.mli
LINK_OPTIONS = $(NETSYS_LINK_OPTIONS)

include $(TOP_DIR)/Makefile.rules

clean::
	rm -f configtests/*.o configtests/*.err
	rm -f configtests/posix_shm

include depend
