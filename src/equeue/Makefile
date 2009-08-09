TOP_DIR=../..

PKGNAME = equeue
OBJECTS = equeue.cmo unixqueue_util.cmo unixqueue_pollset.cmo \
          unixqueue_select.cmo unixqueue.cmo \
	  uq_resolver.cmo \
          uq_engines.cmo uq_socks5.cmo
DOBJECTS = equeue.mli unixqueue.mli unixqueue_pollset.mli \
           unixqueue_select.mli \
	   uq_resolver.mli \
           uq_engines.mli uq_socks5.mli \
           equeue_intro.txt

INCLUDES += $(INC_NETSYS)

include $(TOP_DIR)/Makefile.rules

include depend
