TOP_DIR=../..

PKGNAME = equeue
OBJECTS = equeue.cmo unixqueue_util.cmo unixqueue_pollset.cmo \
          unixqueue_select.cmo unixqueue.cmo \
          uq_engines.cmo uq_socks5.cmo
DOBJECTS = equeue.mli unixqueue.mli unixqueue2.mli \
           uq_engines.mli uq_socks5.mli \
           unixqueue_mt.mli equeue_intro.txt

INCLUDES += $(INC_NETSYS)

include $(TOP_DIR)/Makefile.rules

include depend
