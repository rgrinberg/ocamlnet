TOP_DIR=../..

PKGNAME = equeue
OBJECTS = equeue.cmo unixqueue.cmo uq_engines.cmo uq_socks5.cmo
DOBJECTS = equeue.mli unixqueue.mli uq_engines.mli uq_socks5.mli \
           unixqueue_mt.mli equeue_intro.txt

ALLMT_EXTRA = unixqueue_mt.cmo
OPTMT_EXTRA = unixqueue_mt.cmx

OCAMLC_OPTIONS_FOR_unixqueue_mt.ml = $(THREAD)
OCAMLOPT_OPTIONS_FOR_unixqueue_mt.ml = $(THREAD)

INCLUDES += $(INC_NETSYS)

include $(TOP_DIR)/Makefile.rules

include depend
