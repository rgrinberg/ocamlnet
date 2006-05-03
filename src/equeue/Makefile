TOP_DIR=../..

PKGNAME = equeue
OBJECTS = equeue.cmo unixqueue.cmo uq_engines.cmo uq_socks5.cmo
OCAMLDOC = equeue.mli unixqueue.mli uq_engines.mli uq_socks5.mli

ALL_EXTRA = unixqueue_mt.cmo
OPT_EXTRA = unixqueue_mt.cmx

include $(TOP_DIR)/Makefile.rules


ocamldoc.dump: $(OCAMLDOC)
	ocamldoc -dump ocamldoc.dump -stars $(OCAMLDOC)

include depend
