TOP_DIR = ../..
include $(TOP_DIR)/Makefile.rules
include $(TOP_DIR)/Makefile.xrules

XDRFILES = queues.x

RPCGEN_AUX_OPTIONS    = -int int32 -hyper int64
RPCGEN_CLIENT_OPTIONS = -int int32 -hyper int64
RPCGEN_SERVER_OPTIONS = -int int32 -hyper int64

all:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code all

opt:
	$(MAKE) generate
	$(MAKE) depend
	$(MAKE) -f Makefile.code opt

generate: $(XDRFILES:.x=.astamp) $(XDRFILES:.x=.cstamp) $(XDRFILES:.x=.s2stamp)

depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli >depend

clean:
	rm -f $(CLEAN_LIST) qclient qserver

CLEAN: clean

distclean: clean
	rm -f META

#install:
#	files=`$(COLLECT_FILES) *.mli *.cmi *.cma *.cmxa *.a META` && \
#	$(OCAMLFIND) install rpc $$files

#uninstall:
#	$(OCAMLFIND) remove rpc
